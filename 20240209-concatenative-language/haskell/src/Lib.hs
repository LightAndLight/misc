{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Applicative ((<|>))
import Control.Comonad (extract)
import Control.Lens.Getter ((^.))
import Data.Bifunctor (bimap, first)
import Data.Either (partitionEithers)
import Data.Foldable (fold, foldl', foldlM, toList, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int64)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token hiding (brackets, parens)
import qualified Text.Parser.Token
import Text.Trifecta (DeltaParsing, Result (..), Span, Spanned (..), parseString, span, spanned, _errDoc)
import Prelude hiding (Word, span, words)

data Word a
  = Word String [Spanned a]
  deriving (Show)

parens :: String -> String
parens x = "(" <> x <> ")"

printWord :: (a -> String) -> Word a -> String
printWord printArg (Word name args)
  | null args = name
  | otherwise = name <> parens (intercalate ", " (printArg . extract <$> args))

newtype Program = Program (NonEmpty (Spanned (Word (Either Type (Spanned Program)))))
  deriving (Show)

printProgram :: Program -> String
printProgram (Program words) = unwords . toList $ printWord (either printType (printProgram . extract)) . extract <$> words

data TypeArrow = TypeArrow Type Type
  deriving (Show)

brackets :: String -> String
brackets x = "[" <> x <> "]"

printTypeArrow :: TypeArrow -> String
printTypeArrow (TypeArrow input output) =
  brackets (printType input)
    <> " -> "
    <> brackets (printType output)

data Type
  = Context [Type]
  | Var String
  | Int64
  | Product Type Type
  | Sum Type Type
  deriving (Eq, Show)

printType :: Type -> String
printType (Var a) = a
printType (Context ts) = brackets . intercalate ", " $ printType <$> ts
printType Int64 = "Int64"
printType (Product a b) =
  ( case a of
      Sum{} -> parens
      _ -> id
  )
    (printType a)
    <> " * "
    <> ( case b of
          Product{} -> parens
          Sum{} -> parens
          _ -> id
       )
      (printType b)
printType (Sum a b) =
  printType a
    <> " + "
    <> ( case b of
          Sum{} -> parens
          _ -> id
       )
      (printType b)

data ImportItem = All | Name String
  deriving (Show)

printImportItem :: ImportItem -> String
printImportItem All = ".."
printImportItem (Name n) = n

data Decl
  = Def String [(String, Either Kind TypeArrow)] TypeArrow (Spanned Program)
  | Import [Word ImportItem]
  deriving (Show)

printDecl :: Decl -> String
printDecl (Def name args ty value) =
  "def "
    <> name
    <> (if null args then mempty else parens (intercalate ", " $ (\(arg, argTy) -> arg <> " : " <> either printKind printTypeArrow argTy) <$> args))
    <> " : "
    <> printTypeArrow ty
    <> " = "
    <> printProgram (extract value)
printDecl (Import imports) =
  "import" <> foldMap (\word -> " " <> printWord printImportItem word) imports

newtype Module = Module [Decl]
  deriving (Show)

printModule :: Module -> String
printModule (Module defs) = intercalate "\n\n" $ printDecl <$> defs

data Kind
  = Type
  | Ctx
  deriving (Eq, Show)

printKind :: Kind -> String
printKind Type = "Type"
printKind Ctx = "Ctx"

data WordSignature = WordSignature [Either (String, Kind) TypeArrow] TypeArrow

data Side = Input | Output
  deriving (Show)

data ArgSort = ArgValue | ArgType
  deriving (Show)

data TypeError
  = NotFound String
  | ArgCountMismatch String Int Int
  | StackSizeMismatch Span Side Int Int
  | TypeMismatch Type Type
  | ArgMismatch ArgSort ArgSort
  | KindMismatch Kind Kind
  deriving (Show)

checkType :: (String -> Maybe Kind) -> Type -> Kind -> Either TypeError ()
checkType kinds (Var v) ki =
  case kinds v of
    Nothing ->
      Left $ NotFound v
    Just ki' ->
      if ki == ki' then pure () else Left $ KindMismatch ki ki'
checkType kinds (Context ts) ki = do
  traverse_ (\t -> checkType kinds t Type) ts
  if ki == Ctx
    then pure ()
    else Left $ KindMismatch Type ki
checkType _ Int64 ki =
  if ki == Type
    then pure ()
    else Left $ KindMismatch Type ki
checkType kinds (Product a b) ki = do
  checkType kinds a Type
  checkType kinds b Type
  if ki == Type
    then pure ()
    else Left $ KindMismatch Type ki
checkType kinds (Sum a b) ki = do
  checkType kinds a Type
  checkType kinds b Type
  if ki == Type
    then pure ()
    else Left $ KindMismatch Type ki

appendContexts :: Type -> Type -> Type
appendContexts (Context ts) (Context ts') = Context (ts <> ts')

substTy :: HashMap String Type -> Type -> Type
substTy subst ty@(Var x) =
  fromMaybe ty $ HashMap.lookup x subst
substTy subst (Context ts) =
  Context $ substTy subst <$> ts
substTy _ Int64 = Int64
substTy subst (Product a b) =
  Product (substTy subst a) (substTy subst b)
substTy subst (Sum a b) =
  Sum (substTy subst a) (substTy subst b)

substTypeArrow :: HashMap String Type -> TypeArrow -> TypeArrow
substTypeArrow subst (TypeArrow a b) = TypeArrow (substTy subst a) (substTy subst b)

checkWordInputs ::
  (String -> Maybe WordSignature) ->
  (String -> Maybe Kind) ->
  Spanned (Word (Either Type (Spanned Program))) ->
  Type ->
  Either TypeError Type
checkWordInputs context kinds word expectedInputTy = do
  checkType kinds expectedInputTy Ctx

  let Word name args = extract word
  case context name of
    Just (WordSignature argTys (TypeArrow actualInputTy actualOutputTy)) -> do
      let expectedArgCount = length argTys
      let actualArgCount = length args
      subst <-
        if actualArgCount /= expectedArgCount
          then Left $ ArgCountMismatch name expectedArgCount actualArgCount
          else
            foldlM
              ( \subst (arg, argTy) -> do
                  case (extract arg, argTy) of
                    (Left tyArg, Left (n, tyArgKind)) -> do
                      let tyArg' = substTy subst tyArg
                      checkType kinds tyArg' tyArgKind
                      pure $ HashMap.insert n tyArg' subst
                    (Right valArg, Right valArgTy) -> do
                      checkProgram context kinds valArg (substTypeArrow subst valArgTy)
                      pure subst
                    (Left _, Right _) ->
                      Left $ ArgMismatch ArgValue ArgType
                    (Right _, Left _) ->
                      Left $ ArgMismatch ArgType ArgValue
              )
              mempty
              (zip args argTys)
      prefix <- checkInputTypes (word ^. span) (substTy subst expectedInputTy) (substTy subst actualInputTy)
      pure $ appendContexts prefix (substTy subst actualOutputTy)
    Nothing ->
      case name of
        "#"
          | [Right (Program ((Word input [] :~ _) :| []) :~ _) :~ _] <- args
          , Success n <- parseString intParser mempty input ->
              _
        _ ->
          Left $ NotFound name

checkProgram ::
  (String -> Maybe WordSignature) ->
  (String -> Maybe Kind) ->
  Spanned Program ->
  TypeArrow ->
  Either TypeError ()
checkProgram context kinds program ty = do
  let Program words = extract program
  go (program ^. span) words ty
 where
  go s (word :| words) (TypeArrow expectedInputTy expectedOutputTy) = do
    actualOutputTy <- checkWordInputs context kinds word expectedInputTy
    case words of
      word' : words' ->
        go s (word' :| words') (TypeArrow actualOutputTy expectedOutputTy)
      [] ->
        checkOutputTypes s expectedOutputTy actualOutputTy

equateType :: Type -> Type -> Either TypeError ()
equateType expectedTy actualTy =
  if expectedTy == actualTy
    then pure ()
    else Left $ TypeMismatch expectedTy actualTy

checkInputTypes :: Span -> Type -> Type -> Either TypeError Type
checkInputTypes s (Context expectedTys) (Context actualTys) = do
  let expectedTysCount = length expectedTys
  let actualTysCount = length actualTys
  if expectedTysCount >= actualTysCount
    then do
      let (prefix, suffix) = splitAt (expectedTysCount - actualTysCount) expectedTys
      traverse_ (uncurry equateType) $ zip suffix actualTys
      pure $ Context prefix
    else Left $ StackSizeMismatch s Input expectedTysCount actualTysCount
checkInputTypes s a b =
  error $ show s <> " " <> show a <> " " <> show b

checkOutputTypes :: Span -> Type -> Type -> Either TypeError ()
checkOutputTypes s (Context expectedTys) (Context actualTys) = do
  let expectedTysCount = length expectedTys
  let actualTysCount = length actualTys
  if expectedTysCount >= actualTysCount
    then traverse_ (uncurry equateType) $ zip expectedTys actualTys
    else Left $ StackSizeMismatch s Output expectedTysCount actualTysCount

lookupDef :: String -> Module -> Maybe (String, [(String, Either Kind TypeArrow)], TypeArrow, Spanned Program)
lookupDef name (Module defs) = do
  decl <- find (\case Def name' _ _ _ -> name == name'; _ -> False) defs
  case decl of
    Def a b c d -> pure (a, b, c, d)
    _ -> Nothing

lookupSignature :: String -> Module -> Maybe WordSignature
lookupSignature name module_ = do
  (_, args, ty, _) <- lookupDef name module_
  pure $ WordSignature ((\(argName, argTy) -> first ((,) argName) argTy) <$> args) ty

signatures :: Module -> HashMap String WordSignature
signatures (Module decls) =
  foldl'
    ( \acc decl ->
        case decl of
          Def name args ty _ ->
            HashMap.insert name (WordSignature ((\(argName, argTy) -> first ((,) argName) argTy) <$> args) ty) acc
          _ ->
            acc
    )
    mempty
    decls

checkImportArg :: Module -> ImportItem -> Either TypeError (HashMap String WordSignature)
checkImportArg module_ item =
  case item of
    All ->
      pure $ signatures module_
    Name name ->
      case lookupSignature name module_ of
        Nothing -> Left $ NotFound name
        Just signature -> pure $ HashMap.singleton name signature

checkImport :: (String -> Maybe Module) -> Word ImportItem -> Either TypeError (HashMap String WordSignature)
checkImport modules_ (Word name args) = do
  module_ <- case modules_ name of
    Nothing -> Left $ NotFound name
    Just module_ -> pure module_
  fold <$> traverse (checkImportArg module_ . extract) args

checkDecl ::
  (String -> Maybe Module) ->
  (String -> Maybe WordSignature) ->
  (String -> Maybe Kind) ->
  Decl ->
  Either TypeError (HashMap String WordSignature)
checkDecl _ context kinds (Def name args ty value) = do
  let (localKinds, localContext) =
        bimap HashMap.fromList HashMap.fromList
          . partitionEithers
          $ ( \(argName, argTy) ->
                bimap
                  ((,) argName)
                  ((,) argName . WordSignature [])
                  argTy
            )
          <$> args
  checkProgram (\n -> HashMap.lookup n localContext <|> context n) (\n -> HashMap.lookup n localKinds <|> kinds n) value ty
  pure $ HashMap.singleton name (WordSignature ((\(argName, argTy) -> first ((,) argName) argTy) <$> args) ty)
checkDecl modules _ _ (Import imports) =
  fold <$> traverse (checkImport modules) imports

checkModule :: (String -> Maybe Module) -> (String -> Maybe WordSignature) -> Module -> Either TypeError ()
checkModule modules context (Module defs) = go mempty defs
 where
  go :: HashMap String WordSignature -> [Decl] -> Either TypeError ()
  go _ [] = pure ()
  go localContext (def : defs') = do
    newDefs <- checkDecl modules (\name -> HashMap.lookup name localContext <|> context name) (const Nothing) def
    go (newDefs <> localContext) defs'

builtinsCheck :: HashMap String WordSignature
builtinsCheck =
  HashMap.fromList
    [ -- add : [Int64, Int64] -> [Int64]
      ("add", WordSignature [] $ TypeArrow (Context [Int64, Int64]) (Context [Int64]))
    , -- pair : [a, b] -> [a * b]
      ("pair", WordSignature [] $ TypeArrow (Context [Int64, Int64]) (Context [Product Int64 Int64]))
    , -- unpair : [a * b] -> [a, b]
      ("unpair", WordSignature [] $ TypeArrow (Context [Product Int64 Int64]) (Context [Int64, Int64]))
    , -- inl : [a] -> [a + b]
      ("inl", WordSignature [] $ TypeArrow (Context [Int64]) (Context [Sum Int64 Int64]))
    , -- inr : [b] -> [a + b]
      ("inr", WordSignature [] $ TypeArrow (Context [Int64]) (Context [Sum Int64 Int64]))
    , -- forall x y a b. ([..x, a] -> y, [..x, b] -> y) : [..x, a + b] -> y

      ( "unsum"
      , WordSignature [Right $ TypeArrow (Context [Int64]) (Context [Int64]), Right $ TypeArrow (Context [Int64]) (Context [Int64])]
          $ TypeArrow (Context [Sum Int64 Int64]) (Context [Int64])
      )
    , -- par(x : Ctx, a : Type, b : Type, f : x -> [a], g : x -> [b]) : x -> [a, b]

      ( "par"
      , WordSignature
          [ Left ("x", Ctx)
          , Left ("a", Type)
          , Left ("b", Type)
          , Right $ TypeArrow (Var "x") (Context [Var "a"])
          , Right $ TypeArrow (Var "x") (Context [Var "b"])
          ]
          $ TypeArrow (Var "x") (Context [Var "a", Var "b"])
      )
    ]

intParser :: (TokenParsing m, Monad m) => m Int64
intParser = do
  val <- decimal
  if val <= fromIntegral (maxBound :: Int64)
    then pure $ fromIntegral val
    else unexpected $ show val <> " exceeds max bound of Int64"

syntaxesCheck :: String -> Maybe WordSignature
syntaxesCheck input =
  intSyntax
 where
  intSyntax =
    case parseString intParser mempty input of
      Success{} -> Just $ WordSignature [] (TypeArrow (Context []) (Context [Int64]))
      _ -> Nothing

identParser :: (TokenParsing m) => m String
identParser = token $ some alphaNum

wordParser :: (DeltaParsing m) => m a -> m (Word a)
wordParser argParser =
  Word
    <$> identParser
    <*> fmap (fromMaybe []) (optional (Text.Parser.Token.parens (spanned argParser `sepBy` comma)))

typeParser :: (TokenParsing m) => m Type
typeParser = typeSumParser
 where
  typeSumParser = foldl Sum <$> typeProductParser <*> many (symbolic '+' *> typeProductParser)
  typeProductParser = foldl Product <$> typeAtomParser <*> many (symbolic '*' *> typeAtomParser)
  typeAtomParser =
    (Int64 <$ symbol "Int64")
      <|> (Context <$> Text.Parser.Token.brackets (typeParser `sepBy` comma))
      <|> (Var <$> identParser)

typeArrowParser :: (TokenParsing m) => m TypeArrow
typeArrowParser =
  TypeArrow <$> typeParser <* symbol "->" <*> typeParser

programParser :: (DeltaParsing m) => m Program
programParser =
  fmap Program
    $ (:|)
    <$> spanned (wordParser argParser)
    <*> many (spanned $ wordParser argParser)
 where
  argParser = Left <$ symbol "type" <*> typeParser <|> Right <$> spanned programParser

kindParser :: (TokenParsing m) => m Kind
kindParser =
  Type <$ symbol "Type" <|> Ctx <$ symbol "Ctx"

declParser :: (DeltaParsing m) => m Decl
declParser =
  defParser <|> importParser
 where
  defParser =
    Def
      <$ symbol "def"
      <*> identParser
      <*> fmap
        (fromMaybe [])
        ( optional
            . Text.Parser.Token.parens
            $ ( (,)
                  <$> identParser
                  <* symbolic ':'
                  <*> (Left <$> kindParser <|> Right <$> typeArrowParser)
              )
            `sepBy` comma
        )
      <* symbolic ':'
      <*> typeArrowParser
      <* symbolic '='
      <*> spanned programParser

  importParser =
    Import
      <$ symbol "import"
      <*> many (wordParser $ All <$ symbol ".." <|> Name <$> identParser)

moduleParser :: (DeltaParsing m) => m Module
moduleParser =
  fmap Module $ declParser `sepEndBy` symbolic ';'

newtype ParseError = ParseError String
  deriving (Show)

parseModule :: String -> Either ParseError Module
parseModule input =
  case parseString (moduleParser <* eof) mempty input of
    Success a ->
      Right a
    Failure err ->
      Left . ParseError . show $ _errDoc err

data Value
  = VInt64 Int64
  | VPair Value Value
  | VInl Value
  | VInr Value
  deriving (Eq)

data Values
  = Nil
  | Snoc Values Value
  deriving (Eq)

printValue :: Value -> String
printValue (VInt64 i) = show i
printValue (VPair a b) = parens (printValue a <> ", " <> printValue b)
printValue (VInl a) = "inl" <> parens (printValue a)
printValue (VInr a) = "inr" <> parens (printValue a)

printValues :: Values -> String
printValues vs = "[" <> go vs <> "]"
 where
  go Nil =
    ""
  go (Snoc Nil v) =
    printValue v
  go (Snoc vs'@Snoc{} v) =
    go vs' <> ", " <> printValue v

builtinsEval :: String -> Maybe (EvalContext -> [Either Type Program] -> Values -> Values)
builtinsEval "add" =
  Just $ \_ [] (Snoc (Snoc vs (VInt64 a)) (VInt64 b)) -> Snoc vs (VInt64 $ a + b)
builtinsEval "pair" =
  Just $ \_ [] (Snoc (Snoc vs a) b) -> Snoc vs (VPair a b)
builtinsEval "unpair" =
  Just $ \_ [] (Snoc vs (VPair a b)) -> Snoc (Snoc vs a) b
builtinsEval "inl" =
  Just $ \_ [] (Snoc vs a) -> Snoc vs (VInl a)
builtinsEval "inr" =
  Just $ \_ [] (Snoc vs a) -> Snoc vs (VInr a)
builtinsEval _ = Nothing

syntaxesEval :: String -> Maybe (EvalContext -> [Either Type Program] -> Values -> Values)
syntaxesEval input =
  case parseString intParser mempty input of
    Success i -> Just $ \_ [] vs -> Snoc vs (VInt64 i)
    _ -> Nothing

newtype EvalContext = EvalContext {getEvalContext :: String -> Maybe (EvalContext -> [Either Type Program] -> Values -> Values)}

evalProgram :: EvalContext -> Program -> Values -> Values
evalProgram context (Program (word :| words)) input =
  let Word name args = extract word
   in case getEvalContext context name of
        Nothing ->
          error $ "not found: " <> name
        Just evalWord ->
          let value = evalWord context (fmap extract . extract <$> args) input
           in case words of
                [] ->
                  value
                word' : words' ->
                  evalProgram context (Program (word' :| words')) value
