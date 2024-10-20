# Playing with `ghc-exactprint`

*2024-10-19*

This was an attempt to use [GHC API](https://hackage.haskell.org/package/ghc) and [`ghc-exactprint`](https://hackage.haskell.org/package/ghc-exactprint) for formatting-preserving syntax tranformations.
I plan to use what I've learned to to [Rails-style boilerplate generation](https://guides.rubyonrails.org/command_line.html#bin-rails-generate) for a Haskell web framework[^1].

[^1]: `rails generate controller CONTROLLER ACTION*` creates a new controller by generating some files from templates, and it registers the actions' routes by adding some lines to an existing Ruby file (`routes.rb`).
      This piqued my interest because it's not just adding those lines to the end of a file; it's somewhat syntax-aware.

      It's [a little more finnicky](https://stackoverflow.com/a/39742150/2884502) to add an action to an existing controller via the CLI.
      I think most people just write the code.

      I imagine a workflow in Haskell where there are a few conventional modules that a CLI tool can programmatically updated to add a new endpoint.

I was able to write two functions over Haskell syntax trees:

```haskell
addFieldToRecord ::
  -- | Field name and value
  (String, HsExpr GhcPs) ->
  -- | Expression to modify
  LHsExpr GhcPs ->
  -- | Returns 'Nothing' if the expression was not a record literal
  Maybe (LHsExpr GhcPs)

addFieldToRecordDataDefn ::
  -- | Datatype name to modify
  String ->
  -- | Field name and type
  (String, BangType GhcPs) ->
  -- | Declaration to modify
  LHsDecl GhcPs ->
  -- | Returns 'Nothing' if the declaration was not a record-style data definition
  Maybe (LHsDecl GhcPs)
```

I used [`lens`](https://hackage.haskell.org/package/lens) to apply these transformations a parsed Haskell module.
The exact-print annotations allow me to add record fields in a way that's consistent with the rest of the record.
For example, `addFieldToRecord` and `addFieldToRecordDataDefn` respect the existing comma placement between fields:

<table>
<thead>
<th>Input</th>
<th>Action</th>
<th>Output</th>
</thead>
<tbody>
<tr>
<td>

```haskell
data X = X { a :: Int, b :: Int }
```

</td>
<td>

add `c :: Int`

</td>
<td>

```haskell
data X = X { a :: Int, b :: Int, c :: Int }
```

</td>
</tr>
<tr>
<td>

```haskell
data X
  = X
  { a :: Int
  , b :: Int 
  }
```

</td>
<td>

add `c :: Int`

</td>
<td>

```haskell
data X
  = X
  { a :: Int
  , b :: Int
  , c :: Int
  }
```

</td>
</tr>
<tr>
<td>


```haskell
data X =
  X{
    a :: Int,
    b :: Int
  }
```

</td>
<td>

add `c :: Int`)

</td>
<td>

```haskell
data X =
  X{
    a :: Int,
    b :: Int,
    c :: Int
  }
```

</td>
</tr>
</tbody>
</table>
