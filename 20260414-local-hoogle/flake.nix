{
  inputs = {
    cabal.url = "github:LightAndLight/cabal?ref=3.14.2.1";
    hoogle.url = "github:LightAndLight/hoogle";
  };
  outputs = { self, nixpkgs, cabal, hoogle }: {
    overlays.cabal = cabal.overlays.default;
    overlays.hoogle = hoogle.overlays.default;
  };
}
