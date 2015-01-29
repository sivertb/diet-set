{ cabal, cabalInstall, filterSource, QuickCheck }:

cabal.mkDerivation
( self:
  { pname = "diet-set"
  ; version = "0.1.0"
  ; src = filterSource ./.
  ; buildTools = [ cabalInstall ]
  ; buildDepends = [ ]
  ; testDepends = [ QuickCheck ]
  ; doCheck = true
  ; enableSplitObjs = false
  ;
  }
)
