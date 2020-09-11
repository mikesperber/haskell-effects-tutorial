{ mkDerivation, base, bytestring, containers, extensible-effects
, fused-effects, heap, MonadRandom, mtl, polysemy, polysemy-plugin
, random, rio, stdenv, transformers
}:
mkDerivation {
  pname = "effects-tutorial";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers extensible-effects fused-effects heap
    MonadRandom mtl polysemy polysemy-plugin random rio transformers
  ];
  homepage = "https://github.com/mikesperber/haskell-effects-tutorial";
  license = stdenv.lib.licenses.bsd3;
}
