with (import <nixpkgs> {});
# let
#   gems = bundlerEnv {
#     name = "your-package";
#     inherit ruby;
#     gemdir = ./.;
#   };
# in stdenv.mkDerivation {
stdenv.mkDerivation {
  name = "elm-rcv";
  buildInputs = [
    elmPackages.elm
    # elmPackages.elm-live
    # haskellPackages.elm-init
    # haskellPackages.elm-reactor
  ];
}
