{ mkDerivation, base, stdenv, transformers, ansi-wl-pprint, text, tinytemplate,
  filepath, directory }:

mkDerivation {
  pname = "skeletons";
  version = "0.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base transformers ansi-wl-pprint text tinytemplate filepath
                   directory
                 ];
  homepage = "https://github.com/jb55/skeletons";
  description = "Manage project skeletons";
  license = stdenv.lib.licenses.mit;
}
