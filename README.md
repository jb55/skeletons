
# skeletons

  Project skeleton manager

## Installation

  Install with cabal

    $ cabal install skeletons

## Usage

  Put skeleton files in folders under `$HOME/.config/closet/<skeleton>/{file1,file2}`

  Then call skeletons with `<skeleton>`:

    $ skeletons <skeleton>

  For example: `$HOME/.config/nix-hs/{default,shell}.nix`

    $ skeletons nix-hs

  Will copy those files to your current working directory, prompting for any template
  fields

## Templates

  Files under `closet/<skeleton>` are [tinytemplates](https://hackage.haskell.org/package/tinytemplate)

  Basically they can contain anything, with template fields like: `{{ some_field }}`

### Example Template

    { nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
    nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./{{ name }}.nix { }

  `name` will be prompted

## License

  The MIT License (MIT)

  Copyright (c) 2014 William Casarin

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
