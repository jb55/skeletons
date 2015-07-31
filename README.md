
# skeletons

  Project skeleton manager

## Installation

  Install with cabal (not yet, soon)

    $ cabal install skeletons

  Install with [nix](http://nixos.org/nix/)

    $ git clone https://github.com/jb55/skeletons && cd skeletons && nix-env -i -f .

## Usage

  Put skeleton files in folders under `$HOME/.config/closet/<skeleton>/{file1,file2}`

  Then call skeletons with `<skeleton>`:

    $ skeletons <skeleton>

  For example: `$HOME/.config/closet/nix-hs/{default,shell}.nix`

    $ skeletons nix-hs

  Will copy those files to your current working directory, prompting for any template
  fields

## Templates

  Files under `$HOME/.config/closet/<skeleton>` are [tinytemplates](https://hackage.haskell.org/package/tinytemplate)

  They can contain anything, with template fields like so: `{{ some_field }}`

### Example Template

    { nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
    nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./{{ name }}.nix { }

  `name` will be prompted

### Special Variables

  If you use `{{ $basename }}` in your template, it will fill it with the
  name of the current directory

### Default Variables

  Variables can have default values like so: `{{ name ? bob }}`

  You can even default to special variables: `{{ dir ? $basename }}`

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
