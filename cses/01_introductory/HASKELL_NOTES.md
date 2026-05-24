I want the HLS server in the Haskell extension for VS Code.

To get this working:

1) remove Homebrew ghc (let ghcup manage it).
2) set toolchain pref for Haskell extension to a specific ghc 9.12.4; hls "recommended", cabal null.
3) enable Haskell ext logging in settings.
   e.g. to a specific file `/Users/toby/vs-codium-haskell.log`. this helps debug problems like disk full.
4) clear at least 15gb of disk space.
5) open a .hs file & let 'er rip

## GHC

It's important to decide on a version. I am using `9.12.4`.

This can be set in VS Codium User settings. (shift-command-P, "Open User Settings (JSON)")

```json
		"haskell.toolchain": {
			"ghc": "9.12.4",
			"hls": "latest",
			"cabal": "recommended"
		},
```

```sh
% ls ~/.ghcup/bin
cabal                                           haddock-9.12                                    hp2ps-9.12.4
cabal-3.14.2.0                                  haddock-9.12.4                                  hpc-9.12
cabal-3.16.1.0                                  haskell-language-server-9.10.3~2.14.0.0         hpc-9.12.4
ghc-9.12                                        haskell-language-server-9.12.2~2.14.0.0         hsc2hs-9.12
ghc-9.12.4                                      haskell-language-server-9.12.4~2.14.0.0         hsc2hs-9.12.4
ghc-pkg-9.12                                    haskell-language-server-9.14.1~2.14.0.0         runghc-9.12
ghc-pkg-9.12.4                                  haskell-language-server-9.6.7~2.14.0.0          runghc-9.12.4
ghci-9.12                                       haskell-language-server-9.8.4~2.14.0.0          runhaskell-9.12
ghci-9.12.4                                     haskell-language-server-wrapper-2.14.0.0        runhaskell-9.12.4
ghcup                                           hp2ps-9.12
```

## Set up individual project

To use GHCup binaries:

```sh
export PATH=~/.ghcup/bin:$(ghcup whereis -d ghc 9.12.4):$PATH
```

`cabal` won't work if it can't see `ghc` on `PATH`.

Initial Cabal file:

```sh
cabal init -m --exe --main-is knapsack
```

### The `.cabal` file

* You may want to change `hs-source-dirs` to `.` depending where your source tree is.
* The point of this was dependency management, and having dependencies visible to HLS,
  so you can add things like `containers` to `build-depends`.

Ensure you have a `hie.yaml` file indicating [HLS should look for Cabal configuration](https://github.com/haskell/hie-bios):

```yaml
cradle:
  cabal:
```

(This can be extended for multiple-component projects.)

### Basic tasks

```sh
cabal build

% cabal run knapsack -- 10 1 3                                                                                                          (main !?)
(14,"XX XX     XX XX")
```

## Panic

If you can't run `ghc` like this:

```sh
$(ghcup whereis ghc 9.12.4)
```

or

```sh
% cabal init -m --exe -w $(ghcup whereis ghc 9.12.4) --main-is knapsack

Error: [Cabal-5490]
Cannot find the program 'ghc'. User-specified path '/Users/toby/.ghcup/ghc/9.14.1/bin/ghc' does not refer to an executable and the program is not on the system path.
```

Check:

```sh
% ghcup check tool ghc 9.12.4
Installed artifacts file exists at /Users/toby/.ghcup/db/ghc/9.12.4: True
Installed specification file exists at /Users/toby/.ghcup/db/ghc/9.12.4.spec: False
Can parse specification file: False
Install destination: "/Users/toby/.ghcup/ghc/9.12.4"
Number of files that should be installed: 15258
Missing files:
    none
Stray files:
    none
Symlink status:
    (no information)
Set symlink status:
    (no information)
```
