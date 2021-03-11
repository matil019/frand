# frand: A floating point random number generator

`frand` is a simple commandline tool that generates random floating point numbers. It is implemented with [`mwc-random`](https://hackage.haskell.org/package/mwc-random).

## Installing

This program is currently not published to Hackage. Please download an archive from https://github.com/matil019/frand/releases (look for a `tar.gz` link) and extract it to a directory. Or alternatively, clone this repository.

After that, use the cabal-install tool to build and install `frand`.

```sh
cabal v2-update
cabal v2-install . --overwrite-policy=always
```

`--overwrite-policy=always` is not necessary, but we recommend it because it makes easier to install newer versions. The tool will be installed at `~/.cabal/bin/frand`.

## Examples of usage

Uniformly generate a random number in an interval `[1, 5]`:

```sh
$ frand uniform 1 5
4.551609505727284
```

Generate five random numbers with a normal distribution of mean 5 and standard deviation 2:

```sh
$ frand normal --num 5 5 2
5.235272117068453
4.64985889420898
7.060698043923342
4.834412934580578
6.105681218321858
```

Generate a random number with a given seed for reproducible random numbers:

```sh
$ echo foo > seedfile
$ frand normal --seed-in seedfile 5 2
6.585815226312909
$ frand normal --seed-in seedfile 5 2
6.585815226312909
```

Try `frand --help`, `frand normal --help`, etc. for more exhaustive explanation of the commandline options.
