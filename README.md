# Fast binary serialization for Haskell

[![Linux Build Status](https://img.shields.io/travis/well-typed/binary-serialise-cbor/master.svg?label=Linux%20build)](https://travis-ci.org/well-typed/binary-serialise-cbor)
[![Windows Build Status](https://img.shields.io/appveyor/ci/thoughtpolice/binary-serialise-cbor/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/thoughtpolice/binary-serialise-cbor/branch/master)
[![Hackage version](https://img.shields.io/hackage/v/binary-serialise-cbor.svg?label=Hackage)](https://hackage.haskell.org/package/binary-serialise-cbor)
[![Stackage version](https://www.stackage.org/package/binary-serialise-cbor/badge/lts?label=Stackage)](https://www.stackage.org/package/binary-serialise-cbor)
[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![Haskell](https://img.shields.io/badge/Language-Haskell-yellowgreen.svg)](https://www.haskell.org)

**NOTE**: Currently this library has not been released to the public Hackage
server, as the API is still considered to be in flux. However, the on-disk
formats are relatively stable, and the code has seen _substantial_ production
use by Well-Typed, as well as a number of outsiders and independent companies
(with near-universal positive results), for well over a year.

---

This package provides pure, efficient serialization of Haskell values directly
into `ByteString`s for storage or transmission purposes. By providing a set of
type class instances, you can also serialise any custom data type you have as
well.

The underlying binary format used is the 'Concise Binary Object Representation',
or CBOR, specified in `RFC 7049`. As a result, serialised Haskell values have
implicit structure outside of the Haskell program itself, meaning they can be
inspected or analyzed with custom tools.

This package is eventually intended to essentially replace the venerable
`binary` library. Aside from having a more concise format that has an explicit
structure, it's multiple times faster than competing libraries as well in both
the encode and decode paths. However, the current API only provides CBOR
serialization; it does not provide an independent notion of parsing arbitrary
binary data. This has yet to be designed or implemented.

# Installation

It's just a `cabal install` away on [Hackage][], or through [Stackage][]:

```bash
$ cabal install binary-serialise-cbor
$ stack install binary-serialise-cbor
```

**NOTE**: The above currently **WILL NOT WORK**, as this package is not
publicly released.

[Hackage]:  https://hackage.haskell.org/package/binary-serialise-cbor
[Stackage]: https://www.stackage.org

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/well-typed/binary-serialise-cbor.git`

Once you've done that, you can build it and run the tests:

```bash
$ cabal test
$ stack test
```

Note: the `stack.yaml` file is currently synchronized to **LTS-8.8**. Further
compilers and other LTS releases are currently not supported with Stack
at the moment, but the build *is* tested with older compilers and Cabal
libraries (through Travis CI).

[contribute]: https://github.com/well-typed/binary-serialise-cbor/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/well-typed/binary-serialise-cbor/issues
[gh]: http://github.com/well-typed/binary-serialise-cbor

# Authors

See
[AUTHORS.txt](https://raw.github.com/well-typed/binary-serialise-cbor/master/AUTHORS.txt).

# License

BSD3. See
[LICENSE.txt](https://raw.github.com/well-typed/binary-serialise-cbor/master/LICENSE.txt)
for the exact terms of copyright and redistribution.
