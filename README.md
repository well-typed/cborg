# Fast binary serialization for Haskell

[![Build Status](https://travis-ci.org/well-typed/binary-serialise-cbor.png?branch=master)](https://travis-ci.org/well-typed/binary-serialise-cbor)
[![BSD3](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://www.haskell.org)

This package provides pure, efficient serialization of Haskell values directly
into `ByteString`s for storage or transmission purposes. By providing a set of
type class instances, you can also serialize any custom data type you have as
well.

The underlying binary format used is the 'Consise Binary Object
Representation', or CBOR, specified in `RFC 7049`. As a result, serialized
Haskell values have implicit structure outside of the Haskell program itself,
meaning they can be inspected or analyzed with custom tools.

This package is eventually intended to essentially replace the venerable
`binary` library. Aside from having a more concise format that has an
explicit structure, it's multiple times faster than competing libraries
as well in both the encode and decode paths. However, the current API only
provides CBOR serialization; it does not provide an independent notion of
parsing arbitrary binary data. This has yet to be designed or implemented.

Currently this library has not been released to the public Hackage server,
although it has seen real production use (with good results). Experimentation
is encouraged, with a healthy dose of careful inspection and analysis.

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install binary-serialise-cbor
```

TODO: The above doesn't actually work (yet). Also mention Stack when released.

[Hackage]: http://hackage.haskell.org/package/binary-serialise-cbor

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/well-typed/binary-serialise-cbor.git`

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
