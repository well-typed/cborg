# Revision history for cborg

## 0.2.3.1  -- 2020-05-10

* Bounds updates for GHC 8.10

* `Decoder` is now a `newtype`.

## 0.2.2.1  -- 2019-12-29

* Testsuite updates for GHC 8.8

## 0.2.2.0  -- 2019-07-31

* Add peekByteOffset for the current ByteOffset in the input byte sequence.
  By keeping track of the byte offsets before and after decoding asubterm
  (a pattern captured by decodeWithByteSpan) and if the overall input data
  is retained then this is enables later retrieving the span of bytes for
  the subterm.

* Add encodePreEncoded function. This allows pre-encoded CBOR data to be
  included into an Encoding. This is useful in cases where one has known-valid
  encoded CBOR data, e.g. on disk, that you want to include into a larger CBOR
  data stream. This makes it possible in such cases to avoid decoding and
  re-encoding.

* Improved test suite property coverage. We now have property coverering most
  parts of a commuting diagram, which gives more confidence about what are
  the right properties to test and what is enough.

* Improved test coverage for decoding non-canonical terms

* Fix a bug in the checks in the canonical decoding of float16 NaNs. There
  are multiple bit representations of NaNs and while we were checking this
  correctly for float32 and float64 we were not checking this correctly for
  the float16 encoding.

* Improved test coverage for special float values. We now have pretty
  comprehensive coverage of round-tripping properties for special float
  values, +/- infinity and non-canonical NaNs.

* Improved the structure of the test suite

* Use new GHC primitives castWord{32ToFloat,64ToDouble} rather than home grown

* Support GHC built with integer-simple

* Support GHC 8.8

## 0.2.1.0  -- 2018-10-11

* Bounds bumps and GHC 8.6 compatibility

## 0.2.0.0  -- 2017-11-30

* Improved robustness of non-UTF-8 strings

* Add encoders and decoders for `ByteArray`

* Add decoding variants that check for canonical encodings

* Expose `Codec.CBOR.Read.deserialiseFromBytesWithSize`

## 0.1.0.0  -- 2017-06-28

* First version. Released on an unsuspecting world.

