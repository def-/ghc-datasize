# ghc-datasize

[![Build Status](https://secure.travis-ci.org/def-/ghc-datasize.svg?branch=master)](http://travis-ci.org/def-/ghc-datasize)


Ghc-datasize provides functions to determine the size of Haskell data structures
in memory at run time. Determining the size of circular data structures is
supported. All sizes are reported in bytes.

Determining the size of simple data structures is relatively quick, but large
and complex data structures can be slow. For large and complex structures the
user may want to find a way to calculate the size of a small subset of the data
and estimate the size of the full data structure from that.
