{-|
Module      : Crypto.Lol.Types
Description : Concrete types needed to instantiate cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\R{\mathbb{R}} \)

Exports concrete types needed to instantiate cryptographic applications.
Specifically:

  * "Crypto.Lol.Types.Complex"
  * "Crypto.Lol.Types.IrreducibleChar2"
  * "Crypto.Lol.Types.Random"
  * "Crypto.Lol.Types.RRq"
  * "Crypto.Lol.Types.ZqBasic"
-}

-- EAC: See https://github.com/haskell/haddock/issues/563
module Crypto.Lol.Types
( module Crypto.Lol.Types.Random
, Complex, roundComplex, cis, real, imag, fromReal -- see my comment in the bug report
, RRq
, ZqBasic, goodQs) where

import Crypto.Lol.Types.IrreducibleChar2 ()
import Crypto.Lol.Types.Random
import Crypto.Lol.Types.Unsafe.Complex   hiding (Complex')
import Crypto.Lol.Types.Unsafe.RRq
import Crypto.Lol.Types.Unsafe.ZqBasic   hiding (ZqB)
