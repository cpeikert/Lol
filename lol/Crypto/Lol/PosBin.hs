{-|
Module      : Crypto.Lol.PosBin
Description : Type-level positive naturals in Peano and binary.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Positive naturals in Peano and binary representations,
singletonized and promoted to the type level.  This module relies
on Template Haskell, so parts of the documentation may be difficult
to read.  See source-level comments for further details.
-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}



module Crypto.Lol.PosBin
( module Crypto.Lol.PosBinDefs
-- * Convenient synonyms for 'Pos' and 'Bin' types
, module Crypto.Lol.PosBin
) where

import Crypto.Lol.PosBinDefs

$(mapM posDec [1..16])

$(mapM binDec [1..128])
