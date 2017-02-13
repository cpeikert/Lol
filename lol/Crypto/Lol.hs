{-|
Module      : Crypto.Lol
Description : Primary interface to the Lol library.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\C{\mathbb{C}} \)
  \( \def\Q{\mathbb{Q}} \)
  \( \def\R{\mathbb{R}} \)
  \( \def\F{\mathbb{F}} \)
  \( \def\O{\mathcal{O}} \)

This module re-exports primary interfaces, and should be the only import
needed for most cryptographic application implmenetations. To instantiate an
application with concrete types, you will also need to import
"Crypto.Lol.Types".

Below is a brief mathematical background which serves as reference
material for reading the documentation.

  * \( \Z \) denotes the ring of integers, \( \Q \) is the rational numbers,
    \( \R \) denotes the real numbers, and \( \C \) represents the complex
    numbers.

  * \( \Z_q = \Z/(q\Z) \) is the integers mod \( q \). \( \Z_q \) is
    implemented in "Crypto.Lol.Types.ZqBasic".

  * The finite field of order \( p \) is denoted \( \F_{p} \), and is
    implemented in "Crypto.Lol.Types.FiniteField".

  * \( \zeta_m \in R \) is an arbitrary element of order \( m \) in a
    ring \( R \).

  * Throughout, \( m \) denotes a cyclotomic /index/. In code, the cyclotomic
    index is represented by the type parameter @m :: 'Factored'@.

  * The ring \( R=\O_m=\Z[\zeta_m] \) is the \( m \)th cyclotomic ring.
    We denote the quotient ring \( \Z_q[\zeta_m]) \) by \( R_q \).
    We refer to \( \Z \) or \( \Z_q \) as the /base ring/.
    Cyclotomic rings are encapsulated by "Crypto.Lol.Cyclotomic.Cyc".

  * \( n=\varphi(m) \) is the totient function of the cyclotomic index.
    This is the dimension of a cyclotomic ring over the base ring.
-}

module Crypto.Lol
( module X
) where

import Crypto.Lol.Cyclotomic.Cyc        as X
import Crypto.Lol.Cyclotomic.Linear     as X
import Crypto.Lol.Cyclotomic.RescaleCyc as X
import Crypto.Lol.Gadget                as X
import Crypto.Lol.Prelude               as X
