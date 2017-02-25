{-|
Module      : AllMain
Description : Pulls in all modules.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

This module depends on all source code modules, so it is useful for checking
that all code compiles, including all top-level executables.
-}


import LolTestsMain ()

import RLWEChallengesMain ()

import BenchLolCPPMain ()
import BenchAppsCPPMain ()
import TestLolCPPMain ()
import TestAppsCPPMain ()
import HomomPRFCPPMain ()
import KHPRFCPPMain ()
import SHECPPMain ()

import BenchLolRepaMain ()
import BenchAppsRepaMain ()
import TestLolRepaMain ()
import TestAppsRepaMain ()
import HomomPRFRepaMain ()
import KHPRFRepaMain ()
import SHERepaMain ()