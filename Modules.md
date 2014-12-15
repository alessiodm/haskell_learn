Modules
========

You can import selectively and hide functions from modules:

import Data.List (nub, sort)
import Data.List hiding (nub)

Importing with qualified states that is mandatory to use the fully qualified name of the function along with its module name:

import qualified Data.Map
import qualified Data.Map as M
