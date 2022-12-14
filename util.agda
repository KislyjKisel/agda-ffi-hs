-- * inst names ==> inst postulates
-- #( *)(\w+)(?:\[.+\])?(?: +: +.*)?#$1$2[QQ1] : $2 QQ2

-- * inst postulate ++> same type, inst param
-- #: (\w+)#: ⦃ $1 A ⦄ → $1

-- * inst postulate ==> compile pragmas
-- # *(\w+)\[(.+)\] +: .*#{-# COMPILE GHC $1[$2] = Agda$1 #-}

-- * fn postulates ==> compile pragmas
-- # *([\w'$<>\|]+) +: .*#{-# COMPILE GHC $1 = MODULE.$1 #-}

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified 
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

-- Haddock
-- * remove comments from fns
-- #^(?!->)(?!=>)((?!::).)*$\n#

-- * remove newlines from signatures
-- #\t \n([\-=])># $1>

-- * lowercase variables ==> uppercase variables
-- #(?<=(?:^|\s|\())([a-zA-Z])(?=(?:$|[\s\),]))#\U$1

-- * class constraints ==> instance args
-- #(\w+) (\w+) =>#⦃ $1 $2 ⦄ ->
-- #\((\w+) (\w+), (\w+) (\w+)\) =>#⦃ $1 $2 ⦄ -> ⦃ $3 $4 ⦄ ->

-- * rm trailing whitespace
-- #\s+$#

-- --