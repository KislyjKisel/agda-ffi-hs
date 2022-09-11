-- * inst names ==> inst postulates
-- #( *)(\w+)(?:\[.+\])?(?: +: +.*)?
-- #$1$2[QQ1] : $2 QQ2

-- * inst postulate ++> same type, inst param
-- #: (\w+)
-- #: ⦃ $1 A ⦄ → $1

-- * inst postulate ==> compile pragmas
-- # *(\w+)\[(.+)\] +: .*
-- #{-# COMPILE GHC $1[$2] = Agda$1 #-}

-- * fn postulates ==> compile pragmas
-- # *([\w'$<>\|]+) +: .*
-- #{-# COMPILE GHC $1 = MODULE.$1 #-}

{-# FOREIGN GHC
import qualified 
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

