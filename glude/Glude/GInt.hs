{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Glude.GInt (GInt, (+)) where 

import GHC.Types --https://www.stackage.org/haddock/lts-1.0/ghc-prim-0.3.1.0/src/GHC-Types.html#Int
import GHC.Prim
import GHC.Num.Integer
import GHC.Num


data GInt#
data GInt = GI# GInt#

(+@) :: GInt# -> GInt# -> GInt#
(+@) = let x=x in x

(*@) :: GInt# -> GInt# -> GInt#
(*@) = let x=x in x


gIntegerToGInt# :: Integer -> GInt#
gIntegerToGInt# = let x=x in x

instance Num GInt  where
    (GI# x) + (GI# y) = GI# (x +@ y)

    {-# INLINE fromInteger #-}
    fromInteger i = GI# (gIntegerToGInt# i)

    (GI# x) * (GI# y) = GI# (x *@ y)
    abs (GI# y) = GI# y

    negate (GI# y) = GI# y
    signum (GI# y) = GI# y


