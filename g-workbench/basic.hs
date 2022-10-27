{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Basic where

import Glude.GInt

add:: GInt-> GInt -> GInt
add x y = x + y

main = add 1 2


-- https://hackage.haskell.org/package/ghc-bignum-1.3/docs/src/GHC.Num.BigNat.html#bigNatIndex%23

--https://hackage.haskell.org/package/ghc-prim-0.9.0/docs/src/GHC.Types.html#Bool



