{-# LANGUAGE NoImplicitPrelude #-}
import Protolude
import Basici

main::IO ()
main = putStrLn  ((show (inc 0))::[Char])


