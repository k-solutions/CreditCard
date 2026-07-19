{-# LANGUAGE TypeFamilies #-}

module TFTest where

type family F1 a where
  F1 Int  = Bool
  F1 Char = Double

useF1 :: F1 Int -> F1 Char
useF1 True = 1.0
useF1 False = (-1.0)
