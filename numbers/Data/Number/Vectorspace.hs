{-# LANGUAGE
    FunctionalDependencies,
    MultiParamTypeClasses #-}
module Data.Number.Vectorspace(Vectorspace(..)) where

-- |Class of vector spaces /v/ with scalar /s/.
class Vectorspace s v | v -> s where
    (*>)    :: s -> v -> v
    (<+>)   :: v -> v -> v
    vnegate :: v -> v
    vzero   :: v
