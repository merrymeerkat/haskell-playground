{-# LANGUAGE TemplateHaskell #-}

module MapX where

import Control.Monad (unless)
import Data.Traversable (for)
import Language.Haskell.TH

tag :: Show a => a -> String
tag = (<> "'") . show

-- `arr x y` produces the type `x -> y`
arr :: Type -> Type -> Type
arr = AppT . AppT ArrowT


genTupleMapXClass :: Int -> Q [Dec]
genTupleMapXClass index = do
  unless (index > 0) $
    fail $ "Non-positive index: " ++ index_
  pure [cDecl]
  where
    index_ = show index
    className = mkName $ "TupleMapX" ++ index_
    methodName = mkName $ "_" ++ index_ ++ "_"
    t = mkName "t"
    t' = mkName "t'"
    r = mkName "r"
    r' = mkName "r'"

    -- class TupleMapX t t' r r' | t -> r, t' -> r' where
    cDecl = ClassD 
        []
        className 
        [PlainTV t, PlainTV r, PlainTV t', PlainTV r']
        [FunDep [t] [r], FunDep [t'] [r']]
        [mDecl]

    --   _X_ :: (r -> r') -> t -> t'
    mDecl = SigD methodName $
        arr (arr (VarT r) (VarT r')) (arr (VarT t) (VarT t'))


-- Generate an instance of TupleMapX
genTupleMapXInstance :: Int -> Int -> Q [Dec]
genTupleMapXInstance index size  = do
  unless (index > 0) $
    fail $ "Non-positive index: " ++ index_
  unless (size >= index) $
    fail $ "Can't access index " ++ index_ ++ " of " ++ size_ ++ "-tuple"
  pure [iDecl]
  where
    index_ = show index
    size_  = show size
    
    className = mkName $ "TupleMapX" ++ index_
    methodName = mkName $ "_" ++ index_ ++ "_"
    
    f = mkName "f"
 
    -- Types ----------------------------------------------
    -- vars  = [Var "t1", Var "t2", ..., Var "t{index}", ...]
    vars  = [VarT $ mkName ('t' : show n) | n <- [1..size]]
    -- vars' = [Var "t1", Var "t2", ..., Var "t{index}'", ...]
    vars' = [VarT $ mkName ('t' : show' n) | n <- [1..size]]
      where show' n = if n == index then tag n else show n

    tuple  = mkTuple vars
    tuple' = mkTuple vars'
    term   = mkTerm $ show index
    term'  = mkTerm $ tag index

    -- Helpers
    -- mkTuple :: Foldable t => t Type -> Type
    -- [t1, t2, ...] = (t1, t2, ...)
    mkTuple = foldl AppT $ TupleT size

    -- mkTerm :: [Char] -> Type
    -- mkTerm "3" = Name "t3"
    mkTerm = VarT . mkName . ('t' :)

    ------Pattern------------------------------------
    tuplePattern = TupP [ VarP $ mkName  ('t' : show n) | n <- [1..size] ]

    -----Expression--------------------------
    tupleExpression = TupE [ Just $ mkExpr n | n <- [1..size] ]
      where 
        var n = VarE . mkName $ 't' : show n
        mkExpr n = if n == index
          then AppE (VarE f) $ var n
          else var n
    
    -- Instance declaration -----------------------------
    -- instance TupleMapX (t1, t2, ..., t{index}, ...) t{index} (t1, t2, ..., t{index}', ...) t{index}'
    iDecl = InstanceD 
        Nothing
        []
        (AppT
          (AppT 
            (AppT 
              (AppT 
                (ConT className) -- TupleMapX
                tuple) -- (t1,t2, ..., t_{index}, ...)
              term)    --              t_{index}
            tuple')    -- (t1,t2, ..., t_{index}', ...)
          term')       --              t_{index}'
        [mDecl]

    --- Method Declaration ------------------------------------------- 
    -- _X_ f (t1, t2, ..., t{index}, ...) = (t1, t2, ..., f t{index}, ...)
    mDecl = FunD 
                methodName                    -- _X_
                [Clause                       -- (=)
                    [ VarP f                  -- f
                    , tuplePattern]           -- (t1, t2, ...,   t{index}, ...)
                    (NormalB tupleExpression) -- (t1, t2, ..., f t{index}, ...)
                    []
                ]

-- Given a function that generates a class declaration, 
--   and a function that generates an instance of this class,
--   generate N instances for all M-tuples such that N <= M
genBoilerplate 
  :: (Int        -> Q [Dec])
  -> (Int -> Int -> Q [Dec])
  -> Int
  -> Q [Dec]
genBoilerplate genClass genInstance max =
  concatFor [1..max] $ \classDeclIndex -> do
    cDecl  <- genClass classDeclIndex
    iDecls <- for [1..classDeclIndex] $ \instanceDeclIndex ->
      genInstance instanceDeclIndex classDeclIndex

    pure $ concat (cDecl : iDecls)
  where
    concatFor xs = fmap concat . for xs

genTupleMapXBoilerplate :: Int -> Q [Dec]
genTupleMapXBoilerplate maxTupleSize = genBoilerplate 
  genTupleMapXClass 
  genTupleMapXInstance 
  maxTupleSize

