{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Sp.Combinatorial where

import           Control.Applicative
import           Disorder.Sp.Combinatorial

import           Test.QuickCheck


--

return []
tests :: IO Bool
tests = $quickCheckAll
