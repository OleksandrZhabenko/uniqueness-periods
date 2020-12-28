-- |
-- Module      :  String.Ukrainian.UniquenessPeriods
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Can be used to produce the 'uniquenessPeriods' function and related 
-- functionality for the Ukrainian text.

module String.Ukrainian.UniquenessPeriods (
  -- * Auxiliary functions
  show7s'''
  , show7s5
  , show7s6
  , show7sn4'
  , show7snc
  -- ** Inner predicate (auxiliary)
  , eqSnds
  -- ** Inner backward conversion function
  , listToString
  -- * uniquenessPeriods function
  , uniquenessPeriods
) where

import Data.Char (isSpace)
import qualified Data.Vector as V
import Data.List ((\\),nubBy)
import Melodics.Ukrainian (convertToProperUkrainian)


-- | Function 'listToString' converts the list of Strings being the sequential Ukrainian sounds representations into the Ukrainian text with whitespaces
-- (whitespaces are substituted instead of punctuation symbols, too) and some phonetical conversions.
listToString :: [String] -> String
listToString =
  concatMap (\t ->
    case t of
      "0" -> " "
      "1" -> " "
      "-" -> " "
      x   -> x)

-- | Function 'eqSnds' compares two non-silent Strings representations for Ukrainian sounds by equality. If one of them is a representation for silence (e. g. pause),
-- then the predicate is @False@.
eqSnds :: String -> String -> Bool
eqSnds xs ys | xs `elem` ["-","0","1"] || ys `elem` ["-","0","1"] = False
             | otherwise = xs == ys      

-- | The same as @show7s''@ from MMSyn7s module (@mmsyn7s@ package), but the second element in the resulting tuple is again the Ukrainian text 
-- with whitespaces (whitespaces are substituted instead of punctuation symbols, too) and some phonetical conversions.
show7s''' :: [String] -> ([String],String)
show7s''' zss =
  let (xss, yss) = splitAt 68 zss
      uss = xss \\ nubBy eqSnds xss
      (wss,vss) = if null uss then (xss,[]) else (takeWhile (/= head uss) xss ++ head uss:(takeWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss),
        dropWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss) in 
          (filter (\x -> x /= "-" && x /= "1" && x /= "0") $ wss, listToString $ vss ++ yss)

-- | Function 'show7s5' takes a Ukrainian text being a @String@ and returns a tuple, the first element of which is a list of Strings that correspond to the Ukrainian 
-- sounds representations that (except pauses) are unique and are not repeated starting from the beginning of the given text (this list is filtered from 
-- the representations for the silence), and the second one is a @String@ obtained from the remainder
-- list of Strings starting from the first duplicated non-silent Ukrainian sound representation with whitespaces (whitespaces are substituted
-- instead of punctiuation symbols, too) and some phonetical conversions. 
show7s5 :: String -> ([String], String)
show7s5 = show7s''' . V.toList . convertToProperUkrainian

-- | Function 'show7s6' takes a Ukrainian text being a @String@ and returns a list of lists of Strings, each latter one of which is obtained for the unique parts of
-- the text from the Ukrainian sounds representations point of view. It can show how many and what sound representations are needed to be created to completely cover
-- the given text providing all the needed sound parameters.
show7s6 :: String -> [[String]]
show7s6 t@(_:_) = (fst . show7s5 $ t):(show7s6 . snd . show7s5 $ t)
show7s6 _ = []
      
-- | Function 'uniquenessPeriods' takes a Ukrainian text being a @String@ and returns a list of Ints. Each Int value is a number of 
-- the Ukrainian sounds representations (non-silent ones) being unique and not duplicated alongside the given text starting from the beginning to the end.
-- This function provides some important information about the phonetic and in some cases semantical structures of the text.
uniquenessPeriods :: String -> [Int]
uniquenessPeriods xs | any (not . isSpace) xs = fmap length . show7s6 $ xs
                     | otherwise = [0::Int]

-- | Converts a list of Ukrainian 'String' each one being a Ukrainian non-silent sound representation into a list of 'Int' using recursively @show7sn4'@. 
show7snc :: [String] -> [Int]
show7snc xss = let (tss,vss) = show7sn4' xss in if null vss then [length tss] else length tss:show7snc vss
                           
-- | The same as @show7sn'''@ from the MMSyn7s module from the @mmsyn7s@ package, but does not concatenate the list of 'String' as the second tuple's element.
show7sn4' :: [String] -> ([String],[String])
show7sn4' zss =
  let (xss, yss) = splitAt 68 zss
      uss = xss \\ nubBy eqSnds xss
      (wss,vss) = if null uss then (xss,[]) else (takeWhile (/= head uss) xss ++ head uss:(takeWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss),
        dropWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss) in 
          (filter (\x -> x /= "-" && x /= "1" && x /= "0") $ wss, vss ++ yss)                            
