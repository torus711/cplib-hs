-- BEGIN SNIPLATE template 
-- {{ abbr: template for Haskell }}
-- {{ class: util }}
-- {{ pattern: main\s=\sdo }}
-- Author: @torus711
-- All pasted codes are published at GitHub:
-- https://github.com/torus711/cplib-hs/

{-# LANGUAGE CPP #-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE OverloadedLists #-}   
-- {-# LANGUAGE OverloadedStrings #-}   

{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}   

{-# OPTIONS_GHC -O3 #-}
{-# OPTIONS_GHC -Wno-tabs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Bifunctor

import Data.Bool
import Data.Bits
import Data.Char

import Data.List hiding ( (!?) )
import Data.Maybe
import Data.Either
import Data.Function

import qualified Data.ByteString.Char8 as B

import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Array.ST.Safe
import Data.Array.MArray
import Data.STRef

import Text.Printf
import Debug.Trace

#ifdef MIN_VERSION_extra
import Control.Monad.Extra
import Data.Tuple.Extra hiding ( (&&&), (***) )
import Data.List.Extra hiding ( (!?) )
#endif

#ifdef MIN_VERSION_containers
import qualified Data.Set as Set
import qualified Data.IntSet as ISet
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Sequence as Seq
import Data.Sequence ( (<|), (|>), (><), ViewL(..), ViewR(..) )
#endif

#ifdef MIN_VERSION_vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
#endif
#ifdef MIN_VERSION_vector_algorithms
import qualified Data.Vector.Algorithms.Intro as VA
#endif

readInt = fst . fromJust . B.readInt <$> B.getLine
readInteger = fst . fromJust . B.readInteger <$> B.getLine
readInts = map ( fst . fromJust . B.readInt ) . B.words <$> B.getLine
readIntegers = map ( fst . fromJust . B.readInteger ) . B.words <$> B.getLine
#ifdef MIN_VERSION_extra
readStr = trim . B.unpack <$> B.getLine
#else
readStr = dropWhile isSpace . reverse . dropWhile isSpace . reverse . B.unpack <$> B.getLine
#endif
readStrs = map B.unpack . B.words <$> B.getLine

yesno = bool "No" "Yes"
mp [ a, b ] = ( a, b )
mp _ = undefined

count a as = length $ filter ( == a ) as
countIf f as = length $ filter f as

swapArrayElem a i j = do
	x <- readArray a i
	y <- readArray a j
	writeArray a i y
	writeArray a j x

printList :: Show a => [a] -> IO ()
printList = putStrLn . intercalate " " . map show

printList2d :: Show a => [[a]] -> IO ()
printList2d = mapM_ printList

printArray2d ass = printList2d $ extract $ elems ass
	where
	( ( _, x1 ), ( _, x2 ) ) = bounds ass
#ifdef MIN_VERSION_extra
	extract = chunksOf ( x2 - x1 + 1 )
#else
	extract' [] = Nothing
	extract' as = Just $ splitAt ( x2 - x1 + 1 ) as
	extract = unfoldr extract'
#endif

--      Λ   Λ＿_
--   ／(*ﾟーﾟ)／＼  < Template ends here!
-- ／| ￣U U￣|＼／
--   |      |／

main = do
	-- {{ cursor }}
-- END SNIPLATE
