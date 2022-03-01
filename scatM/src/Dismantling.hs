{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dismantling(getSoundFolder,
	folderCollection,
	findCollection,
	getSubstrate,
	WaveLifeForm(Description, Body)
	) where

import Data.Text(Text,unpack,take)
import Data.Text.Encoding
import Data.Int
import Text.Printf
--import Data.Vector.Unboxed as V
import qualified Data.ByteString as B
import Data.ByteString.Lazy(fromStrict)
import Data.Binary 
import Data.Binary.Get
import Data.Binary.Builder
import Data.String(fromString)
import System.IO(openBinaryFile,IOMode(ReadMode),hSetBinaryMode,putStrLn,hClose)
import System.Directory
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Massiv.Array as A
import Data.Massiv.Core
import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Manifest
import Data.Int.Int24

-- SizeData + 36
type ChunkSize = Int32
-- size of the rest of file from word "fmt ". 16 for PCM
type SubchunkSize = Int32
-- form of copression.  PCM = 1 - linear quanterization
type AudioFormat = Int16
-- 1-mono, 2-stereo , etc.
type NumChannels = Int16 
-- Khz
type SampleRate = Int32
-- SampleRate*BlockAlign
type ByteRate = Int32
-- NumChannels*BitsPerSample/8
type BlockAlign = Int16
-- 8 bit =8, 16 bit = 16 ,etc.
type BitsPerSample = Int16
-- number of additional conteiner bytes
type CbSize = Int16
-- number of bytes in the music conteiner
type SizeData = Int32 
-- path
type WPath = FilePath

data WaveLifeForm a = Description String Int32 (WaveLifeForm a) | Body WPath a   

--instance Functor WaveLifeForm  where 	
instance Show (WaveLifeForm a) where
	show (Description a b c) = (printf "%.8d | " b) ++ show c 
	--show (Body path a) = "|\n"
	show (Body path a) = path ++ "|\n" 

toFormEmpty :: WPath ->  WaveLifeForm (A.Array U Ix1 Double )
toFormEmpty a = Body a $ A.makeArrayR U Seq (A.Sz1 1) $ \ i -> fromIntegral i

folderCollection :: FilePath -> IO [FilePath]
folderCollection =  getDirectoryContents

findCollection :: (Fractional a , Num a) => String -> WaveLifeForm (A.Array U Ix1 Double ) -> a 
findCollection name (Description a b c) = if a == name then fromIntegral b else findCollection name c
findCollection name (Body a b) = 0  

getPath :: WaveLifeForm (A.Array U Ix1 Double ) -> WPath
getPath (Body wPath _) = wPath
getPath (Description a b c) = getPath c

getSubstrate :: WaveLifeForm (A.Array U Ix1 Double ) -> A.Array U Ix1 Double
getSubstrate (Description a b c) = getSubstrate c
getSubstrate (Body a datt) = datt

data Sampling = Sampling { one :: Int8 , two :: Int8 , three :: Int8} 
data Pseudo = Pseudo {oone :: Int32}

getWaveTitle :: (Int, Int, Int) -> WaveLifeForm (A.Array U Ix1 Double ) ->  Get (WaveLifeForm (A.Array U Ix1 Double ))
getWaveTitle (a1, a2, a3) (Body path _) = do 
				skip $ a1
				size <- getInt32le
				--skip $ a2 - a1
				skip $ a2 - a1 - 4
				subChSize <- getInt32le
				--skip $ a2 + 12
				audioFormat <- getInt16le
				--skip $ a2 + 16
				numCHans <- getInt16le
				--skip $ a2 + 20
				sampRate <- getInt32le
				--skip $ a2 + 24
				bRAte <- getInt32le
				--skip $ a2 + 28
				blockA <- getInt16le
				--skip $ a2 + 32
				biPeSamp <- getInt16le
				cbSize <- getInt16le
				skip $ a3 - a2 - 4 - 2 - 2 - 4 - 4 - 2 - 2 -2
				sizeData <- getInt32le
				lsit <- decoderIssue
				return $ snd $ foldr (\(a1,b1) (a2,b2) -> ("empty", Description a1 b1 b2) ) ("body",Body path $ A.fromList Seq lsit)
						$ zip ["ChunkSize", "SubchunkSize", "AudioFormat", "NumChannels", "SampleRate", "ByteRate", "BlockAlign","BitsPerSample","CbSize","SizeData"] 
								[size, subChSize, convertTo32 audioFormat,convertTo32 numCHans, sampRate, bRAte, convertTo32 blockA, convertTo32 biPeSamp, convertTo32 cbSize,sizeData]
		where
			decoderIssue = let deco =  do
										empty <- isEmpty
										if empty
										 	then return []
											else do
												digital <- Sampling <$> getInt8 <*> getInt8 <*> getInt8
												deco >>= \x -> return $ ((fromIntegral $ (one digital)*1000000 + (two digital)*1000 + (three digital) :: Double) : x) 
						   in deco
			convertTo32 :: Int16 -> Int32
			convertTo32 num = fromIntegral num :: Int32		

getSoundFolder :: FilePath -> IO [WaveLifeForm (A.Array U Ix1 Double )]
getSoundFolder root = do
					rootItems <- listDirectory root
					wFiles <- return $ (\x -> toFormEmpty $ (++) ((++) root "/")  x) <$> rootItems
					tampl <- getTamplWaveF $ getPath $ head wFiles
					contents <-sequence $ map (\ x -> singleThread x tampl) wFiles
					return contents 
				where 
				singleThread :: WaveLifeForm (A.Array U Ix1 Double ) -> (Int, Int, Int) -> IO (WaveLifeForm (A.Array U Ix1 Double ))	
				singleThread c w = do 
										handl <- openBinaryFile (getPath c) ReadMode
										hSetBinaryMode handl True
										fullFile <- B.hGetContents handl
										return $ runGet (getWaveTitle w c) $ fromStrict fullFile								  	
			
				getTamplWaveF :: WPath -> IO (Int, Int, Int) 
				getTamplWaveF path = do 
								handl <- openBinaryFile path ReadMode
								hSetBinaryMode handl True
								fullFile <-B.hGetContents handl
								waveAttr <- let attr = fromString "RIFF" :: B.ByteString in return $ (+) 4 $ B.length $ fst $ B.breakSubstring attr fullFile
								fmlAttr <- let attr = fromString "fmt " :: B.ByteString in return $ (+) 4 $ B.length $ fst $ B.breakSubstring attr fullFile
								dataAttr <- let attr = fromString "data" :: B.ByteString in return $ (+) 4 $ B.length $ fst $ B.breakSubstring attr fullFile
								return (waveAttr, fmlAttr, dataAttr)
				