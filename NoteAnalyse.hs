{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NoteAnalyse
    ( imagination2 
    , analyse
    , first
    , plotDigital
    , sFT
    , ko
    , si
    , multipliers
    , fFT
    ) where

import Dismantling
import Control.Monad
import System.Process.Typed
import qualified Data.ByteString as BS
import Data.ByteString.Internal(unpackChars)
import Graphics.Vega.VegaLite
import Data.Text(Text)
import Data.Complex
import qualified Data.Massiv.Array as A
import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Numeric
import Data.Massiv.Core
import Data.Massiv.Core.Index 

ko :: Fractional a => WaveLifeForm (A.Array U Ix1 Double) -> a
ko we  = (findCollection "SizeData" we) / (findCollection "SampleRate" we) 

si :: A.Array U Ix1 Double -> Int
si a =  unSz $ A.size a

confData :: Text -> Data
confData table = dataFromUrl table [TSV]

gaiaData =
  let addFormat n = (n, FoNumber)
      cols = [ "RA_ICRS", "DE_ICRS", "Gmag", "plx", "e_plx" ]
      opts = [ Parse (map addFormat cols) ]
  in dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" opts

stripPlot :: VegaLite
stripPlot = toVegaLite
    [ dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" [TSV]
    , mark Tick []
    , encoding (position X [ PName "plx", PmType Quantitative ] [])
    ]

musicHisto :: VegaLite
musicHisto = let enc = encoding 
							. position X [PName "plx", PmType Quantitative, PBin [], PAxis [AxTitle "octave"] ]
							. position Y [PAggregate Count, PmType Quantitative, PScale [SType ScLog] ]
			 in toVegaLite
				[ gaiaData
				, mark Bar [MFill "orange", MStroke "gray"]
				, enc []
				, height 300
				, title "Octave distribution" []
				]				


imagination2 :: String -> A.Array D Ix1 Double -> IO ()
imagination2 path wf = toHtmlFile path $ plotDigital $ Body "" wf

first :: Text -> VegaLite
first table =  toVegaLite [	dat
							, mark Line []
							, enc []
							, height 300
							, width 400]
		where 
			  dat = confData table
			  enc = encoding . position X [ PName "Quantity", PmType Quantitative, binning, axis]
			  				 . position Y [ PAggregate Count, PmType Quantitative]
			  				 . color [ MName "Cluster", MmType Nominal]
			  binning =  PBin [Step 1]
			  axis = PAxis [AxValues (Numbers [0,1 .. 5]) ]			 


-- integral sum 
sFT ::  Array U Ix1 Double -> Array D Ix1 Double
sFT mas  = A.map realPart $ A.foldlInner (+) 0 $ A.zipWith (*) sec ind
	where
		ind = A.makeArrayR D Seq (A.Sz ((unSz $ A.size mas) :. (unSz $ A.size mas))) $ \(y :. x) -> let [k,r,n] = map fromIntegral [y,x,(unSz $ A.size mas)] in (1/n :+ 0) * exp ( 0 :+ (- 2 * pi * k * r / n))
		sec = A.expandWithin Dim2 ( A.Sz1 (unSz $ A.size mas)) (\ a b -> a :+ 0) mas

-- fast fourier alghorirm. Cooley- Tukey for every number, non power of 2. 

fFT :: Array U Ix1 Double -> Array D Ix1 Double
fFT mas = A.flatten $ A.map realPart $ A.map ((1/(nN) :+ 0) *) $  sec $ A.computeAs U $ ind
	where	
		(n1, n2) = multipliers $ unSz $ A.size mas
		nA1 = fromIntegral n1 :: Double
		nA2 = fromIntegral n2 :: Double
		nN = fromIntegral $ n1 * n2 :: Double
		umas = A.computeAs U mas
		j = A.makeArrayR U Seq (A.Sz (n2 :. n1)) $ \ ( a :. b) -> umas A.! (n1*a + b)
		ind = A.foldlWithin Dim2 (+) (0 :+ 0) $ A.makeArrayR D Seq (A.Sz (n2 :> n2 :. n1 )) $ \(x :> y :. z) -> let [k0I,j1I,j0I] = map fromIntegral [x,y,z] in 
																							let vJ = j A.! y :. z
																								in 	(vJ :+ 0) * exp ( 0 :+ (- 2 * pi * j1I * k0I * nA1 / nN)) 
		sec :: Array U Ix2 (Complex Double) -> Array D Ix2 (Complex Double) 
		sec k = A.foldlWithin Dim1 (+) (0 :+ 0) $ A.makeArrayR D Seq (A.Sz (n1 :> n2 :. n2 )) $ \(x :> y :. z) -> let [k1I,k0I,j0I] = map fromIntegral [x,y,z] in 
																							let vK = k A.! y :. z 
																								in 	vK * exp ( 0 :+ (- 2 * pi * j0I * k1I * nA2 / nN)) * exp (0 :+ (- 2 * pi * j0I * k0I / nN))

multipliers :: Int -> (Int , Int)
multipliers doll = let fil q x = if x `mod` q == 0 then q : fil q (x `div` q) else []
					in (\ t -> (t , doll `div` t)) $ foldl ( \ a b -> if a * a > doll then a else a * b ) 1 $ fil 2 <> fil 3 <> fil 5 $ doll

analyse :: IO ()
analyse = putStrLn "Namefile(tsv-only):" >> getLine >>= BS.readFile >>= (\f -> forever $ do
			str1 <- getLine
			str2 <- getLine
			matX <- return $ toMatrix f
			putStrLn $ show matX
			toHtmlFile "mathH/lol.html" $ arrArgSelector matX (read str1 :: Int) str2
			--3 "-"
			--(read str1 :: Int) str2
			proc <- startProcess $ shell "cd mathH & lol.html"
			return () )
	where
		arrArgSelector :: A.Array P Ix2 Double -> Int -> String -> VegaLite
		arrArgSelector matX num "-" = let
									arY = sliceX matX $ num
									arX = A.makeVectorR D Seq (A.Sz1 $ lastDim . unSz $ A.size matX) $ \ x -> fromIntegral x
								in graphOut arX arY
		arrArgSelector matX num1 num2 = let 	
										arX = sliceX matX $ num1 
										arY = sliceX matX $ ( read num2 :: Int)
								in graphOut arX arY   
		graphOut :: A.Array D Ix1 Double -> A.Array D Ix1 Double -> VegaLite
		graphOut arX arY  = let 
									dat =  dataFromColumns []
									        . dataColumn "Density" (Numbers $ A.toList arX)
									        . dataColumn "Condensate" (Numbers $ A.toList arY)
									enc = encoding
									        . position X [ PName "Density", PmType Quantitative ]
									        . position Y [ PName "Condensate", PmType Quantitative ]
									        . color [ MName "Origin" ]
									bkg = background "rgba(0, 0, 0, 0.05)"        		
							   in toVegaLite [height 1000 , width 1000, bkg, dat [], mark Circle [MTooltip TTEncoding], enc [] ]

		toMatrix :: BS.ByteString -> A.Array P Ix2 Double
		toMatrix word = A.fromLists' Seq $ Prelude.filter (\x -> x /= []) $ map (\x -> map read $ words $ unpackChars x) $ BS.split 10 word 	
		sliceX :: A.Array P Ix2 Double -> Int -> A.Array D Ix1 Double
		sliceX	arr n =   (A.transpose arr) A.!> n
		 	
plotDigital :: WaveLifeForm (A.Array D Ix1 Double) -> VegaLite
plotDigital (Description a b c) = plotDigital c
plotDigital smpl@(Body path datt) =  let 
										dat =  dataFromColumns []
										        . dataColumn "Time" (Numbers $ take 2000 $ [1, 2 .. (fromIntegral $ si $ A.computeAs U datt)])
										        . dataColumn "Amplitude" (Numbers $ take 100 $ A.toList datt )
										enc = encoding
										        . position X [ PName "Time", PmType Quantitative ]
										        . position Y [ PName "Amplitude", PmType Quantitative ]
										        . color [ MName "Origin" ]
										bkg = background "rgba(0, 0, 0, 0.05)"        		
				 					in toVegaLite [height 1000 , width 1000, bkg, dat [], mark Circle [MTooltip TTEncoding], enc [] ]