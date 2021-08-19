{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NoteAnalyse
    ( imagination
    , imagination2 
    , first
    ) where

import Graphics.Vega.VegaLite
import Data.Text(Text)

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


imagination :: String -> IO ()
imagination path  = toHtmlFile path stripPlot

imagination2 :: String -> IO ()
imagination2 path = toHtmlFile path musicHisto

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
