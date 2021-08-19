{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc,
      play,
      sint
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import Data.List.Split
import Data.Text(pack,unpack,split,Text)
import System.Process
import Text.Printf
import Haskore.Composition.Drum
import Haskore.Interface.MIDI 
import Haskore.Music.GeneralMIDI as HMIDI
import qualified Haskore.Music as HMusic
import qualified Haskore.Music.Rhythmic as HMR
import qualified Haskore.Melody as HMelody
import Haskore.Basic.Duration as HBD
import Haskore.Basic.Pitch as HBP
import Haskore.Example.Guitar
import Haskore.Interface.MIDI.Render(fileFromGeneralMIDIMusic, midi)
import Medium.Controlled.List as MCL
import NoteAnalyse
import Dismantling


--import OpenAL
--import Sound

-- params
type Samples = Float
type Seconds = Float
type Hz = Float

tsvParser :: String -> String
tsvParser line = (++) "Octave Note PitchVolume PitchLenght Spacing \n" 
						$ foldr1 (++) [ foldr ((++) . (++) " ") "\n" $ Data.List.Split.splitOneOf "onvl" row  | row <-  Data.List.Split.splitOneOf "/" line ]

outputFilePath :: FilePath 
outputFilePath = "sinWave.bin"

volume :: Float
volume = 0.1

sampleRate :: Samples
sampleRate = 48000

duration :: Seconds
duration = 0.5

herz :: Int -> Hz
herz 1 = 440.00
herz 2 = 493.88
herz 3 = 523.25
herz 4 = 587.32
herz 5 = 659.26
herz 6 = 698.46
herz 7 = 784.00
--


wave :: Int -> [Float] 
wave note = map (* volume) $ map sin $ map (* step) [0.0 .. sampleRate * duration]
	where step = ((herz note) * 2 * pi) / sampleRate

melody :: [Int] -> [Float]
melody  line = wave =<< line

-- melody1 =  foldl ++  map wave [1,2,3]
 
someFunc :: FilePath -> [Int] -> IO ()
someFunc path not = B.writeFile outputFilePath $ B.toLazyByteString $ fold $ map B.floatLE $ melody not

play :: [Int] -> IO ()
play not = do
	someFunc outputFilePath not
	_ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath 
	return ()


-- flute representation inline:
fullFlutesScope :: [HBP.T]
fullFlutesScope = [(x,y) | x <- [1,2,3,4], y <- [Cf,C,Cs,Df,D,Ds,Ef,E,Es,Ff,F,Fs,Gf,G,Gs,Af,A,As,Bf,B,Bs]]

preformClass :: String -> HBP.Class
preformClass "1" = Cf 
preformClass "2" = C
preformClass "3" = Cs
preformClass "4" = Df
preformClass "5" = D
preformClass "6" = Ds
preformClass "7" = Ef
preformClass "8" = E
preformClass "9" = Es
preformClass "10" = Ff
preformClass "11" = F
preformClass "12" = Fs
preformClass "13" = Gf
preformClass "14" = G
preformClass "15" = Gs
preformClass "16" = Af
preformClass "17" = A
preformClass "18" = As
preformClass "19" = Bf
preformClass "20" = B
preformClass "21" = Bs
preformClass _ = Bs

preformNoteLen :: String -> HBD.T
preformNoteLen "1" = bn
preformNoteLen "2" = wn
preformNoteLen "3" = hn
preformNoteLen "4" = qn
preformNoteLen "5" = en
preformNoteLen "6" = sn
preformNoteLen "7" = tn
preformNoteLen "8" = sfn
preformNoteLen "9" = dwn
preformNoteLen "10" = dhn
preformNoteLen "11" = dqn
preformNoteLen "12" = den
preformNoteLen "13" = dsn
preformNoteLen "14" = dtn
preformNoteLen "15" = ddhn
preformNoteLen "16" = ddqn
preformNoteLen "17" = dden
preformNoteLen _ = wn

plsSound  =   MCL.Primitive atom
	where
		instr = Flute
		pitch = (3,C)
		noteBdy = HMR.Tone instr pitch
		note = flip HMR.Note noteBdy $ toRational 1.1 
		atom = HMusic.Atom bn $ Just note

ppSound =  MCL.Primitive atom
	where
		instr = Flute
		pitch = (1,C)
		noteBdy = HMR.Tone instr pitch
		note = flip HMR.Note noteBdy $ toRational 1.1 
		atom = HMusic.Atom bn $ Just note		

decodeF :: String -> [HMIDI.T]
decodeF sequ@(x:xs) = let linelist = Data.Text.split (\x -> x == 'o' || x == 'n' || x == 'v' || x == 'l' || x == '/') $ pack sequ in
	 let 
	 	instr = Flute
	 	keys = zip ( zip3 [ unpack x2 |	(x1, x2) <- zip [1..] $ tail linelist, elem x1 [1,6..(length $ tail linelist)]]
			 			  [ unpack y2 |	(y1, y2) <- zip [1..] $ tail linelist, elem y1 [2,7..(length $ tail linelist)]]
			 			  [ unpack z2 |	(z1, z2) <- zip [1..] $ tail linelist, elem z1 [4,9..(length $ tail linelist)]])
			 			  [ unpack v2 |	(v1, v2) <- zip [1..] $ tail linelist, elem v1 [5,10..(length $ tail linelist)]]
	 	noteBdys = keys >>= (\((a, b, c), d) -> [HMR.Tone instr (read b :: Int, preformClass c)])  
		notes =  [flip HMR.Note oneNoteBody $ toRational $ read $ b | (((x1, x2, x3), b), oneNoteBody) <- zip keys noteBdys]
		atoms = zipWith (\not ((a,b,c),d) -> HMusic.Atom (preformNoteLen d) (Just not)) notes keys 
		--atoms = [ HMusic.Atom (preformNoteLen x4) (Just note) | note <- notes, (x1,x2,x3,x4) <- pitches ]		
	 in   fmap (\x -> MCL.Primitive x)  atoms  

sint :: IO ()
sint = do  
	putStrLn "First stage!\nFlute MIDI interpritation. Command music line:[onvl...]\n  - o(1-4) --- octave\n  - n(1-21) --- every pitch note\n  - v(rational number) --- pitch volume\n  - l(1-17) --- pitch lenght\n  - /(1-4) --- spacing"
	sequence <- getLine
	writeFile "dataTable.tsv" $ tsvParser sequence
	--fef <- return $ decodeF sequence
	--toFile "kuki.midi" $ midi $ foldl (+:+) plsSound fef

	putStrLn "Enough"
	--a <- getLine
	--return ()
	--first "dataTable.tsv"
	--getSoundFolder "D:/code/Haskell/bin/scatM/originS"
	
	play =<< (noteLine [])
		where noteLine x = do
			not <- fmap read $ getLine :: IO Int
			if not == 0 then return x else noteLine (x ++ [not])
			

