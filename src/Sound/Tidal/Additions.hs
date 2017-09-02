module Sound.Tidal.Additions
( phaseShift
, inScale
, harmonized
, inversed
, octShift
, toBass
, withBass
, select
, BrokenChord (..)
, arp
) where

import Data.Maybe
import Sound.Tidal.Context
import Sound.Tidal.Utils
import qualified Sound.Tidal.Chords as Chords
import qualified Sound.Tidal.Scales as Scales

{- |
@toScale@ lets you turn a pattern of notes within a scale (expressed as a list)
to note numbers. For example `toScale [0, 4, 7] 0 "0 1 2 3"` will turn into the
pattern `"0 4 7 12"`. This assumes your scale fits within an octave.

The second argument, @transposition@, allows you to transpose the notes _after_
they have been converted into the given scale.
-}
inScale :: [Int] -> Int -> Pattern Int -> Pattern Int
inScale scale transposition = fmap noteInScale
  where octave x = x `div` length scale
        noteInScale x = (scale !!! x) + 12 * octave x + transposition

{-|
Gradually and cyclically shifts the phase of a pattern.

@shifts@ denotes how many times the pattern should shift before returning to its
original form (and is thus also the inverse of the shift's duration).

@shiftRepeats@ describes how many cycles the pattern should repeat between
shifts.
-}
phaseShift :: Time -> Time -> Pattern a -> Pattern a
phaseShift shifts shiftRepeats = ((slow slowP (run shiftsP) / shiftsP) ~>)
  where shiftsP = return shifts
        slowP = return (shifts * shiftRepeats)

{-|
Superimposes a harmony (transposed by the interval @n@) on top of the given
pattern.
-}
harmonized :: Num a => a -> Pattern a -> Pattern a
harmonized n = superimpose $ fmap (+ n)

{-|
Returns the given pattern with the sign of its notes changed.
-}
inversed :: Num a => Pattern a -> Pattern a
inversed = fmap (* (-1))

{-|
Returns the given pattern with its notes transposed by @octaveP@ octaves.
-}
octShift :: Num a => Pattern a -> Pattern a -> Pattern a
octShift octaveP p = (+) <$> p <*> ((* 12) <$> octaveP)

{-|
Transforms the given pattern into a bassline (reusing some of its notes).
-}
toBass :: Num a => Rational -> Pattern a -> Pattern a
toBass cycles p = fast cyclesP $ stack [rotR (i/cycles) $ octShift (-3) $ zoom (zoomArc i) p | i <- [0..cycles-1]]
  where cyclesP = return cycles
        zoomArc i = (i/cycles, (i/cycles) + 1/4/cycles)

{-|
Same as @toBass@, but superimposes the base over the given pattern.
-}
withBass :: Num a => Rational -> Pattern a -> Pattern a
withBass = superimpose . toBass

{-|
Takes a pattern of harmonies (chords), @harmonyP@, and a pattern of indices
(selections), @selectP@, and returns picking notes from the harmony using the
indices. Keeps the structure of the latter pattern.
-}
select :: Parseable a => Num a => Pattern a -> Pattern Int -> Pattern a
select harmonyP selectP = selectNote <$> selectP <*> groupByTime' harmonyP
  where octave idx xs = fromIntegral $ idx `div` length xs
        selectNote idx xs = xs !!! idx + 12 * octave idx xs

{-|
"Unflattens" a pattern @p@, taking all events occurring at the same time and
grouping them into lists.
-}
groupByTime' :: Pattern a -> Pattern [a]
groupByTime' p = Pattern $ \(s, e) -> groupByTime $ segment' $ arc p (s, e)

{-|
Represents the type of a broken chord (sometimes mistakenly referred to as an
arpeggio).
-}
data BrokenChord
  = Up Int
  | Down Int
  | UpDown Int
  | DownUp Int
  | Custom [Int]

{-|
Arpeggiates over a given harmony, @harmonyP@, according to a given "strategy"
@brokenChord@.
-}
arp :: (Num a, Parseable a) => Pattern a -> Ratio Integer -> BrokenChord -> Pattern a
arp harmonyP noteDuration brokenChord = select harmonyP arpP
  where speedUpFactor = 1 / noteDuration / brokenChordLength brokenChord
        arpP = fast (pure speedUpFactor) $ brokenChordToPattern brokenChord

brokenChordToPattern :: (Enum a, Num a) => BrokenChord -> Pattern a
brokenChordToPattern brokenChord =
  case brokenChord of Up n -> run $ fromIntegral n
                      Down n -> rev $ run $ fromIntegral n
                      UpDown n -> run (fromIntegral $ div n 2) `append` ((+ 1) <$> rev $ run $ fromIntegral $ div n 2)
                      DownUp n -> ((+ 1) <$> rev $ run $ fromIntegral $ div n 2) `append` run (fromIntegral $ div n 2)
                      Custom ns -> listToPat $ map fromIntegral ns

brokenChordLength :: Num a => BrokenChord -> a
brokenChordLength brokenChord = fromIntegral $
  case brokenChord of Up n -> n
                      Down n -> n
                      UpDown n -> n
                      DownUp n -> n
                      Custom ns -> length ns
