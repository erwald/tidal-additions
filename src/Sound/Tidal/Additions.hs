module Sound.Tidal.Additions
( phaseShift
, inScale
, harmonized
, inversed
, octShift
, toBass
, withBass
, select
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

select :: Parseable a => Num a => Pattern a -> Pattern Int -> Pattern a
select harmonyP selectP = (\idx xs -> xs !!! idx + 12 * octave idx xs) <$> selectP <*> groupByTime' harmonyP
  where octave idx xs = fromIntegral $ idx `div` length xs

groupByTime' :: Pattern a -> Pattern [a]
groupByTime' p = Pattern $ \(s, e) -> groupByTime $ segment' $ arc p (s, e)

arp :: Num a => Time -> [Int] -> [[a]] -> Pattern a -> Pattern Int -> Pattern a
arp noteDur arp chords = arpenchord noteDur expandedChords
  where expandedChords = map (expandChord arp) chords

expandChord :: Num a => [Int] -> [a] -> [a]
expandChord selections chord = map select selections
  where notesInChord = length chord
        octave idx = fromIntegral $ idx `div` notesInChord
        select idx = chord !!! idx + octave idx * 7

arpenchord :: Num a => Time -> [[a]] -> Pattern a -> Pattern Int -> Pattern a
arpenchord noteDur chords transP chordP = flatpat' noteDur $ Chords.chordate chords <$> transP <*> chordP

flatpat' :: Num a => Time -> Pattern [a] -> Pattern a
flatpat' noteDur p = stack [rotR (noteDur * fromIntegral i) $ unMaybe $ fmap (`maybeInd` i) p | i <- [0..24]]
  where maybeInd xs i | i < length xs = Just $ xs !!! i
                      | otherwise = Nothing
        unMaybe = (fromJust <$>) . filterValues isJust
