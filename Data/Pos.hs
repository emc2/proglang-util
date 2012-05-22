{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.Pos(
       Pos,
       Position,
       filename,
       line,
       col,
       offset,
       startline,
       startcol,
       startoff,
       endline,
       endcol,
       endoff,
       range,
       point,
       advance,
       newline,
       merge,
       split,
       restart,
       internal,
       pos
       ) where

import Data.Foldable
import Data.Hash
import Text.Format

data Pos = Pos { filename :: !String, startline :: !Int, startcol :: !Int,
                 startoff :: !Int, endline :: !Int, endcol :: !Int,
                 endoff :: !Int }
  deriving Eq

class Position a where
  pos :: a -> Pos

line :: Pos -> Int
line = startline

col :: Pos -> Int
col = startcol

offset :: Pos -> Int
offset = startoff

range :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Pos
range fname sline scol soff eline ecol eoff =
  Pos { filename = fname, startline = sline, startcol = scol, startoff = soff,
        endline = eline, endcol = ecol, endoff = eoff }

point :: String -> Int -> Int -> Int -> Pos
point fname line col off =
  Pos { filename = fname, startline = line, startcol = col, startoff = off,
        endline = line, endcol = col, endoff = off }

advance :: Int -> Pos -> Pos
advance off pos =
  let
    ecol = (endcol pos)
    eoff = (endoff pos)
  in
    pos { endcol = ecol + off, endoff = eoff + off }

newline :: Pos -> Pos
newline pos =
  let
    eline = (endline pos)
    eoff = (endoff pos)
  in
    pos { endline = eline + 1, endcol = 1, endoff = eoff + 1 }

merge :: Pos -> Pos -> Pos
merge start end =
  range (filename start) (startline start) (startcol start) (startoff start)
        (endline end) (endcol end) (endoff end)

restart :: Pos -> Pos
restart pos =
  let
    eline = (endline pos)
    ecol = (endcol pos)
    eoff = (endoff pos)
  in
    pos { startline = eline, startcol = ecol, startoff = eoff }

split :: Pos -> (Pos, Pos)
split pos =
  let
    eline = (endline pos)
    ecol = (endcol pos)
    eoff = (endoff pos)
  in
    (pos { endcol = ecol - 1, endoff = eoff - 1 },
     pos { startline = eline, startcol = ecol, startoff = eoff })

internal :: String -> Pos
internal fname = Pos { filename = fname, startline = -1, startcol = -1,
                       startoff = -1, endline = -1, endcol = -1,
                       endoff = -1 }

instance Position p => Position [p] where
  pos l = merge (pos (head l)) (pos (last l))

instance Hashable Pos where
  hash (Pos { startline = startline, startcol = startcol, startoff = startoff,
              endline = endline, endcol = endcol, endoff = endoff,
              filename = filename }) =
    hashInt startline `combine` hashInt startcol `combine`
    hashInt startoff `combine` hashInt endline `combine`
    hashInt endcol `combine` hashInt endoff `combine`
    hashFoldable filename

instance Ord Pos where
  compare p1 p2 =
    case compare (filename p1) (filename p2) of
      EQ -> case compare (startline p1) (startline p2) of
        EQ -> case compare (startcol p1) (startcol p2) of
          EQ -> case compare (endline p1) (endcol p2) of
            EQ -> compare (endcol p1) (endcol p2)
            out -> out
          out -> out
        out -> out
      out -> out

instance Format Pos where
  format p @ Pos { filename = fname, startline = -1, startcol = -1,
                   endline = -1, endcol = -1 } = text fname
  format p @ Pos { filename = fname, startline = sline, startcol = scol,
                   endline = eline, endcol = ecol }
    | sline == eline && scol == ecol =
      fname <+> sline <> "." <> scol
    | sline == eline =
      fname <+> sline <> "." <> scol <> "-" <> ecol
    | otherwise = fname <+> sline <> "." <> scol <> "-" <> eline <> "." <> ecol

instance Show Pos where
  show = show . format