{-# LANGUAGE TypeSynonymInstances #-}
module Text.Format(
       Format,
       Doc,
       (<>),
       (<+>),
       ($$),
       ($+$),
       format,
       empty,
       semi,
       comma,
       colon,
       equals,
       space,
       lparen,
       rparen,
       lbrack,
       rbrack,
       lbrace,
       rbrace,
       text,
       parens,
       brackets,
       braces,
       quotes,
       doubleQuotes,
       hcat,
       hsep,
       vcat,
       cat,
       sep,
       fcat,
       fsep,
       nest,
       hang,
       punctuate,
       block,
       parenBlock,
       braceBlock,
       bracketBlock,
       parenList,
       braceList,
       bracketList
       ) where

import Data.Int
import Data.Word
import Text.PrettyPrint(Doc)

import qualified Text.PrettyPrint as PP

empty = PP.empty
semi = PP.semi
comma = PP.comma
colon = PP.colon
equals = PP.equals
space = PP.space
lparen = PP.lparen
rparen = PP.rparen
lbrack = PP.lbrack
rbrack = PP.rbrack
lbrace = PP.lbrace
rbrace = PP.rbrace
text = PP.text

parens :: Format f => f -> Doc
parens = PP.parens . format

brackets :: Format f => f -> Doc
brackets = PP.brackets . format

braces :: Format f => f -> Doc
braces = PP.braces . format

quotes :: Format f => f -> Doc
quotes = PP.quotes . format

doubleQuotes :: Format f => f -> Doc
doubleQuotes = PP.doubleQuotes . format

(<>) :: (Format f, Format g) => f -> g -> Doc
f <> g = (format f) PP.<> (format g)

(<+>) :: (Format f, Format g) => f -> g -> Doc
f <+> g = (format f) PP.<+> (format g)

($$) :: (Format f, Format g) => f -> g -> Doc
f $$ g = (format f) PP.$$ (format g)

($+$) :: (Format f, Format g) => f -> g -> Doc
f $+$ g = (format f) PP.$+$ (format g)

hcat :: Format f => [f] -> Doc
hcat = PP.hcat . map format

hsep :: Format f => [f] -> Doc
hsep = PP.hsep . map format

vcat :: Format f => [f] -> Doc
vcat = PP.vcat . map format

cat :: Format f => [f] -> Doc
cat = PP.cat . map format

sep :: Format f => [f] -> Doc
sep = PP.sep . map format

fcat :: Format f => [f] -> Doc
fcat = PP.fcat . map format

fsep :: Format f => [f] -> Doc
fsep = PP.fsep . map format

nest :: Format f => Int -> f -> Doc
nest n = PP.nest n . format

hang :: (Format f, Format g) => f -> Int -> g -> Doc
hang f n = PP.hang (format f) n . format

punctuate :: (Format f, Format g) => f -> [g] -> [Doc]
punctuate f = PP.punctuate (format f) . map format

block :: (Format f, Format g, Format h) =>
         Int -> f -> g -> h -> Doc
block lvl head body tail =
  sep [ hang head lvl body, format tail ]

parenBlock :: (Format f, Format g) => f -> [g] -> Doc
parenBlock head body =
  block 2 (head <+> lparen) (sep body) rparen

bracketBlock :: (Format f, Format g) => f -> [g] -> Doc
bracketBlock head body =
  block 2 (head <+> lbrack) (sep body) rbrack

braceBlock :: (Format f, Format g) => f -> [g] -> Doc
braceBlock head body =
  block 2 (head <+> lbrace) (sep body) rbrace

parenList :: (Format f, Format g) => f -> [g] -> Doc
parenList head body =
  head <> lparen <> (nest 2 (sep (punctuate comma body))) <> rparen

bracketList :: (Format f, Format g) => f -> [g] -> Doc
bracketList head body =
  head <> lbrack <> (nest 2 (sep (punctuate comma body))) <> rbrack

braceList :: (Format f, Format g) => f -> [g] -> Doc
braceList head body =
  head <> lbrack <> (nest 2 (sep (punctuate comma body))) <> rbrack

class Format f where
  format :: f -> Doc

instance Format Doc where
  format d = d

instance Format String where
  format s = PP.text s

instance Format Bool where
  format i = PP.text (show i)

instance Format Int where
  format i = PP.int i

instance Format Word where
  format i = PP.text (show i)

instance Format Int8 where
  format i = PP.text (show i)

instance Format Int16 where
  format i = PP.text (show i)

instance Format Int32 where
  format i = PP.text (show i)

instance Format Int64 where
  format i = PP.text (show i)

instance Format Word8 where
  format i = PP.text (show i)

instance Format Word16 where
  format i = PP.text (show i)

instance Format Word32 where
  format i = PP.text (show i)

instance Format Word64 where
  format i = PP.text (show i)

instance Format Integer where
  format i = PP.integer i

instance Format Char where
  format c = PP.char c

instance Format Float where
  format f = PP.float f

instance Format Double where
  format d = PP.double d

instance Format Rational where
  format r = PP.rational r