module Text.Lipsum.Gen (
  text, sizedText,
  paragraph, sizedParagraph,
  sentence, sizedSentence,
  word
  )where

import Data.Char
import Data.List
import Test.QuickCheck
import Text.Lipsum.Gen.Initials
import Text.Lipsum.Gen.Intermediates
import Text.Lipsum.Gen.Finals

-- | Generates a text of random length. 
text :: Gen String
text = sized sizedText

-- | Generates a text consisting of a number of paragraphs separated by two 
--   newlines. 
sizedText :: Int -> Gen String
sizedText size = do
  ss <- resize size $ listOf1 paragraph
  return $ intercalate "\n\n" ss

-- | Generates a paragraph of random length. 
paragraph :: Gen String
paragraph = sized sizedParagraph

-- | Generates a paragraph, consisting of a number of sentences (controlled by 
--   the size parameter), separated by spaces. 
sizedParagraph :: Int -> Gen String
sizedParagraph size = do
  ss <- resize size $ listOf1 sentence
  return $ intercalate " " ss

-- | Generates a sentence of random length. 
sentence :: Gen String
sentence = sized sizedSentence

-- | Generates a sentence with length based on the size parameter, 
--   with puncuation (;,.?!). 
sizedSentence :: Int -> Gen String 
sizedSentence size = do
    ws <- resize size $ listOf1 word
    ss <- infiniteListOf separator
    t  <- terminator
    let (c:cs) = concat $ separate ws ss  
    return $ (toUpper c:cs) ++ t 
      where
        separate (w:[]) _ = [w]
        separate (w:ws) (s:ss) = w:s:(separate ws ss) 

separator :: Gen String 
separator = frequency [
    (6, space)
  , (1, symbol)
  ]
  where
    space = return " "
    symbol = frequency [
        (5, return ", ")
      , (1, return "; ")
      ]

terminator :: Gen String 
terminator = frequency [
    (5, return ".")
  , (1, return "!")
  , (1, return "?")
  ]

-- | Generates a word.
word :: Gen String
word = frequency [
    (4, initials),
    (7, (bigword 2)),
    (2, (bigword 4)),
    (1, (bigword 7))
    ]
  where
    bigword s = do
        start <- initials
        mids <- resize s $ listOf intermediates
        end <- finals
        return $ concat (start:(mids ++ [end]))


