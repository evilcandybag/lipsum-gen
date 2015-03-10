module Text.Lipsum.Gen where

import Test.QuickCheck
import Text.Lipsum.Gen.Initials
import Text.Lipsum.Gen.Intermediates
import Text.Lipsum.Gen.Finals

sentence :: Gen String 
sentence = do
    ws <- listOf1 word
    ss <- infiniteListOf separator
    t  <- terminator  
    return $ (concat $ separate ws ss) ++ t 
      where
        separate (w:[]) _ = [w]
        separate (w:ws) (s:ss) = w:s:(separate ws ss) 

separator :: Gen String 
separator = frequency [
    (4, space)
  , (1, symbol)
  ]
  where
    space = return " "
    symbol = frequency [
        (4, return ", ")
      , (1, return "; ")
      ]

terminator :: Gen String 
terminator = frequency [
    (4, return ".")
  , (1, return "!")
  , (1, return "?")
  ]

word :: Gen String
word = frequency [
    (3, initials),
    (6, (bigword 2)),
    (1, (bigword 7))
    ]
  where
    bigword s = do
        start <- initials
        mids <- resize s $ listOf intermediates
        end <- finals
        return $ concat (start:(mids ++ [end]))

