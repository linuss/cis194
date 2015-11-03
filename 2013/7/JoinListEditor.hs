module Main where

import JoinList
import Editor
import Buffer
import Sized
import Scrabble

main = runEditor editor $ map fromString
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

