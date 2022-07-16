

module Main where

import Data.Maybe
import Parser
import Output
import DataStructures
import Data.Text

inputfunc=myrunparser  (pack "extern  function int cos(int x) ;      function int main ( ) {int y = 2; int y = ((8+9)+4);int x = ((5+3)+y) ; return cos(x)  }")

main :: IO ()
main = do 
    print   inputfunc

main2 :: IO ()
main2 = do 
    print (getStringOutput (fromJust (inputfunc) ))
main3 :: IO ()
main3 = do 
    writeFile "examplefile.lc"  (show(getStringOutput (fromJust (inputfunc) )))