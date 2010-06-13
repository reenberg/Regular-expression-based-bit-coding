module DTDParser
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types

import Data.Maybe

import qualified Data.Map as Map

import System.Environment
import System.IO.Unsafe -- For performing unsafePerformIO debug prints.
--import System.Console.GetOpt

import DTDParser.TypeDef
import DTDParser.ProcessDTD


main = do
  argv <- getArgs
  if length argv /= 1 then print "dtd file needs to be passed as first argument"
     else do
       dtdStr <- readFile $ argv!!0
       -- The "foobar" value is mandatory, the name of the original file for
       -- error reporting. But we are just using dtdParse which crashes if
       -- errors are encountered.
       let (elemMap, attMap, entMap) = processDTD $ fromJust $ dtdParse "foobar" dtdStr 
           -- Produce a new map that has all (element, Maybe attList)
           elmAttMap = Map.mapWithKey (\k a -> (a, Map.lookup k attMap)) elemMap           
       print elmAttMap

       --sequence_ z        
       --sequence_ $ map (print . show) z
       
