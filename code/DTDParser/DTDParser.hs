module DTDParser.DTDParser
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types

import Data.Maybe

import qualified Data.Map as Map

import System.Environment


import DTDParser.TypeDef
import DTDParser.ProcessDTD
import DTDParser.GenerateRegex

import qualified Regex as Rx


main = do
  argv <- getArgs
  if length argv /= 1 then print "dtd file needs to be passed as first argument"
     else do
       dtdStr <- readFile $ argv!!0
       -- The "foobar" value is mandatory, the name of the original file for
       -- error reporting. But we are just using dtdParse which crashes if
       -- errors are encountered.
       let (elemMap, attMap, entMap) = processDTD $ fromJust $ dtdParse "foobar" dtdStr      
       rootElem <- getRootElementName elemMap
       putStrLn $ "You have chosen '" ++ rootElem ++ "' as root element"
           -- Produce a new map that has all (element, Maybe attList)
       let elmAttMap = Map.mapWithKey (\k a -> (a, Map.lookup k attMap)) elemMap           
           regex = generateRegex elmAttMap rootElem
       putStr $ show regex
           
 
       --putStrLn $ "Regex: \n" ++ show regex
       --print elmAttMap

       --sequence_ z        
       --sequence_ $ map (print . show) z
       
getRootElementName elemMap = 
    do
      putStrLn "Type the name of the root element:"
      let keys = Map.keys elemMap
      sequence_ $ map putStrLn keys
      loop ()
          where
            loop () = do
              rootElem <- getLine             
              case Map.lookup rootElem elemMap of
                Just _ -> return rootElem
                Nothing -> do
                        putStrLn $ "you entered: '" ++ rootElem ++ "'. This element was not found in the list"
                        loop ()
      
