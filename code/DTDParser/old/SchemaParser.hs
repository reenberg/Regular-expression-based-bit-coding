module DTDparser
where

import Text.XML.HXT.Arrow
import qualified Data.Tree.NTree.TypeDefs as NTree
import qualified Text.XML.HXT.DOM.XmlNode as XN

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe



main :: IO()
main = do
  argv <- getArgs
  (al, xmlFileName, schemaFileName) <- cmdlineOpts argv
  schemaXmlTreeLst <- runX (parseSchemaFile al schemaFileName)                      
  print $ length schemaXmlTreeLst
--  processSchemaXmlTree $ head schemaXmlTreeLst
  if length schemaXmlTreeLst /= 1
     then print "Schema doesn't seem valid. There must be ONLY one root node."
          >> exitWith (ExitFailure (-1))
     else processRootXmlTree (head schemaXmlTreeLst)
         --processSchemaXmlTree (head schemaXmlTreeLst) ""
          >> exitWith ExitSuccess

cmdlineOpts :: [String] -> IO (Attributes, String, String)
cmdlineOpts argv = return ([(a_validate, "0")], argv!!0, argv!!1)

parseSchemaFile :: Attributes -> String -> IOSArrow XmlTree XmlTree
parseSchemaFile al schemaFileName =
    readDocument al schemaFileName >>>
    processTopDown isElem >>> -- Remove all elements that are not XTag (eg, XText
    getChildren               -- Get the actual schema file root elemtn (removing the HXT introduced root node)


processRootXmlTree (NTree.NTree xnode xtrees) =     
      if name == "schema"
      then processSchemaXmlTrees xtrees ""
      else print "The root node is not a 'schema' tag"
    where
      name = fromJust $ XN.getLocalPart xnode

      

processSchemaXmlTrees :: XmlTrees -> String -> IO ()
processSchemaXmlTrees [] _ = print ""
processSchemaXmlTrees (xt:xts) indent = 
    processSchemaXmlTree xt indent
    >> processSchemaXmlTrees xts indent

processSchemaXmlTree :: XmlTree -> String -> IO ()
processSchemaXmlTree xt@(NTree.NTree xnode xtrees) indent = 
    processSchemaXnode xnode indent
    >> processSchemaXmlTrees xtrees ("    " ++ indent)
                        

processSchemaXnode :: XNode -> String -> IO ()
processSchemaXnode xn@(XTag qname attrl) indent = print (indent ++ (getQname qname))
--      >> print (show (XTag qname xs))
--      >> print (show (XN.getAttrl(XTag qname xs))) 
--      >> printQname qname                                              
--num = length ( NTree.getChildren ((XTag qname xs))) 
--processSchemaXnode (XTag qname (x:xs)) = printQname qname 
processSchemaXnode _ _ = print "Not a XTag"


-- Getting attribute list
--XN.getAttrl(XTag qname xs)

getQname qname = (namePrefix qname) ++ " - " ++ (localPart qname) ++ " - " ++ (namespaceUri qname)

