-- module SplitXML.SplitXML
module Main
where

import Text.XML.HXT.Arrow
import qualified Data.Tree.NTree.TypeDefs as NTree
import qualified Text.XML.HXT.DOM.TypeDefs as XNode
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.QualifiedName as QN

import System.IO
import System.IO.Unsafe
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe

{-
main :: IO ()
main = do
  argv <- getArgs -- argv!!0 should contain xml file name
  if length argv /= 1
     then do print "You need to supply the filename as first argument, and nothing more."
             exitWith (ExitFailure (-1))
     else do let conf = [(a_validate, "0")]
             xml <- runX (readDocument conf (argv!!0))
             let xml' = stripMarkup $ head xml
             print xml'
             exitWith ExitSuccess
-}

main :: IO ()
main = do
  runX test
  putStrLn "Done!!"

test :: IOSArrow XmlTree XmlTree
test = readDocument [(a_validate, v_0)] "../../data/dblp_small.xml" >>>
       --processChildren (deep $ setNode (XNode.XText "") `when` isText ) >>>
       -- putXmlTree "test.before.tmp" >>>
       processChildren  (processTopDown $ arr fixNode) >>>
       -- putXmlTree "" >>>
       writeDocument [] "../../data/dblp_small.meta" -- "-" means stdout


dataFolder = "../../data/dblp_small.data/"
dataFile = ""


fixNode :: XmlTree -> XmlTree
fixNode (NTree.NTree n ts) =
    NTree.NTree
             (case n of
                XNode.XText _ -> XN.setText "" n
                XNode.XTag _ _ -> (unsafePerformIO $ sequence_ $ map (saveTextChilds (QN.localPart $ fromJust $ XN.getElemName n)) ts) `seq`
                                  XN.changeAttrl (map rmAttrValue) n
                n -> n)
             ts
                 where
                   saveTextChilds elmName (NTree.NTree n@(XNode.XText _) _) = appendFile (dataFolder ++ elmName) (fromJust $ XN.getText n)
                   saveTextChilds _ s = return ()


rmAttrValue :: XmlTree -> XmlTree
rmAttrValue (NTree.NTree n ts) =
    (unsafePerformIO $ appendFile (dataFolder ++ (QN.localPart $ fromJust $ XN.getAttrName n)) (concatMap getText ts)) `seq`
    NTree.NTree n []
        where
          getText (NTree.NTree n ts) = fromJust $ XN.getText n




{-
    isText `orElse`
               (isElem `guards`
                           (neg (getChildren `notContaining` isText) `guards`
                                                               this))
-}
--isText `orElse` (isElem `containing` (isText))

--isElem `guards` (changeChildren onlyTextChildren)



--modify = arr stripMarkup
