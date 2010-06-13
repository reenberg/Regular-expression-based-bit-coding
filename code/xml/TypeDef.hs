module TypeDef
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types

import qualified Data.Map as Map 

type ElmAttEntMap = (Map.Map String MarkupDecl, Map.Map String MarkupDecl, Map.Map String MarkupDecl)


-- Just for temporary fun and debug

instance Show MarkupDecl where
    show (Element (ElementDecl name cntSpec)) = "Element " ++ name ++ " " ++ show cntSpec
    show (AttList (AttListDecl name attDefs)) = "AttList: " ++ name ++ " " ++ show attDefs
--    show (Entity entDecl) = "Entity: "(ElementDecl name cntSpec) ++ show entDecl
--    show (Notation notDecl) = "Notation: " ++ show notDecl
--    show (MarkupMisc misc) = "MarkupMisc: " ++ show misc


instance Show ContentSpec where
    show EMPTY = "EMPTY"
    show ANY = "ANY"
    show (Mixed mix) = "Mixed: NOT IMPLEMENTED!!!!"
    show (ContentSpec cp) = "ContentSpec: " ++ show cp

instance Show AttDef where
    show (AttDef name _ _) = name



{-
-- Implementing show for some of the HT data types.

instance Show HT.Modifier where
    show HT.None = ""
    show HT.Query = "?"
    show HT.Star = "*"
    show HT.Plus = "+"

instance Show HT.CP where
    show (HT.TagName name modif) = show name ++ show modif

    show (HT.Choice cps modif) = "[" ++ loop cps ++ "]" ++ show modif
        where -- show choise/alternation wth '|'
          loop (cp1:cp2:cps) = show cp1 ++ "|" ++ loop (cp2:cps)
          loop (cp:cps) = show cp
          loop [] = ""

    show (HT.Seq cps modif) = "[" ++ loop cps ++ "]" ++ show modif
        where -- show sequence with ' '
          loop (cp1:cp2:cps) = show cp1 ++ " " ++ loop (cp2:cps)
          loop (cp:cps) = show cp
          loop [] = ""
-}          

