module DTDParser.ProcessDTD 
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types

import qualified Data.Map as Map

import DTDParser.TypeDef




processDTD :: DocTypeDecl -> ElmAttEntMap
processDTD (DTD name extId mkupDeclLst) = loop mkupDeclLst (Map.empty, Map.empty, Map.empty)
    where
      loop [] (elmMap, attMap, entMap) = (elmMap, attMap, entMap)
      loop (x:xs) (elmMap, attMap, entMap) = loop xs (processMkupDecl x (elmMap, attMap, entMap))

processMkupDecl :: MarkupDecl -> ElmAttEntMap -> ElmAttEntMap
processMkupDecl elem@(Element (ElementDecl name cntSpec)) (elmMap, attMap, entMap) =
    case Map.lookup name elmMap of
      Just _ ->  error $ "Element: " ++ name ++ " was already in the element map. Aborting!"
      Nothing -> (Map.insert (show name) elem elmMap, attMap, entMap)

processMkupDecl att@(AttList (AttListDecl name attDefs) ) (elmMap, attMap, entMap) =
    case Map.lookup name attMap of 
      Just _ -> error $ "AttList: " ++ name ++ " was already in the attribute list map. Aborting!"
      Nothing -> (elmMap, Map.insert (show name) att attMap, entMap)

processMkupDecl (Entity entDecl) (elmMap, attMap, entMap) = (elmMap, attMap, entMap)
processMkupDecl (Notation _) (elmMap, attMap, entMap) = (elmMap, attMap, entMap)
processMkupDecl (MarkupMisc _) (elmMap, attMap, entMap) = (elmMap, attMap, entMap)





processContentSpec (EMPTY) = error "EMPTY - NOT IMP!!!"
processContentSpec (ANY) = error "ANY -  NOT IMP!!!"
processContentSpec (Mixed mix) = processMixed mix
    where
      processMixed (PCDATA) =  ["#PCDATA"] -- only #PCDATA
      processMixed (PCDATAplus nameLst) = ("#PCDATA" : nameLst) -- #PCDATA alternated with the name list

processContentSpec (ContentSpec cp) = processCP cp
    where
      -- modif holds the information of *, alternation etc..
      processCP (TagName name modif) = [name]
      processCP (Choice cps modif) = "Choise" : loop cps
      processCP (Seq cps modif) = error "Not Implemented" --loop cps

      loop [] = []
      loop (cp:cps) = processCP cp ++ loop cps


entityDecl (EntityGEDecl geDecl) = putStr "(GEDecl) .. NOTIMPL"
entityDecl (EntityPEDecl peDecl) = putStr "(PEDecl) " >> entityPEDecl peDecl

entityPEDecl (PEDecl name peDef) = putStr (name ++ " ") >> entityPEDef peDef

entityPEDef (PEDefEntityValue entValue) = putStr "(PEDefEntVal) " >> entityPEDefVal entValue
entityPEDef (PEDefExternalID ectID) = putStr "(PEDefExtID) .. NOTIMPL"

entityPEDefVal (EntityValue evs) = entityValues evs

entityValues [] = return ()
entityValues (ev:evs) = putStr (show ev) >> entityValues evs
