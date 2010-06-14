module DTDParser.GenerateRegex
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types


import qualified Data.Map as Map

import DTDParser.TypeDef


generateRegex :: ElmMaybeeAttMap -> String -> String
generateRegex elmAttMap rootTagName =
    let 
        (elm, attMaybee) = elmAttMap Map.! rootTagName
    in
      generateElementRegex elmAttMap elm 


generateElementRegex :: ElmMaybeeAttMap -> ElementDecl -> String
generateElementRegex elmAttMap (ElementDecl name cntSpec) =
    let
        genRegex content = "<" ++ name 
                           ++ generateAttributeRegex
                           ++ ">"
                           ++ content
                           ++ "</" ++  name ++ ">"

        -- Needs MODIF !!!!!!
        processCP (TagName name modif) = generateRegex elmAttMap name
        processCP (Choice cps modif) = loop processCP cps
        processCP (Seq cps modif) = error "Not Implemented" --loop cps

        processMixed (PCDATA) = "E*"
        -- DTD contains inf recursion.. BASTARD (both sup and sub tags contaons * of itself (among others) )
        processMixed (PCDATAplus refs) = "" -- "E*" ++ " | " ++  loop (generateRegex elmAttMap) refs
                                         
        loop _ [] = ""
        loop f (cp:cps) = f cp ++ loop f cps        
                          
        generateAttributeRegex = ""
    in
      case cntSpec of 
        (EMPTY) -> error "CP EMPTY not implemented!!!"
        (ANY) -> error "CP ANY not implemented!!!"
        (Mixed mix) -> genRegex $ processMixed mix
        (ContentSpec cp) ->  genRegex $ processCP cp
       

    
    
