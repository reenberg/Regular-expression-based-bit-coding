module DTDParser.GenerateRegex
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types


import qualified Data.Map as Map

import DTDParser.TypeDef

import Regex (Regex (..))
import qualified Regex as Rx


generateRegex :: ElmMaybeeAttMap -> String -> Regex Char
generateRegex elmAttMap rootTagName =
    let 
        (elm, attMaybee) = elmAttMap Map.! rootTagName
    in
      generateElementRegex elmAttMap elm 


generateElementRegex :: ElmMaybeeAttMap -> ElementDecl -> Regex Char
generateElementRegex elmAttMap (ElementDecl name cntSpec) =
    let
        genRegex :: Regex Char -> Regex Char
        genRegex content = (Rx.Lit '<') 
                           :*: (Rx.string name)
                           :*: (generateAttributeRegex)
                           :*: (Rx.Lit '>')
                           :*: (content)
                           :*: (Rx.string $ "</" ++ name ++  ">")

        -- Needs MODIF !!!!!!
        processCP :: CP -> Regex Char
        processCP (TagName name modif) = generateRegex elmAttMap name
        processCP (Choice cps modif) = Rx.sum $ loop processCP cps
        processCP (Seq cps modif) = error "Not Implemented" --loop cps

        -- Parsed Character data.
        processMixed :: Mixed -> Regex Char
        processMixed (PCDATA) = Rx.pcdata       
        processMixed (PCDATAplus refs) = 
            checkForRecursiveDefinitions name refs `seq`
            Rx.sum (processMixed (PCDATA) : loop (generateElementRegex elmAttMap . getElementDecl) refs)


        loop :: (a -> Regex Char) -> [a] -> [Regex Char]
        loop _ [] = []
        loop f (cp:cps) = f cp : loop f cps        
                          

        generateAttributeRegex = Rx.E -- Regex empty word

        -- This is a poor check and should be expanded to be fully recursive all children of this element
        checkForRecursiveDefinitions name children =
            if (name `elem` children) 
              then error $ "The tag '" ++ name ++ "' contains itself as direct child. \nIt is thus impossible to generate a regex for this schema. Aborting!!!."
              else ()
            
        -- Lookup the name in the elm attribute map and pick the first of the two (the elementDecl)
        getElementDecl name = fst $ elmAttMap Map.! name

    in
      case cntSpec of 
        (EMPTY) -> error "CP EMPTY not implemented!!!"
        (ANY) -> error "CP ANY not implemented!!!"
        (Mixed mix) -> genRegex $ processMixed mix
        (ContentSpec cp) ->  genRegex $ processCP cp
       

    
    
