module DTDParser.GenerateRegex
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types


import qualified Data.Map as Map

import DTDParser.TypeDef

import RegexExt (RegexExt((:+:), (:*:)))
import qualified  RegexExt as RxExt

xmlHeader = RxExt.prod
            [RxExt.Str "<?xml",
             RxExt.whitespaces,
             RxExt.Str "version=\"",
             RxExt.digit,
             RxExt.Lit '.',
             RxExt.digit,
             RxExt.Lit '"',
             RxExt.whitespaces,
             RxExt.Str "encoding=\"",
             RxExt.Star $ RxExt.alphanum RxExt.:+: RxExt.Lit '-',
             RxExt.Lit '"',
             RxExt.whitespaces,
             RxExt.Str "?>"]

xmlDocType = RxExt.prod
             [RxExt.Str "<!DOCTYPE",
              RxExt.Star $ RxExt.sum
                   [RxExt.alphanum,
                    RxExt.whitespace,
                    RxExt.Lit '"',
                    RxExt.Lit '.'],
              RxExt.Lit '>']

generateRootRegex :: ElmMaybeeAttMap -> String -> RegexExt
generateRootRegex elmAttMap rootTagName = RxExt.prod
                                          [RxExt.whitespaces,
                                           xmlHeader,
                                           RxExt.whitespaces,
                                           xmlDocType,
                                           -- RxExt.whitespaces,
                                           generateRegex elmAttMap rootTagName,
                                           RxExt.whitespaces
                                          ]

-- Takes the element name to process
generateRegex :: ElmMaybeeAttMap -> String -> RegexExt
generateRegex elmAttMap rootTagName = generateElementRegex elmAttMap $ elmAttMap Map.! rootTagName

-- Processes the given element and a possible attribute list.
generateElementRegex :: ElmMaybeeAttMap -> (ElementDecl, Maybe AttListDecl) -> RegexExt
generateElementRegex elmAttMap ((ElementDecl name cntSpec), attMaybee) =
    let
        genRegex :: RegexExt -> RegexExt
        genRegex content = RxExt.prod
                           [RxExt.whitespaces,
                            RxExt.Str $ "<" ++  name,
                            generateAttributeRegex attMaybee,
                            RxExt.whitespaces,
                            RxExt.Lit '>',
                            content,
                            RxExt.whitespaces,
                            RxExt.Str $ "</" ++ name,
                            RxExt.whitespaces,
                            RxExt.Lit '>']

        -- Needs MODIF !!!!!!
        processCP :: CP -> RegexExt
        processCP (TagName name modif) = modifierToRegex modif $ generateRegex elmAttMap name
        processCP (Choice cps modif) = modifierToRegex modif $ RxExt.sum $ loop processCP cps
        processCP (Seq cps modif) = error "Not Implemented" -- Should properly use Rx.prod

        modifierToRegex :: Modifier -> RegexExt -> RegexExt
        modifierToRegex modif reg =
            case modif of
              None -> reg                       -- | Just One
              Query -> RxExt.Query reg          -- | Zero Or One
              Star -> RxExt.Star reg            -- | Zero Or More
              Plus -> RxExt.Plus reg            -- | One Or More

        -- Parsed Character data.
        processMixed :: Mixed -> RegexExt
        processMixed (PCDATA) = RxExt.Star $ RxExt.CClass RxExt.PCDATA
        processMixed (PCDATAplus refs) = -- PCDATAplus is always with a STAR around
            checkForRecursiveDefinitions name refs `seq`
            RxExt.Star $ RxExt.sum $ (RxExt.CClass RxExt.PCDATA) : loop (generateElementRegex elmAttMap . getElementDecl) refs

        -- Lookup the name in the elm attribute map and pick the first of the two (the elementDecl)
        getElementDecl name = elmAttMap Map.! name

        generateAttributeRegex Nothing = RxExt.E -- Regex empty word
        -- name is the element name of which this attribute list belongs
        generateAttributeRegex (Just (AttListDecl name attDefs)) = RxExt.sum $ loop processAttDef attDefs

        processAttDef (AttDef name attType defaultDecl) = processDefaultDecl defaultDecl
                                                          $ RxExt.prod
                                                                [RxExt.whitespaces,
                                                                 RxExt.Str $ name ++ "=\"",
                                                                 processAttType attType,
                                                                 RxExt.Lit '"']

        processAttType (StringType) = RxExt.Star $ RxExt.CClass RxExt.CDATA
        processAttType (TokenizedType tokType) = error "Token type not implemented"
        processAttType (EnumeratedType enumType) = error "Enumerated type not implemented"

        processDefaultDecl  REQUIRED reg = reg                                                          -- | The attribute is required
        processDefaultDecl  IMPLIED  reg = RxExt.Query reg                                              -- | The attribute is not required
        processDefaultDecl  (DefaultTo attValue (maybeFIXED)) _ = error "Default to is not implemented)" -- | The attribute value is fixed

        loop :: (a -> RegexExt) -> [a] -> [RegexExt]
        loop _ [] = []
        loop f (cp:cps) = f cp : loop f cps

        -- This is a poor check and should be expanded to be fully recursive all children of this element
        checkForRecursiveDefinitions name children =
            if (name `elem` children)
              then error $ "The tag '" ++ name ++ "' contains itself as direct child.\n"
                       ++ "It is thus impossible to generate a regex for this schema. Aborting!!!."
              else ()


    in
      case cntSpec of
        (EMPTY) -> error "CP EMPTY not implemented!!!"
        (ANY) -> error "CP ANY not implemented!!!"
        (Mixed mix) -> genRegex $ processMixed mix
        (ContentSpec cp) ->  genRegex $ processCP cp

