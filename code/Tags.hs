--
--
--

module Tags
(
)
where

import Regex (Regex (..), STree (..))
import Coding (code, decode)
import Parse (match, parse, flatten)

string :: String
string = "<?xml version=\"1.0\"?>\n<dblptags>\n<tag key=\"conf/rsctc/WengZ04\">Plagiarized Papers in DBLP</tag>\n<tag key=\"books/ph/KemperM94\">Access Support Relations in Textbooks</tag>\n<tag key=\"conf/icde/LitwinL86\">1986</tag>\n<tag key=\"journals/software/LitwinL87\">1987</tag>\n<tag key=\"journals/tods/Lomet87\">1987</tag>\n<tag key=\"journals/tods/Lomet88\">1988</tag>\n<tag key=\"conf/pods/RamakrishnaM88\">1988</tag>\n<tag key=\"conf/icdt/MatsliachS90\">1990</tag>\n<tag key=\"conf/pods/Matsliach91\">1991</tag>\n<tag key=\"journals/is/TharpB91\">1991</tag>\n<tag key=\"journals/tkde/Ramakrishna94\">1994</tag>\n<tag key=\"conf/mfcs/Baeza-Yates94\">1994</tag>\n<tag key=\"journals/acta/BayerM72\">The Original Publication</tag>\n<tag key=\"journals/csur/Comer79\">A Well-Known Survey</tag>\n<tag key=\"journals/ipl/Samadi76\">Concurrency Control</tag>\n<tag key=\"journals/acta/BayerS77\">Concurrency Control</tag>\n<tag key=\"conf/mfcs/KwongW80\">Concurrency Control</tag>\n<tag key=\"journals/acta/Ellis80\">Concurrency Control</tag>\n<tag key=\"journals/tods/LehmanY81\">Concurrency Control</tag>\n<tag key=\"journals/tse/KwongW82\">Concurrency Control</tag>\n<tag key=\"journals/computing/Lausen84\">Concurrency Control</tag>\n<tag key=\"journals/spe/KerstenT84\">Concurrency Control</tag>\n<tag key=\"conf/vldb/MondR85\">Concurrency Control</tag>\n<tag key=\"conf/pods/Sagiv85\">Concurrency Control</tag>\n<tag key=\"journals/jcss/Sagiv86\">Concurrency Control</tag>\n<tag key=\"conf/pods/GoodmanS85\">Concurrency Control</tag>\n<tag key=\"journals/debu/Shasha85\">Concurrency Control</tag>\n<tag key=\"conf/fjcc/LaninS86\">Concurrency Control</tag>\n<tag key=\"conf/awoc/Biliris86\">Concurrency Control</tag>\n<tag key=\"conf/pods/Biliris87\">Concurrency Control</tag>\n<tag key=\"conf/pods/NurmiSW87\">Concurrency Control</tag></dblptags>"

title :: Regex Char
title = sum " ,./():-" :+: alphanum

key :: Regex Char
key = S (sum ['a' .. 'z'] :.: Lit '/') :.: alphanum

tag :: Regex Char
tag = prod [string "<tag kay=\"", key, string "\">", title, string "</tag>\n"

tags :: Regex Char
tags = prod [string "<?xml version=\"1.0\"?>\n<dblptags>\n",
             S tag,
             string "</dblptags>"]

main :: IO ()
main =
    do putStrLn "regex 1:"
       putStrLn ""
       print $ match tags string
       print $ tags
       print $ parse tags string
       print $ (code tags . parse tags) string
       print $ (length . code tags . parse tags) string
       print $ (flatten . decode tags . code tags . parse tags) string
