module RegexExt
where

import qualified RegKleene as Rx

data RegexExt
  = O                      -- | 0
  | E                      -- | 1
  | Lit Char               -- | literal
  | Str String             -- | String (prod of literals)
  | RegexExt :+: RegexExt  -- | +
  | RegexExt :*: RegexExt  -- | x
  | Query RegexExt         -- | Zero or one "" + E
  | Star RegexExt          -- | Zero or more  "" + E* (kleene-star)
  | Plus RegexExt          -- | One or more E + E*
  | CClass CClass          -- | Different Character classes
  deriving (Eq, Ord)

data CClass  
    = Alpha
    | LowerAlpha
    | UpperAlpha
    | Num
    | AlphaNum
    | Dot
    | PCDATA
    | WhiteSpace
    | CDATA
    | Ptr
      deriving (Eq, Ord)

instance Show RegexExt where
  show O         = "()"
  show E         = ""
  show (Lit c)   = show c
  show (Str s)   = show s
  show (e :+: f) = 
      case f of 
        (_ :*: _) -> show e ++ "|(" ++ show f ++ ")"
        _ -> show e ++ "|" ++ show f
  show (e :*: f) = 
      case f of
        (_ :+: _) -> show e ++ "(" ++ show f ++ ")"
        _ ->       show e ++ show f
  show (Star r)  = 
      case r of 
        (_ :*: _) -> "(" ++ show r ++ ")*"
        (_ :+: _) -> "(" ++ show r ++ ")*"
        E -> ""
        _ -> show r ++ "*"
  show (Plus r) =
      case r of 
        (_ :*: _) -> "(" ++ show r ++ ")+"
        (_ :+: _) -> "(" ++ show r ++ ")+"
        E -> ""
        _ -> show r ++ "+"
  show (Query r) =
      case r of 
        (_ :*: _) -> "(" ++ show r ++ ")?"
        (_ :+: _) -> "(" ++ show r ++ ")?"
        E -> ""
        _ -> show r ++ "+"
  show (CClass a) = show a

instance Show CClass where
    show Alpha = "[a-zA-Z]"
    show LowerAlpha = "[a-z]"
    show UpperAlpha = "[A-Z]"
    show Num = "[0-9]"
    show AlphaNum = "[a-zA-Z0-9]"
    show Dot = "."
    show PCDATA = "[#PCDATA]"
    show WhiteSpace = "[ws]"
    show CDATA = "[CDATA]"
    show Ptr = "[PTR]"

toRegex :: RegexExt -> Rx.Reg Char
toRegex regexExt =
    case regexExt of
      O -> Rx.O
      E -> Rx.E
      Lit a -> Rx.Lit a
      Str s -> Rx.prod $ fmap Rx.Lit s
      (a :+: b) -> ((toRegex a) Rx.:+: (toRegex b))
      (a :*: b) -> ((toRegex a) Rx.:*: (toRegex b))
      Star a -> Rx.Star (toRegex a)
      Plus a -> (toRegex a) Rx.:*: (Rx.Star (toRegex a))
      Query a -> Rx.E Rx.:+: (toRegex a)
      CClass c -> case c of
                    Alpha -> Rx.alpha
                    LowerAlpha -> Rx.loweralpha
                    UpperAlpha -> Rx.upperalpha
                    Num -> Rx.num
                    AlphaNum -> Rx.alphanum
                    Dot -> Rx.dot
                    PCDATA -> Rx.pcdata
                    WhiteSpace -> Rx.whitespace
                    CDATA -> Rx.cdata
                    Ptr -> Rx.Star $ Rx.num

sum :: [RegexExt] -> RegexExt
sum []     = E
sum (c:cs) = foldl (:+:) c cs

prod :: [RegexExt] -> RegexExt
prod []     = O
prod (c:cs) = foldl (:*:) c cs

whitespace = CClass WhiteSpace
whitespaces = Star $ CClass WhiteSpace

alphanum = CClass AlphaNum
digit = CClass Num
ptr = CClass Ptr