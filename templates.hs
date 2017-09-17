{-# LANGUAGE TemplateHaskell  #-}

-- A useful reference:
-- https://hackage.haskell.org/package/template-haskell-2.8.0.0/docs/Language-Haskell-TH.html
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax(Name(..), NameFlavour(..), showName)

-- https://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec.html
import Text.Parsec hiding (upper,lower)
import qualified Text.Parsec
import qualified Text.Parsec.String as PS
type Parser = PS.Parser

-- Note that ExpQ is Q TH.Exp, which is noted as Expr in the paper.
printf :: String -> ExpQ
printf s = gen (parsef s) [| "" |]


{-
  To test, 
  > ghci
  *Main> :set -XTemplateHaskell
  *Main> :load templates.hs
  *Main> $(printf "%d is %s") 10 "ten"
  "10 is ten"

  To see spliced code,
  *Main> :set -ddump-splices
-}


data Format = D | S | L String
 deriving Show

{- Non-recursive version of gen. -}
gen1 :: Format -> ExpQ
gen1 D = [| \n -> show n |]
gen1 S = [| \s -> s |]
gen1 (L s) = TH.litE (TH.stringL s)


gen :: [Format] -> ExpQ -> ExpQ
gen []         x = x
gen (D   : xs) x = [| \n -> $(gen xs [| $x ++ show n |]) |]
gen (S   : xs) x = [| \s -> $(gen xs [| $x ++ s      |]) |]
gen (L s : xs) x =            gen xs [| $x ++ s      |]



{- 
   sel i n generates code to select the ith component from an n-ary tuple.
-}
sel :: Int -> Int -> ExpQ
sel i n = lamE [varP (mkName "x")] (caseE (varE (mkName "x")) [alt])
 where alt :: MatchQ
       alt = match pat rhs []

       pat :: PatQ
       pat = tupP (map varP args)

       rhs :: BodyQ
       rhs = normalB (varE (args !! (i-1))) -- !! is 0 based; (!!) :: [a] -> Int -> a
       
       args :: [Name]
       args = map mkName ["a"++ show j | j <- [1..n]]

{- 
  Usage:
   x = (1,2,3)
   y = $(sel 1 3) x
-}

{-
  Changes from the paper:
  -- Syntax for Patterns:
    Patt  becomes PatQ
    pvar  becomes varP
    ptup  becomes tupP
    pcon  becomes conP
    pwild becomes wildP

  -- Syntax for Expressions
    var     becomes varE
    tup     becomes tupE
    app     becomes appE
    lam     becomes lamE
    simpleM becomes match

-}

sel2 :: Int -> Int -> ExpQ
sel2 i n = [| \x -> $(caseE [| x |] [alt]) |]
  where
    alt = match pat rhs []
    pat = tupP (map varP args)
    rhs = normalB (varE (args !! (i-1)))
    args = map mkName ["a"++ show j | j <- [1..n]]

{-
cross2a :: ExpQ -> ExpQ -> ExpQ
cross2a f g = [| \(x,y) -> ($f x, $g y) |]
-}




{-
  Routines for parsing format strings using Parsec.
-}
parsef :: String -> [Format]
parsef input = 
 case parse parseFormats "function argument" input of
   Left error -> []  -- Parser error
   Right fs ->fs

parseFormats :: Parser [Format]
parseFormats = do 
   { f <- parseFormat
   ; do { eof
        ; return [f]
        }
     <|> 
     do { fs <- parseFormats
        ; return (f : fs)
        }
   }

parseFormat :: Parser Format
parseFormat = 
      try parseDecimal
  <|> try parseString
  <|> parseStrLiteral


parseDecimal :: Parser Format
parseDecimal = do 
 { char '%'
 ; char 'd'
 ; return D
 }

parseString :: Parser Format
parseString = do 
 { char '%'
 ; char 's'
 ; return S
 }

parseStrLiteral :: Parser Format
parseStrLiteral = do
  { str <- manyTill anyChar  (lookAhead (char '%') <|> do {eof; return 'c'})
  ; return (L str)
  }
-- parsef s = [D, L " is ", S]

