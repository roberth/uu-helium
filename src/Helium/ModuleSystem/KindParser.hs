-- | An ad-hoc kind parser
module Helium.ModuleSystem.KindParser
  ( parseKind
  ) where

import Helium.Parser.Lexer
import Helium.Parser.ParseLibrary
import Text.ParserCombinators.Parsec hiding(satisfy)
import Top.Types.Primitive

import Debug.Trace

-- | Parse kind for the purpose of importing compiled modules
--
-- >>> :l src/Helium/ModuleSystem/KindParser.hs
-- >>> parseKind "*"
-- Right (*)
-- >>> parseKind "(*)"
-- Right (*)
-- >>> parseKind "Constraint"
-- Right (Constraint)
-- >>> parseKind "(Constraint)"
-- Right (Constraint)
-- >>> parseKind "* -> *"
-- Right (* -> *)
-- >>> parseKind "* *"
-- Right (* *)
--
-- Christmas tree
--
-- >>> parseKind "(* -> *) -> (* -> Constraint -> *) -> *"
-- Right ((* -> *) -> (* -> Constraint -> *) -> *)
-- >>> parseKind "(* -> * -> *) -> (* -> *) -> Constraint"
-- Right ((* -> * -> *) -> (* -> *) -> Constraint)
--
parseKind :: String -> Either String Tp
parseKind s =
  let fname = "<kind signature in compiled module>"
  in case lexer [] fname s of
       Left _err -> Left "lexer error"
       Right (tokens, _warnings) ->
         case runHParser pKind fname tokens True of
           Left err -> Left (show err)
           Right tp -> Right tp

-- * -> *
-- * -> (* -> *)

pKind :: HParser Tp
pKind = (\l more -> more l)
        <$> (    try pKindParens
             <|> try pStar
             <|> pConstraint
            )
        <*> (    (try ((\r ->    \l -> (l $->$ r))
                  <$  pArr
                  <*> pKind))
             <|> (try ((\r ->   \l -> (l $$ r))
                  <$> pKind))
             <|> (pure           id)
            )

--pKindExpr :: HParser Tp
--pKindExpr = pStar <|> pConstraint

{-pKindExpr :: HParser Tp
pKindExpr = (\left right -> right left
               ) <$> pKindSym <*> pMore
            where pMore =  (\r -> \l -> l $->$ r) <$ pArr <*> pKind
                       <|> (\r -> \l -> l $$ r) <$> pKind

pKindSym = pStar <|> pConstraint <|> pKindParens
-}

a $$ b = a `TApp` b
a $->$ b = TCon "->" $$ a $$ b

pKindParens :: HParser Tp
pKindParens = parens pKind

pStar :: HParser Tp
pStar = satisfy $ \l -> case l of { LexVarSym "*" -> Just (TCon "*"); _ -> Nothing }

pConstraint :: HParser Tp
pConstraint = satisfy $ \l -> case l of { LexCon "Constraint" -> Just (TCon "Constraint"); _ -> Nothing }

pArr :: HParser Tp
pArr = satisfy $ \l -> case l of { LexResVarSym "->" -> Just (TCon "->"); _ -> Nothing }
