module CoreToImportEnv(getImportEnvironment) where

import Core
import Utils
import TypeConversion
import ParseLibrary
import Lexer(lexer)
import Parser(type_, contextAndType)
import ImportEnvironment
import UHA_Utils
import Id
import UHA_Syntax
import OperatorTable
import Types
import Byte(stringFromBytes)
import UHA_Range(makeImportRange, setNameRange)

typeFromCustoms :: String -> [Custom] -> TpScheme
typeFromCustoms n [] =
    internalError "CoreToImportEnv" "typeFromCustoms"
        ("function import without type: " ++ n)
typeFromCustoms n ( CustomDecl (DeclKindCustom id) [CustomBytes bytes] : cs) 
    | stringFromId id == "type" =
        let string = filter (/= '!') (Byte.stringFromBytes bytes) 
        in makeTpSchemeFromType (parseFromString contextAndType string)
    | otherwise =
        typeFromCustoms n cs

parseFromString :: HParser a -> String -> a
parseFromString p string = 
    case lexer "CoreToImportEnv" string of 
        Left lexErr -> internalError "CoreToImportEnv" "parseFromString" ("lex error in " ++ string)
        Right (tokens, _) ->
            case runHParser p "CoreToImportEnv" tokens True {- wait for EOF -} of
                Left parseError  -> internalError "CoreToImportEnv" "parseFromString" ("parse error in " ++ string)
                Right x -> x

typeSynFromCustoms :: String -> [Custom] -> (Int, Tps -> Tp) -- !!! yuck
typeSynFromCustoms n (CustomBytes bs:cs) =
    let
        typeSynDecl = Byte.stringFromBytes bs
        -- (too?) simple parser; works because type variables in synonym decls are renamed to 1 letter
        ids         = ( map (\x -> nameFromString [x])
                      . filter    (' '/=)
                      . takeWhile ('='/=)
                      . drop (length n + 1)
                      )
                        typeSynDecl
        rhsType     = ( drop 1
                      . dropWhile ('='/=)
                      )
                        typeSynDecl
    in
        ( arityFromCustoms n cs
        , \ts -> makeTpFromType (zip ids ts) (parseFromString type_ rhsType)
        )
typeSynFromCustoms n _ =
    internalError "CoreToImportEnv" "typeSynFromCustoms"
        ("type synonym import missing definition: " ++ n)

-- in compiled Core files types have a kind (e.g. * -> *), 
-- in Helium the have a number indicating the arity
arityFromCustoms :: String -> [Custom] -> Int
arityFromCustoms n [] =
    internalError "CoreToImportEnv" "arityFromCustoms"
        ("type constructor import without kind: " ++ n)
arityFromCustoms n ( CustomInt arity : _ ) = arity
arityFromCustoms n ( CustomDecl (DeclKindCustom id) [CustomBytes bytes] : cs ) 
    | stringFromId id == "kind" = 
        (length . filter ('*'==) . Byte.stringFromBytes) bytes - 1
        -- the number of stars minus 1 is the arity
arityFromCustoms n (_:cs) = arityFromCustoms n cs

makeOperatorTable :: Name -> [Custom] -> [(Name, (Int, Assoc))]
makeOperatorTable op (Core.CustomInt i : Core.CustomBytes bs : cs) =
    let
        assoc =
            case stringFromBytes bs of
                "left"   -> AssocLeft
                "right"  -> AssocRight
                "none"   -> AssocNone
                assocStr -> intErr ("unknown associativity: " ++ assocStr)
        
        intErr = internalError "CoreToImportEnv" "makeOperatorTable"
    in
        if getNameName op == "-" then
            -- special rule: unary minus has the assoc
            -- and the priority of the infix operator -
            [ (op, (i, assoc))
            , (intUnaryMinusName, (i, assoc))
            , (floatUnaryMinusName, (i, assoc))
            ]
        else
            [(op, (i, assoc))]
makeOperatorTable op cs = 
    internalError "CoreToImportEnv" "makeOperatorTable"
        ("infix decl missing priority or associativity: " ++ show op)

makeImportName :: String -> Id -> Id -> Name
makeImportName importedInMod importedFromMod n =
    setNameRange 
        (nameFromId n)
        (makeImportRange (idFromString importedInMod) importedFromMod)

getImportEnvironment :: String -> [CoreDecl] -> ImportEnvironment
getImportEnvironment importedInModule = foldr insert emptyEnvironment
   where
      insert decl = 
         case decl of 
         
           -- functions
           DeclAbstract { declName    = n
                        , declAccess  = Imported{importModule = importedFromModId}
                        , declCustoms = cs
                        } ->
              addType
                 (makeImportName importedInModule importedFromModId n)
                 (typeFromCustoms (stringFromId n) cs)
          
           -- functions from non-core/non-lvm libraries and lvm-instructions
           DeclExtern { declName = n
                      , declAccess  = Imported{importModule = importedFromModId}
                      , declCustoms = cs
                      } ->
              addType
                 (makeImportName importedInModule importedFromModId n)
                 (typeFromCustoms (stringFromId n) cs)
            
           -- constructors
           DeclCon { declName    = n
                   , declAccess  = Imported{importModule = importedFromModId}
                   , declArity   = arity
                   , declCustoms = cs
                   } ->
              addValueConstructor
                (makeImportName importedInModule importedFromModId n)
                (typeFromCustoms (stringFromId n) cs)

           -- type constructor import
           DeclCustom { declName    = n
                      , declAccess  = Imported{importModule = importedFromModId}
                      , declKind    = DeclKindCustom id
                      , declCustoms = cs 
                      } 
                      | stringFromId id == "data" ->
              addTypeConstructor
                 (makeImportName importedInModule importedFromModId n)
                 (arityFromCustoms (stringFromId n) cs)
            
           -- type synonym declarations
           -- important: a type synonym also introduces a new type constructor!
           DeclCustom { declName    = n
                      , declAccess  = Imported{importModule = importedFromModId}
                      , declKind    = DeclKindCustom id
                      , declCustoms = cs
                      }
                      | stringFromId id == "typedecl" ->
              let name = makeImportName importedInModule importedFromModId n
                  pair = typeSynFromCustoms (stringFromId n) cs
              in addTypeSynonym name pair . addTypeConstructor name (fst pair)
                             
           -- infix decls
           DeclCustom { declName    = n
                      , declKind    = DeclKindCustom id
                      , declCustoms = cs
                      }
                      | stringFromId id == "infix" ->
              flip (foldr (uncurry addOperator)) (makeOperatorTable (nameFromId n) cs)

           -- typing strategies
           DeclCustom { declName    = _
                      , declKind    = DeclKindCustom id
                      , declCustoms = cs
                      }
                      | stringFromId id == "strategy" ->
              let (CustomDecl _  [CustomBytes bytes]) = head cs
                  text = stringFromBytes bytes
              in addTypingStrategies (read text)

           -- !!! Print importedFromModId from "declAccess = Imported{importModule = importedFromModId}" as well
           DeclAbstract{ declName = n } ->
              intErr  ("don't know how to handle declared DeclAbstract: " ++ stringFromId n)
           DeclExtern  { declName = n } ->
              intErr  ("don't know how to handle declared DeclExtern: "   ++ stringFromId n)
           DeclCon     { declName = n } ->
              intErr  ("don't know how to handle declared DeclCon: "      ++ stringFromId n)
           DeclCustom  { declName = n } ->
              intErr  ("don't know how to handle DeclCustom: "            ++ stringFromId n)
           DeclValue   { declName = n } ->
              intErr  ("don't know how to handle DeclValue: "             ++ stringFromId n)
           DeclImport  { declName = n } ->
              intErr  ("don't know how to handle DeclImport: "            ++ stringFromId n)
           _ ->
              intErr "unknown kind of declaration in import declarations"
        
      intErr = internalError "CoreToImportEnv" "getImportEnvironment"
         