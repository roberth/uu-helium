-- do not edit; automatically generated by UU_AG
module TS_Apply where

import UHA_Syntax
import TS_CoreSyntax
import ConstraintTree
import Constraints
import HeliumConstraintInfo
import TypeGraphConstraintInfo
import Types
import List
import UHA_Range (noRange)
import Utils (internalError)
import OneLiner
import Messages
import TypeErrors
import ImportEnvironment
import OperatorTable (OperatorTable)
import Parser (exp_)
import ParseLibrary (runHParser)
import qualified ResolveOperators
import TS_Attributes

type MetaVariableTable info = [(String, (ConstraintSet, info))]
type MetaVariableInfo = (Tp, Tree, Range)

applyTypingStrategy :: Core_TypingStrategy -> (ConstraintSet, MetaVariableInfo) -> MetaVariableTable MetaVariableInfo -> Int -> (ConstraintSet, IO (), Int)
applyTypingStrategy = sem_Core_TypingStrategy

matchInformation :: ImportEnvironment -> Core_TypingStrategy -> (Expression, [String])
matchInformation importEnvironment typingStrategy = 
   case typingStrategy of 
      TypingStrategy (TypeRule premises conclusion) _ -> 
         let Judgement exprstring _ = conclusion
             expression = expressionParser (operatorTable importEnvironment) exprstring
             metas      = [ s | Judgement s t <- premises ]
         in (expression, metas)
      _ -> internalError "TS_Apply.ag" "n/a" "unknown TypingStrategy"
      
expressionParser :: OperatorTable -> String -> Expression
expressionParser operatorTable string = 
   case runHParser exp_ "TS_Apply" string True {- wait for EOF -} False {- no layout rule -} of
      Left parseError  -> internalError "TS_Apply.ag" "n/a" ("unparsable expression: "++show parseError)
      Right expression -> ResolveOperators.expression operatorTable expression

standardConstraintInfo :: (Int, Int) -> (Tp, Tp) -> HeliumConstraintInfo
standardConstraintInfo pos tppair =
   CInfo { info       = (NTBody, AltBody, "Strategy, user constraint, "++show pos)
         , location   = "Typing Strategy"
         , errorrange = noRange
         , sources    = [ ]
         , typepair   = tppair
         , properties = [ ]
         }

typeRuleCInfo :: String -> MetaVariableInfo -> (Tp, Tp) -> HeliumConstraintInfo
typeRuleCInfo = 
   \string (tp1,tree,range) tppair ->
      CInfo { info       = (NTBody, AltBody, "Typing Strategy, meta-typevariable " ++ string)
            , location   = "Typing Strategy"
            , errorrange = noRange
            , sources    = [ ]
            , typepair   = tppair
            , properties = [ HighlyTrusted ] 
            }

setCustomTypeError :: MetaVariableInfo -> HeliumConstraintInfo -> HeliumConstraintInfo
setCustomTypeError (tp, tree, range) cinfo =
   addProperty (WithTypeError customTypeError) cinfo        

     where customTypeError = CustomTypeError [range] message
           message = [ MessageOneLiner (MessageString ("Type error in "++"Typing Strategy")) -- !!!
                     , MessageTable
                       [ (MessageString "Expression", MessageOneLineTree tree) ]                     
                     , MessageOneLiner (MessageString "   implies that the following types are equal:")
                     , MessageTable 
                       [ (MessageString "Type 1", MessageType (fst (typepair cinfo)))
                       , (MessageString "Type 2", MessageType (snd (typepair cinfo)))
                       ]                     
                     ]  

makeMessageAlgebra :: AttributeTable MessageBlock -> AttributeAlgebra MessageBlock
makeMessageAlgebra table = ( MessageString
                           , table
                           , \attribute -> internalError 
                                              "TS_Apply" "makeMessageAlgebra"
                                              ("unknown attribute " ++ showAttribute attribute ++
                                               "; known attributes are " ++ show (map (showAttribute . fst) table))
                           )

makeAttributeTable :: MetaVariableInfo -> MetaVariableTable MetaVariableInfo -> FiniteMapSubstitution -> [((String, Maybe String), MessageBlock)]
makeAttributeTable local table substitution = 
   let f :: String -> MetaVariableInfo -> [((String, Maybe String), MessageBlock)]
       f string (tp, tree, range) = [ ((string, Just "type" ), MessageType tp)
                                    , ((string, Just "pp"   ), MessageOneLineTree tree)
                                    , ((string, Just "range"), MessageRange range)
                                    ]
   in f "expr" local 
   ++ concatMap (\(s,(_,info)) -> f s info) table 
   ++ [ ((show i, Nothing), MessageType (substitution |-> TVar i)) | i <- dom substitution ]  
-- Core_Judgement ----------------------------------------------
-- semantic domain
type T_Core_Judgement = ((ConstraintSet, MetaVariableInfo)) ->
                        (MetaVariableTable MetaVariableInfo) ->
                        (FiniteMapSubstitution) ->
                        (([Int]),([(String, Tp)]))
-- cata
sem_Core_Judgement :: (Core_Judgement) ->
                      (T_Core_Judgement)
sem_Core_Judgement ((Judgement (_expression) (_type))) =
    (sem_Core_Judgement_Judgement (_expression) (_type))
sem_Core_Judgement_Judgement :: (String) ->
                                (Tp) ->
                                (T_Core_Judgement)
sem_Core_Judgement_Judgement (_expression) (_type) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution) =
    let 
    in  (ftv _type,[(_expression, _type)])
-- Core_Judgements ---------------------------------------------
-- semantic domain
type T_Core_Judgements = ((ConstraintSet, MetaVariableInfo)) ->
                         (MetaVariableTable MetaVariableInfo) ->
                         (FiniteMapSubstitution) ->
                         (([Int]),([(String, Tp)]))
-- cata
sem_Core_Judgements :: (Core_Judgements) ->
                       (T_Core_Judgements)
sem_Core_Judgements (list) =
    (foldr (sem_Core_Judgements_Cons) (sem_Core_Judgements_Nil) ((map sem_Core_Judgement list)))
sem_Core_Judgements_Cons :: (T_Core_Judgement) ->
                            (T_Core_Judgements) ->
                            (T_Core_Judgements)
sem_Core_Judgements_Cons (_hd) (_tl) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution) =
    let ( _hd_ftv,_hd_judgements) =
            (_hd (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution))
        ( _tl_ftv,_tl_judgements) =
            (_tl (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution))
    in  (_hd_ftv ++ _tl_ftv,_hd_judgements ++ _tl_judgements)
sem_Core_Judgements_Nil :: (T_Core_Judgements)
sem_Core_Judgements_Nil (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution) =
    let 
    in  ([],[])
-- Core_TypeRule -----------------------------------------------
-- semantic domain
type T_Core_TypeRule = ((ConstraintSet, MetaVariableInfo)) ->
                       (MetaVariableTable MetaVariableInfo) ->
                       (FiniteMapSubstitution) ->
                       ((Constraints HeliumConstraintInfo),([Int]))
-- cata
sem_Core_TypeRule :: (Core_TypeRule) ->
                     (T_Core_TypeRule)
sem_Core_TypeRule ((TypeRule (_premises) (_conclusion))) =
    (sem_Core_TypeRule_TypeRule ((sem_Core_Judgements (_premises))) ((sem_Core_Judgement (_conclusion))))
sem_Core_TypeRule_TypeRule :: (T_Core_Judgements) ->
                              (T_Core_Judgement) ->
                              (T_Core_TypeRule)
sem_Core_TypeRule_TypeRule (_premises) (_conclusion) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution) =
    let ( _premises_ftv,_premises_judgements) =
            (_premises (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution))
        ( _conclusion_ftv,_conclusion_judgements) =
            (_conclusion (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_substitution))
    in  ([ (_lhs_substitution |-> tp1 .==. tp2) (typeRuleCInfo "conclusion" mvinfo)
         | (s1, tp1) <- _conclusion_judgements
         , let (_, mvinfo@(tp2,_,_)) = _lhs_localInfo
         ]
         ++
         [ (_lhs_substitution |-> tp1 .==. tp2) (typeRuleCInfo s1 mvinfo)
         | (s1, tp1)                   <- _premises_judgements
         , (s2, (_, mvinfo@(tp2,_,_))) <- _lhs_metaVariableTable
         , s1 == s2
         ]
        ,_premises_ftv ++ _conclusion_ftv
        )
-- Core_TypingStrategy -----------------------------------------
-- semantic domain
type T_Core_TypingStrategy = ((ConstraintSet, MetaVariableInfo)) ->
                             (MetaVariableTable MetaVariableInfo) ->
                             (Int) ->
                             ((ConstraintSet),(IO ()),(Int))
-- cata
sem_Core_TypingStrategy :: (Core_TypingStrategy) ->
                           (T_Core_TypingStrategy)
sem_Core_TypingStrategy ((TypingStrategy (_typerule) (_statements))) =
    (sem_Core_TypingStrategy_TypingStrategy ((sem_Core_TypeRule (_typerule))) ((sem_Core_UserStatements (_statements))))
sem_Core_TypingStrategy_TypingStrategy :: (T_Core_TypeRule) ->
                                          (T_Core_UserStatements) ->
                                          (T_Core_TypingStrategy)
sem_Core_TypingStrategy_TypingStrategy (_typerule) (_statements) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_unique) =
    let (_substitution) =
            listToSubstitution (zip _ftv (map TVar [_lhs_unique..]))
        (_ftv) =
            _typerule_ftv `union` _statements_ftv
        (_allConstraintTrees) =
            ctSingle (reverse _typerule_constraints) :
            (map snd _statements_metavarConstraints) ++
            (reverse _statements_collectConstraints)
        ( _typerule_constraints,_typerule_ftv) =
            (_typerule (_lhs_localInfo) (_lhs_metaVariableTable) (_substitution))
        ( _statements_collectConstraints,_statements_currentPhase,_statements_currentPosition,_statements_ftv,_statements_metavarConstraints) =
            (_statements (makeAttributeTable (snd _lhs_localInfo) _lhs_metaVariableTable _substitution) ([]) (Nothing) ((_lhs_unique, 0)) (_lhs_localInfo) (_lhs_metaVariableTable) ([ (s,cs) | (s,(cs,_)) <- _lhs_metaVariableTable ]) (_substitution))
    in  (ctNode _allConstraintTrees,putStrLn "applying typing strategy",length _ftv + _lhs_unique)
-- Core_UserStatement ------------------------------------------
-- semantic domain
type T_Core_UserStatement = ([((String, Maybe String), MessageBlock)]) ->
                            (ConstraintTrees HeliumConstraintInfo) ->
                            (Maybe Int) ->
                            ((Int, Int)) ->
                            ((ConstraintSet, MetaVariableInfo)) ->
                            (MetaVariableTable MetaVariableInfo) ->
                            ([(String,ConstraintTree HeliumConstraintInfo)]) ->
                            (FiniteMapSubstitution) ->
                            ((ConstraintTrees HeliumConstraintInfo),(Maybe Int),((Int, Int)),([Int]),([(String,ConstraintTree HeliumConstraintInfo)]))
-- cata
sem_Core_UserStatement :: (Core_UserStatement) ->
                          (T_Core_UserStatement)
sem_Core_UserStatement ((Constraint (_leftType) (_rightType) (_message))) =
    (sem_Core_UserStatement_Constraint (_leftType) (_rightType) (_message))
sem_Core_UserStatement ((MetaVariableConstraints (_name))) =
    (sem_Core_UserStatement_MetaVariableConstraints (_name))
sem_Core_UserStatement ((Phase (_phase))) =
    (sem_Core_UserStatement_Phase (_phase))
sem_Core_UserStatement_Constraint :: (Tp) ->
                                     (Tp) ->
                                     (String) ->
                                     (T_Core_UserStatement)
sem_Core_UserStatement_Constraint (_leftType) (_rightType) (_message) (_lhs_attributeTable) (_lhs_collectConstraints) (_lhs_currentPhase) (_lhs_currentPosition) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_metavarConstraints) (_lhs_substitution) =
    let (_newConstraint) =
            let cinfo   = addProperty (WithTypeError (CustomTypeError [] message)) .
                          addProperty (uncurry IsUserConstraint _lhs_currentPosition) .
                          standardConstraintInfo _lhs_currentPosition
                message = [MessageOneLiner (MessageCompose (substituteAttributes (makeMessageAlgebra _lhs_attributeTable) _message))]
            in (_lhs_substitution |-> _leftType .==. _lhs_substitution |-> _rightType) cinfo
    in  (case _lhs_currentPhase of
            Just phase -> ctPhased phase [ _newConstraint ] : _lhs_collectConstraints
            Nothing    -> ctSingle       [ _newConstraint ] : _lhs_collectConstraints
        ,_lhs_currentPhase
        ,(\(x, y) -> (x, y+1)) _lhs_currentPosition
        ,ftv [_leftType, _rightType]
        ,_lhs_metavarConstraints
        )
sem_Core_UserStatement_MetaVariableConstraints :: (String) ->
                                                  (T_Core_UserStatement)
sem_Core_UserStatement_MetaVariableConstraints (_name) (_lhs_attributeTable) (_lhs_collectConstraints) (_lhs_currentPhase) (_lhs_currentPosition) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_metavarConstraints) (_lhs_substitution) =
    let 
    in  (case lookup _name _lhs_metavarConstraints of
             Just tree -> tree : _lhs_collectConstraints
             Nothing   -> internalError "TS_Apply.ag" "n/a" "unknown constraint set"
        ,_lhs_currentPhase
        ,_lhs_currentPosition
        ,[]
        ,filter ((_name /=) . fst) _lhs_metavarConstraints
        )
sem_Core_UserStatement_Phase :: (Int) ->
                                (T_Core_UserStatement)
sem_Core_UserStatement_Phase (_phase) (_lhs_attributeTable) (_lhs_collectConstraints) (_lhs_currentPhase) (_lhs_currentPosition) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_metavarConstraints) (_lhs_substitution) =
    let 
    in  (_lhs_collectConstraints,Just _phase,_lhs_currentPosition,[],_lhs_metavarConstraints)
-- Core_UserStatements -----------------------------------------
-- semantic domain
type T_Core_UserStatements = ([((String, Maybe String), MessageBlock)]) ->
                             (ConstraintTrees HeliumConstraintInfo) ->
                             (Maybe Int) ->
                             ((Int, Int)) ->
                             ((ConstraintSet, MetaVariableInfo)) ->
                             (MetaVariableTable MetaVariableInfo) ->
                             ([(String,ConstraintTree HeliumConstraintInfo)]) ->
                             (FiniteMapSubstitution) ->
                             ((ConstraintTrees HeliumConstraintInfo),(Maybe Int),((Int, Int)),([Int]),([(String,ConstraintTree HeliumConstraintInfo)]))
-- cata
sem_Core_UserStatements :: (Core_UserStatements) ->
                           (T_Core_UserStatements)
sem_Core_UserStatements (list) =
    (foldr (sem_Core_UserStatements_Cons) (sem_Core_UserStatements_Nil) ((map sem_Core_UserStatement list)))
sem_Core_UserStatements_Cons :: (T_Core_UserStatement) ->
                                (T_Core_UserStatements) ->
                                (T_Core_UserStatements)
sem_Core_UserStatements_Cons (_hd) (_tl) (_lhs_attributeTable) (_lhs_collectConstraints) (_lhs_currentPhase) (_lhs_currentPosition) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_metavarConstraints) (_lhs_substitution) =
    let ( _hd_collectConstraints,_hd_currentPhase,_hd_currentPosition,_hd_ftv,_hd_metavarConstraints) =
            (_hd (_lhs_attributeTable) (_lhs_collectConstraints) (_lhs_currentPhase) (_lhs_currentPosition) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_metavarConstraints) (_lhs_substitution))
        ( _tl_collectConstraints,_tl_currentPhase,_tl_currentPosition,_tl_ftv,_tl_metavarConstraints) =
            (_tl (_lhs_attributeTable) (_hd_collectConstraints) (_hd_currentPhase) (_hd_currentPosition) (_lhs_localInfo) (_lhs_metaVariableTable) (_hd_metavarConstraints) (_lhs_substitution))
    in  (_tl_collectConstraints,_tl_currentPhase,_tl_currentPosition,_hd_ftv ++ _tl_ftv,_tl_metavarConstraints)
sem_Core_UserStatements_Nil :: (T_Core_UserStatements)
sem_Core_UserStatements_Nil (_lhs_attributeTable) (_lhs_collectConstraints) (_lhs_currentPhase) (_lhs_currentPosition) (_lhs_localInfo) (_lhs_metaVariableTable) (_lhs_metavarConstraints) (_lhs_substitution) =
    let 
    in  (_lhs_collectConstraints,_lhs_currentPhase,_lhs_currentPosition,[],_lhs_metavarConstraints)

