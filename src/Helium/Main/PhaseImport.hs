{-| Module      :  PhaseImport
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Main.PhaseImport(phaseImport) where

import Helium.Main.CompileUtils
import qualified Lvm.Core.Expr as Core
import qualified Lvm.Core.Utils as Core
import Lvm.Common.Id(Id, stringFromId)
import Helium.Syntax.UHA_Syntax
import Helium.Syntax.UHA_Utils
import Helium.Syntax.UHA_Range(noRange)
import Lvm.Read(lvmRead)
import Lvm.Import(lvmImportDecls')
import Helium.ModuleSystem.CoreToImportEnv(getImportEnvironment)
import qualified Helium.ModuleSystem.ExtractImportDecls as EID
import Data.List(isPrefixOf)
import Helium.MonadCompile

phaseImport :: MonadCompile m => String -> Module -> m ([Core.CoreDecl], [ImportEnvironment])
phaseImport fullName module_ = do
    enterNewPhase "Importing"

    let (_, baseName, _) = splitFilePath fullName

    -- Add HeliumLang and Prelude import
    let moduleWithExtraImports = addImplicitImports module_

    -- Chase imports
    chasedImpsList <- chaseImports moduleWithExtraImports

    let indirectionDecls   = concat chasedImpsList
        importEnvs =
            map (getImportEnvironment baseName) chasedImpsList

    return (indirectionDecls, importEnvs)

chaseImports :: MonadCompile m => Module -> m [[Core.CoreDecl]]
chaseImports fromModule =
    let coreImports   = EID.coreImportDecls_Syn_Module $ EID.wrap_Module (EID.sem_Module fromModule) EID.Inh_Module -- Expand imports
        findModule n  = do fileBytes <- findLvmFile n
                           ns <- createNameSupply
                           return $ lvmRead ns (stringFromId n) fileBytes
        doImport :: MonadCompile m => (Core.CoreDecl,[Id]) -> m [Core.CoreDecl]
        doImport (importDecl,hidings)
          = do decls <- lvmImportDecls' findModule [importDecl]
               return [ d
                      | d <- concat decls
                      , let name = Core.declName d
                      , "show" `isPrefixOf` stringFromId name || name `notElem` hidings
                      ]

    in mapM doImport coreImports
        -- zipWith ($) filterImports (lvmImportDecls findModule coreImportDecls)

-- Add "import Prelude" if
--   the currently compiled module is not the Prelude and
--   the Prelude is not explicitly imported
-- Always add "import HeliumLang
addImplicitImports :: Module -> Module
addImplicitImports (Module_Module moduleRange maybeName exports
                   (Body_Body bodyRange explicitImportDecls decls)) =
    Module_Module
        moduleRange
        maybeName
        exports
        (Body_Body
            bodyRange
            ( case maybeName of
                MaybeName_Just n
                    | getNameName n == "Prelude" -> []
                _ -> if "Prelude" `elem` map stringFromImportDeclaration explicitImportDecls
                     then []
                     else [ implicitImportDecl "Prelude" ]
            ++ [ implicitImportDecl "HeliumLang" ]
            ++ explicitImportDecls
            ) decls
        )
  where
    -- Artificial import declaration for implicit Prelude import
    implicitImportDecl :: String -> ImportDeclaration
    implicitImportDecl moduleName =
        ImportDeclaration_Import
            noRange
            False
            (Name_Identifier noRange [] moduleName) -- !!!Name
            MaybeName_Nothing
            MaybeImportSpecification_Nothing
addImplicitImports (Module_Module _ _ _ (Body_Hole _ _)) = error "not supported"
