{-| Module      :  CoreToLvm
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.CodeGeneration.CoreToLvm (coreToLvm) where

import Lvm.Core.Expr  (CoreModule)
import Lvm.Core.ToAsm (coreToAsm)         -- enriched lambda expressions (Core) to Asm
import Lvm.Asm.ToLvm  (asmToLvm)          -- translate Asm to instructions
import Lvm.Asm.Inline (asmInline)       -- optimize Asm (ie. inlining)
import Lvm.Write      (lvmToBytes)
import Helium.MonadCompile
import Lvm.Common.Byte (Bytes)

coreToLvm :: (MonadCompile m) => CoreModule -> m Bytes
coreToLvm coremod = do
    nameSupply <- createNameSupply

    -- coreRemoveDead gebeurt al in PhaseDesugarer.hs
    let asmmod  = coreToAsm nameSupply coremod
        asmopt  = asmInline asmmod
        lvmmod  = asmToLvm  asmopt

    return (lvmToBytes lvmmod)
