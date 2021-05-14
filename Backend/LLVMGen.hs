{-|
Module      : Backend.LLVMGen
Description : STG to LLVM translation
Copyright   : Elias Storme, 2020
              Gert-Jan Bottu, 2020

Module containing all translation and support code for translating an STG program into an LLVM module.
Monadic LLVM generation largely based on <https://www.stephendiehl.com/llvm/ this tutorial> by one of the 
authors of the llvm-hs package.
-}

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backend.LLVMGen where

import Backend.STGTypes

import Utils.Errors
import Utils.Trace
import Utils.Unique

import LLVM.AST
import LLVM.AST.Instruction
import qualified LLVM.AST as AST
import LLVM.IRBuilder.Module (buildModuleT)
import qualified LLVM.IRBuilder.Module      as IRB
import qualified LLVM.IRBuilder.Monad       as IRB
import qualified LLVM.IRBuilder.Instruction as IRB

import Control.Monad.State 
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Writer (Writer, runWriter)
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Short as BS

-- | Monad containing infrastructure for building the LLVM module
-- | concerned with global function definitions, etc
type LGenM = UniqueSupplyT (IRB.ModuleBuilderT (ExceptT String (Writer Trace)))
-- | Monad for building pieces of LLVM IR
-- | building the body of a function happens in this monad, which then gets unwrapped by functions in LGenM
type CGenM = IRB.IRBuilderT LGenM

-- | Translate a program from STG to LLVM
tlSProg :: SProg -> LGenM ()
tlSProg pgm = throwUnimplErrorM

tlGblBind :: SBind -> LGenM ()
tlGblBind (SBind x lf) = throwUnimplErrorM 

tlLocBind :: SBind -> LGenM ()
tlLocBind _ = throwUnimplErrorM 

-- | Generate an LLVM module from an STG program
llvmGenModule :: String
              -> UniqueSupply
              -> SProg
              -> (Either String AST.Module, Trace)
llvmGenModule name us pgm = runWriter
                          $ runExceptT
                          $ buildModuleT (BS.toShort $ BU.fromString name)
                          $ flip runUniqueSupplyT us
                          $ tlSProg pgm