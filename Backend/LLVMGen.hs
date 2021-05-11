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

import Control.Monad.State
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT)
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Short as BS

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

type LGenM = UniqueSupplyT (ExceptT String (WriterT Trace LLVM))

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

-- | Create module with the given definition, and without definitions
emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = BS.toShort $ BU.fromString label }

-- | Add a top-level definition
addDefn :: Definition -> LGenM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

data CodegenState 
  = CodegenState {
    currentBlock :: AST.Name                -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState -- Blocks for function
  }

data BlockState
  = BlockState {
    idx    :: Int                      -- Block index
  , stack  :: [Named Instruction]      -- Stack of instructions
  , term   :: Maybe (Named Terminator) -- Block terminator
  }

-- | Translate a program from STG to LLVM
tlSProg :: SProg -> LGenM ()
tlSProg pgm = throwUnimplErrorM

-- | Generate an LLVM module from an STG program
llvmGenModule :: String -> UniqueSupply -> SProg -> Either String (AST.Module, Trace)
llvmGenModule name us pgm = runLLVM (emptyModule name)
                          $ runWriterT
                          $ runExceptT
                          $ flip runUniqueSupplyT us
                          $ tlSProg pgm
