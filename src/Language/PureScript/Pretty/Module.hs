{-# LANGUAGE CPP #-}

-- |
-- Pretty printing modules
--
---------------------------------------------------------------------------
module Language.PureScript.Pretty.Module (prettyPrintModule) where

import Control.Monad.State
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Maybe (fromMaybe)

import Language.PureScript.AST.Declarations 
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Values (prettyPrintDeclaration)


prettyPrintModule :: Module -> String
prettyPrintModule = fromMaybe (error "Incomplete pattern")
                    . flip evalStateT (PrinterState 0)
                    . ppModule

ppModule :: Module -> StateT PrinterState Maybe String
ppModule (Module _ _ name decls _) = concat <$> sequence
  [ return "module "
  , return $ show name
  , return " where\n"
  , prettyPrintMany prettyPrintDeclaration decls
  ]

