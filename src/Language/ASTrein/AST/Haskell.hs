{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.ASTrein.AST.Haskell where

import Language.ASTrein.AST
import Language.ASTrein.AST.Template
import Language.Haskell.Exts.Annotated

import Data.Text (Text)

-- | a Haskell AST
data HaskellAST = HaskellAST
    { moduleHead :: Maybe (ModuleHead SrcSpanInfo)
    , modulePragmas :: [ModulePragma SrcSpanInfo]
    , imports :: [ImportDecl SrcSpanInfo]
    , decls :: [Decl SrcSpanInfo]
    } deriving (Show, Read, Eq)

data ModuleHeadQuery
    = MHNameQuery String
    | MHExportQuery ExportSpecQuery

data ExportSpecQuery = ESName String

data ImportQuery = IName String

data DeclQuery
    = DTypeName String
    | DTypeSig () -- ^ TODO
    | DClassDecl () -- ^ TODO
    | DInstanceDecl () -- ^ TODO
    | FunBind () -- ^ TODO
