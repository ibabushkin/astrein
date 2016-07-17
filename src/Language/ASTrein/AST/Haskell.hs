{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.ASTrein.AST.Haskell where

import Language.ASTrein.AST
import Language.ASTrein.AST.Template
import qualified Language.Haskell.Exts.Annotated as H

import Data.Text (Text)

-- | a Haskell AST
data HaskellAST = HaskellAST
    { moduleHead :: Maybe (H.ModuleHead H.SrcSpanInfo)
    , modulePragmas :: [H.ModulePragma H.SrcSpanInfo]
    , imports :: [H.ImportDecl H.SrcSpanInfo]
    , decls :: [H.Decl H.SrcSpanInfo]
    } deriving (Show, Eq)

-- | query a module name
newtype NQuery = NQuery Text

-- | query an export declaration
newtype EQuery = EQuery Text

-- | query an import declaration
newtype IQuery = IQuery Text

data DQuery
    = TypeName Text -- ^ query for a type's origin - this might be a data decl
    | TypeFamily Text -- ^ query for a type family - this might be a data fam
    | Class Text -- ^ query for a typeclass
    | Instance Text Text -- ^ query for some instance
    | TypeSignature () -- TODO: implement
    | Func Text -- ^ query for a function

instance AST HaskellAST where
    data Query HaskellAST
        = MName NQuery
        | EName EQuery
        | IName IQuery
        | DName DQuery
    match a _ = a
    parsers = Parsers [] []
