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
    deriving (Show, Eq)

-- | query an export declaration
newtype EQuery = EQuery Text
    deriving (Show, Eq)

-- | query an import declaration
newtype IQuery = IQuery Text
    deriving (Show, Eq)

data DQuery
    = TypeName Text -- ^ query for a type's origin
    | TypeFamilyName Text -- ^ query for a type family
    | ClassName Text -- ^ query for a typeclass
    | Instance Text Text -- ^ query for some instance
    | TypeSignature () -- TODO: implement
    | FuncName Text -- ^ query for a function
    deriving (Show, Eq)

instance AST HaskellAST where
    data Query HaskellAST
        = MName NQuery
        | EName EQuery
        | IName IQuery
        | DName DQuery
        | Range (Query HaskellAST) (Query HaskellAST)
        deriving (Show, Eq)
    match a _ = a
    parsers = Parsers
        { elements = [ typeParser (DName . TypeName)
                     -- TODO: type families
                     , classParser (DName . ClassName)
                     , instanceParser (\a b -> DName (Instance a b))
                     -- TODO: type sigs
                     , valueParser (DName . FuncName)
                     ]
        , chains = [ chainingParser " - " Range ]
        }
