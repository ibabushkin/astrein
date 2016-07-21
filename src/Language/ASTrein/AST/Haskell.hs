{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Language.ASTrein.AST.Haskell where

import Language.ASTrein.AST
import Language.ASTrein.AST.Template
import Language.Haskell.Exts.Annotated as H

import Data.Text (Text, pack)

-- | a Haskell AST
data HaskellAST = HaskellAST
    { moduleHead :: Maybe (H.ModuleHead H.SrcSpanInfo)
    , modulePragmas :: [H.ModulePragma H.SrcSpanInfo]
    , imports :: [H.ImportDecl H.SrcSpanInfo]
    , decls :: [H.Decl H.SrcSpanInfo]
    } deriving (Show, Eq)

-- | query a module name
newtype MQuery = MQuery Text
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
        = MName MQuery
        | EName EQuery
        | IName IQuery
        | DName DQuery
        | Range (Query HaskellAST) (Query HaskellAST)
        deriving (Show, Eq)
    match = haskellMatchAST
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

-- | match a query on an AST
haskellMatchAST :: HaskellAST -> Query HaskellAST -> Maybe HaskellAST
haskellMatchAST ast (MName mQuery) = matchMQuery ast mQuery
haskellMatchAST ast (EName eQuery) = matchEQuery ast eQuery
haskellMatchAST ast (IName iQuery) = matchIQuery ast iQuery

-- | match for a module name
matchMQuery :: HaskellAST -> MQuery -> Maybe HaskellAST
matchMQuery ast (MQuery queryName)
    | Just (H.ModuleHead _ (H.ModuleName _ name) _ _) <- moduleHead ast
    , pack name == queryName = Just ast
    | otherwise = Nothing

-- | match export queries by searching for an appropriate export and querying
-- for it's definition
matchEQuery :: HaskellAST -> EQuery -> Maybe HaskellAST
matchEQuery ast (EQuery queryExport)
    | Just (H.ModuleHead _ _ _ (Just exports)) <- moduleHead ast
    , Just dquery <- findName exports = matchDQuery ast dquery
    | otherwise = Nothing
    where findName (ExportSpecList _ exportList) = foldr go Nothing exportList
          go _ res@(Just _) = res
          go (EVar _ varName) _
              | Just name <- getQName varName, name == queryExport =
                  Just (FuncName name)
              | otherwise = Nothing
          go (EAbs _ _ typeName) _
              | Just name <- getQName typeName, name == queryExport =
                  Just (TypeName name)
              | otherwise = Nothing
          go (EThingAll _ _) _ = Nothing -- TODO
          go (EThingWith _ _ _) _ = Nothing -- TODO
          go (EModuleContents _ _) _ = Nothing -- TODO

matchIQuery :: HaskellAST -> IQuery -> Maybe HaskellAST
matchIQuery ast@HaskellAST{ imports = imports } (IQuery queryImport) =
    foldr go Nothing imports
    where go _ res@(Just _) = res
          go ImportDecl{ importModule = ModuleName _ importName } _
              | pack importName == queryImport = Just ast
              | otherwise = Nothing


matchDQuery _ _ = Nothing

getQName :: QName a -> Maybe Text
getQName (UnQual _ (Ident _ name)) = Just $ pack name -- TODO: find differences
getQName (UnQual _ (Symbol _ name)) = Just $ pack name
getQName _ = Nothing

