{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Language.ASTrein.AST.Haskell where

import Language.ASTrein.AST
import Language.ASTrein.AST.Template
import Language.Haskell.Exts

import Data.Monoid ((<>))
import Data.Text (Text, pack)

-- | a Haskell AST
data HaskellAST = HaskellAST
    { moduleHead :: Maybe (ModuleHead SrcSpanInfo) -- ^ module head
    , modulePragmas :: [ModulePragma SrcSpanInfo] -- ^ pragmas
    , imports :: [ImportDecl SrcSpanInfo] -- ^ imported modules
    , decls :: [Decl SrcSpanInfo] -- ^ normal declarations
    } deriving (Show, Eq)

-- | query for a declaration in the module head (including imports)
data HQuery
    = MName Text -- ^ query for a module name
    | EName Text -- ^ query for an export declaration
    | IName Text -- ^ query an import declaration
    deriving (Show, Eq)

-- | query for a declaration in the source's body
data DQuery
    = TypeName Text -- ^ query for a type's origin
    | FamilyName Text -- ^ query for a type- or data family
    | ClassName Text -- ^ query for a typeclass
    | Instance Text Text -- ^ query for some instance
    | TypeSignature () -- TODO: implement
    | FuncName Text -- ^ query for a function
    deriving (Show, Eq)

instance AST HaskellAST where
    data Query HaskellAST
        = HName HQuery
        | DName DQuery
        | Range (Query HaskellAST) (Query HaskellAST)
        deriving (Show, Eq)
    match = haskellMatchAST
    parsers = Parsers
        { elements = [ elementParser "m." (HName . MName)
                     , elementParser "e." (HName . EName)
                     , elementParser "i." (HName . IName)
                     , typeParser (DName . TypeName)
                     , elementParser "|" (DName . FamilyName)
                     , classParser (DName . ClassName)
                     , instanceParser (\a b -> DName (Instance a b))
                     -- TODO: type sigs
                     , valueParser (DName . FuncName)
                     ]
        , chains = [ chainingParser " - " Range ]
        }

-- | match a query on an AST
haskellMatchAST :: HaskellAST -> Query HaskellAST -> Maybe HaskellAST
haskellMatchAST ast (HName hQuery) = matchHQuery ast hQuery
haskellMatchAST ast (DName dQuery) = matchDQuery ast dQuery

-- | match a query on an AST's head
matchHQuery :: HaskellAST -> HQuery -> Maybe HaskellAST
matchHQuery ast (MName queryName)
    | Just (ModuleHead _ (ModuleName _ name) _ _) <- moduleHead ast
    , pack name == queryName = Just ast
    | otherwise = Nothing
matchHQuery ast (EName queryExport)
    | Just (ModuleHead _ _ _ (Just exports)) <- moduleHead ast
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
          go (EThingWith _ _ thingName _) _
              | Just name <- getQName thingName, name == queryExport =
                  Just (TypeName name) -- FIXME: what about typeclasses?
              | otherwise = Nothing
          -- ignore module reexports for now
          go (EModuleContents _ _) _ = Nothing
matchHQuery ast@HaskellAST{ imports = imports } (IName queryImport) =
    foldr go Nothing imports
    where go _ res@(Just _) = res
          go ImportDecl{ importModule = ModuleName _ importName } _
              | pack importName == queryImport = Just ast
              | otherwise = Nothing

-- | match a query on an AST's body
-- TODO: implement properly
matchDQuery :: HaskellAST -> DQuery -> Maybe HaskellAST
matchDQuery _ _ = Nothing

-- | get a textual representation of a (possibly qualified name)
getQName :: QName a -> Maybe Text
getQName (UnQual _ name) = Just $ getName name
getQName (Qual _ mName name) =
    Just $ getModuleName mName <> "." <> getName name
getQName _ = Nothing

-- | get a textual representation of a name
getName :: Name a -> Text
getName (Ident _ name) = pack name -- TODO: find differences between the cases
getName (Symbol _ name) = pack name

-- | get a textual representation of a module name
getModuleName :: ModuleName a -> Text
getModuleName (ModuleName _ name) = pack name
