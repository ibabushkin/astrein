{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Language.ASTrein.AST.Haskell where

import Language.ASTrein.AST
import Language.ASTrein.AST.Template
import Language.Haskell.Exts

import Data.Maybe (mapMaybe)
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
    data QueryResult HaskellAST
        = ModuleNameMatch
        | ImportMatch [ImportDecl SrcSpanInfo]
        | DeclMatch [Decl SrcSpanInfo]
        | NoMatch
    match = undefined
    parsers = Parsers
        { elements =
            [ elementParser "m." (HName . MName)
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
haskellMatchAST :: HaskellAST -> Query HaskellAST -> QueryResult HaskellAST
haskellMatchAST ast (HName hQuery) = matchHQuery ast hQuery
haskellMatchAST ast (DName dQuery) = matchDQuery (decls ast) dQuery

-- | match a query on an AST's head
matchHQuery :: HaskellAST -> HQuery -> QueryResult HaskellAST
matchHQuery ast (MName queryName)
    | Just (ModuleHead _ (ModuleName _ name) _ _) <- moduleHead ast
    , pack name == queryName = ModuleNameMatch
    | otherwise = NoMatch
matchHQuery ast (EName queryExport)
    | Just (ModuleHead _ _ _ (Just exports)) <- moduleHead ast
    , Just dquery <- findName exports = matchDQuery (decls ast) dquery
    | otherwise = NoMatch
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
    case filter pred imports of
      [] -> NoMatch
      is -> ImportMatch is
    where pred d@ImportDecl{ importModule = ModuleName _ importName } =
              pack importName == queryImport

-- | match a query on an AST's body
-- TODO: implement properly
matchDQuery :: [Decl SrcSpanInfo] -> DQuery -> QueryResult HaskellAST
matchDQuery decls query =
    case mapMaybe (matchDQuery' query) decls of
      [] -> NoMatch
      ds -> DeclMatch ds

-- match a DQuery on a toplevel declaration
matchDQuery' :: DQuery -> Decl SrcSpanInfo -> Maybe (Decl SrcSpanInfo)
matchDQuery' query@(TypeName queryName) tDecl@(TypeDecl ann dHead body)
    | DHead _ tName <- dHead, getName tName == queryName = Just tDecl
    | DHInfix _ _ tName <- dHead, getName tName == queryName = Just tDecl
    | DHParen _ tHead' <- dHead =
        matchDQuery' query (TypeDecl ann tHead' body) >> Just tDecl
    | DHApp _ tHead' _ <- dHead =
        matchDQuery' query (TypeDecl ann tHead' body) >> Just tDecl
matchDQuery' query@(FamilyName queryName) fDecl@(TypeFamDecl ann dHead res inj)
    | DHead _ fName <- dHead, getName fName == queryName = Just fDecl
    | DHInfix _ _ fName <- dHead, getName fName == queryName = Just fDecl
    | DHParen _ fHead' <- dHead =
        matchDQuery' query (TypeFamDecl ann fHead' res inj) >> Just fDecl
    | DHApp _ fHead' _ <- dHead =
        matchDQuery' query (TypeFamDecl ann fHead' res inj) >> Just fDecl
matchDQuery' query@(FamilyName queryName) fDecl@(ClosedTypeFamDecl ann dHead res inj eqn)
    | DHead _ fName <- dHead, getName fName == queryName = Just fDecl
    | DHInfix _ _ fName <- dHead, getName fName == queryName = Just fDecl
    | DHParen _ fHead' <- dHead =
        matchDQuery' query (ClosedTypeFamDecl ann fHead' res inj eqn) >> Just fDecl
    | DHApp _ fHead' _ <- dHead =
        matchDQuery' query (ClosedTypeFamDecl ann fHead' res inj eqn) >> Just fDecl
matchDQuery' query@(TypeName queryName) dDecl@(DataDecl ann don con dHead cons der)
    | DHead _ dName <- dHead, getName dName == queryName = Just dDecl
    | DHInfix _ _ dName <- dHead, getName dName == queryName = Just dDecl
    | DHParen _ dHead' <- dHead =
        matchDQuery' query (DataDecl ann don con dHead' cons der) >> Just dDecl
    | DHApp _ dHead' _ <- dHead =
        matchDQuery' query (DataDecl ann don con dHead' cons der) >> Just dDecl
matchDQuery' _ _ = Nothing

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
