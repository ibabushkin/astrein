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
matchDQuery :: [Decl SrcSpanInfo] -> DQuery -> QueryResult HaskellAST
matchDQuery decls query =
    case mapMaybe (matchDQuery' query) decls of
      [] -> NoMatch
      ds -> DeclMatch ds

-- match a DQuery on a toplevel declaration
matchDQuery' :: DQuery -> Decl SrcSpanInfo -> Maybe (Decl SrcSpanInfo)
matchDQuery' (TypeName queryName) tDecl
    | TypeDecl _ dHead _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | DataDecl _ _ _ dHead _ _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | GDataDecl _ _ _ dHead _ _ _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | otherwise = Nothing
matchDQuery' (FamilyName queryName) tDecl
    | TypeFamDecl _ dHead _ _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | ClosedTypeFamDecl _ dHead _ _ _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | DataFamDecl _ _ dHead _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | otherwise = Nothing
matchDQuery' (ClassName queryName) tDecl
    | ClassDecl _ _ dHead _ _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | otherwise = Nothing
matchDQuery' (Instance queryClass queryName) tDecl
    | TypeInsDecl _ _ _ <- tDecl = Nothing -- TODO: implement
    | DataInsDecl _ _ _ _ _ <- tDecl = Nothing -- TODO: implement
    | GDataInsDecl _ _ _ _ _ _ <- tDecl = Nothing -- TODO: implement
    | InstDecl _ _ _ _ <- tDecl = Nothing -- TODO: implement
    | DerivDecl _ _ _ <- tDecl = Nothing -- TODO: implement
    | otherwise = Nothing
matchDQuery' (TypeSignature ()) tDecl
    | TypeSig _ _ _ <- tDecl = Nothing -- TODO: implement
    | otherwise = Nothing
matchDQuery' (FuncName queryName) tDecl
    | FunBind _ ((Match _ fName _ _ _):_) <- tDecl
    , getName fName == queryName = Just tDecl
    | FunBind _ ((InfixMatch _ _ fName _ _ _):_) <- tDecl
    , getName fName == queryName = Just tDecl
    | ForImp _ _ _ _ fName _ <- tDecl
    , getName fName == queryName = Just tDecl
    | otherwise = Nothing

-- | get a textual representation of a declaration head's name
getDeclHeadName :: DeclHead SrcSpanInfo -> Text
getDeclHeadName (DHead _ tName) = getName tName
getDeclHeadName (DHInfix _ _ tName) = getName tName
getDeclHeadName (DHParen _ tHead) = getDeclHeadName tHead
getDeclHeadName (DHApp _ tHead _) = getDeclHeadName tHead

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
