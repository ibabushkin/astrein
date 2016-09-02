{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Language.ASTrein.AST.Haskell where

import Language.ASTrein.AST
import Language.ASTrein.AST.Template
import Language.Haskell.Exts

import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
        deriving (Show, Eq)
    parseAST' = parseHaskellAST
    match = haskellMatchAST
    render = haskellRender
    parsers = Parsers
        { elements =
            [ elementParser "m." (HName . MName)
            , elementParser "e." (HName . EName)
            , elementParser "i." (HName . IName)
            , typeParser (DName . TypeName)
            , elementParser "|" (DName . FamilyName)
            , classParser (DName . ClassName)
            , instanceParser (\a b -> DName (Instance a b))
            , valueParser (DName . FuncName)
            ]
        , chains = [ chainingParser " - " Range ]
        }

parseHaskellAST :: FilePath -> IO (Maybe HaskellAST)
parseHaskellAST file = do
    res <- parseFile file
    case res of
      ParseOk (Module _ mHead mPragmas imports decls) ->
          return . Just $ HaskellAST mHead mPragmas imports decls
      ParseFailed _ _ -> return Nothing

-- | match a query on an AST
haskellMatchAST :: Query HaskellAST -> HaskellAST -> QueryResult HaskellAST
haskellMatchAST (HName hQuery) ast = matchHQuery ast hQuery
haskellMatchAST (DName dQuery) ast = matchDQuery (decls ast) dQuery
haskellMatchAST (Range (DName q1) (DName q2)) HaskellAST{ decls = decls }
    | DeclMatch xs@(x:_) <- matchDQuery decls q1
    , Just decls' <- stripPrefix xs (dropWhile (/= x) decls)
    , DeclMatch ys <- matchDQuery decls' q2 =
        DeclMatch . takeWhile (/= last ys) $ dropWhile (/= x) decls
    | otherwise = NoMatch

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
    | TypeInsDecl _ (TyApp _ fType iType) _ <- tDecl
    , Just fName <- getTypeName fType, Just iName <- getTypeName iType
    , fName == queryClass && iName == queryName = Just tDecl
    | DataInsDecl _ _ (TyApp _ fType iType) _ _ <- tDecl
    , Just fName <- getTypeName fType, Just iName <- getTypeName iType
    , fName == queryClass && iName == queryName = Just tDecl
    | GDataInsDecl _ _ (TyApp _ fType iType) _ _ _ <- tDecl
    , Just fName <- getTypeName fType, Just iName <- getTypeName iType
    , fName == queryClass && iName == queryName = Just tDecl
    | InstDecl _ _ iRule _ <- tDecl, Just (qn, t) <- matchIRule iRule
    , qn == queryClass && t == queryName = Just tDecl
    | DerivDecl _ _ iRule <- tDecl, Just (qn, t) <- matchIRule iRule
    , qn == queryClass && t == queryName = Just tDecl
    | otherwise = Nothing
    where matchIRule (IParen _ iRule) = matchIRule iRule
          matchIRule (IRule _ _ _ iHead) = matchIHead iHead
          matchIHead (IHParen _ iHead) = matchIHead iHead
          matchIHead (IHApp _ (IHCon _ qn) t) =
              (,) <$> getQName qn <*> getTypeName t
          matchIHead _ = Nothing
matchDQuery' (FuncName queryName) tDecl
    | FunBind _ (Match _ fName _ _ _:_) <- tDecl
    , getName fName == queryName = Just tDecl
    | FunBind _ (InfixMatch _ _ fName _ _ _:_) <- tDecl
    , getName fName == queryName = Just tDecl
    | ForImp _ _ _ _ fName _ <- tDecl
    , getName fName == queryName = Just tDecl
    | PatBind _ (PVar _ fName) _ _ <- tDecl
    , getName fName == queryName = Just tDecl
    | TypeSig _ fNames _ <- tDecl
    , queryName `elem` map getName fNames = Just tDecl
    | otherwise = Nothing

-- | get a textual representation of a module name
getModuleName :: ModuleName a -> Text
getModuleName (ModuleName _ name) = pack name

-- | get a textual representation of a name
getName :: Name a -> Text
getName (Ident _ name) = pack name -- TODO: find differences between the cases
getName (Symbol _ name) = pack name

-- | get a textual representation of a (possibly qualified name)
getQName :: QName a -> Maybe Text
getQName (UnQual _ name) = Just $ getName name
getQName (Qual _ mName name) =
    Just $ getModuleName mName <> "." <> getName name
getQName _ = Nothing

-- | get a textual representation of a declaration head's name
getDeclHeadName :: DeclHead a -> Text
getDeclHeadName (DHead _ tName) = getName tName
getDeclHeadName (DHInfix _ _ tName) = getName tName
getDeclHeadName (DHParen _ tHead) = getDeclHeadName tHead
getDeclHeadName (DHApp _ tHead _) = getDeclHeadName tHead

-- | get a textual representation of a type's name
-- TODO: maybe we should return a result for everything?
getTypeName :: Type a -> Maybe Text
getTypeName (TyForall _ _ _ t) = getTypeName t
getTypeName (TyApp _ t _) = getTypeName t
getTypeName (TyCon _ qn) = getQName qn
getTypeName (TyParen _ t) = getTypeName t
getTypeName (TyInfix _ _ qn _) = getQName qn
getTypeName (TyKind _ t _) = getTypeName t
getTypeName (TyBang _ _ _ t) = getTypeName t
getTypeName _ = Nothing

haskellRender :: FileMatches HaskellAST -> IO Text
haskellRender (Right (ASTMatches file res)) =
    mappend ("file " <> pack file <> ":\n") <$> (haskellRender' res)
haskellRender (Left file) =
    return $ "file " <> pack file <> ": could not be parsed\n"

-- | render a QueryResult
haskellRender' :: QueryResult HaskellAST -> IO Text
haskellRender' ModuleNameMatch = return "module name matched."
haskellRender' (ImportMatch ids) = do
    decls <- mapM renderImportDecl ids
    return $ "imports matched:\n" <> T.unlines decls
    where renderImportDecl (ImportDecl s _ _ _ _ _ _ _) = renderSrcSpanInfo s
haskellRender' (DeclMatch ds) = do
    decls <- mapM renderDecl ds
    return $ "declarations matched:\n" <> T.intercalate "\n" decls
    where renderDecl = renderSrcSpanInfo . declToSrcSpanInfo
haskellRender' NoMatch = return "no matches."

-- | show a part of a file denoted by a SrcSpanInfo
renderSrcSpanInfo :: SrcSpanInfo -> IO Text
renderSrcSpanInfo (SrcSpanInfo (SrcSpan f sl sc el ec) _) =
    (T.unlines . take (el - sl + 1) . drop (sl - 1) . T.lines) <$>
        TIO.readFile f

-- | get a SrcSpanInfo from a Decl
declToSrcSpanInfo :: Decl SrcSpanInfo -> SrcSpanInfo
declToSrcSpanInfo decl =
    case decl of
      TypeDecl s _ _ -> s
      TypeFamDecl s _ _ _ -> s
      ClosedTypeFamDecl s _ _ _ _ -> s
      DataDecl s _ _ _ _ _ -> s
      GDataDecl s _ _ _ _ _ _ -> s
      DataFamDecl s _ _ _ -> s
      TypeInsDecl s _ _ -> s
      DataInsDecl s _ _ _ _ -> s
      GDataInsDecl s _ _ _ _ _ -> s
      ClassDecl s _ _ _ _ -> s
      InstDecl s _ _ _ -> s
      DerivDecl s _ _ -> s
      InfixDecl s _ _ _ -> s
      DefaultDecl s _ -> s
      SpliceDecl s _ -> s
      TypeSig s _ _ -> s
      PatSynSig s _ _ _ _ _ -> s
      FunBind s _ -> s
      PatBind s _ _ _ -> s
      PatSyn s _ _ _ -> s
      ForImp s _ _ _ _ _ -> s
      ForExp s _ _ _ _ -> s
      RulePragmaDecl s _ -> s
      DeprPragmaDecl s _ -> s
      WarnPragmaDecl s _ -> s
      InlineSig s _ _ _ -> s
      InlineConlikeSig s _ _ -> s
      SpecSig s _ _ _ -> s
      SpecInlineSig s _ _ _ _ -> s
      InstSig s _ -> s
      AnnPragma s _ -> s
      MinimalPragma s _ -> s
      RoleAnnotDecl s _ _ -> s
