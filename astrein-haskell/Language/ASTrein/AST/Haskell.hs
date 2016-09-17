{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, RecordWildCards #-}
module Language.ASTrein.AST.Haskell (HaskellAST(..)) where

import Data.List (intercalate, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import Language.ASTrein.AST
import Language.ASTrein.AST.Haskell.Name
import Language.ASTrein.QueryParser (RawQuery)
import qualified Language.ASTrein.QueryParser as QP
import Language.Haskell.Exts hiding (fileName, name)

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
    | ClassName Text -- ^ query for a typeclass
    | Instance Text Text -- ^ query for some instance
    | FuncName Text -- ^ query for a function
    | Range DQuery DQuery -- ^ query for a range of declarations
    | Or DQuery DQuery -- ^ query for a set of alternative subqueries
    | Nest Text Text -- ^ search for an internal declaration
    deriving (Show, Eq)

instance AST HaskellAST where
    data Query HaskellAST
        = HName HQuery
        | DName DQuery
        deriving (Show, Eq)
    data QueryResult HaskellAST
        = ModuleNameMatch
        | ImportMatch [ImportDecl SrcSpanInfo]
        | DeclMatch [Decl SrcSpanInfo]
        deriving (Show, Eq)
    verifyQuery = verifyHaskellQuery
    parseAST' = parseHaskellAST
    match = haskellMatchAST
    renderMatches = haskellRender

-- | verify a Haskell query from a pre-parsed representation.
verifyHaskellQuery :: RawQuery -> Maybe (Query HaskellAST)
verifyHaskellQuery (QP.Named "module" n) = Just . HName $ MName n
verifyHaskellQuery (QP.Named "export" n) = Just . HName $ EName n
verifyHaskellQuery (QP.Named "import" n) = Just . HName $ IName n
verifyHaskellQuery q = DName <$> verifyDQuery q

-- | verify a DQuery from a pre-parsed representation.
verifyDQuery :: RawQuery -> Maybe DQuery
verifyDQuery (QP.Nest (QP.FuncName o) (QP.FuncName i)) = Just $ Nest o i
verifyDQuery (QP.Or l r) = Or <$> verifyDQuery l <*> verifyDQuery r
verifyDQuery (QP.Range s e) = Range <$> verifyDQuery s <*> verifyDQuery e
verifyDQuery (QP.TypeName n) = Just $ TypeName n
verifyDQuery (QP.ClassName n) = Just $ ClassName n
verifyDQuery (QP.Instance c t) = Just $ Instance c t
verifyDQuery (QP.FuncName n) = Just $ FuncName n
verifyDQuery _ = Nothing

-- | parse a Haskell AST from a file
parseHaskellAST :: Text -> Maybe HaskellAST
parseHaskellAST fileContent =
    case parseFileContents (unpack fileContent) of
      ParseOk (Module _ mHead mPragmas imports decls) ->
          Just $ HaskellAST mHead mPragmas imports decls
      ParseFailed _ _ -> Nothing

-- | match a query on an AST
haskellMatchAST :: Query HaskellAST
                -> HaskellAST
                -> Maybe (QueryResult HaskellAST)
haskellMatchAST (HName hQuery) ast = matchHQuery hQuery ast
haskellMatchAST (DName dQuery) ast = matchDQuery dQuery (decls ast)
haskellMatchAST _ _ = Nothing

data RecursiveHQuery
    = RDQuery DQuery
    | RHQuery HQuery

-- | match a query on an AST's head
matchHQuery :: HQuery -> HaskellAST -> Maybe (QueryResult HaskellAST)
matchHQuery (MName queryName) ast
    | Just (ModuleHead _ (ModuleName _ name) _ _) <- moduleHead ast
    , pack name == queryName = Just ModuleNameMatch
    | otherwise = Nothing
matchHQuery (EName queryExport) ast
    | Just (ModuleHead _ _ _ (Just exports)) <- moduleHead ast
    , Just newQuery <- findName exports =
        case newQuery of
          RDQuery dquery -> matchDQuery dquery (decls ast)
          RHQuery hquery -> matchHQuery hquery ast
    | otherwise = Nothing
    where findName (ExportSpecList _ exportList) = foldr go Nothing exportList
          go _ res@(Just _) = res
          go (EVar _ varName) _
              | Just name <- getQName varName, name == queryExport =
                  Just . RDQuery $ FuncName name
              | otherwise = Nothing
          go (EAbs _ namespace typeName) _
              | Just name <- getQName typeName, name == queryExport =
                  case namespace of
                    NoNamespace _ -> Just . RDQuery $ ClassName name
                    TypeNamespace _ -> Just . RDQuery $ TypeName name
                    PatternNamespace _ -> Just . RDQuery $ FuncName name
              | otherwise = Nothing
          go (EThingWith _ _ thingName cNames) _
              | Just name <- getQName thingName, name == queryExport =
                  Just . RDQuery $ Or (TypeName name) (ClassName name)
              | names <- map getCName cNames, queryExport `elem` names =
                  Just . RDQuery $ FuncName queryExport
              | otherwise = Nothing
          go (EModuleContents _ moduleName) _
              | getModuleName moduleName == queryExport =
                  Just . RHQuery $ IName queryExport
              | otherwise = Nothing
matchHQuery (IName queryImport) HaskellAST{ imports = imports } =
    case filter f imports of
      [] -> Nothing
      is -> Just $ ImportMatch is
      where f ImportDecl {..} = queryImport == getModuleName importModule
                || Just queryImport == (getModuleName <$> importAs)
                || queryImport `elem` getImportNames importSpecs
            getImportNames (Just (ImportSpecList _ _ iSpecs)) =
                concatMap getImportSpecNames iSpecs
            getImportNames _ = []

-- | match a query on an AST's body
matchDQuery :: DQuery -> [Decl SrcSpanInfo] -> Maybe (QueryResult HaskellAST)
 -- FIXME: 2nd and latter matches of first query could come after the last
 -- match of the second query - maybe sort them all?.
matchDQuery (Range q1 q2) decls
    | Just (DeclMatch xs@(x:_)) <- matchDQuery q1 decls
    , Just decls' <- stripPrefix xs (dropWhile (/= x) decls)
    , Just (DeclMatch ys) <- matchDQuery q2 decls'
    , l <- last ys = Just . DeclMatch . (++ [l]) . takeWhile (/= l) $
        dropWhile (/= x) decls
    | otherwise = Nothing
matchDQuery (Nest outer inner) decls
    | Just (DeclMatch res) <- matchDQuery (FuncName outer) decls =
        DeclMatch <$> mconcat (map (matchNestedDecl (FuncName inner)) res)
matchDQuery query decls =
    case mapMaybe (matchDQuery' query) decls of
      [] -> Nothing
      ds -> Just $ DeclMatch ds

-- match a DQuery on a toplevel declaration
matchDQuery' :: DQuery -> Decl SrcSpanInfo -> Maybe (Decl SrcSpanInfo)
matchDQuery' (Or q1 q2) decl
    | Just res <- matchDQuery' q1 decl = Just res
    | otherwise = matchDQuery' q2 decl
matchDQuery' (TypeName queryName) tDecl
    | getTypeDeclName tDecl == Just queryName = Just tDecl
    -- class declarations' members (associated types and data declarations)
    | ClassDecl _ _ _ _ (Just decls) <- tDecl
    , matchClassDecls queryName decls = Just tDecl
    | otherwise = Nothing
matchDQuery' (ClassName queryName) tDecl
    -- class declarations
    | ClassDecl _ _ dHead _ _ <- tDecl
    , getDeclHeadName dHead == queryName = Just tDecl
    | otherwise = Nothing
matchDQuery' (Instance queryClass queryName) tDecl
    -- type instance declarations
    | TypeInsDecl _ (TyApp _ fType iType) _ <- tDecl
    , Just fName <- getTypeName fType, Just iName <- getTypeName iType
    , fName == queryClass && iName == queryName = Just tDecl
    -- data instance declarations
    | DataInsDecl _ _ (TyApp _ fType iType) _ _ <- tDecl
    , Just fName <- getTypeName fType, Just iName <- getTypeName iType
    , fName == queryClass && iName == queryName = Just tDecl
    -- GADT instance declarations
    | GDataInsDecl _ _ (TyApp _ fType iType) _ _ _ <- tDecl
    , Just fName <- getTypeName fType, Just iName <- getTypeName iType
    , fName == queryClass && iName == queryName = Just tDecl
    -- typeclass instance declarations
    | InstDecl _ _ iRule _ <- tDecl, Just (qn, t) <- matchIRule iRule
    , qn == queryClass && t == queryName = Just tDecl
    -- deriving declarations
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
    -- "normal" toplevel value declarations
    | getValueDeclName tDecl == Just queryName = Just tDecl
    -- data constructors
    | Just cNames <- getValueConsNames tDecl
    , queryName `elem` cNames  = Just tDecl
    -- record fields
    | Just fNames <- getRecordFieldNames tDecl
    , queryName `elem` fNames  = Just tDecl
    -- type signatures for toplevel declarations
    | TypeSig _ fNames _ <- tDecl
    , queryName `elem` map getName fNames = Just tDecl
    -- pattern synonyms can be used as values/patterns
    | PatSyn _ (PApp _ cName _) _ _ <- tDecl
    , Just constructorName <- getQName cName
    , constructorName == queryName = Just tDecl
    -- pattern synonyms also have type signatures
    | PatSynSig _ cName _ _ _ _ <- tDecl
    , getName cName == queryName = Just tDecl
    -- fixity declarations for operators
    | InfixDecl _ _ _ ops <- tDecl
    , queryName `elem` map getOpName ops = Just tDecl
    -- class declarations' members
    | ClassDecl _ _ _ _ (Just decls) <- tDecl
    , matchClassDecls queryName decls = Just tDecl
    | otherwise = Nothing
matchDQuery' _ _ = Nothing

-- | match a `DQuery` on the nested declarations of a declaration.
matchNestedDecl :: DQuery -> Decl SrcSpanInfo -> Maybe [Decl SrcSpanInfo]
matchNestedDecl queryName decl =
    mapMaybe (matchDQuery' queryName) <$> getValueDeclNestedDecls decl

-- | check whether a class declaration contains a name somewhere
matchClassDecls :: Text -> [ClassDecl SrcSpanInfo] -> Bool
matchClassDecls queryName = any matchClassDecl
    where matchClassDecl (ClsDecl _ (TypeSig _ names _)) =
              any ((== queryName) . getName) names
          matchClassDecl (ClsDataFam _ _ dHead _) =
              getDeclHeadName dHead == queryName
          matchClassDecl (ClsTyFam _ dHead _ _) =
              getDeclHeadName dHead == queryName
          matchClassDecl _ = False

-- | render the matches on an AST
haskellRender :: ASTMatches HaskellAST -> Text
haskellRender (ASTMatches fileName fileContents (Just res)) =
    "file " <> pack fileName <> ":\n" <> renderQueryResult res
    where renderImportDecl (ImportDecl s _ _ _ _ _ _ _) =
              renderSrcSpanInfo fileContents s
          renderDecl = renderSrcSpanInfo fileContents . declToSrcSpanInfo
          renderQueryResult ModuleNameMatch = "module name matched."
          renderQueryResult (ImportMatch ids) =
              "imports matched:\n" <> mconcat (map renderImportDecl ids)
          renderQueryResult (DeclMatch ds) =
              "declarations matched:\n" <> mconcat (groupedMatches ds)
          groupedMatches ds = intercalate ["\n"] . map (map renderDecl) $
              foldr groupMatches [] ds
          groupMatches m1@TypeSig{} ([m2@FunBind{}]:ms) =
              [m1,m2]:ms
          groupMatches m1@TypeSig{} ([m2@PatBind{}]:ms) =
              [m1,m2]:ms
          groupMatches m1@InfixDecl{} ([m2@TypeSig{}]:ms) =
              [m1,m2]:ms
          groupMatches m1@InfixDecl{} ([m2@FunBind{}]:ms) =
              [m1,m2]:ms
          groupMatches m1@InfixDecl{} ([m2@PatBind{}]:ms) =
              [m1,m2]:ms
          groupMatches m1@PatSynSig{} ([m2@PatSyn{}]:ms) =
              [m1,m2]:ms
          groupMatches m ms = [m]:ms
haskellRender (ASTMatches fileName _ Nothing) =
    "no matches in " <> pack fileName <> "."

-- | show a part of a file denoted by a SrcSpanInfo
renderSrcSpanInfo :: Text -> SrcSpanInfo -> Text
renderSrcSpanInfo text (SrcSpanInfo (SrcSpan _ sl _ el _) _) =
    (T.unlines . take (el - sl + 1) . drop (sl - 1) . T.lines) text

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
