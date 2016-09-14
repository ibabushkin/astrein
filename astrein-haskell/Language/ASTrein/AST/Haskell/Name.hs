{-# LANGUAGE OverloadedStrings #-}
module Language.ASTrein.AST.Haskell.Name where

import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)

import Language.Haskell.Exts

-- | check for a type declaration, and optionally get it's name.
-- this includes data declarations and other things that yield a type.
getTypeDeclName :: Decl a -> Maybe Text
getTypeDeclName (TypeDecl _ dHead _) = Just $ getDeclHeadName dHead
getTypeDeclName (DataDecl _ _ _ dHead _ _) = Just $ getDeclHeadName dHead
getTypeDeclName (GDataDecl _ _ _ dHead _ _ _) = Just $ getDeclHeadName dHead
getTypeDeclName (TypeFamDecl _ dHead _ _) = Just $ getDeclHeadName dHead
getTypeDeclName (ClosedTypeFamDecl _ dHead _ _ _) =
    Just $ getDeclHeadName dHead
getTypeDeclName (DataFamDecl _ _ dHead _) = Just $ getDeclHeadName dHead
getTypeDeclName _ = Nothing

-- | check for a value declaration, and optionally get it's name.
getValueDeclName :: Decl a -> Maybe Text
getValueDeclName (FunBind _ (Match _ fName _ _ _:_)) = Just $ getName fName
getValueDeclName (FunBind _ (InfixMatch _ _ fName _ _ _:_)) =
    Just $ getName fName
getValueDeclName (PatBind _ (PVar _ fName) _ _) = Just $ getName fName
getValueDeclName (ForImp _ _ _ _ fName _) = Just $ getName fName

-- | check for a data declaration, and optionally get the name of all value
-- constructors defined by it.
getValueConsNames :: Decl a -> Maybe [Text]
getValueConsNames (DataDecl _ _ _ _ conDecls _) =
    Just $ map getQualConDeclName conDecls
getValueConsNames (GDataDecl _ _ _ _ _ conDecls _) =
    Just $ map getGadtDeclName conDecls
getValueConsNames (DataInsDecl _ _ _ conDecls _) =
    Just $ map getQualConDeclName conDecls
getValueConsNames (GDataInsDecl _ _ _ _ conDecls _) =
    Just $ map getGadtDeclName conDecls
getValueConsNames _ = Nothing

-- | check for a data declaration, and optionally get the name of all record
-- fields defined by it.
getRecordFieldNames :: Decl a -> Maybe [Text]
getRecordFieldNames (DataDecl _ _ _ _ conDecls _) =
    Just $ concatMap getQualConDeclRecordFields conDecls
getRecordFieldNames (GDataDecl _ _ _ _ _ conDecls _) =
    Just $ concatMap getGadtDeclRecordFields conDecls
getRecordFieldNames (DataInsDecl _ _ _ conDecls _) =
    Just $ concatMap getQualConDeclRecordFields conDecls
getRecordFieldNames (GDataInsDecl _ _ _ _ conDecls _) =
    Just $ concatMap getGadtDeclRecordFields conDecls
getRecordFieldNames _ = Nothing

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
-- TODO: maybe we should return a result for everything? Or should we restrict
-- outselves to TyApp and TyCon, along with TyParen?
getTypeName :: Type a -> Maybe Text
getTypeName (TyForall _ _ _ t) = getTypeName t
getTypeName (TyApp _ t _) = getTypeName t
getTypeName (TyCon _ qn) = getQName qn
getTypeName (TyParen _ t) = getTypeName t
getTypeName (TyInfix _ _ qn _) = getQName qn
getTypeName (TyKind _ t _) = getTypeName t
getTypeName (TyBang _ _ _ t) = getTypeName t
getTypeName _ = Nothing

-- | get a textual representation of the names of a record field.
getFieldNames :: FieldDecl a -> [Text]
getFieldNames (FieldDecl _ names _) = map getName names

-- | get a textual representation of a data constructor's name.
getQualConDeclName :: QualConDecl a -> Text
getQualConDeclName (QualConDecl _ _ _ conDecl) = getConDeclName conDecl
    where getConDeclName (ConDecl _ cName _) = getName cName
          getConDeclName (InfixConDecl _ _ cName _) = getName cName
          getConDeclName (RecDecl _ cName _) = getName cName

-- | get a textual representation of a data constructor's record fields.
getQualConDeclRecordFields :: QualConDecl a -> [Text]
getQualConDeclRecordFields (QualConDecl _ _ _ conDecl) =
    getConDeclRecordFields conDecl
    where getConDeclRecordFields (RecDecl _ _ fs) = concatMap getFieldNames fs
          getConDeclRecordFields _ = []

-- | get a textual representation of a GADT's data constructor's name.
getGadtDeclName :: GadtDecl a -> Text
getGadtDeclName (GadtDecl _ gName _ _) = getName gName

-- | get a textual representation of a GADT's data constructor's record fields.
getGadtDeclRecordFields :: GadtDecl a -> [Text]
getGadtDeclRecordFields (GadtDecl _ _ (Just fs) _) = concatMap getFieldNames fs
getGadtDeclRecordFields _ = []

-- | get a textual representation of an operator's name.
getOpName :: Op a -> Text
getOpName (VarOp _ name) = getName name
getOpName (ConOp _ name) = getName name
