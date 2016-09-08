import Language.ASTrein.AST.Haskell (HaskellAST)
import Language.ASTrein.LanguageMain

main = languageMain (dispatchMatch :: Dispatcher HaskellAST)
