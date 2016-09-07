import Language.ASTrein.AST.Haskell (HaskellAST)
import Language.ASTrein.Util

main = languageMain (dispatchMatch :: Dispatcher HaskellAST)
