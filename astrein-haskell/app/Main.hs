import Language.ASTrein.AST.Haskell (HaskellAST)
import Language.ASTrein.LanguageMain

import qualified Data.Text.IO as TIO

main = languageMain (dispatchMatch TIO.readFile :: Dispatcher HaskellAST)
