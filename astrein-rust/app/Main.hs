import qualified Data.Text.IO as TIO

import Language.ASTrein.AST.Rust (RustAST)
import Language.ASTrein.LanguageMain

main = languageMain (dispatchMatch TIO.readFile :: Dispatcher RustAST)
