import Test.Hspec

import qualified ParserSpec
import qualified TypeCheckSpec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Parsing test" ParserSpec.spec
  describe "Typechecking test" TypeCheckSpec.spec
