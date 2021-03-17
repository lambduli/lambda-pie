import Test.Hspec

import qualified DepParserSpec
import qualified DepTypeCheckSpec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Parsing test" DepParserSpec.spec
  describe "Typechecking test" DepTypeCheckSpec.spec
