import Test.Hspec

import qualified DepParserSpec
import qualified DepTypeCheckSpec

import qualified SimplyParserSpec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Simply Typed Parsing Test" SimplyParserSpec.spec
  
  describe "Dependently Typed Parsing Test" DepParserSpec.spec
  describe "Dependently Typed Typechecking Test" DepTypeCheckSpec.spec
