import Test.QuickCheck (quickCheck)
import Wine (prop_shareWine)

main :: IO ()
main = quickCheck prop_shareWine