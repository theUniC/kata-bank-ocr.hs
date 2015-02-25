import Test.Hspec
import Bank

main :: IO ()
main = hspec $ do
  describe "Bank" $ do
    context "Parse account numbers" $ do
      it "should not parse any account number if no account number is passed" $ do
        parseAccount "" `shouldBe` Nothing

      it "should parse an account number composed of zeros" $ do
        (parseAccount zeroes) `shouldBe` Just (AccountNumber [0, 0, 0, 0, 0, 0, 0, 0, 0])

      it "should parse an Account number composed of ones" $ do
        (parseAccount ones) `shouldBe` Just (AccountNumber [1, 1, 1, 1, 1, 1, 1, 1, 1])

      it "should parse an Account number composer of twos" $ do
        (parseAccount twos) `shouldBe` Just (AccountNumber [2, 2, 2, 2, 2, 2, 2, 2, 2])

      it "should parse an Account number composed of threes" $ do
        (parseAccount threes) `shouldBe` Just (AccountNumber [3, 3, 3, 3, 3, 3, 3, 3, 3])

      it "should parse an Account number composed of fours" $ do
        (parseAccount fours) `shouldBe` Just (AccountNumber [4, 4, 4, 4, 4, 4, 4, 4, 4])

      it "should parse an Account number composed of fives" $ do
        (parseAccount fives) `shouldBe` Just (AccountNumber [5, 5, 5, 5, 5, 5, 5, 5, 5])

      it "should parse an Account number composed of sixes" $ do
        (parseAccount sixes) `shouldBe` Just (AccountNumber [6, 6, 6, 6, 6, 6, 6, 6, 6])

      it "should parse an Account number composed of sevens" $ do
        (parseAccount sevens) `shouldBe` Just (AccountNumber [7, 7, 7, 7, 7, 7, 7, 7, 7])

      it "should parse an Account number composed of eights" $ do
        (parseAccount eights) `shouldBe` Just (AccountNumber [8, 8, 8, 8, 8, 8, 8, 8, 8])

      it "should parse an Account number composed of nines" $ do
        (parseAccount nines) `shouldBe` Just (AccountNumber [9, 9, 9, 9, 9, 9, 9, 9, 9])

      it "should return an Account number composed of 123456789" $ do
        (parseAccount allNumbers) `shouldBe` Just (AccountNumber [1, 2, 3, 4, 5, 6, 7, 8, 9])

    context "Account number validation" $ do
      it "should tell that account number 111111111 is invalid" $ do
        (isValid 111111111) `shouldBe` False

      it "should tell that account number 123456789 is valid" $ do
        (isValid 123456789) `shouldBe` True

      it "should tell that account number 457508000 is a valid account number" $ do
        isValid 457508000 `shouldBe` True

zeroes :: String
zeroes = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    "| || || || || || || || || |",
    "|_||_||_||_||_||_||_||_||_|",
    "                           "
  ]

ones :: String
ones = unlines
  [
    "                           ",
    "  |  |  |  |  |  |  |  |  |",
    "  |  |  |  |  |  |  |  |  |",
    "                           "
  ]

twos :: String
twos = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    " _| _| _| _| _| _| _| _| _|",
    "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
    "                           "
  ]

threes :: String
threes = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    " _| _| _| _| _| _| _| _| _|",
    " _| _| _| _| _| _| _| _| _|",
    "                           "
  ]

fours :: String
fours = unlines
  [
    "                           ",
    "|_||_||_||_||_||_||_||_||_|",
    "  |  |  |  |  |  |  |  |  |",
    "                           "
  ]

fives :: String
fives = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
    " _| _| _| _| _| _| _| _| _|",
    "                           "
  ]

sixes :: String
sixes = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
    "|_||_||_||_||_||_||_||_||_|",
    "                           "
  ]

sevens :: String
sevens = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    "  |  |  |  |  |  |  |  |  |",
    "  |  |  |  |  |  |  |  |  |",
    "                           "
  ]

eights :: String
eights = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    "|_||_||_||_||_||_||_||_||_|",
    "|_||_||_||_||_||_||_||_||_|",
    "                           "
  ]

nines :: String
nines = unlines
  [
    " _  _  _  _  _  _  _  _  _ ",
    "|_||_||_||_||_||_||_||_||_|",
    " _| _| _| _| _| _| _| _| _|",
    "                           "
  ]

allNumbers :: String
allNumbers = unlines
  [
    "    _  _     _  _  _  _  _ ",
    "  | _| _||_||_ |_   ||_||_|",
    "  ||_  _|  | _||_|  ||_| _|",
    "                           "
  ]
