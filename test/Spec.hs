import Test.HUnit
import System.Exit
import Elevanagram

bruteForceT1 = TestCase (assertEqual "bruteForce sample1" True (bruteForce [0,0,2,0,0,1,0,0,0]))
bruteForceT2 = TestCase (assertEqual "bruteForce sample2" True (bruteForce [0,0,0,0,0,0,0,0,12]))
bruteForceT3 = TestCase (assertEqual "bruteForce sample3" False (bruteForce [0,0,0,0,2,0,1,1,0]))
bruteForceT4 = TestCase (assertEqual "bruteForce sample4" True (bruteForce [3,1,1,1,0,0,0,0,0]))
bruteForceT5 = TestCase (assertEqual "bruteForce sample5" True (bruteForce [3,0,0,0,0,0,3,0,2]))
bruteForceT6 = TestCase (assertEqual "bruteForce sample6" False (bruteForce [0,0,0,0,0,0,0,1,0]))

bruteForceAltSumT1 = TestCase (assertEqual "bruteForceAltSum sample1" True (bruteForceAltSum [0,0,2,0,0,1,0,0,0]))
bruteForceAltSumT2 = TestCase (assertEqual "bruteForceAltSum sample2" True (bruteForceAltSum [0,0,0,0,0,0,0,0,12]))
bruteForceAltSumT3 = TestCase (assertEqual "bruteForceAltSum sample3" False (bruteForceAltSum [0,0,0,0,2,0,1,1,0]))
bruteForceAltSumT4 = TestCase (assertEqual "bruteForceAltSum sample4" True (bruteForceAltSum [3,1,1,1,0,0,0,0,0]))
bruteForceAltSumT5 = TestCase (assertEqual "bruteForceAltSum sample5" True (bruteForceAltSum [3,0,0,0,0,0,3,0,2]))
bruteForceAltSumT6 = TestCase (assertEqual "bruteForceAltSum sample6" False (bruteForceAltSum [0,0,0,0,0,0,0,1,0]))

shortcutBruteForceT1 = TestCase (assertEqual "shortcut bruteForce sample1" True (shortcut bruteForce [0,0,2,0,0,1,0,0,0]))
shortcutBruteForceT2 = TestCase (assertEqual "shortcut bruteForce sample2" True (shortcut bruteForce [0,0,0,0,0,0,0,0,12]))
shortcutBruteForceT3 = TestCase (assertEqual "shortcut bruteForce sample3" False (shortcut bruteForce [0,0,0,0,2,0,1,1,0]))
shortcutBruteForceT4 = TestCase (assertEqual "shortcut bruteForce sample4" True (shortcut bruteForce [3,1,1,1,0,0,0,0,0]))
shortcutBruteForceT5 = TestCase (assertEqual "shortcut bruteForce sample5" True (shortcut bruteForce [3,0,0,0,0,0,3,0,2]))
shortcutBruteForceT6 = TestCase (assertEqual "shortcut bruteForce sample6" False (shortcut bruteForce [0,0,0,0,0,0,0,1,0]))

shortcutBruteForceAltSumT1 = TestCase (assertEqual "shortcut bruteForceAltSum sample1" True (shortcut bruteForceAltSum [0,0,2,0,0,1,0,0,0]))
shortcutBruteForceAltSumT2 = TestCase (assertEqual "shortcut bruteForceAltSum sample2" True (shortcut bruteForceAltSum [0,0,0,0,0,0,0,0,12]))
shortcutBruteForceAltSumT3 = TestCase (assertEqual "shortcut bruteForceAltSum sample3" False (shortcut bruteForceAltSum [0,0,0,0,2,0,1,1,0]))
shortcutBruteForceAltSumT4 = TestCase (assertEqual "shortcut bruteForceAltSum sample4" True (shortcut bruteForceAltSum [3,1,1,1,0,0,0,0,0]))
shortcutBruteForceAltSumT5 = TestCase (assertEqual "shortcut bruteForceAltSum sample5" True (shortcut bruteForceAltSum [3,0,0,0,0,0,3,0,2]))
shortcutBruteForceAltSumT6 = TestCase (assertEqual "shortcut bruteForceAltSum sample6" False (shortcut bruteForceAltSum [0,0,0,0,0,0,0,1,0]))

bruteForceTests = TestList [bruteForceT1, bruteForceT2, bruteForceT3, bruteForceT4, bruteForceT5, bruteForceT6]
bruteForceAltSumTests = TestList [bruteForceAltSumT1, bruteForceAltSumT2, bruteForceAltSumT3, bruteForceAltSumT4, bruteForceAltSumT5, bruteForceAltSumT6]
shortcutBruteForceTests = TestList [shortcutBruteForceT1, shortcutBruteForceT2, shortcutBruteForceT3, shortcutBruteForceT4, shortcutBruteForceT5, shortcutBruteForceT6]
shortcutBruteForceAltSumTests = TestList [shortcutBruteForceAltSumT1, shortcutBruteForceAltSumT2, shortcutBruteForceAltSumT3, shortcutBruteForceAltSumT4, shortcutBruteForceAltSumT5, shortcutBruteForceAltSumT6]

main :: IO ()
main = do
  counts <- runTestTT (test [bruteForceTests, bruteForceAltSumTests, shortcutBruteForceTests, shortcutBruteForceAltSumTests])
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
