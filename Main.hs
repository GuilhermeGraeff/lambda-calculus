import Control.Monad.IO.Class (liftIO)
import System.IO
import Test.QuickCheck
import System.Process
import System.Exit (ExitCode(..))
import ExprGenerator
import TypeGenerator
import Lexer
import Control.Monad (mapM_)

exprGenerator :: Int -> FilePath -> Gen Expr
exprGenerator depth _ = do
  types <- typeGenerator depth
  expr <- genExpr depth [] types
  return expr

writeExpressionToFile :: FilePath -> Int -> IO ()
writeExpressionToFile filePath depth = do
  let expressionGen = exprGenerator depth filePath
  expression <- generate expressionGen
  writeFile filePath ("main :: IO ()\nmain = print (" ++ (show expression) ++ ")")

runHaskellCode :: FilePath -> Int -> IO (String, String)
runHaskellCode filePath testNum = do
  let compilationCommand = "ghc -o programa " ++ filePath
  compilationResult <- system compilationCommand
  case compilationResult of
    ExitSuccess -> do
      putStrLn "Compilation completed successfully"
      (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell "./programa") ""
      case exitCode of
        ExitSuccess -> do
          putStrLn "Program executed successfully"
          return ("Test " ++ show testNum ++ ": Passed", stdout)
        ExitFailure code -> do
          putStrLn $ "Error during program execution (exit code: " ++ show code ++ ")"
          return ("Test " ++ show testNum ++ ": Failed", "")
    ExitFailure _ -> do
      putStrLn "Error during Haskell code compilation"
      return ("Test " ++ show testNum ++ ": Compilation Error", "")

writeStatistics :: FilePath -> [(String, String)] -> IO ()
writeStatistics filePath statistics = do
  let formattedStatistics = unlines [stat ++ "\nResult: " ++ result | (stat, result) <- statistics]
  writeFile filePath formattedStatistics

testMultipleTimes :: Int -> FilePath -> Int -> IO ()
testMultipleTimes numTests filePath depth = do
  statistics <- mapM (\testNum -> do
                        writeExpressionToFile (filePath ++ "_test_" ++ show testNum ++ ".hs") depth
                        putStrLn $ "Expression generated and written to file " ++ filePath ++ "_test_" ++ show testNum ++ ".hs"
                        (stat, result) <- runHaskellCode (filePath ++ "_test_" ++ show testNum ++ ".hs") testNum
                        return (stat, result)
                    ) [1..numTests]
  writeStatistics (filePath ++ "_statistics.txt") statistics

main :: IO ()
main = do
  let numTests = 1000
      depth = 50
      filePath = "testes/expressions"
  testMultipleTimes numTests filePath depth
  putStrLn "Testing completed"