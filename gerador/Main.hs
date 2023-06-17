import Control.Monad (void)
import System.Process (system, readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))
import System.IO
import Test.QuickCheck
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

runHaskellCode :: FilePath -> FilePath -> Int -> IO (String, String)
runHaskellCode filePath optimizedFilePath testNum = do
  let compilationCommand = "ghc -o programa " ++ filePath
      optimizedCompilationCommand = "ghc -O -o programa_optimized " ++ optimizedFilePath
  compilationResult <- system compilationCommand
  optimizedCompilationResult <- system optimizedCompilationCommand
  case (compilationResult, optimizedCompilationResult) of
    (ExitSuccess, ExitSuccess) -> do
      putStrLn "Compilation completed successfully"
      (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell "./programa") ""
      (optimizedExitCode, optimizedStdout, optimizedStderr) <- readCreateProcessWithExitCode (shell "./programa_optimized") ""
      case (exitCode, optimizedExitCode) of
        (ExitSuccess, ExitSuccess) -> do
          putStrLn "Program executed successfully"
          return ("Test " ++ show testNum ++ ": Passed", compareResults stdout optimizedStdout)
        (ExitSuccess, ExitFailure code) -> do
          putStrLn $ "Error during optimized program execution (exit code: " ++ show code ++ ")"
          return ("Test " ++ show testNum ++ ": Failed", "")
        (ExitFailure code, _) -> do
          putStrLn $ "Error during program execution (exit code: " ++ show code ++ ")"
          return ("Test " ++ show testNum ++ ": Failed", "")
    (ExitFailure _, _) -> do
      putStrLn "Error during Haskell code compilation"
      return ("Test " ++ show testNum ++ ": Compilation Error", "")

writeStatistics :: FilePath -> [(String, String)] -> IO ()
writeStatistics filePath statistics = do
  let formattedStatistics = unlines [stat ++ "\nResult: " ++ result | (stat, result) <- statistics]
  writeFile filePath formattedStatistics

compareResults :: String -> String -> String
compareResults result1 result2
  | result1 == result2 = "Results match"
  | otherwise = "Results do not match\n" ++ result1 ++ "\n" ++ result2

testMultipleTimes :: Int -> FilePath -> Int -> IO ()
testMultipleTimes numTests filePath depth = do
  let optimizedFilePath = filePath ++ "_optimized"
  mapM_ (\testNum -> do
           writeExpressionToFile ((insertNumberAfterSlash filePath testNum) ++ "_test.hs") depth
           writeExpressionToFile ((insertNumberAfterSlash optimizedFilePath testNum) ++ "_test.hs") depth -- Generate optimized file
           putStrLn $ "Expression generated and written to file " ++ (insertNumberAfterSlash filePath testNum) ++ "_test.hs"
           (stat, result) <- runHaskellCode ((insertNumberAfterSlash filePath testNum) ++ "_test.hs") ((insertNumberAfterSlash optimizedFilePath testNum) ++ "_test.hs") testNum
           putStrLn stat
           putStrLn result
       ) [1..numTests]

insertNumberAfterSlash :: String -> Int -> String
insertNumberAfterSlash str num = case break (== '/') str of
                                (before, '/':after) -> before ++ "/" ++ show num ++ "_" ++ after
                                _ -> str

main :: IO ()
main = do
  let numTests = 10
      depth = 5
      filePath = "testes/expressions"
  testMultipleTimes numTests filePath depth
  putStrLn "Testing completed"