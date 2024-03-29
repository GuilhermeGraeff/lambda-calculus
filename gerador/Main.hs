import Control.Monad (void)
import System.Process (system, readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))
import System.IO
import Test.QuickCheck
import ExprGenerator
import TypeGenerator
import Lexer
import Control.Monad (mapM_)
import System.Directory (removeFile)

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


--                -O
runHaskellCode :: String -> FilePath -> Int -> IO (String, String)
runHaskellCode optimization filePath testNum = do
  let compilationCommand = "ghc " ++ optimization ++ " " ++ filePath ++ " -o programa"
  putStrLn $ "Compiling Haskell code with command: " ++ compilationCommand
  compilationResult <- system compilationCommand
  case (compilationResult) of
    (ExitSuccess) -> do
      putStrLn "Compilation completed successfully"
      (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell "./programa") ""
      case (exitCode) of
        (ExitSuccess) -> do
          putStrLn "Program executed successfully"
          removeFile ((take (length filePath - 3) filePath) ++ ".o")
          removeFile ((take (length filePath - 3) filePath) ++ ".hi")
          return ("Test " ++ show testNum ++ ": Passed", stdout)
        (ExitFailure code) -> do
          putStrLn $ "Error during program execution (exit code: " ++ show code ++ ")"
          return ("Test " ++ show testNum ++ ": Failed", "")
    (ExitFailure _) -> do
      putStrLn "Error during Haskell code compilation"
      return ("Test " ++ show testNum ++ ": Compilation Error", "")

writeStatistics :: FilePath -> (String, String) -> IO ()
writeStatistics filePath (stat,result) = do
  let formattedStatistics = stat ++ "\nResult: \n" ++ result
  appendFile filePath formattedStatistics

compareResults :: String -> String -> String
compareResults result1 result2
  | lines result1 == lines result2 = "Results match"
  | otherwise = "Results do not match\n" ++ result1 ++ "\n" ++ result2

testMultipleTimes :: Int -> FilePath -> Int -> IO ()
testMultipleTimes numTests filePath depth = do
  statistics <- mapM (\testNum -> do
                        let filePathName = insertNumberAfterSlash filePath testNum ++ "_test.hs"
                        writeExpressionToFile filePathName depth
                        putStrLn $ "Expression generated and written to file " ++ filePathName
                        (stat_0, result_0) <- runHaskellCode "-O0" filePathName testNum
                        (stat, result) <- runHaskellCode "" filePathName testNum
                        (stat_1, result_1) <- runHaskellCode "-O1" filePathName testNum
                        (stat_2, result_2) <- runHaskellCode "-O2" filePathName testNum
                        (stat_3, result_3) <- runHaskellCode "-O3" filePathName testNum
                        putStrLn "--Normal--"
                        putStrLn stat
                        putStr result
                        putStrLn "--Optimized 0--"
                        putStrLn stat_0
                        putStr result_0
                        putStrLn "--Optimized 1--"
                        putStrLn stat_1
                        putStr result_1
                        putStrLn "--Optimized 2--"
                        putStrLn stat_2
                        putStr result_2
                        putStrLn "--Optimized 3--"
                        putStrLn stat_3
                        putStr result_3
                        putStrLn "===="
                        putStrLn $ compareResults stat_0 result_0
                        putStrLn $ compareResults stat result
                        putStrLn $ compareResults stat_1 result_1
                        putStrLn $ compareResults stat_2 result_2
                        putStrLn $ compareResults stat_3 result_3
                        let statuses = unlines [stat, stat_0, stat_1, stat_2, stat_3]
                            resultuses = unlines [result, result_0, result_1, result_2, result_3]
                        let statisticsFilePath = filePath ++ "_statistics.txt"
                        writeStatistics statisticsFilePath (statuses, resultuses)
                        return (statuses, "\n" ++ resultuses)
                      ) [1..numTests]
  let statisticsFilePath = filePath ++ "_statistics.txt"
  putStrLn $ "Test ended, statistics at: " ++ statisticsFilePath

insertNumberAfterSlash :: String -> Int -> String
insertNumberAfterSlash str num = case break (== '/') str of
                                (before, '/':after) -> before ++ "/" ++ show num ++ "_" ++ after
                                _ -> str

main :: IO ()
main = do
  let numTests = 1000
      depth = 5
      filePath = "testes/expressions"
  testMultipleTimes numTests filePath depth
  putStrLn "Testing completed"