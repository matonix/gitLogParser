{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( gitPeriods
    ) where

import           Control.Monad    (liftM2)
import           Csv
import           Data.Char        (isDigit)
import           Data.Csv
import           Data.List
import           Data.Time
import           GitLogParser
import           System.Directory
import           System.Process
import qualified Text.Parsec      as P

resultDir = "./results/"
reposDir = "/home/maton/experimentBTS/repos/"
-- reposDir = "/home/maton/experimentBTS/defects4j/project_repos/"
-- aGitRepos = "commons-lang"
-- aGitRepos = "commons-math"
aGitRepos = "joda-time"

type Predicate = String -> Bool

(.&&.) = liftM2 (&&)

gitPeriods :: IO ()
gitPeriods = do
  let p = hasNumber in do
    logs <- gitFixedCommit p aGitRepos
    writeCSV (resultDir ++ aGitRepos ++ ".csv")
      $ map (toLogCsv aGitRepos p) logs
--   gitReposList reposDir
--   >>= mapM gitFixedCommitPretty
--   >> return ()

gitReposList :: FilePath -> IO [FilePath]
gitReposList repos =
  -- filter (isSuffixOf ".git")
  listDirectory repos

gitPeriod :: FilePath -> IO (String, String, String)
gitPeriod gitRepos = do
  logs <- doLog gitRepos
  let Right r = parseGitLog logs
  return (gitRepos, date $ head r, date $ last r)

gitFixedCommit :: Predicate -> FilePath -> IO [Log]
gitFixedCommit p gitRepos =
  filter ((hasPredicate p) .&&. inPeriod .&&. isFixed)
  . parseGitLogRight
  <$> doLog gitRepos

-- for test
gitFixedCommitPretty :: Predicate -> FilePath -> IO ()
gitFixedCommitPretty p gitRepos = do
  logs <- doLog gitRepos
  putStrLn gitRepos
  let Right r = parseGitLog logs
  putStrLn $ show (length r) ++ " commits found."
  let r' = filter isFixed r
  putStrLn $ show (length r') ++ " fixed commits found."
  let r'' = filter inPeriod r'
  putStrLn $ show (length r'') ++ " newer fixed commits found."
  let r''' = filter (hasPredicate p) r''
  putStrLn $ show (length r''') ++ " numbered newer fixed commits found."
  mapM_ prettyLog r'''

doLog :: String -> IO String
doLog gitRepos = withCurrentDirectory (reposDir ++ gitRepos) $
  readProcess "git" ["log"] ""

-- Parser and Pretty

parseGitLog :: String -> Either P.ParseError [Log]
parseGitLog logs = P.parse fileParser "" (logs ++ "\n")

parseGitLogRight :: String -> [Log]
parseGitLogRight = (\(Right r) -> r) . parseGitLog

prettyLog :: Log -> IO ()
prettyLog l = do
  putStrLn $ "commit: " ++ commit l
  putStrLn $ "merge: " ++ merge l
  putStrLn $ "author: " ++ (\(Author n m) -> n ++ "<" ++ m ++ ">") (author l)
  putStrLn $ "date: " ++ date l
  putStrLn $ message l ++ "\n"

toLogCsv :: String -> Predicate -> Log -> LogCsv
toLogCsv r p l = LogCsv
  r
  (commit l)
  (prettyGitLogTime . parseGitLogTime $ date l)
  (unwords . map (filter isDigit) . filter p . words $ message l)

prettyGitLogTime :: UTCTime -> String
prettyGitLogTime = formatTime defaultTimeLocale "%s"

parseGitLogTime :: String -> UTCTime
parseGitLogTime = parseTimeOrError True defaultTimeLocale fmt where
  fmt = "%a %b %e %H:%M:%S %Y %Z"

-- Predicates

isFixed :: Log -> Bool
isFixed = isInfixOf "fix" . message

inPeriod :: Log -> Bool
inPeriod = inPeriod' . parseGitLogTime . date where
  inPeriod' d = old <= d && d < new where
    old = UTCTime (fromGregorian 2014 1 1) (secondsToDiffTime 0)
    new = UTCTime (fromGregorian 2016 1 1) (secondsToDiffTime 0)

hasPredicate :: Predicate -> Log -> Bool
hasPredicate p = not . null . filter p . words . message

hasNumber :: Predicate
hasNumber [] = False
hasNumber [_] = False
hasNumber (s:n:_) = s == '#' && isDigit n

hasMath :: Predicate
hasMath = isPrefixOf "MATH-"

hasLang :: Predicate
hasLang = isPrefixOf "LANG-"
