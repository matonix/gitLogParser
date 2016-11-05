module Lib
    ( gitPeriods
    ) where

import           Data.List
import           Data.Time
import           GitLogParser
import           System.Directory
import           System.Process
import qualified Text.Parsec      as P

reposDir = "/home/maton/experimentBTS/repos/"
-- reposDir = "/home/maton/experimentBTS/defects4j/project_repos/"
-- aGitRepos = "joda-time.git"

gitPeriods :: IO ()
gitPeriods =
  gitReposList reposDir
  >>= mapM gitFixedCommit
  >>= print

gitReposList :: FilePath -> IO [FilePath]
gitReposList repos =
  -- filter (isSuffixOf ".git")
  listDirectory repos

gitPeriod :: FilePath -> IO (String, String, String)
gitPeriod gitRepos = do
  logs <- doLog gitRepos
  let Right r = parseGitLog logs
  return (gitRepos, date $ head r, date $ last r)

gitFixedCommit :: FilePath -> IO ()
gitFixedCommit gitRepos = do
  logs <- doLog gitRepos
  let Right r = parseGitLog logs
  print $ show (length r) ++ " commits found."
  let r' = filter isFixed r
  print $ show (length r') ++ " fixed commits found."
  let r'' = filter inPeriod r'
  print $ show (length r'') ++ " newer fixed commits found."
  mapM_ prettyLog r''
  where
    isFixed = isInfixOf "fix" . message
    inPeriod = inPeriod' . parseGitLogTime . date where
      inPeriod' d = old <= d && d < new where
        old = UTCTime (fromGregorian 2014 1 1) (secondsToDiffTime 0)
        new = UTCTime (fromGregorian 2016 1 1) (secondsToDiffTime 0)

doLog :: String -> IO String
doLog gitRepos = withCurrentDirectory (reposDir ++ gitRepos) $
  readProcess "git" ["log"] ""

parseGitLog :: String -> Either P.ParseError [Log]
parseGitLog logs = P.parse fileParser "" (logs ++ "\n")

prettyLog :: Log -> IO ()
prettyLog l = do
  putStrLn $ "commit: " ++ commit l
  putStrLn $ "merge: " ++ merge l
  putStrLn $ "author: " ++ (\(Author n m) -> n ++ "<" ++ m ++ ">") (author l)
  putStrLn $ "date: " ++ date l
  putStrLn $ message l ++ "\n"

parseGitLogTime :: String -> UTCTime
parseGitLogTime = parseTimeOrError True defaultTimeLocale fmt where
  fmt = "%a %b %e %H:%M:%S %Y %Z"
