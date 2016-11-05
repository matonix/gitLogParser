module Lib
    ( gitPeriods
    ) where

import           Data.List
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
  print r'
  where
    isFixed = isInfixOf "fix" . message

doLog :: String -> IO String
doLog gitRepos = withCurrentDirectory (reposDir ++ gitRepos) $
  readProcess "git" ["log"] ""

parseGitLog :: String -> Either P.ParseError [Log]
parseGitLog logs = P.parse fileParser "" (logs ++ "\n")
