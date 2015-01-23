{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.ByteString.Char8 hiding (head, zip, concat, map, tail, concatMap, init)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Csv hiding (lookup)
import Data.Maybe
import Data.String
import qualified Data.Vector as V
import Network.HTTP.Conduit
import System.Environment
import Text.HTML.TagSoup
import Text.Printf
import Text.StringLike


data Grade = NoGrade | Grade Double

instance Show Grade where
  show NoGrade = "0"
  show (Grade n) = printf "%.2f" n

-- | Convert a string to a grade.
stringToGrade :: String -> Grade
stringToGrade s 
  | s == "-" = NoGrade
  | otherwise = Grade (read s)

data Submission = Submission { firstName :: String
                             , lastName :: String
                             , email :: String
                             , studentID :: Integer
                             , ccID :: String
                             , grade :: Grade
                             }
                             deriving (Show)

-- | Convert a vector of ByteStrings of the format:
-- |
-- | [first_name, last_name, email_address, student_id, ccid, grade]
-- |
-- | into a Submission.
vecToSub :: V.Vector ByteString -> Submission
vecToSub v = Submission first last email sid ccid grade
  where [first, last, email, sidStr, ccid, gradeStr] = map unpack (V.toList v)
        grade = stringToGrade gradeStr
        sid = read sidStr

-- | Create a list of submissions from a vector of vector ByteStrings.
vecToSubs :: V.Vector (V.Vector ByteString) -> [Submission]
vecToSubs vs = map vecToSub (V.toList vs)

main :: IO ()
main = do [user, pass, course, gradeFile, assignment] <- getArgs
                                                         
          subs <- LB.readFile gradeFile
          let (Right decodedSubs) = decode HasHeader subs :: Either String (V.Vector (V.Vector ByteString))

          -- Login to DocsDB, and get the Oracle password.
          request <- getLogin user pass
          res <- withManager (httpLbs request)
          let oraclePass = LB.unpack . extractPass $ responseBody res
          
          -- Fetch the assignment information in order to get the secret number.
          request <- getAssign user oraclePass course assignment
          res <- withManager (httpLbs request)
          let secretNum =  LB.unpack . extractSecretNum $ responseBody res
          let gradeList = extractGrades $ responseBody res

          -- Upload the grades to docsdb.
          request <- uploadGrades user oraclePass course secretNum 100 (vecToSubs decodedSubs) gradeList
          res <- withManager (httpLbs request)

          -- Print the response just in case it's useful.
          print $ responseBody res

-- | Given a user and pass, send a login request to DocsDB
getLogin user pass = do initReq <- parseUrl "https://docsdb.cs.ualberta.ca/Prod/login.cgi"
                        let req = initReq { method = "POST"
                                          , secure = True}
                        return $ urlEncodedBody [("oracle.login", pack user)
                                                ,("oracle.password", pack pass)
                                                ,("season", "Fall")
                                                ,("year", "2014")
                                                ,("abbrev", "CMPUT")
                                                ,("coursenum", "")
                                                ,("secttype", "All+Sections")
                                                ,("sectpre", "")
                                                ,("sectnum", "")] req

-- | From a response get the oracle password
extractPass :: StringLike t => t -> t
extractPass res = pass
  where _:TagOpen _ (_:_:(_,pass):_):_ = head $ partitions (~== (" Docsdb Password: " :: String)) tags
        tags = parseTags res


-- | From a response get the assignment grades. (CCID, Mark) pairs.
extractGrades :: (StringLike t, IsString t, Show t) => t -> [(Integer, (String, String))]
extractGrades res = map getPair (init ((TagClose "" : firstGrade) : otherGrades))
  where firstGrade:otherGrades = partitions (~== ("<br>" :: String)) gradeTags
        gradeTags = head $ sections (~== ("  Id            Name          Mark   EA " :: String)) tags
        tags = parseTags res
        getPair tagList = (read . toString $ fromAttrib "value" (tagList !! 3), (toString $ fromAttrib "value" (tagList !! 5), toString $ fromAttrib "value" (tagList !! 13)))

-- | From a response get the secret number for the assignment
extractSecretNum :: StringLike t => t -> t
extractSecretNum res = secretNum
   where TagOpen _ [_,_,(_,secretNum)] = secretTag
         secretTag = head (partitions (~== ("Leave the mark field blank\n\t    for work not completed; only enter zero for work completed which\n\t    received a grade of zero." :: String)) tags) !! 9
         tags = parseTags res

-- | Given a user, and oracle password, return an assignment
getAssign user pass course assign = do initReq <- parseUrl "https://docsdb.cs.ualberta.ca/Prod/entersection2.cgi"
                                       let req = initReq { method = "POST"
                                                         , secure = True}
                                       return $ urlEncodedBody [("oracle.login", pack user)
                                                               ,("oracle.password", pack pass)
                                                               ,("season", "Fall")
                                                               ,("year", "2014")
                                                               ,("abbrev", "CMPUT")
                                                               ,("coursenum", pack course)
                                                               ,("secttype", "All Sections")
                                                               ,("sectpre", "")
                                                               ,("sectnum", "")
                                                               ,("type", "")
                                                               ,("num", "")
                                                               ,("order_by", "Student ID")
                                                               ,(".submit", "Get List")
                                                               ,("assignment", pack assign)
                                                               ,("term", "1500")] req

-- | Send submissions to DocsDB
uploadGrades user pass course secretNum maxMark subs oldMarks = 
  do initReq <- parseUrl "https://docsdb.cs.ualberta.ca/Prod/entersection3.cgi"
     let req = initReq {method = "POST"
                       ,secure = True}
     return $ urlEncodedBody (grades ++ [(".submit", "Enter Marks")
                                        ,("oracle.login", pack user)
                                        ,("oracle.password", pack pass)
                                        ,("season", "Fall")
                                        ,("year", "2014")
                                        ,("abbrev", "CMPUT")
                                        ,("coursenum", pack course)
                                        ,("secttype", "All Sections")
                                        ,("sectnum", "")
                                        ,("type", "")
                                        ,("num", "")
                                        ,("order_by", "Student ID")
                                        ,("earole", "0")
                                        ,("maxmark", pack $ show maxMark)
                                        ,("dbarole", "0")
                                        ,("secretnum", pack secretNum)]) req
     where grades = concatMap makeGrade (zip [0..] subs)
           makeGrade (id, sub) = let sid = show id
                                     (oldMark, oldEa) = fromMaybe ("", "") (lookup (studentID sub) oldMarks)
                                 in [(pack $ "id" ++ sid, pack . show $ studentID sub)
                                    ,(pack $ "mark" ++ sid, pack . show $ grade sub)
                                    ,(pack $ "oldmark" ++ sid, pack oldMark)
                                    ,(pack $ "eaflag" ++ sid, pack oldEa)
                                    ,(pack $ "oldeaflag" ++ sid, pack oldEa)]
