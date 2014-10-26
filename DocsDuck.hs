{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 hiding (head)
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.HTTP.Conduit
import System.Environment
import Text.HTML.TagSoup
import Text.StringLike


main :: IO ()
main = do [user, pass] <- getArgs
          request <- getLogin user pass
          res <- withManager (httpLbs request)
          let oraclePass = extractPass $ responseBody res
          request <- getAssign user (LB.unpack oraclePass)
          res <- withManager (httpLbs request)
          print $ responseBody res                     

-- | Given a user and pass, send a login request to DocsDB
getLogin user pass = do initReq <- parseUrl "https://docsdb.cs.ualberta.ca/Prod/login.cgi"
                        let req = initReq {method = "POST"
                                          ,secure = True}
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
  where _:(TagOpen _ (_:_:(_,pass):_)):_ = head $ partitions (~== (" Docsdb Password: " :: String)) tags
        tags = parseTags res


-- | Given a user, and oracle password, return an assignment
getAssign user pass = do initReq <- parseUrl "https://docsdb.cs.ualberta.ca/Prod/entersection2.cgi"
                         let req = initReq {method = "POST"
                                           ,secure = True}
                         return $ urlEncodedBody [("oracle.login", pack user)
                                                 ,("oracle.password", pack pass)
                                                 ,("season", "Fall")
                                                 ,("year", "2014")
                                                 ,("abbrev", "CMPUT")
                                                 ,("coursenum", "274")
                                                 ,("secttype", "All Sections")
                                                 ,("sectpre", "")
                                                 ,("sectnum", "")
                                                 ,("type", "")
                                                 ,("num", "")
                                                 ,("order_by", "Student ID")
                                                 ,(".submit", "Get List")
                                                 ,("assignment", "A1-;1")
                                                 ,("term", "1490")] req

{-
 earole:"0"
 maxmark:"10"
 dbarole:"0"
 secretnum:"40777" -- Gotten from assign.
 id0:"1111151"
 mark0:""
 oldmark0:""
 eaflag0:""
 oldeaflag0:""
 .submit:"Enter Marks"
("oracle.login", pack user)
,("oracle.password", pack pass)
,("season", "Fall")
,("year", "2014")
,("abbrev", "CMPUT")
,("coursenum", "274")
,("secttype", "All Sections")
,("sectnum", "")
-}
