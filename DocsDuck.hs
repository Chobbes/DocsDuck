import Data.ByteString.Char8 hiding (head)
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.HTTP.Conduit
import System.Environment
import Text.HTML.TagSoup


main :: IO ()
main = do [user, pass] <- getArgs
          request <- getLogin user pass
          res <- withManager (httpLbs request)
          let oraclePass = extractPass $ head $ partitions (~== (" Docsdb Password: " :: String)) $ parseTags $ responseBody res
          request <- getAssign user (LB.unpack oraclePass)
          res <- withManager (httpLbs request)
          print $ responseBody res                     

--getLogin :: Control.Monad.Catch.MonadThrow m => String -> String -> m Request          
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

extractPass :: [Tag t] -> t
extractPass tags = pass
  where _:(TagOpen _ (_:_:(_,pass):_)):_ = tags -- Super readable pattern match ;D


--getAssign :: Control.Monad.Catch.MonadThrow m => String -> String -> m Request          
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
