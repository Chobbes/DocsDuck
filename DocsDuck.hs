import Data.ByteString.Char8 hiding (head)
import Network.HTTP.Conduit
import System.Environment
import Text.HTML.TagSoup


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

main = do [user, pass] <- getArgs
          request <- getLogin user pass
          res <- withManager (httpLbs request)
          print $ extractPass $ head $ partitions (~== (" Docsdb Password: " :: String)) $ parseTags $ responseBody res

extractPass xs = pass
  where _:(TagOpen _ (_:_:(_,pass):_)):_ = xs -- Super readable pattern match ;D
