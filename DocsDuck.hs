import Data.ByteString.Char8 hiding (head)
import Network.HTTP.Conduit
import System.Environment
import Text.HTML.TagSoup


main :: IO ()
main = do [user, pass] <- getArgs
          request <- getLogin user pass
          res <- withManager (httpLbs request)
          print $ extractPass $ head $ partitions (~== (" Docsdb Password: " :: String)) $ parseTags $ responseBody res

getLogin :: Control.Monad.Catch.MonadThrow m => String -> String -> m Request          
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
