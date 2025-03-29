import Network.Curl (curlGetString, CurlOption(CurlNoProgress))
import Text.Regex.Posix
import Text.HTML.TagSoup
import Data.Foldable (fold)

and' :: Tag String -> [(Tag String -> Bool)] -> Bool
and' _ [] = True
and' tag (f:fs) = if f tag then and' tag fs else False

tagTextMatcher :: String -> Tag String -> Bool
tagTextMatcher regex tag = fromTagText tag =~ regex :: Bool

type Proxy = (String, String)

parseTbl :: [Tag String] -> [Proxy]
parseTbl [] = []
parseTbl (x:xs)
  | ip x = [( fromTagText x
            , fromTagText
              . head
              . dropWhile (not . port)
              $ xs
            )] ++ parseTbl xs
  | otherwise = parseTbl xs
  where ip, port :: Tag String -> Bool
        ip tag = and' tag [ isTagText, tagTextMatcher ipPattern ]
        port tag = and' tag [ isTagText, tagTextMatcher portPattern ]

sources :: [IO String]
sources = [ curlGetString url [CurlNoProgress True] >>= pure . f
          | (url, f) <- [("https://spys.me/proxy.txt"
                           , unlines
                           . map matchPair
                           . init . init . drop 6
                           . lines
                           . snd
                         )
                        ,
                         ("https://free-proxy-list.net/"
                           , unlines
                           . toPair
                           . parseTbl
                           . parseTags
                           . tbody
                           . snd
                         )
                        ,
                         ("https://api.proxyscrape.com/v4/free-proxy-list/get?request=display_proxies&proxy_format=ipport&format=text"
                          , snd
                         )
                        ]
          ]
  where tbody :: String -> String
        tbody x = x =~ "<tbody>.*</tbody>" :: String
        matchPair :: String -> String
        matchPair x = x =~ "[0-9\\.]+:[0-9]+" :: String
        toPair [] = []
        toPair ((ip, port):xs) = [ip ++ ":" ++ port] ++ toPair xs

main = fold sources >>= writeFile "proxies.txt"

ipPattern = "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$"
portPattern = "^[0-9]+$"
