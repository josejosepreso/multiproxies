import Network.Curl (curlGetString, CurlOption(CurlNoProgress))
import Text.Regex.Posix
import Text.HTML.TagSoup
import Data.Foldable (fold)

type Proxy = (String, String)

parseTbl :: [Tag String] -> [Proxy]
parseTbl [] = []
parseTbl (x:xs)
  | ip x = [( fromTagText x
            , fromTagText . head . dropWhile (not . port) $ xs
            )] ++ parseTbl xs
  | otherwise = parseTbl xs
  where ip, port :: Tag String -> Bool
        ip tag = and [ isTagText tag
                     , fromTagText tag =~ "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$" :: Bool
                     ]
        port tag = and [ isTagText tag
                       , fromTagText tag =~ "^[0-9]+$" :: Bool
                       ]

sources :: [IO String]
sources = [ curlGetString "https://spys.me/proxy.txt" [CurlNoProgress True]
            >>= pure
            . unlines
            . map matchPair
            . init . init . drop 6
            . lines
            . snd
          ,
            curlGetString "https://free-proxy-list.net/" [CurlNoProgress True]
            >>= pure
            . unlines
            . toPair
            . parseTbl
            . parseTags
            . tbody
            . snd
          ]
  where tbody :: String -> String
        tbody x = x =~ "<tbody>.*</tbody>" :: String
        matchPair :: String -> String
        matchPair x = x =~ "[0-9\\.]+:[0-9]+" :: String
        toPair [] = []
        toPair ((ip, port):xs) = [ip ++ ":" ++ port] ++ toPair xs

main = fold sources >>= writeFile "proxies.txt"
