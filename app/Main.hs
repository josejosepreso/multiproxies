import Network.Curl (curlGetString, CurlOption(CurlNoProgress))
import Text.Regex.Posix
import Text.HTML.TagSoup
import Data.Foldable (fold)

parsetbl :: [Tag String] -> [String]
parsetbl [] = []
parsetbl (x:xs)
  | notipport x = parsetbl xs
  | otherwise = [fromTagText x] ++ (parsetbl xs)
  where notipport :: Tag String -> Bool
        notipport tag = not . and $ [ isTagText tag,
                                  or [ (fromTagText tag =~ "^[0-9\\.]+$") :: Bool,
                                       (fromTagText tag =~ "^[0-9]+$") :: Bool
                                     ]
                                ]

sources :: [IO String]
sources = [ curlGetString "https://spys.me/proxy.txt" [CurlNoProgress True]
            >>= pure
            . unlines
            . map getpair
            . init . init . drop 6
            . lines
            . snd
          ,
            curlGetString "https://free-proxy-list.net/" [CurlNoProgress True]
            >>= pure
            . unlines
            . topair
            . parsetbl
            . parseTags
            . tbody
            . snd
          ]
  where getpair :: String -> String
        getpair x = x =~ "[0-9\\.]+:[0-9]+" :: String
        tbody :: String -> String
        tbody x = x =~ "<tbody>.*</tbody>" :: String
        topair [] = []
        topair (x:y:xs) = [x ++ ":" ++ y] ++ topair xs

main = fold sources >>= writeFile "proxies.txt"
