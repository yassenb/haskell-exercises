import Control.Applicative ((<$>))

-- requires the regex-posix and text-format-simple packages
import Text.Regex.Posix ((=~))
import Text.Format (format)

plausibleStr :: String -> String -> [String] -> String
plausibleStr regex1 regex2 words = format "{0} with a ratio of {1} ({2} / {3})"
    [description, show ratio, show nPositive, show nNegative] where
        description = if plausible then "plausible" else "implausible" where
            plausible = ratio > 2
        nPositive = nOccurences regex1
        nNegative = nOccurences regex2
        ratio = fromIntegral nPositive / fromIntegral nNegative
        nOccurences regex = length $ filter (=~ regex) words

main = do
    words <- lines <$> getContents
    putStrLn $ format
        "I before E when not preseded by C is {0}\n\
        \E before I when preseded by C is {1}"
        [plausibleStr "[^c]ie" "[^c]ei" words, plausibleStr "cei" "cie" words]
