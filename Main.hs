{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- data RouteConfig
--   = Comment ByteString
--   | Resource

-- data Resource = Resource
--   { resourceSourceLine :: Int
--   , resourceName :: ByteString
--   , resourceRoutePattern :: ByteString
--   , resourceMethod :: ByteString
--   }

-- data Line
--   = CommentLine ByteString
--   | RouteLine [ByteString]

-- parseLine :: ByteString -> [ByteString]
-- parseLine

-- whitespace :: ByteString -> ByteString
-- whitespace = Char8.dropWhileEnd ' ' . Char8.dropWhile ' '

-- isCommentLine :: ByteString -> Bool
-- isCommentLine bs = Char8.take 2 bs == '--'

data ConfigLine
  = Comment Int Text
  | Line Int Text [Text]
  | EmptyLine Int
  | ErrorLine Int
  deriving (Show)

isMethodName :: Text -> Bool
isMethodName t = T.take 1 t /= "!"

parseLine :: Int -> Text -> ConfigLine
parseLine lineNumber line = parse $ T.strip line
  where
    parse t
      | T.isPrefixOf "--" t = Comment lineNumber t
      | otherwise =
          case T.words t of
            [] ->
              EmptyLine lineNumber
            (_ : resourceName : methods) ->
              Line lineNumber resourceName (filter isMethodName methods)
            _ ->
              ErrorLine lineNumber

parseRoutes :: Text -> [ConfigLine]
parseRoutes file =
  let fileLines = T.lines file
    in go fileLines 1 []
  where
    go lines lineNumber result =
      case lines of
        [] ->
          result
        (line:rest) ->
          go rest (lineNumber + 1) (parseLine lineNumber line : result)

main :: IO ()
main = do
  file <- BS.readFile "routes.yesodroutes"
  let lines = parseRoutes $ TE.decodeUtf8 file
  forM_ lines $ \line ->
    putStrLn $ show line
