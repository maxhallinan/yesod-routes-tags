{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Functor ((<&>))
import Data.Char qualified as C
import Data.Lists qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO

-- postSigninBiometricsR

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

-- data Route = Route
--   { routeName :: Text
--   , routeHandlerName :: Text
--   , routeMethod :: Text
--   , routeSourceLine :: Int
--   }

data ConfigLine
  = Comment Int Text
  | Line Int Text Text [Text]
  | EmptyLine Int
  | ErrorLine Int
  deriving (Show)

isMethodName :: Text -> Bool
isMethodName t = T.take 1 t /= "!"

isCommentLine :: Text -> Bool
isCommentLine = T.isPrefixOf "--"

parseLine :: Int -> Text -> Text -> ConfigLine
parseLine lineNumber filePath line = parse $ T.strip line
  where
    parse t
      | T.isPrefixOf "--" t = Comment lineNumber t
      | otherwise =
          case T.words t of
            [] ->
              EmptyLine lineNumber
            (_ : resourceName : methods) ->
              Line lineNumber filePath resourceName (filter isMethodName methods)
            _ ->
              ErrorLine lineNumber

parseRoutes :: Text -> Text -> [ConfigLine]
parseRoutes filePath file =
  let fileLines = T.lines file
    in go fileLines 1 []
  where
    go lines lineNumber result =
      case lines of
        [] ->
          result
        (line:rest) ->
          go rest (lineNumber + 1) (parseLine lineNumber filePath line : result)

renderTags :: ConfigLine -> [Text]
renderTags cl = case cl of
  Comment _ _ ->
    []
  Line lineNumber filePath resourceName methodNames ->
    methodNames <&> \mn ->
      T.intercalate "\t" [T.toLower mn <> resourceName, filePath, (T.pack $ show lineNumber) <> ";\"", "f"]
  EmptyLine _ ->
    []
  ErrorLine _ ->
    []

data Tag = Tag
  { tagName :: Text
  , tagRest :: Text
  }
  deriving (Show)

collectExistingTags :: Text -> [Tag]
collectExistingTags tagsFile =
  let tagLines = filter (not . T.isPrefixOf "!_TAG") $ T.lines tagsFile
    in tagLines <&> \tl ->
          let
            (name, rest) = T.breakOn "\t" tl
          in
          Tag {tagName = name, tagRest = rest}

main :: IO ()
main = do
  let routesFilePath = "routes.yesodroutes"
      tagsFilePath = "mwb-tags"
  routesFile <- TIO.readFile routesFilePath
  tagsFile <- TIO.readFile tagsFilePath
  let lines = parseRoutes (T.pack routesFilePath) routesFile
      routesTags = concatMap renderTags lines
      -- existingTags = parseTags tagsFile
  pure ()
  -- forM_ existingTags \t ->
  --   putStrLn (show t)
  TIO.writeFile "tags" $ T.intercalate "\n" routesTags <> "\n"
