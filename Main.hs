{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Char qualified as C
import Data.List.Extra (nubOrd)
import Data.Lists (merge, sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO

data Route = Route
  { routeHandlerName :: Text
  , routeHandlerTags :: [Text]
  , routeMethod :: Text
  , routeName :: Text
  , routeSourceFileName :: Text
  , routeSourceLine :: Int
  }
  deriving Show

isMethod :: Text -> Bool
isMethod = not . T.isPrefixOf "!"

parseRoutesFile :: Text -> Text -> [Route]
parseRoutesFile fileName file = fst $ foldl' go ([], 1) (T.lines file)
  where
    go (result, lineNumber) l =
      case tokens of
        [] ->
          skipLine
        (_ : []) ->
          skipLine
        (_ : routeName : rest) ->
          let
            methods = filter isMethod rest
            routes = methods <&> \routeMethod ->
              Route
                { routeHandlerName = T.toLower routeMethod <> routeName
                , routeHandlerTags = []
                , routeMethod
                , routeName
                , routeSourceFileName = fileName
                , routeSourceLine = lineNumber
                }
          in
          (routes ++ result, lineNumber + 1)
      where
        tokens = T.words $ fst $ T.breakOn "--" $ T.strip l
        skipLine = (result, lineNumber + 1)

renderRouteTags :: Route -> [Text]
renderRouteTags Route {..} = routeTag : handlerTags
  where
    routeTag =
      T.intercalate
        "\t"
        [ routeHandlerName
        , routeSourceFileName
        , (T.pack $ show routeSourceLine) <> ";\""
        , "f"
        ]
    handlerTags =
      routeHandlerTags <&> \t -> routeName <> "\t" <> t

-- collectExistingTags :: Text -> Map Text [Text]
-- collectExistingTags tagsFile = foldr go mempty (T.lines tagsFile)
--   where
--     go
--   let tagLines = filter (not . T.isPrefixOf "!_TAG") $ T.lines tagsFile
--     in tagLines <&> \tl ->
--           let
--             (name, rest) = T.breakOn "\t" tl
--           in
--           Tag {tagName = name, tagRest = rest}

main :: IO ()
main = do
  let routesFilePath = "routes.yesodroutes"
      tagsFilePath = "mwb-tags"
  routesFile <- TIO.readFile $ T.unpack routesFilePath
  existingTagsFile <- TIO.readFile $ T.unpack tagsFilePath
  let existingTags = T.lines existingTagsFile
      parsedRoutes = parseRoutesFile routesFilePath routesFile
      routeTags = concatMap renderRouteTags parsedRoutes
      allTags = nubOrd $ merge existingTags $ sort routeTags
  TIO.writeFile "tags" $ T.intercalate "\n" allTags <> "\n"
