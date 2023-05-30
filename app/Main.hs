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
import Data.Maybe (fromMaybe)
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

parseRoutesFile :: Map Text [Text] -> Text -> Text -> [Route]
parseRoutesFile existingTagsByTagName fileName file = fst $ foldl' go ([], 1) (T.lines file)
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
              let
                routeHandlerName = T.toLower routeMethod <> routeName
              in
              Route
                { routeHandlerName
                , routeHandlerTags = fromMaybe [] $ M.lookup routeHandlerName existingTagsByTagName
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

groupExistingTagsByTagName :: [Text] -> Map Text [Text]
groupExistingTagsByTagName existingTags = foldl' go mempty existingTags
  where
    go result l =
      if T.isPrefixOf "!_TAG" line
        then
          result
        else
          M.insertWith (<>) tagName [T.strip tagRemainder] result
      where
        line = T.strip l
        (tagName, tagRemainder) = T.breakOn "\t" line

main :: IO ()
main = do
  let routesFilePath = "config/routes.yesodroutes"
      tagsFilePath = "mwb-tags"
  routesFile <- TIO.readFile $ T.unpack routesFilePath
  existingTagsFile <- TIO.readFile $ T.unpack tagsFilePath
  let existingTags = T.lines existingTagsFile
      existingTagsByTagName = groupExistingTagsByTagName existingTags
      parsedRoutes = parseRoutesFile existingTagsByTagName routesFilePath routesFile
      routeTags = concatMap renderRouteTags parsedRoutes
      allTags = nubOrd $ merge existingTags $ sort routeTags
  TIO.writeFile "tags" $ T.intercalate "\n" allTags <> "\n"
