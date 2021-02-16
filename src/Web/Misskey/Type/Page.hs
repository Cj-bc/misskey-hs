{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}
module Web.Misskey.Type.Page (
  Page
  -- ** Lenses for Page
, page_id, page_createdAt, page_updatedAt, page_title, page_name
, page_summary, page_content, page_variables, page_userId, page_user
, page_hideTitleWhenPinned, page_alignCenter, page_font
, page_eyeCatchingImageId, page_eyeCatchingImage, page_attachedFiles
, page_likedCount

  -- ** Lenses for PageVArg
, pageVArg_id, pageVArg_type, pageVArg_value

  -- ** Lenses for PageV
, pageV_id, pageV_args, pageV_name, pageV_type, pageV_value


) where
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, defaultOptions)
import Control.Lens (makeLenses)
import Data.Time (UTCTime)
import Control.Lens (makePrisms, makeLenses)
import Web.Misskey.Type.Common
import Web.Misskey.Type.File


-- | A Enum of page content
--
-- This is mainly used in 'Page' to represent page content
--
-- Those values are manually picked up from the Page making webpage
-- (https://<your-domain>/i/pages/new) by me.
--
-- So It might have some mistakes.
--
-- TODO: Generate Prism for this data type
data PageContent = PageContentText { _pageContent_id   :: Id
                                   , _pageContent_text :: String
                                   }
                 | PageContentSection { _pageContent_id       :: Id
                                      , _pageContent_title    :: String
                                      , _pageContent_children :: [PageContent]
                                      }
                 | PageContentImage { _pageContent_id     :: Id
                                    , _pageContent_fileId :: Id
                                    }
                 | PageContentTextArea { _pageContent_id   :: Id
                                       , _pageContent_text :: String
                                       }
                 | PageContentButton { _pageContent_id      :: Id
                                     , _pageContent_var     :: Maybe String
                                     , _pageContent_text    :: String
                                     , _pageContent_event   :: Maybe String  -- TODO: Check those type
                                     , _pageContent_action  :: Maybe String  -- TODO: Check those type
                                     , _pageContent_content :: Maybe String  -- TODO: Check those type
                                     , _pageContent_message :: Maybe String  -- TODO: Check those type
                                     , _pageContent_primary :: Bool
                                     }
                 | PageContentRadioButton { _pageContent_id       :: Id
                                          , _pageContent_name     :: String
                                          , _pageContent_title    :: String
                                          , _pageContent_values   :: String
                                          , _pageContent_defuault :: Maybe String
                                          }
                 | PageContentTextInput { _pageContent_id       :: Id
                                        , _pageContent_name     :: String
                                        , _pageContent_text     :: String
                                        , _pageContent_defuault :: Maybe String
                                        }
                 | PageContentTextAreaInput { _pageContent_id       :: Id
                                            , _pageContent_name     :: String
                                            , _pageContent_text     :: String
                                            , _pageContent_defuault :: Maybe String
                                            }
                 | PageContentSwitch { _pageContent_id      :: Id
                                     , _pageContent_name    :: String
                                     , _pageContent_text    :: String
                                     , _pageContent_default :: Maybe String
                                     }
                 | PageContentCounter { _pageContent_id   :: Id
                                      , _pageContent_name :: String
                                      , _pageContent_text :: String
                                      , _pageContent_inc  :: Maybe String
                                      }
                 | PageContentIf { _pageContent_id       :: Id
                                 , _pageContent_var      :: Maybe String
                                 , _pageContent_children :: [PageContent]
                                 }
                 | PageContentPost { _pageContent_id   :: Id
                                   , _pageContent_text :: String
                                   }
                    deriving (Show)


-- instance FromJSON PageContent where {{{
instance FromJSON PageContent where
    parseJSON (Object v) = do
        (ctype :: String) <- v .: "type"
        littleParser ctype
        where
            littleParser "text"          = PageContentText          <$> v .: "id"       <*> v .: "text"
            littleParser "section"       = PageContentSection       <$> v .: "id"       <*> v .: "title"
                                                                    <*> v .: "children"
            littleParser "image"         = PageContentImage         <$> v .: "id"       <*> v .: "fileId"
            littleParser "textarea"      = PageContentTextArea      <$> v .: "id"       <*> v .: "text"
            littleParser "button"        = PageContentButton        <$> v .: "id"       <*> v .:? "var"
                                                                    <*> v .: "text"     <*> v .:? "event"
                                                                    <*> v .:? "action"  <*> v .:? "content"
                                                                    <*> v .:? "message" <*> v .: "primary"
            littleParser "radioButton"   = PageContentRadioButton   <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "title"    <*> v .: "values"
                                                                    <*> v .:? "default"
            littleParser "textInput"     = PageContentTextInput     <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "default"
            littleParser "textareaInput" = PageContentTextAreaInput <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "default"
            littleParser "switch"        = PageContentSwitch        <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "default"
            littleParser "counter"       = PageContentCounter       <$> v .: "id"       <*> v .: "name"
                                                                    <*> v .: "text"     <*> v .:? "inc"
            littleParser "if"            = PageContentIf            <$> v .: "id"       <*> v .:? "var"
                                                                    <*> v .: "children"
            littleParser "post"          = PageContentPost          <$> v .: "id"       <*> v .: "text"
            littleParser x               = prependFailure ("parsing PageContent's type failed, " ++ x ++ " is not valid")
                                            (typeMismatch "Object" (Object v))
    -- We do not expect a non-Object value here.
    -- We could use empty to fail, but typeMismatch
    -- gives a much more informative error message.
    parseJSON invalid    =
        prependFailure "parsing PageContent failed, "
            (typeMismatch "Object" invalid)
-- }}}
makePrisms ''PageContent
makeLenses ''PageContent

data PageVariableArg = PageVariableArg { _pageVArg_id    :: Id
                                       , _pageVArg_type  :: String
                                       , _pageVArg_value :: String
                                       } deriving (Show)

makeLenses ''PageVariableArg
$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''PageVariableArg)

-- TODO: Implement this
--
-- This is temporary set to 'String', though it should be enum
-- To Implement this, I should know all sort of variable type
type PageVariableType = String

-- | Page variable that can be declared for each page
data PageVariable = PageVariable { _pageV_id   :: Id
                                 , _pageV_args :: [PageVariableArg]
                                 , _pageV_name :: String
                                 , _pageV_type :: PageVariableType
                                 , _pageV_value :: Maybe String -- ^ TODO: what type is this?
                                 } deriving (Show)

makeLenses ''PageVariable
$(deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''PageVariable)

-- | Page
--
-- Docs:
--
--   * https://misskey.io/api-doc#operation/pages/show
--
--   * https://join.misskey.page/ja/wiki/usage/pages
data Page = Page { _page_id  :: Id
                 , _page_createdAt :: UTCTime
                 , _page_updatedAt :: UTCTime
                 , _page_title        :: String
                 , _page_name         :: String
                 , _page_summary      :: Maybe String
                 , _page_content      :: [PageContent]
                 , _page_variables    :: [PageVariable]
                 , _page_userId       :: Id
                 , _page_user         :: BaseUser
                 -- Those fields below are undocumented
                 , _page_hideTitleWhenPinned :: Bool
                 , _page_alignCenter         :: Bool
                 , _page_font                :: String
                 , _page_eyeCatchingImageId  :: Maybe String -- ^ TODO: Check whether this type correct
                 , _page_eyeCatchingImage    :: Maybe String -- ^ TODO: Check whether this type correct
                 , _page_attachedFiles       :: [File] -- ^ TODO: Check whether this type correct
                 , _page_likedCount          :: Int
                 } deriving (Show)

makeLenses ''Page

-- instance FromJSON Page where {{{
instance FromJSON Page where
    parseJSON (Object v) = Page <$> v .:  "id"
                                <*> v `parseDateUnsafe` "createdAt"
                                <*> v `parseDateUnsafe` "updatedAt"
                                <*> v .:  "title"
                                <*> v .:  "name"
                                <*> v .:? "summary"
                                <*> v .:  "content"
                                <*> v .:  "variables"
                                <*> v .:  "userId"
                                <*> v .:  "user"
                                <*> v .:  "hideTitleWhenPinned"
                                <*> v .:  "alignCenter"
                                <*> v .:  "font"
                                <*> v .:? "eyeCatchingImageId"
                                <*> v .:? "eyeCatchingImage"
                                <*> v .:  "attachedFiles"
                                <*> v .:  "likedCount"
    parseJSON _          = fail "Failed to parse Page object"
-- }}}
