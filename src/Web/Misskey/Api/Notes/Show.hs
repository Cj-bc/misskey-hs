{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Web.Misskey.Api.Notes.Show (
  APIRequest(..)
, notesShow
) where

import RIO
import Data.Aeson ((.=), ToJSON(..), object)
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data APIRequest = NoteId Id

instance ToJSON APIRequest where
  toJSON (NoteId i) = object ["noteId" .= i]

-- | Call notes/show API
notesShow :: (HasMisskeyEnv env) => APIRequest -> RIO env Note
notesShow = postRequest "/api/notes/show" . toJSON
