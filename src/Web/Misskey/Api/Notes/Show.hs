{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Web.Misskey.Api.Notes.Show (
  APIRequest(..)
, notesShow
) where

import RIO
import Data.Aeson ((.=))
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data APIRequest = NoteId Id

-- | Call notes/show API
notesShow :: (HasMisskeyEnv env) => APIRequest -> RIO env Note
notesShow (NoteId i) = postRequest "/api/notes/show" $ [ "noteId" .= i]
