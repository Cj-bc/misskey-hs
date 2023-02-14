{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Web.Misskey.Api.Notes.Show (
  NotesShow(..)
, notesShow
) where

import RIO
import Data.Aeson ((.=), ToJSON(toJSON), object)
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data NotesShow = NoteId Id

instance ToJSON NotesShow where
  toJSON (NoteId i) = object ["noteId" .= i]

-- | Call notes/show API
notesShow :: (HasMisskeyEnv env) => NotesShow -> RIO env Note
notesShow = postRequest "/api/notes/show" . toJSON
