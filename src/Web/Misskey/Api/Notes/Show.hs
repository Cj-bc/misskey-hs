{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Misskey.Api.Notes.Show (
  NotesShow(..)
) where

import RIO
import Data.Aeson ((.=), ToJSON(toJSON), object)
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data NotesShow = NoteId Id

instance ToJSON NotesShow where
  toJSON (NoteId i) = object ["noteId" .= i]

instance APIRequest NotesShow where
  type APIResponse NotesShow = Note
  apiPath _ = "/api/notes/show" 
