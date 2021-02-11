{-# LANGUAGE OverloadedStrings #-}
module Web.Misskey.Api.Notes.Show (
  APIRequest(..)
, notesShow
) where

import Data.Aeson ((.=))
import Web.Misskey.Type
import Web.Misskey.Api.Internal

data APIRequest = NoteId Id

-- | Call notes/show API
notesShow :: APIRequest -> Misskey Note
notesShow (NoteId i) = postRequest "/api/notes/show" $ [ "noteId" .= i]
