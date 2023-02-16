{- |
Module      :  Web.Misskey.Api
Description :  For user module that exports all Api endpoints
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc 2023
License     :  BSD3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

Functions and data types for library user.
You might want to import lenses from each module individually.

* Typical usage of this module

1. Build your API request
2. Feed it to 'call'
3. You'll get the response


-}

module Web.Misskey.Api (
  APIRequest(call)
  -- * API Request data types
  , NotesCreate(NotesCreate)
  , NotesShow(NoteId)
  , NotesTimeline(NotesTimeline)
  , UsersUsers(UsersUsers)
  , UsersShow
  , _UserId, _UserIds, _UserName
  , UsersFollowing(UsersFollowing)
  , UsersFollowers(UsersFollowers)
  , UsersSearch(UsersSearch)
  , UsersNotes(UsersNotes)

  -- * Some related data types
  , Visibility(..)
  ) where
import Web.Misskey.Api.Internal
import Web.Misskey.Api.Notes.Create
import Web.Misskey.Api.Notes.Show
import Web.Misskey.Api.Notes.Timeline
import Web.Misskey.Api.Users.Users
import Web.Misskey.Api.Users.Show
import Web.Misskey.Api.Users.Following
import Web.Misskey.Api.Users.Followers
import Web.Misskey.Api.Users.Search
import Web.Misskey.Api.Users.Notes
