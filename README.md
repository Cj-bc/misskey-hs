[日本語](JA-README.md)
---

# misskey-hs

A Haskell library for [syuilo/Misskey](https://github.com/syuilo/misskey)'s API

# Usage

## As CLI tool

`stack run --` to call each API.  
Specify API by subcommand, and pass args by options.  
`stack run -- --help` to more help.


### Usage sample

```sh
$ stack run -- users/show --username cj_bc_sd
[User {_user_id = ...}]
```

```
$ stack run -- --help
Usage: misskey-hs-exe COMMAND
  call Misskey API

Available options:
  -h,--help                Show this help text

Available commands:
  users/show               call users/show API
  users/notes              call users/notes API
  users/search             call users/search API
  users                    call users API
```

## As Library

This package adds modules below:

| module                                | description                                 |
|:-:|:-:|
| `Network.Misskey.Type`                | Provide types for general usage             |
| `Network.Misskey.Api.Users.Show`      | Command and APIRequest for `users/show`     |
| `Network.Misskey.Api.Users.Search`    | Command and APIRequest for `users/search`   |
| `Network.Misskey.Api.Users.Notes`     | Command and APIRequest for `users/Notes`    |
| `Network.Misskey.Api.Users.Users`     | Command and APIRequest for `users`          |
| `Network.Misskey.Api.Users.Following` | Command and APIRequest for `users/following |
| `Network.Misskey.Api.Users.Followers` | Command and APIRequest for `users/followers |


### Basic usage

All API-calling functions are placed in `Network.Misskey.Api.*`  
You can pick one(e.g. `usersShow`), give it APIRequest(defined in each module),
and do `runMisskey` with `MisskeyEnv` to send API request.  


e.g. Sending `users/show` API request to me(`cj_bc_sd@virtual-kaf.fun`),
and print result.
```haskell
import Network.Misskey.Api.Users.Show (usersShow, APIRequest(..))
import Network.Misskey.Type (User(..), MisskeyEnv(..))


main :: IO ()
main = do
    let env = MisskeyEnv ""                -- A Misskey token. I omit this because we don't need it in this time
                         "virtual-kaf.fun" -- Domain which to send API request
        req = UserName "cj_bc_sd"          -- `UserName` is value constructor of APIRequest (for `usersShow`)

    -- Post API Request and get result
    usr <- runMisskey (usersShow req) env

    print usr
```

See haddock and [misskey's api document](https://misskey.io/api-doc)(Currently lacks some data)


