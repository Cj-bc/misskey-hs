# misskey-hs

A Haskell library for [syuilo/Misskey](https://github.com/syuilo/misskey)'s API

# Usage

## As API caller

I'm implementing a simple command-line program to execute each API.


## As Library

This package adds modules below:

| module                             | description                              |
|:-:|:-:|
| `Network.Misskey.Type`             | Provide types for general usage          |
| `Network.Misskey.Api.Users.Show`   | Command and APIRequest for `users/show`  |
| `Network.Misskey.Api.Users.Search` | Command and APIRequest for `users/search`|
| `Network.Misskey.Api.Users.Notes`  | Command and APIRequest for `users/Notes` |


### Basic usage

All API-calling functions are placed in `Network.Misskey.Api.*`  
You can pick one(e.g. `usersShow`), give it APIRequest(defined in each module),
and do `runMisskey` with `MisskeyEnv` to send API request.  


e.g. Sending `usersShow` request to me(`cj_bc_sd@virtual-kaf.fun`),
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
