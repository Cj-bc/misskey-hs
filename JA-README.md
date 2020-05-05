[English](README.md)

---

# misskey-hs

[syuilo/Misskey](https://github.com/syuilo/misskey)のAPIのHaskellライブラリ

# 使用方法

## CLIツールとして

### Configを準備する

Configファイルを以下の形式で作成し、`~/.config/misskey-hs/config.yaml`に配置してください。

```yaml
token: <your_token>
instance_url: <your_instance_url>
```

例) Tokenが`MYTOKENHERE`で、所属しているインスタンスが`misskey.io`の場合

```yaml
token: MYTOKENHERE
instance_url: misskey.io
```



### ツールの使い方

`stack run --`で、各種APIを呼び出すコマンドを使うことができます。  
`optparse-applicative`を使っているので、`stack run -- --help`でヘルプが出ます。  
サブコマンドで使いたいAPIを指定、各APIのRequestとして渡す値はオプションで指定します。


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

## ライブラリとして

このライブラリは以下のモジュールを提供します:

| module                                | 説明                                         |
|:-:|:-:|
| `Web.Misskey.Type`                | ライブラリで共通して使う方が定義されています |
| `Web.Misskey.Api.Users.Show`      | `users/show`用のAPIRequestと関数             |
| `Web.Misskey.Api.Users.Search`    | `users/search`用のAPIRequestと関数           |
| `Web.Misskey.Api.Users.Notes`     | `users/Notes`用のAPIRequestと関数            |
| `Web.Misskey.Api.Users.Users`     | `users`用のAPIRequestと関数                  |
| `Web.Misskey.Api.Users.Following` | `users/following`用のAPIRequestと関数        |
| `Web.Misskey.Api.Users.Followers` | `users/followers`用のAPIRequestと関数        |
| `Web.Misskey.Api.Notes.Create`    | `notes/create`用のAPIRequestと関数           |
| `Web.Misskey.Api.Notes.Timeline`  | `notes/timeline`用のAPIRequestと関数         |

### Basic usage

APIを呼び出す関数は`Web.Misskey.Api.*`にあります。  
呼びたいAPIに対応する関数(例えば`users/show`なら`usersShow`)に、
各モジュールで定義されている`APIRequest`型の値を与え、
`MisskeyEnv`とともに`runMisskey`を呼び出してください。


例. `users/show` APIリクエストを筆者(`cj_bc_sd@virtual-kaf.fun`)について飛ばして結果を出力する
```haskell
import Web.Misskey.Api.Users.Show (usersShow, APIRequest(..))
import Web.Misskey.Type (User(..), MisskeyEnv(..))


main :: IO ()
main = do
    let env = MisskeyEnv ""                -- Misskey token.今回は必要ないので空文字列にします。
                         "virtual-kaf.fun" -- APIリクエストを飛ばすドメイン
        req = UserName "cj_bc_sd"          -- `UserName`は`usersShow`用のAPIRequestの値コンストラクターです

    -- APIを叩き、結果を返します
    usr <- runMisskey (usersShow req) env

    print usr
```

詳細は[misskey's api document](https://misskey.io/api-doc)(少し情報が欠けている模様)とHaddockを確認してください。


