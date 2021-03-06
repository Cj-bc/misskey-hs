# Changelog for misskey-hs
All notable changes to this project will be documented in this file.

The format is based on [keep a changelog](https://keepachangelog.com/ja/1.0.0/)
And this project adheres to [Haskell Package Versioning Policy](https://pvp.haskell.org/)

## Unreleased changes

## [0.2.0.0] - 2020-05-05
### Changed
- Base module is now belong to 'Web', instead of 'Network'


## [0.1.4.0] - 2020-01-06
### Added
- New modules:
  - Network.Misskey.Api.Notes.Timeline
- 'createUTCTimeObj' to 'Network.Misskey.Api.Internal'

### Changed
- app/Main support added modules
- executable for app/Main is now renamed to 'misskey-cli'

### Fixed
- Fixed option names for 'usersNotes' command


## [0.1.3.0] - 2020-01-06
### Added
- New modules:
  - Network.Misskey.Api.Notes.Create
- `config.yaml` file should be at `~/.config/misskey-hs/config.yaml`
- `config.yaml` contains 'token' and 'instance_url' (more info in README)
- ToJSON instances for Poll/Geo

### Changed
- app/Main supports added modules
- app/Main has 'GeneralOption' to treat 'generally used option'
- postRequest adds token to request body automatically
- Export 'Network.Misskey.Type.Geo'
- Only `app/Main.hs` depends on `unicode-show`


## [0.1.2.0] - 2020-01-03
### Added
- New modules:
  - Network.Misskey.Api.Users.Following
  - Network.Misskey.Api.Users.Followers
- app/Main supports added modules

### Fixed
- app/Main will parse argument properly

### Changed
- Update README's module list

## [0.1.1.0] - 2020-01-03
### Added
- New modules:
  - Network.Misskey.Type
  - Network.Misskey.Api.Internal
  - Network.Misskey.Api.Users.Notes
  - Network.Misskey.Api.Users.Show
  - Network.Misskey.Api.Users.Search
  - Network.Misskey.Api.Users.Users
- Add API caller for:
  - users/notes
  - users/show
  - users/search
  - users
- CLI tool to call those APIs
- README/JA-README
