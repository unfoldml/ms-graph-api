# Changelog for `ms-graph-api`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- get rid of `hoauth` in favor of `ms-auth`

## 0.11.0.0

.Users.Group
- add listTeamChannels
- add listChannelMessages, listMessageReplies

*Breaking changes*
Functions in .Users.Group are now called 'list..' rather than 'get..' to correspond with the REST API


## 0.10.0.0

*Breaking changes*
- .ChangeNotifications.Subscription : Subscription has fewer fields (the ID and TLS version fields are optional)

## 0.9.0.0

.Files.Drive

*Breaking changes*
- withTLS changed signature: the inner continuation has an additional Manager parameter


## 0.8.0.0

.Files.DriveItem :
- custom FromJSON instance using a sum type for the various types of drive item. Makes it convenient for users to pattern match on type. So far only File, Folder and Package drive item types are parsed further.

New MSGraphAPI module to re-expose internals


## 0.7.0.0

.ChangeNotifications.Subscription:
- add createSubscription

*Breaking changes*
- Moved the Network/* module hierarchy to the `ms-auth` package shared with `ms-azure-api`.

## 0.6.0.0

.Users.Group :
- Group
- getUserJoinedTeams
- getGroupsDriveItems

Depend on `validation-micro` rather than `validation-selective`.

## 0.5.0.0

Add 'getE', 'postE', 'tryReq' to pattern match against HTTP exceptions

## 0.4.0.0

Add Session.tokensToList and Session.newTokens
