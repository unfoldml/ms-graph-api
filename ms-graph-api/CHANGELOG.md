# Changelog for `ms-graph-api`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased


## 1.0.0.0

MSGraphAPI.ChangeNotifications.Subscription

== Breaking changes:

Moved the Network/* module hierarchy to the @ms-auth@ package shared with @ms-azure-api@


## 0.6.0.0

MSGraphAPI.Users.Group :
- Group
- getUserJoinedTeams
- getGroupsDriveItems

Depend on `validation-micro` rather than `validation-selective`.

## 0.5.0.0

Add 'getE', 'postE', 'tryReq' to pattern match against HTTP exceptions

## 0.4.0.0

Add Session.tokensToList and Session.newTokens
