# ms-auth

Haskell client bindings to the [Microsoft Identity / Active Directory API]().
    
[![Hackage](https://img.shields.io/hackage/v/ms-auth?style=for-the-badge)](https://hackage.haskell.org/package/ms-auth)

![main](https://github.com/unfoldml/ms-graph-api/actions/workflows/haskell.yml/badge.svg?branch=main)


## Introduction

This library provides helpers for building token-based authentication flows within server-based web apps e.g. 

* [Client Credentials](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-client-creds-grant-flow) (server/server or automation accounts)
* [Authorization Code](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow) (with human users being prompted to delegate some access rights to the app)

, as well as for keeping tokens up to date in the background.


## Status

This library is functional but still in development.

## Evolution of the library

Some breaking changes might also be introduced as the library matures.

We adhere to the [Package Versioning Policy](https://pvp.haskell.org/): major breaking changes or API refactors are signaled by increasing the first major version number (i.e. 0.0.0.0 -> 1.0.0.0 ) whereas less significant ones are indicated by increasing the second one (0.0.0.0 -> 0.1.0.0).

Significant changes in the SDK will be documented in the respective CHANGELOG.


## Copyright

(c) 2023-, Marco Zocca, UnfoldML AB
