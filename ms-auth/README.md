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

We adhere to a simplified version of the [Package Versioning Policy](https://pvp.haskell.org/): breaking changes are signaled by increasing the major version number (e.g. 0.x -> 1.x ).


## Copyright

(c) 2023-, Marco Zocca, UnfoldML AB
