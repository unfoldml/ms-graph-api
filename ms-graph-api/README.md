# ms-graph-api

Haskell client bindings to the [Microsoft Graph API v1.0](https://learn.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0).
    
[![Hackage](https://img.shields.io/hackage/v/ms-graph-api?style=for-the-badge)](https://hackage.haskell.org/package/ms-graph-api)

![main](https://github.com/unfoldml/ms-graph-api/actions/workflows/haskell.yml/badge.svg?branch=main)


## Introduction

This library provides both the client interface (under the `MSGraphAPI` namespace) as well as helpers to integrate with Microsoft infrastructure, e.g. using ActiveDirectory as an OAuth2 identity provider.

With the provided auth functions (see `Network.OAuth2.Session`) you can easily implement OAuth2 for your application, and the resulting token store is thread-safe and automatically renews before expiring.

## Status

This library is still in development, so expect missing functionality.
If there's anything you would like to see added, feel free to
[open an issue](https://github.com/unfoldml/ms-graph-api/issues/new).
In general, since the MS Graph API is quite large, features will be added to this library on a need basis.

## Evolution of the library

Some breaking changes might also be introduced as the library matures.

We adhere to a simplified version of the [Package Versioning Policy](https://pvp.haskell.org/): breaking changes are signaled by increasing the major version number (e.g. 0.x -> 1.x ).


## Copyright

(c) 2023-, Marco Zocca, UnfoldML AB
