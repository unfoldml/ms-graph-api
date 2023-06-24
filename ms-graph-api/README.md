# ms-graph-api

Haskell client bindings to the [Microsoft Graph API v1.0](https://learn.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0).
    
[![Hackage](https://img.shields.io/hackage/v/ms-graph-api?style=for-the-badge)](https://hackage.haskell.org/package/ms-graph-api)

![main](https://github.com/unfoldml/ms-graph-api/actions/workflows/haskell.yml/badge.svg?branch=main)


## Introduction

This library provides both client interface to the MS Grapi API (under the `MSGraphAPI` namespace).

Authentication can be implemented with the `ms-auth` library.

## Status

This library is still in development, so expect missing functionality.
If there's anything you would like to see added, feel free to
[open an issue](https://github.com/unfoldml/ms-graph-api/issues/new).
In general, since the MS Graph API is quite large, features will be added to this library on a need basis.

## Evolution of the library

Some breaking changes might also be introduced as the library matures.

We adhere to the [Package Versioning Policy](https://pvp.haskell.org/): major breaking changes or API refactors are signaled by increasing the first major version number (i.e. 0.0.0.0 -> 1.0.0.0 ) whereas less significant ones are indicated by increasing the second one (0.0.0.0 -> 0.1.0.0)

Significant changes in the SDK will be documented in the respective CHANGELOG.

## Copyright

(c) 2023-, Marco Zocca, UnfoldML AB
