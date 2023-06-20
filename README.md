# ms-api

[![ms-graph-api-CI](https://github.com/unfoldml/ms-graph-api/actions/workflows/haskell.yml/badge.svg)](https://github.com/unfoldml/ms-graph-api/actions/workflows/haskell.yml)

Haskell SDK for the Microsoft APIs

| Service  | Release |
|---|---|
| [Identity / Active Directory](https://learn.microsoft.com/en-us/graph/azuread-identity-access-management-concept-overview) | [![Hackage](https://img.shields.io/hackage/v/ms-auth?style=for-the-badge)](https://hackage.haskell.org/package/ms-auth) |
| [Graph](https://learn.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0) | [![Hackage](https://img.shields.io/hackage/v/ms-graph-api?style=for-the-badge)](https://hackage.haskell.org/package/ms-graph-api) |
| [Azure](https://learn.microsoft.com/en-us/rest/api/azure/) | [![Hackage](https://img.shields.io/hackage/v/ms-azure-api?style=for-the-badge)](https://hackage.haskell.org/package/ms-azure-api) |


## Introduction

We provide separate libraries for authentication (`ms-auth`), Graph (`ms-graph-api`) and Azure (`ms-azure-api`) since they cater to different use cases; they are designed to work together at any rate. 

In the majority of cases a user will want to install `ms-auth` as well as the API binding they need (unsure whether Azure or Graph allow anonymous usage of any of their endpoints).

## Examples

* OAuth flow : see `ms-graph-api-test/app/Main.hs` 

## Status

This library is still in development, so expect missing functionality.
If there's anything you would like to see added, feel free to
[open an issue](https://github.com/unfoldml/ms-graph-api/issues/new), or even better contribute some code.
In general, since the API surface is quite large, features will be added to this library on a need basis.

## Evolution of the library

Some breaking changes might also be introduced as the library matures.

We adhere to a simplified version of the [Package Versioning Policy](https://pvp.haskell.org/): breaking changes are signaled by increasing the major version number (e.g. 0.x -> 1.x ).

Significant changes in the SDK will be documented in the respective CHANGELOG.

## LICENSE

BSD 3

## Copyright

(c) 2023-, Marco Zocca, UnfoldML AB
