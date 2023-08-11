# Changelog for `ms-auth`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased


## 0.4.0.0

Add Bot Framework support

Breaking changes:
- MSAuth is the only public interface module
- 'newNoToken' and 'fetchUpdateToken' are not expored anymore from Session and MSAuth, in favor of a single function 'tokenUpdateLoop' which does both the initialization and the refresh loop
- 'applyDotEnv' and the 'DotEnv' module are gone. Please use the equivalent package 'dotenv-micro'

## 0.3.0.0

defaultAzureCredential - simplified version of the Microsoft Identity SDK

introduced MSAuth module that re-exports internal functions

Breaking changes:
- module Network.OAuth2.JWT is not exposed anymore
- OAuthCfg does not contain fields for client ID and secret anymore
- client ID and client secret can only be loaded from environment variables
- Network.OAuth2.Provider.AzureAD azureADApp and azureOAuthADApp return in MonadIO since they look up client ID and secret from the environment

## 0.1.0.0

Network.OAuth2.Session : Add App-only functionality
