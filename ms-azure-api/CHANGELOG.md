# Changelog for `ms-azure-api`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

get rid of `hoauth` dependency in favor of `ms-auth`.

.CostManagement
.MachineLearning.OnlineEndpoints

## 0.6.0.0

.BotService

## 0.5.0.0

ToJSON instance of Location renders the full name e.g. "West Europe"

.ServiceBus

add 'http-client' as an explicit dependency

* breaking changes
Fixed definition of 'put' to use the correct HTTP verb
Add constructor to 'APIPlane' to reflect service bus usage

## 0.4

TLS support


## 0.3.1.0

.StorageServices.FileService : add listDirectoriesAndFilesC (stream all response pages from listDirectoriesAndFiles)

## 0.3.0.0

add 'conduit' as a dependency

.MachineLearning.Compute
.MachineLearning.Jobs
.MachineLearning.Usages

* breaking changes:
.StorageServices.FileService. listDirectoriesAndFiles now has an extra parameter to support paginated results, as well as a more informative return type.

## 0.2.0.0

.StorageServices.FileService : add listDirectoriesAndFiles

Add XML support via `xeno` and `xmlbf` to parse `listDirectoriesAndFiles` response bodies

## 0.1.0.0

First release
