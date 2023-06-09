# Changelog for `ms-azure-api`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.4

TLS support


## 0.3.1.0

MSAzureAPI.StorageServices.FileService : add listDirectoriesAndFilesC (stream all response pages from listDirectoriesAndFiles)

## 0.3.0.0

add 'conduit' as a dependency

MSAzureAPI.MachineLearning.Compute
MSAzureAPI.MachineLearning.Jobs
MSAzureAPI.MachineLearning.Usages

* breaking changes:
MSAzureAPI.StorageServices.FileService. listDirectoriesAndFiles now has an extra parameter to support paginated results, as well as a more informative return type.

## 0.2.0.0

MSAzureAPI.StorageServices.FileService : add listDirectoriesAndFiles

Add XML support via `xeno` and `xmlbf` to parse `listDirectoriesAndFiles` response bodies

## 0.1.0.0

First release
