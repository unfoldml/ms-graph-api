# Changelog for `ms-azure-api`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.3.0.0

MSAzureAPI.MachineLearning.Compute
MSAzureAPI.MachineLearning.Jobs
MSAzureAPI.MachineLearning.Usages

* breaking changes:
MSAzureAPI.StorageServices.FileService. listDirectoriesAndFiles now has an extra parameter to support paginated results

## 0.2.0.0

MSAzureAPI.StorageServices.FileService. listDirectoriesAndFiles

Add XML support via `xeno` and `xmlbf` to parse `listDirectoriesAndFiles` response bodies

## 0.1.0.0

First release
