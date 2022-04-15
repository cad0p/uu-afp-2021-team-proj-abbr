# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2022-04-15

### Added

- Knowledge base: automatic abbreviation conflict detection and resolution.
- CLI Tool: inplace functionality for replace.
- Extension (Elm frontend): provide functionality for single abbreviation expansion in VS Code.
- CLI Tool: `AKeyword`: generalized `Keyword` type to unlock usage of `Functor` and `Applicative` operators.
- Testing: HUnit for Parser and Adapters, DocTests for the rest.
- CLI Tool: error handling using monadic operators.

### Changed

- CLI Tool: renamed `team-proj-abbr` executable to `shorthndr`.
- LibCore: renamed `Keyword` constructor to `Key`, `Keyword` becomes the type `AKeyword String`.
- LibCore: all the custom translations from string to `Keyword` are now handled with `pure` (in `Handlers` and `Adapters`).
- cleanup of READMEs.
- Handlers: moved and cleaned up wrt variable naming schemes.

### Removed

- `InputInterface` and `OutputInterface`

### Fixed

- Parsing of whitespaces and punctuation/special characters.
- knowledge base file locking that prevented us from implementing inplace before.

## [0.0.1] - 2022-04-06

### Added

- First release.
- Abbreviation syntax: support for basic abbreviation syntax.
- Abbreviation syntax: support for abbreviation syntax supporting plural expansion.
- Knowledge base: file format definition for specifying basic abbreviation records.
- Knowledge base: file format definition for with plural expansion support.
- CLI tool: load abbreviation dictionary from a locally stored file.
- CLI tool: expand in bulk mode with input as input stream.
- CLI tool: expand in bulk mode with input as a local file.
- CLI tool: expand in bulk mode with output to standard output stream.
- CLI tool: expand in bulk mode with output to a specified file.
- CLI tool: expand in bulk mode with output to the input file.
- Extension: expand abbreviations in bulk mode on file save.
- Knowledge base: CRUD support for management of abbreviations in the dictionary file.
- CLI Tool: provide CRUD interface for dictionary management.
- CLI tool: expand in single mode with all input/output combinations.

[Unreleased]: https://github.com/cad0p/uu-afp-2021-team-proj-abbr/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/cad0p/uu-afp-2021-team-proj-abbr/compare/v0.0.1...v0.1.0
[0.0.1]: https://github.com/cad0p/uu-afp-2021-team-proj-abbr/compare/6098930...v0.0.1
<!-- How to find the initial commit: `git log --oneline --reverse | head -n 1`
source: https://stackoverflow.com/questions/44854556/how-can-i-compare-changes-against-a-github-projects-first-commit -->
