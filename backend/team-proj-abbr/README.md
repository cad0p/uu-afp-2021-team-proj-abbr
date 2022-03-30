# team-proj-abbr
## To setup:
```sh
cd team-proj-abbr
stack setup
```

## To run:
```sh
stack build
stack ghci
stack test
stack haddock
```

### CLI

The CLI can be run as follows:
```sh
stack exec team-proj-abbr-cli -- --help
```
(the double dash `--` passes all the following command line arguments to the executable)

To debug:
```sh
stack ghci team-proj-abbr:team-proj-abbr-test
```

And if you want `:r` to work: [(source)](https://stackoverflow.com/questions/39938101/how-to-load-tests-in-ghci-with-stack)
```sh
stack ghci --ghci-options -isrc --ghci-options -itest team-proj-abbr:team-proj-abbr-test
```
