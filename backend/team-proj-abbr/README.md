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

#### Supported demo commands

1. `Expand`: - standard input/output approach

    ```bash
    $ stack exec team-proj-abbr-cli -- expand -k="data/kb_example.csv" -a="@@hl"
    hello
    $ stack exec team-proj-abbr-cli -- expand -k="data/kb_example.csv" -a="@@hl @@hl people" 
    hello hello people
    ```

2. `Replace`: - expansion of the full file content

    ```bash
    # create demo file:
    $ echo "@@hl @@hl people" >> data/demo_file.txt
    $ stack exec team-proj-abbr-cli --  \
        replace                         \
        --input="data/demo_file.txt"    \
        -o="./demo_file_o.txt"          \
        -k="data/kb_example.csv"
    $ cat ./demo_file_o.txt
    hello hello people
    # remove the demo files:
    $ rm data/demo_file.txt ./demo_file_o.txt
    ```
