# team-proj-abbr

## Documentation and Test Coverage

The Haddock documentation and the test coverage for this backend can be found here:

https://cad0p.github.io/uu-afp-2021-team-proj-abbr/

It gets automatically updated with each new stable version, thanks to GitHub Actions. We also have a dev version to rapidly see the changes without affecting the stable documentation.

## To install

`stack install` will install the `shorthndr` CLI, which you can easily call from anywhere in your terminal of choice.

## To setup

```sh
cd team-proj-abbr
stack setup
```

## To run

```sh
stack build
stack ghci
stack test
stack haddock
```

### CLI

The CLI can be run as follows:

```sh
stack exec shorthndr -- --help
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

> The CLI execution can be tested using `stack run` or `stack exec`.  
> `stack run shorthndr -- ...` builds the source each time it is run.  
> `stack exec shorthndr -- ...` uses the latest available build.  

1. `Expand`: - standard input/output approach

    ```bash
    $ stack exec shorthndr -- expand -k="data/kb_example.csv" -a="@@hl"
    hello
    $ stack exec shorthndr -- expand -k="data/kb_example.csv" -a="@@hl @@hl people" 
    hello hello people
    ```

2. `Replace`: - expansion of the full file content

    - _Basic_ approach, with `--input` and `--output`: 

      ```bash
      # create demo file:
      $ echo "@@hl @@hl people" >> data/demo_file.txt
      # demo with input and output
      $ stack exec shorthndr --  \
          replace                         \
          --input="data/demo_file.txt"    \
          -o="./demo_file_o.txt"          \
          -k="data/kb_example.csv"
      $ cat ./demo_file_o.txt
      hello hello people
      # remove the demo files:
      $ rm data/demo_file.txt ./demo_file_o.txt
      ```

    - _Inplace_ approach, with `--input` and `--inplace`: 

      ```bash
      # create demo file:
      $ echo "@@hl @@hl people" >> data/demo_file.txt
      # demo with inplace
      $ stack exec shorthndr --  \
          replace                         \
          --inplace                       \
          --input="data/demo_file.txt"    \
          -k="data/kb_example.csv"
      $ cat ./data/demo_file.txt
      hello hello people
      $ rm data/demo_file.txt
      ```

3. `List`: - get all the knowledge base contents

    ```bash
    $ stack exec shorthndr -- list -k="data/kb_example.csv"
    Key: ax --> Value: axiom
    Key: hl --> Value: hello
    Key: lmm --> Value: lemma
    Key: prf --> Value: proof
    Key: thm --> Value: theorem
    ```

4. `Add`: - add new abbrevation record to the KB

    ```bash
    $ stack exec shorthndr --  \
        add                             \
        -k="data/kb_example.csv"        \
        -a="brb"                        \
        -e="be right back"
    Added: Keyword {keyword = "be right back", plural = False}
    # check modification:
    $ cat data/kb_example.csv
    abbreviation,expansion
    ...
    brb,be right back
    ...
    ```

5. `Delete`: - delete an existing abbrevation record from the KB

    ```bash
    # check the deletion target
    $ cat data/kb_example.csv | grep hl
    hl,hello
    $ stack exec shorthndr -- delete -k="data/kb_example.csv" -a="hl"
    Removed: Keyword {keyword = "hl", plural = False}
    # nothing can be found
    $ cat data/kb_example.csv | grep hl -c 
    0
    $ stack exec shorthndr -- delete -k="data/kb_example.csv" -a="hl"
    shorthndr: StandardError "no record found for this keyword : Keyword {keyword = \"hl\", plural = False}"
    ...
    ```

6. `Update`: - update an existing abbrevation record in the KB

    ```bash
    $ stack exec shorthndr --  \
        update                          \
        -k="data/kb_example.csv"        \
        -a="hl"                         \
        -e="HELLO\!"
    Updated: Keyword {keyword = "hl", plural = False} to Keyword {keyword = "HELLO!", plural = False}
    # check modification:
    $ cat data/kb_example.csv | grep hl
    hl,"HELLO!"
    ```
