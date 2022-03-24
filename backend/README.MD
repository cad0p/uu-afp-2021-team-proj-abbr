Add the following to your settings.json in VS Code, they are settings for the [recommended extensions](.vscode/extensions.json)
```json
    "emeraldwalk.runonsave": {
        "commands": [
            {
                "match": "\\.hs$",
                "cmd": "stylish-haskell -i ${file}"
            }
        ]
    },
    "haskell.hlint.executablePath": 
        "~/.local/bin/hlint",
    "[haskell]": {
        "editor.tabSize": 2,
        "editor.defaultFormatter": 
            "vigoo.stylish-haskell"
    },
```