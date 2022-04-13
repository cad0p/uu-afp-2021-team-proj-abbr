const vscode = require("vscode");

// The `app` wired up with the Elm brain.
const { setupApp } = require("./wiring/ExpandApp");

/**
 * Editor interface.
 */
const editor = vscode.window.activeTextEditor;

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
  // TODO: instantiate with the correct KB path from settings.
  const kbPath = context.asAbsolutePath(".demo/data/example_kb.csv");
  const expandApp = setupApp({ kbPath });
  console.debug('Congratulations, your extension "ShortHndr" is now active!');

  // expandApp subscriptions
  console.debug(expandApp.ports);
  expandApp.ports.toExtensionInfo.subscribe(function (msg) {
    vscode.window.showWarningMessage(msg);
  });
  expandApp.ports.toExtensionError.subscribe(function (msg) {
    vscode.window.showErrorMessage(msg);
  });
  expandApp.ports.toExtensionExpand.subscribe(function (msg) {
    if (!!editor) {
      editor.edit((editBuilder) => {
        editBuilder.replace(editor.selection, msg);
      });
    } else {
      throw new Error("no editor");
    }
  });

  // Extension command subscriptions
  const ping = vscode.commands.registerCommand("shorthndr.ping", () => {
    expandApp.ports.fromExtension.send("Ping!");
  });
  const expand = vscode.commands.registerCommand("shorthndr.expand", () => {
    if (!!editor) {
      const text = editor.document.getText(editor.selection);
      expandApp.ports.fromExtensionExpand.send(text);
    } else {
      throw new Error("no editor");
    }
  });

  context.subscriptions.push(ping, expand);
}

function deactivate() {}

// eslint-disable-next-line no-undef
module.exports = {
  activate,
  deactivate,
};
