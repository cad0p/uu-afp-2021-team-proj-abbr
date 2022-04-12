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
  const expandApp = setupApp({ kbPath: "data/kb_example.csv" });
  console.debug(expandApp);
  console.log('Congratulations, your extension "ShortHndr" is now active!');

  // expandApp subscriptions
  expandApp.ports.toExtensionInfo.subscribe(function (msg) {
    console.debug("got from port", msg);
    vscode.window.showWarningMessage(msg);
  });
  expandApp.ports.toExtensionError.subscribe(function (msg) {
    console.debug("got from port", msg);
    vscode.window.showErrorMessage(msg);
  });
  expandApp.ports.toExtensionSuccess.subscribe(function (msg) {
    console.debug("got from port", msg);
    vscode.window.showInformationMessage(msg);
  });

  // Extension command subscriptions
  const ping = vscode.commands.registerCommand("shorthndr.ping", () => {
    expandApp.ports.fromExtension.send("Ping!");
  });
  const expand = vscode.commands.registerCommand("shorthndr.expand", () => {
    if (!!editor) {
      const document = editor.document;
      const selection = editor.selection;
      const text = document.getText(selection);
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
