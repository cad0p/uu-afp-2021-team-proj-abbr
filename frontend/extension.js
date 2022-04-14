const vscode = require("vscode");

// The `app` wired up with the Elm brain.
const { setupApps } = require("./wiring/Apps");

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
  const { expandApp, replaceApp } = setupApps({ kbPath });

  // expandApp subscriptions
  console.debug(expandApp.ports);
  expandApp.ports.toExtensionInfo.subscribe(function (msg) {
    vscode.window.showWarningMessage(`ExpandApp: ${msg}`);
  });
  expandApp.ports.toExtensionError.subscribe(function (msg) {
    vscode.window.showErrorMessage(`ExpandApp: ${msg}`);
  });
  expandApp.ports.toExtensionContent.subscribe(function (msg) {
    if (!!editor) {
      editor.edit((editBuilder) => {
        editBuilder.replace(editor.selection, msg);
      });
    } else {
      throw new Error("no editor");
    }
  });
  // replaceApp subscriptions
  replaceApp.ports.toExtensionInfo.subscribe(function (msg) {
    vscode.window.showWarningMessage(`ReplaceApp: ${msg}`);
  });
  replaceApp.ports.toExtensionError.subscribe(function (msg) {
    vscode.window.showErrorMessage(`ReplaceApp: ${msg}`);
  });

  // Extension command subscriptions
  const ping = vscode.commands.registerCommand("shorthndr.ping", () => {
    expandApp.ports.fromExtension.send("Ping!");
    replaceApp.ports.fromExtension.send("Ping!");
  });
  const expand = vscode.commands.registerCommand("shorthndr.expand", () => {
    if (!!editor) {
      const text = editor.document.getText(editor.selection);
      expandApp.ports.fromExtensionExpand.send(text);
    } else {
      throw new Error("no editor");
    }
  });
  const replace = vscode.commands.registerCommand("shorthndr.replace", () => {
    if (!!editor) {
      const openedFile = vscode.window.activeTextEditor.document.uri.fsPath;
      replaceApp.ports.fromExtensionReplace.send(openedFile);
    } else {
      throw new Error("no editor");
    }
  });

  context.subscriptions.push(ping, expand, replace);
  console.debug('Congratulations, your extension "ShortHndr" is now ready!');
}

function deactivate() {}

// eslint-disable-next-line no-undef
module.exports = {
  activate,
  deactivate,
};
