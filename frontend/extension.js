const vscode = require("vscode");

// The `app` wired up with the Elm brain.
const { setupApp } = require("./wiring/ExpandApp");

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
  // TODO: instantiate with the correct KB path from settings.
  const expandApp = setupApp({ kbPath: "data/kb_example.csv" });
  console.debug(expandApp);
  console.log('Congratulations, your extension "ShortHndr" is now active!');

  // expandApp subscriptions
  expandApp.ports.toExtension.subscribe(function (msg) {
    console.debug("got from port", msg);
    vscode.window.showInformationMessage(msg);
  });

  // TODO: make the setup somehow easier...
  // Extension command subscriptions
  let disposable = vscode.commands.registerCommand("shorthndr.ping", () => {
    expandApp.ports.fromExtension.send("Ping!");
  });

  context.subscriptions.push(disposable);
}

function deactivate() {}

// eslint-disable-next-line no-undef
module.exports = {
  activate,
  deactivate,
};
