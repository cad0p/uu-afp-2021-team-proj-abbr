const vscode = require("vscode");
const worker = require("./dist/VsCodeWorker.js")


/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
  const app = worker.Elm.VsCodeWorker.init({});

  console.log(
    'Congratulations, your extension "helloworld-minimal-sample" is now active!'
  );

  let disposable = vscode.commands.registerCommand(
    "extension.helloWorld",
    () => {
      // The code you place here will be executed every time your command is executed

      // Display a message box to the user
      vscode.window.showInformationMessage("Hello World!");
    }
  );

  context.subscriptions.push(disposable);
}

function deactivate() {}

// eslint-disable-next-line no-undef
module.exports = {
  activate,
  deactivate,
};
