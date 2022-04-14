/**
 * This module is used for manual experiments and
 * demonstration of the JS->Elm interaction.
 */

const { setupApps } = require("./wiring/Apps");

const { expandApp } = setupApps({ kbPath: ".demo/data/example_kb.csv" });

// Debug on expand App
const app = expandApp;

// Setup subscriptions.
app.ports.toExtensionInfo.subscribe(function (msg) {
  // Define message handler.
  console.debug("INFO: got from port", msg);
});
app.ports.toExtensionError.subscribe(function (msg) {
  // Define message handler.
  console.error("ERR: got from port", msg);
});
app.ports.toExtensionContent.subscribe(function (msg) {
  // Define message handler.
  console.info("CONTENT: got from port ->", msg);
});

// Send message.
console.log("Sending to Elm...");
app.ports.fromExtension.send("Ping!");
app.ports.fromExtensionExpand.send("@@hl");
// Output:
// > Sending to Elm...
// > INFO: got from port Pong!
// > CONTENT: got from port -> hello