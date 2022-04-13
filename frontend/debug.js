const { setupApps } = require("./wiring/Apps");

const { expandApp } = setupApps({ kbPath: ".demo/data/example_kb.csv" });

// Debug on expand App
const app = expandApp;

// Setup subscriptions.
app.ports.toExtensionInfo.subscribe(function (msg) {
  console.debug("INFO: got from port", msg);
});
app.ports.toExtensionError.subscribe(function (msg) {
  console.error("ERR: got from port", msg);
});
app.ports.toExtensionContent.subscribe(function (msg) {
  console.info("CONTENT: got from port ->", msg);
});

// Send message.
console.log("Sending to Elm...");
app.ports.fromExtension.send("Ping!");
app.ports.fromExtensionExpand.send("@@hl");
