const { setupApp } = require("./wiring/ExpandApp");

const app = setupApp({ kbPath: "data/kb_example.csv" });

// Setup subscriptions.
app.ports.toExtensionInfo.subscribe(function (msg) {
  console.debug("INFO: got from port", msg);
});
app.ports.toExtensionError.subscribe(function (msg) {
  console.error("ERR: got from port", msg);
});
app.ports.toExtensionSuccess.subscribe(function (msg) {
  console.info("OK: got from port", msg);
});

// Send message.
console.log("Sending to Elm...");
app.ports.fromExtensionExpand.send("@@hl");
