const worker = require("./dist/VsCodeWorker.js.js");
const { ShortHndr } = require("./lib/ShortHndr");

const app = worker.Elm.VsCodeWorker.init({});

app.ports.toExtension.subscribe(function (msg) {
  console.log("got from port", msg);
});

// Make wiring between the command interface
app.ports.toShortHndr.subscribe(function (msg) {
  ShortHndr.call(
    msg,
    [],
    (error) => console.error(`Error: ${error}`),
    (ok) => console.log(ok)
  );
});

module.exports.app = app;
