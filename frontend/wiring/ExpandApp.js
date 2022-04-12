const worker = require("./dist/VsCodeWorker.js.js");
const { ShortHndr } = require("./lib/ShortHndr");

// TODO: add argument types...
const setupApp = function ({ kbPath }) {
  const app = worker.Elm.ExpandWorker.init({ flags: { kbPath } });

  app.ports.toExtension.subscribe(function (msg) {
    console.log("got from port", msg);
  });

  // Make wiring to ShortHndr side-car service.
  app.ports.toShortHndr.subscribe(function (msg) {
    ShortHndr.call(
      msg,
      [],
      (error) => console.error(`Error: ${error}`),
      (ok) => console.log(ok)
    );
  });
};

module.exports.worker = worker;
module.exports.setupApp = setupApp;
