const worker = require("../dist/ExpandWorker");
const { ShortHndrCli } = require("./ShortHndrCli");

// TODO: add argument types...
const setupApp = function ({ kbPath }) {
  const app = worker.Elm.ExpandWorker.init({ flags: { kbPath } });

  app.ports.toExtension.subscribe(function (msg) {
    console.log("got from port", msg);
  });

  // Make wiring to ShortHndr side-car service.
  app.ports.toShortHndr.subscribe(function (msg) {
    ShortHndrCli.call(
      msg,
      [],
      (error) => console.error(`Error: ${error}`),
      (ok) => console.log(ok)
    );
  });

  return app;
};

module.exports.worker = worker;
module.exports.setupApp = setupApp;
