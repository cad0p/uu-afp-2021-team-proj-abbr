const worker = require("../dist/ExpandWorker");
const { ShortHndrCli } = require("./ShortHndrCli");

// TODO: add argument types...
const setupApp = function ({ kbPath }) {
  const app = worker.Elm.ExpandWorker.init({ flags: { kbPath } });

  // Make wiring to ShortHndr side-car service.
  app.ports.toShortHndr.subscribe(function (msg) {
    ShortHndrCli.call(
      msg,
      [],
      // error handler
      ({ error, stderr }) => app.ports.fromShortHndrError.send(stderr),
      // stdout flow
      ({ stdout, stderr }) =>
        !!stdout
          ? app.ports.fromShortHndrSuccess.send(stdout)
          : app.ports.fromShortHndrError.send(stderr)
    );
  });

  return app;
};

module.exports.worker = worker;
module.exports.setupApp = setupApp;
