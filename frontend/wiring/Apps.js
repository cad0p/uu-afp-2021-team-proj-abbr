const expandWorker = require("../dist/ExpandWorker");
const replaceWorker = require("../dist/ReplaceWorker");
const { ShortHndrCli } = require("./ShortHndrCli");

console.debug(expandWorker);
console.debug(replaceWorker);

// TODO(tech debt): add argument types and generics...
const configureApp = function (app) {
  // Make wiring to ShortHndr side-car service.
  app.ports.toShortHndr.subscribe(function (msg) {
    ShortHndrCli.call(
      msg,
      [],
      // error handler
      ({ error, stderr }) => app.ports.fromShortHndrError.send(stderr),
      // stdout flow
      ({ stdout, stderr }) =>
        !!stderr
          ? app.ports.fromShortHndrError.send(stderr)
          : app.ports.fromShortHndrSuccess.send(stdout)
    );
  });

  return app;
};

const setupApps = function ({ kbPath }) {
  return {
    expandApp: configureApp(
      expandWorker.Elm.ExpandWorker.init({ flags: { kbPath } })
    ),
    replaceApp: configureApp(
      replaceWorker.Elm.ReplaceWorker.init({ flags: { kbPath } })
    ),
  };
};

module.exports.expandApp = expandWorker;
module.exports.replaceWorker = replaceWorker;
module.exports.setupApps = setupApps;
