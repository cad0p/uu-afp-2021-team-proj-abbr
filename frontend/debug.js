// const worker = require("./dist/ExpandWorker.js");
// const { ShortHndrCli } = require("./wiring/ShortHndrCli");

const { setupApp } = require("./wiring/ExpandApp");

const app = setupApp({ kbPath: "data/kb_example.csv" });
console.log(app);

// console.log(worker);
// console.log(ShortHndrCli);

// const app = worker.Elm.ExpandWorker.init({
//   flags: { kbPath: "data/lb_example.csv" },
// });
// console.log(app);

// app.ports.toExtension.subscribe(function (msg) {
//   console.log("got from port", msg);
// });
// app.ports.toShortHndr.subscribe(function (msg) {
//   ShortHndrCli.call(
//     msg,
//     [],
//     (error) => console.error(`Error: ${error}`),
//     (ok) => console.log(ok)
//   );
// });

// console.log(app);

app.ports.fromExtensionExpand.send("@@hl");
