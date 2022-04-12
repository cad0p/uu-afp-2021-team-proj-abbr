const worker = require("./dist/ExpandWorker.js");
const { ShortHndrCli } = require("./wiring/ShortHndrCli");

console.log(worker);
console.log(ShortHndrCli);

const app = worker.Elm.ExpandWorker.init({
  flags: { kbPath: "data/lb_example.csv" },
});
console.log(app);

app.ports.toExtension.subscribe(function (msg) {
  console.log("got from port", msg);
});
app.ports.toShortHndr.subscribe(function (msg) {
  ShortHndrCli.call(
    msg,
    [],
    (error) => console.error(`Error: ${error}`),
    (ok) => console.log(ok)
  );
});

console.log(app);

app.ports.fromExtensionExpand.send("@@hl");

// function sleep(ms) {
//   return new Promise((resolve) => {
//     setTimeout(resolve, ms);
//   });
// }

// async function init() {
//   while (true) {
//     console.log(1);
//     console.log("Sending Ping ...");
//     app.ports.fromExtension.send("Ping");
//     await sleep(1000);
//     console.log(2);
//     console.log("Sending replace ...");
//     app.ports.fromExtensionExtend.send("@@hl");
//   }
// }

// init();
