const worker = require("./dist/VsCodeWorker.js");

console.log(worker);

const app = worker.Elm.VsCodeWorker.init({});
console.log(app);

app.ports.toExtension.subscribe(function (msg) {
  console.log("got from port", msg);
});
app.ports.toShortHndr.subscribe(function (msg) {
  console.log("will call shorthndr with", msg);
});

console.log(app);

function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

async function init() {
  while (true) {
    console.log(1);
    console.log("Sending Ping ...");
    app.ports.fromExtension.send("Ping");
    await sleep(1000);
    console.log(2);
    console.log("Sending replace ...");
    app.ports.fromExtensionExtend.send("@@hl");
  }
}

init();
