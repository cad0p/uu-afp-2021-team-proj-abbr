const worker = require("./dist/VsCodeWorker.js")

console.log(worker);

const app = worker.Elm.VsCodeWorker.init({});
