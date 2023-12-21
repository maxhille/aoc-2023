importScripts("elm-worker.js");

const worker = Elm.Worker.init();

onmessage = (e) => { worker.ports.fromUi.send(e.data) };
worker.ports.toUi.subscribe( (data) => postMessage(data) );
