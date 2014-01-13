
function run() {
  var ws = new WebSocket('ws://localhost:8042');
  window.karlWS = ws;
  var ab = new ArrayBuffer(4);
  var abv = new Uint8Array(ab);
  for (var i = 0; i < 4; ++i) {
    abv[i] = i + 1;
  }
  ws.addEventListener('open', function() {
    ws.send(abv);
    ws.close();
  });
}

