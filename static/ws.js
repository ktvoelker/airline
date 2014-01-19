
define(['classy'], function(classy) {

  var next = 0;

  function start() {
    var index = next;
    next += 1;
    var ws = new WebSocket('ws://localhost:8042');
    ws.addEventListener('message', function(evt) {
      console.log(index);
      console.log(JSON.parse(evt.data));
    });
    ws.addEventListener('open', function() {
      console.log("" + index + " OPEN");
    });
    return ws;
  }

  function send(ws) {
    ws.send(JSON.stringify({key: "Hello, world!"}));
  }

  return {
    start: start,
    send: send
  };

});

