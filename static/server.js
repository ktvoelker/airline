
define(['classy'], function(Class) {

  const SERVER_URL = 'ws://localhost:8042';

  var encode = JSON.stringify;

  var decode = JSON.parse;

  var Connection = Class.$extend({
    
    __init__: function(handler) {
      this._handler = handler;
      this._open();
      this._outbox = [];
    },

    _open: function() {
      this._ws = new WebSocket(SERVER_URL);
      var that = this;
      this._ws.addEventListener('open', function() {
        that._outbox.forEach(function(msg) {
          that._ws.send(msg);
        });
        that._outbox = [];
      });
      var handler = this._handler;
      this._ws.addEventListener('message', function(evt) {
        handler(decode(evt.data));
      });
      this._ws.addEventListener('close', function() {
        console.error("Connection closed!");
      });
    },

    send: function(obj) {
      var msg = encode(obj);
      if (this._ws.readyState === WebSocket.OPEN) {
        this._ws.send(msg);
      } else {
        this._outbox.push(msg);
      }
    }

  });

  return {
    encode: encode,
    decode: decode,
    Connection: Connection
  };

});

