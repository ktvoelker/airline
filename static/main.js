
require(['server'], function(server) {

  console.log("MAIN");
  var conn = new server.Connection(console.log.bind(console));
  conn.send({a: 3, b: 4});
  conn.send({c: 5, d: 6});

});

