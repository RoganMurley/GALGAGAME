'use strict';

var params = new URLSearchParams(window.location.search);
var embedElement = document.getElementById('elm');
var hostname = window.location.hostname;
var httpPort = window.location.port;
var portProtocol = httpPort ?  ":" + httpPort : "";
var app = Elm.Main.init({node: embedElement, flags: {
  hostname: hostname,
  httpPort: httpPort,
  seed: new Date().getTime(),
  dimensions: [ window.innerWidth, window.innerHeight ],
  time: 0,
  username: null,
}});

app.ports.selectAllInput.subscribe(function (elementId) {
  document.getElementById(elementId).select();
});

app.ports.copyInput.subscribe(function (elementId) {
  document.getElementById(elementId).select();
  document.execCommand('copy');
})

var playedSounds = {};
app.ports.playAudio.subscribe(function (input) {
  var src = input[0];
  var loop = input[1];
  var once = input[2];
  var volume = input[3];

  if (once) {
    if (playedSounds[input]) {
      return;
    }
    playedSounds[input] = true;
  }
  var sound = new Howl({
    src: [src],
    loop: loop,
    volume: volume
  });
  sound.play();
});

app.ports.loadAudio.subscribe(function (src) {
  new Howl({src: [src]});
});

app.ports.volume.subscribe(function (input) {
  var v = input / 100;
  Howler.volume(Math.pow(v, 4));
});

app.ports.log.subscribe(function (input) {
  console.log(input);
});

app.ports.reload.subscribe(function () {
  location.reload();
});

app.ports.analytics.subscribe(function () {
  if (typeof ga !== 'undefined') {
    ga('set', 'page', location.pathname);
    ga('send', 'pageview');
  }
});

function handleClick (e) {
  app.ports.click.send({
    x: Math.floor(e.clientX),
    y: Math.floor(e.clientY),
  });
};

var touched = false;

function handleMouseMove (e) {
  if (touched) {
    return;
  }
  app.ports.touch.send({
    x: Math.floor(e.clientX),
    y: Math.floor(e.clientY),
  });
}

function handleTouch (e) {
  touched = true;
  var touch = e.touches[0];
  app.ports.touch.send({
    x: Math.floor(touch.clientX),
    y: Math.floor(touch.clientY),
  });
};

function handleTouchEnd (e) {
  touched = true;
  app.ports.touch.send(null);
};

document.body.addEventListener('click', handleClick, false);
document.body.addEventListener('mousemove', handleMouseMove, false);
document.body.addEventListener('touchstart', handleTouch, false);
document.body.addEventListener('touchmove', handleTouch, false);
document.body.addEventListener('touchend', handleTouchEnd, false);

window.god = app.ports.godModeCommand.send;

window.requestFullscreen = function () {
  var element = document.body;
  if (element.requestFullscreen) {
    element.requestFullscreen();
  } else if (element.webkitRequestFullscreen) {
    element.webkitRequestFullscreen();
  } else if (element.mozRequestFullScreen) {
    element.mozRequestFullScreen();
  } else if (element.msRequestFullscreen) {
    element.msRequestFullscreen();
  }
}

// Websockets
var websocketMessageQueue = [];

function createSocket() {
  var socket = new WebSocket("wss://" + hostname + portProtocol + "/game/");

  socket.addEventListener('open', function (event) {
    websocketMessageQueue.forEach(function (input) {socket.send(input)});
    websocketMessageQueue = [];
  });

  socket.addEventListener('message', function (event) {
    app.ports.websocketListen.send(event.data);
  });

  return socket
}

var socket = createSocket();

app.ports.websocketSend.subscribe(function (input) {
  if (socket.readyState === WebSocket.OPEN) {
    socket.send(input);
  } else {
    websocketMessageQueue.push(input);
  }
  if (socket.readyState === WebSocket.CLOSED) {
    socket = createSocket();
  }
});

app.ports.websocketReconnect.subscribe(function () {
  socket.close(1000, "Reconnecting");
});
