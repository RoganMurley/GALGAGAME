'use strict';

var params = new URLSearchParams(window.location.search);
var hostname = window.location.hostname;
var httpPort = window.location.port;
var portProtocol = httpPort ? ":" + httpPort : "";

var initialVolume = localStorage.getItem("volume");
if (initialVolume === null) {
  initialVolume = 100;
}
setVolume(initialVolume);


var initialMusicVolume = localStorage.getItem("musicVolume");
if (initialMusicVolume === null) {
  initialMusicVolume = 100;
}
var musicVolume;
setMusicVolume(initialMusicVolume);

var initialScaling = localStorage.getItem("scaling");
if (initialScaling === null) {
  initialScaling = 1;
}

var visits = localStorage.getItem("visits");
if (visits === null) {
  visits = 1;
} else {
  visits++;
}
localStorage.setItem('visits', visits);

var cookies = document.cookie.split('; ');
var userCookie = cookies.find(function (row) { return row.startsWith('user='); });
var username = userCookie ? userCookie.split('=')[1] : null;

var app = Elm.Main.init({
  flags: {
    hostname: hostname,
    httpPort: httpPort,
    seed: new Date().getTime(),
    dimensions: [window.innerWidth, window.innerHeight],
    time: 0,
    username: username,
    pixelRatio: window.devicePixelRatio,
    initialMusicVolume: parseFloat(musicVolume, 10),
    initialVolume: parseFloat(initialVolume, 10),
    initialScaling: parseFloat(initialScaling, 10),
    visits: visits,
  },
});

app.ports.selectAllInput.subscribe(function (elementId) {
  document.getElementById(elementId).select();
});

app.ports.copyInput.subscribe(function (elementId) {
  document.getElementById(elementId).select();
  document.execCommand('copy');
})

var playedSounds = {};
var music = [];
app.ports.playAudio.subscribe(function (input) {
  var src = input.name;
  var loop = input.loop;
  var once = input.once;
  var volume = input.vol;

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
  if (loop) {
    music.push(sound);
    setMusicVolume(musicVolume);
  }
  sound.play();
});

app.ports.loadAudio.subscribe(function (src) {
  new Howl({ src: [src] });
});

function setVolume(v) {
  Howler.volume(v / 100);
};

app.ports.volume.subscribe(function (input) {
  setVolume(input);
  localStorage.setItem('volume', input);
});

function setMusicVolume(v) {
  musicVolume = v;
  if (!music) {
    return;
  }
  music.forEach(function (howl) {
    howl.volume(musicVolume / 100);
  });
};

app.ports.musicVolume.subscribe(function (input) {
  setMusicVolume(input);
  localStorage.setItem('musicVolume', input);
});

app.ports.scaling.subscribe(function (input) {
  localStorage.setItem('scaling', input);
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

app.ports.setTitle.subscribe(function (input) {
  document.title = input;
});

var touched = false;

function handleMouseDown(e) {
  if (touched) {
    return;
  }
  app.ports.mouseDown.send({
    x: Math.floor(e.clientX),
    y: Math.floor(e.clientY),
  });
};

function handleMouseUp(e) {
  if (touched) {
    return;
  }
  app.ports.mouseUp.send({
    x: Math.floor(e.clientX),
    y: Math.floor(e.clientY),
  });
};

function handleMouseMove(e) {
  if (touched) {
    return;
  }
  app.ports.mouseMove.send({
    x: Math.floor(e.clientX),
    y: Math.floor(e.clientY),
  });
}

function handleTouch(e) {
  touched = true;
  var touch = e.touches[0];
  app.ports.touch.send({
    x: Math.floor(touch.clientX),
    y: Math.floor(touch.clientY),
  });
};

function handleTouchEnd(e) {
  touched = true;
  app.ports.touch.send(null);
};

// iOS doesn't handle click events on the body so get the elm element.
document.body.firstChild.addEventListener('mousedown', handleMouseDown, false);
document.body.firstChild.addEventListener('mouseup', handleMouseUp, false);

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
  var socket = new WebSocket("wss://" + hostname + portProtocol + "/game");

  socket.addEventListener('open', function (event) {
    websocketMessageQueue.forEach(function (input) { socket.send(input) });
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
    return;
  }
  if (socket.readyState === WebSocket.CLOSED) {
    socket = createSocket();
  }
  websocketMessageQueue.push(input);
});

app.ports.websocketReconnect.subscribe(function () {
  socket.close(1000, "Reconnecting");
});

window.onbeforeunload = function () {
  socket.close()
};
