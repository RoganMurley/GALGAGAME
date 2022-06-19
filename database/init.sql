CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email TEXT,
  username TEXT UNIQUE,
  passhash TEXT,
  superuser BOOLEAN NOT NULL DEFAULT FALSE,
  contactable BOOLEAN NOT NULL DEFAULT FALSE,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE stats (
  user__id INTEGER REFERENCES users UNIQUE,
  experience BIGINT DEFAULT 0,
  progress TEXT,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);
CREATE INDEX stats_user__id ON stats(user__id);
CREATE INDEX experience ON stats(experience DESC);

CREATE TABLE statsguest (
  cid TEXT PRIMARY KEY,
  experience BIGINT DEFAULT 0,
  progress TEXT,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE replays (
  id SERIAL PRIMARY KEY,
  replay TEXT,
  player_a__id INTEGER REFERENCES users,
  player_b__id INTEGER REFERENCES users,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  display_username_a TEXT,
  display_username_b TEXT
);
CREATE INDEX replays_player_a__id ON replays(player_a__id);
CREATE INDEX replays_player_b__id ON replays(player_b__id);
CREATE INDEX replays_created ON replays(created DESC);

CREATE TABLE feedback (
  id SERIAL PRIMARY KEY,
  body TEXT,
  user__id INTEGER REFERENCES users,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE league (
  id SERIAL PRIMARY KEY,
  user__id INTEGER NOT NULL REFERENCES users,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (user__username)
);
