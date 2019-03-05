CREATE TABLE users (
  email TEXT,
  username TEXT PRIMARY KEY,
  passhash TEXT,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE replays (
  id SERIAL PRIMARY KEY,
  replay TEXT,
  player_a__username TEXT REFERENCES users,
  player_b__username TEXT REFERENCES users,
  created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);
