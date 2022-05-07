-- Add ID columns to all tables
ALTER TABLE stats ADD COLUMN user__id INTEGER;
ALTER TABLE feedback ADD COLUMN user__id INTEGER;
ALTER TABLE league ADD COLUMN user__id INTEGER;
ALTER TABLE replays ADD COLUMN player_a__id INTEGER;
ALTER TABLE replays ADD COLUMN player_b__id INTEGER;

-- Hydrate with IDs
UPDATE stats SET user__id = users.id FROM users WHERE users.username = stats.user__username;
UPDATE feedback SET user__id = users.id FROM users WHERE users.username = feedback.user__username;
UPDATE league SET user__id = users.id FROM users WHERE users.username = league.user__username;
UPDATE replays SET player_a__id = users.id FROM users WHERE users.username = replays.player_a__username;
UPDATE replays SET player_b__id = users.id FROM users WHERE users.username = replays.player_b__username;

-- Drop unique constraints
ALTER TABLE stats DROP CONSTRAINT stats_user__username_key;
--ALTER TABLE feedback DROP CONSTRAINT feedback_user__username_key;
ALTER TABLE league DROP CONSTRAINT league_user__username_key;
--ALTER TABLE replays DROP CONSTRAINT replay_player_a__id_key;
--ALTER TABLE replays DROP CONSTRAINT replay_player_b__id_key;

-- Drop foreign keys.
ALTER TABLE stats DROP CONSTRAINT stats_user__username_fkey;
ALTER TABLE feedback DROP CONSTRAINT feedback_user__username_fkey;
ALTER TABLE league DROP CONSTRAINT league_user__username_fkey;
ALTER TABLE replays DROP CONSTRAINT replays_player_a__username_fkey;
ALTER TABLE replays DROP CONSTRAINT replays_player_b__username_fkey;

-- Change users PK
ALTER TABLE users DROP CONSTRAINT users_pkey;
ALTER TABLE users ADD PRIMARY KEY (id);

-- Add foreign key constraints
ALTER TABLE stats ADD CONSTRAINT stats_user__id_fkey FOREIGN KEY (user__id) REFERENCES users (id);
ALTER TABLE feedback ADD CONSTRAINT feedback_user__id_fkey FOREIGN KEY (user__id) REFERENCES users (id);
ALTER TABLE league ADD CONSTRAINT league_user__id_fkey FOREIGN KEY (user__id) REFERENCES users (id);
ALTER TABLE replays ADD CONSTRAINT replays_player_a__id_fkey FOREIGN KEY (player_a__id) REFERENCES users (id);
ALTER TABLE replays ADD CONSTRAINT replays_player_b__id_fkey FOREIGN KEY (player_b__id) REFERENCES users (id);

-- Drop username columns
ALTER TABLE stats DROP COLUMN user__username;
ALTER TABLE feedback DROP COLUMN user__username;
ALTER TABLE league DROP COLUMN user__username;
ALTER TABLE replays DROP COLUMN player_a__username;
ALTER TABLE replays DROP COLUMN player_b__username;

-- Add unique and not null constraints
ALTER TABLE users ADD CONSTRAINT users_username_unique UNIQUE (username);
ALTER TABLE stats ADD CONSTRAINT stats_user__id_unique UNIQUE (user__id);
ALTER TABLE league ALTER COLUMN user__id SET NOT NULL;
