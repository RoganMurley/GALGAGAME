CREATE INDEX replays_player_a__id ON replays(player_a__id);
CREATE INDEX replays_player_b__id ON replays(player_b__id);
CREATE INDEX replays_created ON replays(created DESC);

CREATE INDEX stats_user__id ON stats(user__id);
