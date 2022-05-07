ALTER TABLE replays ADD COLUMN display_username_a TEXT;
ALTER TABLE replays ADD COLUMN display_username_b TEXT;

UPDATE replays SET display_username_a=json_extract_path_text(cast(replay as json), 'pa');
UPDATE replays SET display_username_b=json_extract_path_text(cast(replay as json), 'pb');
