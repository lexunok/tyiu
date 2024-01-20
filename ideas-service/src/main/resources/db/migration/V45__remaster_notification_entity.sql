ALTER TABLE users_telegram
DROP COLUMN user_tag,
ADD COLUMN user_tag TEXT UNIQUE,
DROP COLUMN is_visible,
ADD COLUMN is_visible BOOLEAN;

ALTER TABLE notification
DROP COLUMN publisher_email,
DROP COLUMN consumer_email,
DROP COLUMN consumer_tag,
ADD COLUMN publisher_email TEXT REFERENCES users (email) ON UPDATE CASCADE,
ADD COLUMN consumer_email TEXT REFERENCES users (email) ON UPDATE CASCADE,
ADD COLUMN consumer_tag TEXT REFERENCES users_telegram (user_tag) ON UPDATE CASCADE;