ALTER TABLE notification
DROP COLUMN user_id,
DROP COLUMN is_readed,
ADD COLUMN is_read TEXT,
ADD COLUMN publisher_email TEXT,
ADD COLUMN consumer_email TEXT,
ADD COLUMN consumer_tag TEXT,
ADD COLUMN notification_type TEXT;