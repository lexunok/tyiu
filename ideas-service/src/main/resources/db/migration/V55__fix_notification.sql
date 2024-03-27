ALTER TABLE notification
DROP COLUMN notification_type,
DROP COLUMN consumer_tag;

ALTER TABLE notification
ADD COLUMN button_name TEXT;
