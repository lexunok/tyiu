ALTER TABLE notification
DROP COLUMN is_read,
ADD COLUMN is_read BOOLEAN,
ADD COLUMN is_sent_by_email_service BOOLEAN DEFAULT FALSE,
ADD COLUMN is_sent_by_telegram_service BOOLEAN DEFAULT FALSE;