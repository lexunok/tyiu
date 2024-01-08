ALTER TABLE users
ADD COLUMN created_at TIMESTAMP;

UPDATE users
SET created_at = '2023-12-11 12:34:56';

