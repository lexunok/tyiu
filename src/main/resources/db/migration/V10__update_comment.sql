ALTER TABLE comment DROP COLUMN checked_by;
ALTER TABLE comment ADD COLUMN checked_by TEXT[];