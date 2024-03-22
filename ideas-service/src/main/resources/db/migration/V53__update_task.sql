ALTER TABLE task
ADD COLUMN position INT,
ADD COLUMN leader_comment TEXT,
ALTER COLUMN work_hour TYPE INT;