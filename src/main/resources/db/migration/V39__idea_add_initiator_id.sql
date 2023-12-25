ALTER TABLE idea
ADD COLUMN initiator_id TEXT REFERENCES users (id) ON DELETE CASCADE;

INSERT INTO idea (initiator_id)
SELECT u.id
FROM users u
JOIN idea i on u.email = i.initiator_email;

ALTER TABLE idea
DROP COLUMN initiator_email;