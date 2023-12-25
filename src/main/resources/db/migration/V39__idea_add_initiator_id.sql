ALTER TABLE idea
ADD COLUMN initiator_id TEXT REFERENCES users (id) ON DELETE CASCADE;

UPDATE idea
SET initiator_id = (
    SELECT u.id
    FROM users u
    WHERE u.email = idea.initiator_email
);

ALTER TABLE idea
DROP COLUMN initiator_email;