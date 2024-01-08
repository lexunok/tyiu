ALTER TABLE comment
ADD COLUMN sender_id TEXT REFERENCES users (id) ON DELETE CASCADE;

UPDATE comment
SET sender_id = (
    SELECT u.id
    FROM users u
    WHERE u.email = comment.sender_email
);

ALTER TABLE comment
DROP COLUMN sender_email;

UPDATE comment
SET checked_by = ARRAY(
    SELECT id
    FROM users u
    WHERE u.email = ANY(comment.checked_by)
);
