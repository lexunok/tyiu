ALTER TABLE idea
ADD COLUMN initiator_email TEXT;

UPDATE idea
SET initiator_email = (SELECT email FROM users WHERE users.id = idea.initiator_id);

ALTER TABLE idea
DROP COLUMN initiator_id;

ALTER TABLE idea
ADD CONSTRAINT initiator_email_fk FOREIGN KEY (initiator_email) REFERENCES users (email);