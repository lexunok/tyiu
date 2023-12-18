CREATE TABLE IF NOT EXISTS invitation (
    id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY,
    date_expired TIMESTAMP,
    email TEXT,
    roles TEXT[]
);

INSERT INTO invitation (id, date_expired, email, roles)
SELECT url, date_expired, email, roles
FROM temporary
WHERE roles IS NOT NULL;
DELETE FROM temporary WHERE roles IS NOT NULL;

CREATE TABLE IF NOT EXISTS change_email_data (
    id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY,
    new_email TEXT,
    old_email TEXT,
    code INT,
    wrong_tries INT DEFAULT 0::INT,
    date_expired TIMESTAMP
);
INSERT INTO change_email_data (id, new_email, old_email, code, wrong_tries ,date_expired)
SELECT url, new_email, old_email, code, 0, date_expired FROM temporary WHERE old_email IS NOT NULL;
DELETE FROM temporary WHERE old_email IS NOT NULL;

CREATE TABLE IF NOT EXISTS change_password_data (
    id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY,
    email TEXT,
    code INT,
    wrong_tries INT DEFAULT 0::INT,
    date_expired TIMESTAMP
);

INSERT INTO change_password_data (id, email, code, wrong_tries, date_expired)
SELECT url, email, code, 0, date_expired FROM temporary;

DROP TABLE temporary;