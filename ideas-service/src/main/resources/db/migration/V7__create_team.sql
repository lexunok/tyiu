CREATE TABLE IF NOT EXISTS team_accession (
    id BIGSERIAL PRIMARY KEY,
    team_id BIGINT REFERENCES team (id),
    user_id BIGINT REFERENCES users (id),
    modified_at TIMESTAMP,
    text TEXT,
    request_type TEXT NOT NULL,
    accession_stage TEXT NOT NULL,
    target_registered BOOLEAN NOT NULL,
    target_email TEXT NOT NULL
);
DROP TABLE team_invitation;