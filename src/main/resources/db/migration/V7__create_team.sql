CREATE TABLE IF NOT EXISTS team_accessıon(
    id BIGSERIAL PRIMARY KEY,
    team_id BIGINT REFERENCES team (id),
    user_id BIGINT REFERENCES user (id),
    modified_at TIMESTAMP,
    text TEXT,
    request_type TEXT NOT NULL,
    accession_stage TEXT NOT NULL,
    target_registered BOOLEAN NOT NULL,
    target_email TEXT NOT NULL,
    inviter_user_id BIGINT REFERENCES team_member(id),
    team_dto_id REFERENCES team (id)
);
DROP TABLE team_invitation;