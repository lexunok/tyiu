DROP TABLE team_invitation;

CREATE TABLE IF NOT EXISTS team_invitation (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    status TEXT,
    team_id TEXT REFERENCES team (id),
    user_id TEXT REFERENCES user (id),
);

