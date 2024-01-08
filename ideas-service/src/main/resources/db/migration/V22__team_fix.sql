DROP TABLE team_invitation;

CREATE TABLE IF NOT EXISTS team_invitation (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    status TEXT,
    email TEXT,
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE,
    user_id TEXT REFERENCES users (id) ON DELETE CASCADE
);