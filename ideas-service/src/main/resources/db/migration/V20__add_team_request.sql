CREATE TABLE IF NOT EXISTS team_request (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE,
    user_id TEXT REFERENCES users (id) ON DELETE CASCADE,
    email TEXT NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    status TEXT
);
