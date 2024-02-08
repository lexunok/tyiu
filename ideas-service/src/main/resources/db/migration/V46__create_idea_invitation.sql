CREATE TABLE IF NOT EXISTS idea_invitation (
    id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY,
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE,
    idea_id TEXT REFERENCES idea (id) ON DELETE CASCADE,
    status TEXT NOT NULL
);
