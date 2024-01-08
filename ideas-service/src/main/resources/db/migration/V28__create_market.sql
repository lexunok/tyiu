CREATE TABLE IF NOT EXISTS team_refused (
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE,
    user_id TEXT REFERENCES users (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS market (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    name TEXT NOT NULL,
    start_date TIMESTAMP,
    finish_date TIMESTAMP,
    status TEXT
);

CREATE TABLE IF NOT EXISTS idea_market_refused (
    idea_market_id TEXT REFERENCES idea_market (id) ON DELETE CASCADE,
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE
);

ALTER TABLE idea_market
ADD COLUMN market_id TEXT REFERENCES market (id) ON DELETE CASCADE;