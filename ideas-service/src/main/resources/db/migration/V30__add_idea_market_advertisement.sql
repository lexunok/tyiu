CREATE TABLE IF NOT EXISTS idea_market_advertisement (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    idea_market_id TEXT REFERENCES idea_market (id) ON DELETE CASCADE,
    created_at TIMESTAMP,
    text TEXT NOT NULL,
    sender_id TEXT REFERENCES users (id),
    checked_by TEXT[]
);