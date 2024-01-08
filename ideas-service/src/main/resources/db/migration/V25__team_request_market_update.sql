DROP TABLE team_market_request;

CREATE TABLE IF NOT EXISTS team_market_request (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    idea_market_id TEXT REFERENCES idea_market (id) ON DELETE CASCADE,
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE,
    name TEXT NOT NULL,
    status TEXT NOT NULL,
    letter TEXT
);

ALTER TABLE idea_market
DROP COLUMN requests,
DROP COLUMN accepted_requests;