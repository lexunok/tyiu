ALTER TABLE team ADD COLUMN market_id TEXT REFERENCES market (id);