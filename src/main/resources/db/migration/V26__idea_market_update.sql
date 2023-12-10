ALTER TABLE idea_market
ADD COLUMN team_id TEXT REFERENCES team (id);