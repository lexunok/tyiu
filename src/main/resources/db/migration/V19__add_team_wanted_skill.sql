CREATE TABLE IF NOT EXISTS team_wanted_skill (
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE,
    skill_id TEXT REFERENCES skill (id) ON DELETE CASCADE
);