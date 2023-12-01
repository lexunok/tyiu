ALTER TABLE team
DROP COLUMN desired_skills;

CREATE TABLE IF NOT EXISTS team_invitation (
    id TEXT PRIMARY KEY,
    team_name TEXT NOT NULL,
    team_id TEXT REFERENCES team (id),
    receiverId TEXT NOT NULL,
    createdAt TIMESTAMP NOT NULL
);