DROP TABLE team_invitation;

CREATE TABLE IF NOT EXISTS team_invitation (
    id TEXT PRIMARY KEY,
    team_name TEXT NOT NULL,
    team_id TEXT REFERENCES team (id) ON DELETE CASCADE,
    receiverId TEXT NOT NULL,
    createdAt TIMESTAMP NOT NULL
);