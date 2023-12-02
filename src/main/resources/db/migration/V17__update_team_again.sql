ALTER TABLE team_invitation
DROP COLUMN team_name,
DROP COLUMN createdAt,
DROP COLUMN receiverId TEXT,
ADD COLUMN first_name TEXT NOT NULL,
ADD COLUMN last_name TEXT NOT NULL,
ADD COLUMN request_status TEXT,
ADD COLUMN user_id TEXT;
