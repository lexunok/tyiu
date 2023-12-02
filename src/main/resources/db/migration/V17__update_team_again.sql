ALTER TABLE team_invitation
DROP COLUMN team_name,
DROP COLUMN createdAt,
ADD COLUMN first_name TEXT NOT NULL,
ADD COLUMN last_name TEXT NOT NULL,
ADD COLUMN request_status TEXT[],
DROP COLUMN receiverId TEXT,
ADD COLUMN user_id TEXT;
