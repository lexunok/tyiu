CREATE TABLE IF NOT EXISTS user_idea (
    user_id BIGINT REFERENCES users (id) ON DELETE CASCADE,
    idea_id BIGINT REFERENCES idea (id) ON DELETE CASCADE
);