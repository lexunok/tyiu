DROP TABLE user_idea;
CREATE TABLE IF NOT EXISTS user_skill (
    user_id BIGINT REFERENCES users (id) ON DELETE CASCADE,
    skill_id BIGINT REFERENCES skill (id) ON DELETE CASCADE
);