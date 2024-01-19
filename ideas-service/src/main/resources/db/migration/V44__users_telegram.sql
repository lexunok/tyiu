CREATE TABLE IF NOT EXISTS users_telegram (
    user_email TEXT REFERENCES users (email) ON UPDATE CASCADE,
    user_tag TEXT,
    chat_id BIGINT,
    is_visible BOOLEAN DEFAULT false::BOOLEAN
);