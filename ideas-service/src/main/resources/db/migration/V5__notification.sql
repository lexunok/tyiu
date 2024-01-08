CREATE TABLE IF NOT EXISTS notification (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT REFERENCES users (id) ON DELETE CASCADE,
    title TEXT NOT NULL,
    message TEXT NOT NULL,
    is_showed BOOLEAN NOT NULL,
    is_readed BOOLEAN NOT NULL,
    is_favourite BOOLEAN NOT NULL,
    created_at TIMESTAMP
);