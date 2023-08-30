CREATE TABLE IF NOT EXISTS notifications (
                id BIGSERIAL PRIMARY KEY,
                message TEXT NOT NULL,
                date TIMESTAMP NOT NULL,
                receiver BIGSERIAL NOT NULL
);