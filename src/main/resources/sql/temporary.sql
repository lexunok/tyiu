CREATE TABLE IF NOT EXISTS temporary (
                                     id BIGSERIAL PRIMARY KEY,
                                     url TEXT NOT NULL,
                                     dateExpired TIMESTAMP NOT NULL,
                                     email TEXT NOT NULL,
                                     newEmail TEXT,
                                     oldEmail TEXT,
                                     code BIGSERIAL NOT NULL,
                                     roles TEXT[] NOT NULL
);