CREATE TABLE IF NOT EXISTS chats (
     id BIGSERIAL PRIMARY KEY,
     messages TEXT[] NOT NULL,
     members TEXT[] NOT NULL
);