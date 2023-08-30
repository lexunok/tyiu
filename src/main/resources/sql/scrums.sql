CREATE TABLE IF NOT EXISTS scrums (
     id BIGSERIAL PRIMARY KEY,
     name TEXT NOT NULL,
     description TEXT NOT NULL,
     count BIGSERIAL NOT NULL,
     profiles TEXT[] NOT NULL,
     tasks TEXT[] NOT NULL
);