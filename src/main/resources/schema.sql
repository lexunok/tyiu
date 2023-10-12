CREATE TABLE IF NOT EXISTS users (
    id BIGSERIAL PRIMARY KEY,
    email TEXT NOT NULL UNIQUE,
    password TEXT NOT NULL,
    roles TEXT[] NOT NULL,
    first_name TEXT,
    last_name TEXT
);

CREATE TABLE IF NOT EXISTS groups (
     id BIGSERIAL PRIMARY KEY,
     name TEXT NOT NULL,
     users_id BIGINT[],
     roles TEXT[] NOT NULL
);
