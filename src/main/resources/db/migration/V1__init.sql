CREATE TABLE IF NOT EXISTS users (
    id BIGSERIAL PRIMARY KEY,
    email TEXT NOT NULL UNIQUE,
    password TEXT NOT NULL,
    roles TEXT[] NOT NULL,
    first_name TEXT,
    last_name TEXT
);
CREATE TABLE IF NOT EXISTS idea (
    id BIGSERIAL PRIMARY KEY,
    initiator TEXT NOT NULL UNIQUE,
    name TEXT,
    group_expert_id BIGINT NOT NULL UNIQUE,
    group_project_office_id BIGINT NOT NULL UNIQUE,
    status TEXT,
    created_at TIMESTAMP,
    modified_at TIMESTAMP,
    project_type TEXT,
    problem TEXT,
    solution TEXT,
    result TEXT,
    customer TEXT,
    contact_person TEXT,
    description TEXT,
    suitability INT,
    budget INT,
    technical_realizability INT,
    pre_assessment REAL,
    rating REAL
);
CREATE TABLE IF NOT EXISTS groups (
     id BIGSERIAL PRIMARY KEY,
     name TEXT NOT NULL,
     users_id BIGINT[],
     roles TEXT[] NOT NULL
);
