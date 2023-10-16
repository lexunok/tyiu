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
    roles TEXT[] NOT NULL
);
CREATE TABLE IF NOT EXISTS temporary (
    id BIGSERIAL PRIMARY KEY,
    url TEXT,
    date_expired TIMESTAMP,
    email TEXT,
    new_email TEXT,
    old_email TEXT,
    code INT,
    roles TEXT[]
);
CREATE TABLE IF NOT EXISTS group_user (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    group_id BIGINT NOT NULL
);
CREATE TABLE IF NOT EXISTS comment (
    id BIGSERIAL PRIMARY KEY,
    text TEXT NOT NULL,
    senderEmail TEXT NOT NULL,
    createdAt TIMESTAMP,
    ideaId BIGINT NOT NULL UNIQUE
);
