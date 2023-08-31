CREATE TABLE IF NOT EXISTS comments (
    id BIGSERIAL PRIMARY KEY,
    messages TEXT NOT NULL,
    sender TEXt NOT NULL,
    checkedBy TEXT[] NOT NULL,
    dateCreated TIMESTAMP NOT NULL,
    idea_id BIGSERIAL NOT NULL
);