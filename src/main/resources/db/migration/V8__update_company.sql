ALTER TABLE company
ADD owner_id BIGINT REFERENCES users (id);