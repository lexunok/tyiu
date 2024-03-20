DROP TABLE task_tag CASCADE;
DROP TABLE task_to_tag;

CREATE TABLE IF NOT EXISTS tag (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    name TEXT,
    color TEXT,
    confirmed BOOLEAN,
    creator_id TEXT REFERENCES users (id),
    updater_id TEXT REFERENCES users (id),
    deleter_id TEXT REFERENCES users (id)
);

CREATE TABLE IF NOT EXISTS task_tag (
    task_id TEXT REFERENCES task (id) ON DELETE CASCADE,
    tag_id TEXT REFERENCES tag (id) ON DELETE CASCADE
);