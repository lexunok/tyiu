DROP TABLE sprint_marks;

CREATE TABLE IF NOT EXISTS sprint_mark (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    sprint_id TEXT REFERENCES sprint (id) ON DELETE CASCADE,
    user_id TEXT REFERENCES users (id),
    project_role TEXT,
    mark DOUBLE PRECISION
);

CREATE TABLE IF NOT EXISTS sprint_mark_task (
    sprint_mark_id TEXT REFERENCES sprint_mark (id) ON DELETE CASCADE,
    task_id TEXT REFERENCES task (id) ON DELETE CASCADE
);