CREATE TABLE IF NOT EXISTS task_history (
    task_id TEXT REFERENCES task (id) ON DELETE CASCADE,
    sprint_id TEXT REFERENCES sprint (id) ON DELETE CASCADE,
    status TEXT,
    executor_id TEXT REFERENCES users (id)
    );