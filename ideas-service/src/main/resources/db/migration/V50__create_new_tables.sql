DROP TABLE team_skill;

CREATE TABLE IF NOT EXISTS project (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    idea_id TEXT REFERENCES idea (id),
    team_id TEXT REFERENCES team (id),
    report TEXT,
    start_date TIMESTAMP,
    finish_date TIMESTAMP,
    status TEXT
);

CREATE TABLE IF NOT EXISTS project_marks (
    project_id TEXT REFERENCES project (id),
    user_id TEXT REFERENCES users (id),
    mark DOUBLE PRECISION
);

CREATE TABLE IF NOT EXISTS project_member (
    project_id TEXT REFERENCES project (id),
    user_id TEXT REFERENCES users (id),
    team_id TEXT REFERENCES team (id),
    project_role TEXT,
    start_date TIMESTAMP,
    finish_date TIMESTAMP
);

CREATE TABLE IF NOT EXISTS sprint (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    project_id TEXT REFERENCES project (id),
    name TEXT NOT NULL,
    goal TEXT,
    report TEXT,
    start_date TIMESTAMP,
    finish_date TIMESTAMP,
    working_hours BIGINT,
    status TEXT
);

CREATE TABLE IF NOT EXISTS sprint_marks (
    project_id TEXT REFERENCES project (id),
    sprint_id TEXT REFERENCES sprint (id),
    user_id TEXT REFERENCES users (id),
    mark DOUBLE PRECISION
);

CREATE TABLE IF NOT EXISTS task (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    sprint_id TEXT REFERENCES sprint (id),
    project_id TEXT REFERENCES project (id),
    name TEXT NOT NULL,
    description TEXT,
    initiator_id TEXT REFERENCES users (id),
    executor_id TEXT REFERENCES users (id),
    work_hour BIGINT,
    start_date TIMESTAMP,
    finish_date TIMESTAMP,
    status TEXT
);

CREATE TABLE IF NOT EXISTS task_movement_log (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    project_id TEXT REFERENCES project (id),
    task_id TEXT REFERENCES task (id),
    executor_id TEXT REFERENCES users (id),
    user_id TEXT REFERENCES users (id),
    start_date TIMESTAMP,
    end_date TIMESTAMP,
    status TEXT
);

CREATE TABLE IF NOT EXISTS task_tag (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    project_id TEXT REFERENCES project (id),
    name TEXT NOT NULL,
    color TEXT
);

CREATE TABLE IF NOT EXISTS team_member (
    team_id TEXT REFERENCES team (id),
    user_id TEXT REFERENCES users (id),
    team_role TEXT,
    start_date TIMESTAMP,
    finish_date TIMESTAMP
);