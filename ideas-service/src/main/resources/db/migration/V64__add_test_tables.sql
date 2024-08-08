CREATE TABLE IF NOT EXISTS test (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    test_name TEXT NOT NULL,
    name TEXT NOT NULL,
    description TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS test_question (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    test_name TEXT NOT NULL,
    question_number INT NOT NULL,
    question_name TEXT NOT NULL,
    question_module_number INT,
    question_module TEXT,
    question TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS test_answer (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    test_name TEXT NOT NULL,
    user_id TEXT REFERENCES users (id),
    question_name TEXT NOT NULL,
    question_module_number INT,
    question_number INT NOT NULL,
    answer TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS test_result (
    id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::TEXT,
    user_id TEXT REFERENCES users (id),
    test_name TEXT NOT NULL,
    score INT[],
    test_result TEXT NOT NULL
);