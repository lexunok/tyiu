CREATE TABLE IF NOT EXISTS tasks (
                                      id BIGSERIAL PRIMARY KEY,
                                      title TEXT NOT NULL,
                                      description TEXT NOT NULL,
                                      assignedTo TEXT NOT NULL,
                                      priority TEXT NOT NULL,
                                      deadline TEXT NOT NULL,
                                      status TEXT NOT NULL,
                                      createdAt TIMESTAMP NOT NULL,
                                      scrum TEXT NOT NULL,
                                      profile TEXT NOT NULL
);