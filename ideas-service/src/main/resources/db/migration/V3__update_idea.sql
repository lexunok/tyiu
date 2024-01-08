ALTER TABLE idea
DROP COLUMN project_type,
DROP COLUMN technical_realizability,
ADD min_team_size SMALLINT,
ADD max_team_size SMALLINT;