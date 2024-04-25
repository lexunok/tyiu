ALTER TABLE idea_market_refused ADD COLUMN idea_id TEXT REFERENCES idea (id) ON DELETE CASCADE;
ALTER TABLE team_market_request ADD COLUMN idea_id TEXT REFERENCES idea (id) ON DELETE CASCADE;

UPDATE idea_market_refused SET idea_id = (SELECT idea_id FROM idea_market WHERE id = idea_market_refused.idea_market_id);
UPDATE team_market_request SET idea_id = (SELECT idea_id FROM idea_market WHERE id = team_market_request.idea_market_id);

ALTER TABLE idea_market_refused DROP COLUMN idea_market_id;
ALTER TABLE team_market_request DROP COLUMN idea_market_id;

ALTER TABLE task ADD COLUMN double_work_hour DOUBLE PRECISION;

UPDATE task SET double_work_hour = work_hour;

ALTER TABLE task DROP COLUMN work_hour;

ALTER TABLE task RENAME COLUMN double_work_hour TO work_hour
