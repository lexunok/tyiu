ALTER TABLE idea_market
RENAME COLUMN initiator TO initiator_id;

ALTER TABLE team_market_request
RENAME COLUMN updated_at TO created_at;
