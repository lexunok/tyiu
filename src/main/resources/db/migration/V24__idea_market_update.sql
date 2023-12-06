ALTER TABLE idea_market
DROP COLUMN position,
ADD COLUMN problem TEXT,
ADD COLUMN solution TEXT,
ADD COLUMN result TEXT,
ADD COLUMN customer TEXT;
