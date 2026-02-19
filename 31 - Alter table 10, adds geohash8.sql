-- ============================================================================
--
-- Purpose  : Add geo_hash8 column, backfill from geo_hash10 (first 8 chars),
--            and create an index for equality lookups on geo_hash8.
-- Table    : my_mapdata
--
-- ============================================================================

ALTER TABLE my_mapdata
   ADD COLUMN IF NOT EXISTS geo_hash8 TEXT;

UPDATE my_mapdata
SET
   geo_hash8 = LEFT(geo_hash10, 8)
WHERE
   geo_hash10 IS NOT NULL
   AND (geo_hash8 IS NULL OR geo_hash8 <> LEFT(geo_hash10, 8));

CREATE INDEX IF NOT EXISTS my_mapdata_geo_hash8_idx
ON my_mapdata (geo_hash8);


ALTER TABLE my_mapdata
   ADD COLUMN IF NOT EXISTS geo_hash5 TEXT;

UPDATE my_mapdata
SET
   geo_hash5 = LEFT(geo_hash10, 5)
WHERE
   geo_hash10 IS NOT NULL
   AND (geo_hash5 IS NULL OR geo_hash5 <> LEFT(geo_hash10, 5));

CREATE INDEX IF NOT EXISTS my_mapdata_geo_hash5_idx
ON my_mapdata (geo_hash5);


ANALYZE my_mapdata;


