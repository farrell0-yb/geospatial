

DROP TABLE IF EXISTS my_mapdata;

CREATE TABLE my_mapdata
   (
   md_pk                 BIGINT NOT NULL,
   md_lat                TEXT,
   md_lng                TEXT,
   geo_hash10            TEXT,
   md_name               TEXT,
   md_address            TEXT,
   md_city               TEXT,
   md_province           TEXT,
   md_country            TEXT,
   md_postcode           TEXT,
   md_phone              TEXT,
   md_category           TEXT,
   md_subcategory        TEXT,
   md_mysource           TEXT,
   md_tags               TEXT,
   md_type               TEXT,
   PRIMARY KEY ((md_pk) HASH)
   );

CREATE INDEX ix_my_mapdata2
   ON my_mapdata (geo_hash10, md_name);


--  Best index for the speed = 80 use case,
--  because of how we built that data set.
--
CREATE INDEX IF NOT EXISTS ix_mapdata3
ON my_mapdata (left(geo_hash10, 5), md_name);


--  Best index for the walking use case
--
CREATE INDEX IF NOT EXISTS ix_mapdata4
ON my_mapdata (left(geo_hash10, 6), md_name);




