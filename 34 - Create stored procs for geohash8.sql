

-- ============================================================================
--
-- Purpose  : Polygon coverage returning geohash precision 8 cells fully contained
--
-- Notes    : This expects these functions already exist in your DB:
--              - geohash_encode(lat, lon, precision)
--              - geohash_decode_bbox(geohash)
--              - geohash_cell_center(geohash)
--              - point_in_polygon(lon,lat, lon[],lat[])
--
-- ============================================================================

--
-- Given an array of 3-5 geohashes that define the vertices of a polygon, this
-- function finds every precision-8 geohash cell whose bounding box falls
-- entirely within that polygon. It converts each vertex geohash to its center
-- lat/lon, builds a bounding box over the polygon, walks the area in
-- geohash-8-sized steps, and returns only cells where all four corners pass the
-- point-in-polygon test.
--
-- Example:
--   SELECT * FROM geohash8_fully_within_polygon(
--       ARRAY['9x0qs0fd', '9x0qs0ff', '9x0qs0fu', '9x0qs0fs']
--   );
--   -- Returns rows such as: '9x0qs0fd', '9x0qs0ff', ...
--   -- (all precision-8 cells fully inside the quad defined by those 4 vertices)
--
CREATE OR REPLACE FUNCTION geohash8_fully_within_polygon(p_hashes text[])
RETURNS SETOF text
LANGUAGE plpgsql
AS $$
DECLARE
   n integer;

   v_lon double precision[] := '{}';
   v_lat double precision[] := '{}';

   i integer;

   poly_lon_min double precision :=  180.0;
   poly_lon_max double precision := -180.0;
   poly_lat_min double precision :=   90.0;
   poly_lat_max double precision :=  -90.0;

   mid_lon double precision;
   mid_lat double precision;

   cell_bbox record;
   lat_step double precision;
   lon_step double precision;

   lat_cur double precision;
   lon_cur double precision;

   gh8 text;
   gh8_bbox record;

   c1 boolean; c2 boolean; c3 boolean; c4 boolean;
BEGIN
   n := coalesce(array_length(p_hashes, 1), 0);

   IF n < 3 OR n > 5 THEN
      RAISE EXCEPTION 'geohash8_fully_within_polygon: supply 3, 4, or 5 geohashes (got %)', n;
   END IF;

   -- Build polygon vertices from geohash centers
   FOR i IN 1..n LOOP
      SELECT lat, lon INTO mid_lat, mid_lon
      FROM geohash_cell_center(p_hashes[i]);

      v_lat := v_lat || mid_lat;
      v_lon := v_lon || mid_lon;

      poly_lat_min := LEAST(poly_lat_min, mid_lat);
      poly_lat_max := GREATEST(poly_lat_max, mid_lat);
      poly_lon_min := LEAST(poly_lon_min, mid_lon);
      poly_lon_max := GREATEST(poly_lon_max, mid_lon);
   END LOOP;

   -- Representative cell size for precision 8 near polygon center
   --
   mid_lat := (poly_lat_min + poly_lat_max) / 2.0;
   mid_lon := (poly_lon_min + poly_lon_max) / 2.0;

   SELECT * INTO cell_bbox
   FROM geohash_decode_bbox(geohash_encode(mid_lat, mid_lon, 8));

   lat_step := cell_bbox.lat_max - cell_bbox.lat_min;
   lon_step := cell_bbox.lon_max - cell_bbox.lon_min;

   IF lat_step <= 0 OR lon_step <= 0 THEN
      RAISE EXCEPTION 'geohash8_fully_within_polygon: invalid cell step computed';
   END IF;

   -- Walk polygon bbox in geohash8-sized steps and keep cells whose 4 corners are inside.
   --
   lat_cur := poly_lat_min;
   WHILE lat_cur <= poly_lat_max LOOP
      lon_cur := poly_lon_min;
      WHILE lon_cur <= poly_lon_max LOOP
         gh8 := geohash_encode(lat_cur, lon_cur, 8);

         SELECT * INTO gh8_bbox FROM geohash_decode_bbox(gh8);

         c1 := point_in_polygon(gh8_bbox.lon_min, gh8_bbox.lat_min, v_lon, v_lat);
         c2 := point_in_polygon(gh8_bbox.lon_min, gh8_bbox.lat_max, v_lon, v_lat);
         c3 := point_in_polygon(gh8_bbox.lon_max, gh8_bbox.lat_min, v_lon, v_lat);
         c4 := point_in_polygon(gh8_bbox.lon_max, gh8_bbox.lat_max, v_lon, v_lat);

         IF c1 AND c2 AND c3 AND c4 THEN
            RETURN NEXT gh8;
         END IF;

         lon_cur := lon_cur + lon_step;
      END LOOP;

      lat_cur := lat_cur + lat_step;
   END LOOP;

   RETURN;
END;
$$;

-- ============================================================================

-- Convenience overload that accepts 3-5 individual geohash arguments instead of
-- an array. Internally bundles them into an array and delegates to the array
-- version of geohash8_fully_within_polygon. The 4th and 5th parameters are
-- optional, so you can call it with just 3, 4, or 5 vertex geohashes.
--
-- Example:
--   SELECT * FROM geohash8_fully_within_polygon(
--       '9x0qs0fd', '9x0qs0ff', '9x0qs0fu', '9x0qs0fs'
--   );
--   -- Returns the same rows as the array version above.
--
CREATE OR REPLACE FUNCTION geohash8_fully_within_polygon(
   p1 text, p2 text, p3 text,
   p4 text DEFAULT NULL,
   p5 text DEFAULT NULL
)
RETURNS SETOF text
LANGUAGE plpgsql
AS $$
DECLARE
   arr text[] := ARRAY[p1, p2, p3];
BEGIN
   IF p4 IS NOT NULL THEN
      arr := arr || p4;
   END IF;

   IF p5 IS NOT NULL THEN
      arr := arr || p5;
   END IF;

   RETURN QUERY
   SELECT * FROM geohash8_fully_within_polygon(arr);
END;
$$;







