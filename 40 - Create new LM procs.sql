

-- ============================================================================
--
-- YugabyteDB YSQL / PostgreSQL-compatible geometry functions
-- (pure SQL + PL/pgSQL; no extensions required)
--
-- PostGIS-equivalent "LM" functions for use with the geohash-based
-- map-data system.  These operate on the same polygon representation
-- used elsewhere in this project: parallel double-precision arrays
-- for lon[] and lat[] vertices.
--
-- What you get in this file:
--   1) lm_st_xmin(lon[])                          -> min longitude
--   2) lm_st_xmax(lon[])                          -> max longitude
--   3) lm_st_ymin(lat[])                          -> min latitude
--   4) lm_st_ymax(lat[])                          -> max latitude
--   5) lm_st_translate(lon[], lat[], dx, dy)       -> shifted polygon
--   6) lm_st_intersects(lon_a[], lat_a[],
--                        lon_b[], lat_b[])          -> boolean
--   7) lm_st_contains(lon_a[], lat_a[],
--                      lon_b[], lat_b[])            -> boolean
--
-- Conventions:
--   * X = longitude, Y = latitude  (matches PostGIS ST_X / ST_Y).
--   * Polygons are defined by parallel arrays of vertices listed in
--     order (CW or CCW).  They do NOT need to be explicitly closed
--     (first vertex != last vertex) -- the functions close them
--     implicitly where needed.
--   * A single point is represented as a 1-element array.
--   * A bounding box (from geohash_decode_bbox) can be passed as the
--     4-corner rectangle:
--       lon[] = ARRAY[lon_min, lon_max, lon_max, lon_min]
--       lat[] = ARRAY[lat_min, lat_min, lat_max, lat_max]
--
-- Prerequisites (from 30 - Make base stored procs.sql):
--   - point_in_polygon(lon, lat, poly_lon[], poly_lat[])
--
-- ============================================================================


-- ============================================================
-- 1)  lm_st_xmin  --  PostGIS ST_XMin equivalent
-- ============================================================
--
-- Returns the minimum X (longitude) value from a geometry represented
-- as a longitude array.  Equivalent to ST_XMin on a bounding box or
-- geometry envelope.
--
-- Example:
--   SELECT lm_st_xmin(ARRAY[-112.0, -111.9, -111.9, -112.0]);
--   -- Returns: -112.0
--

CREATE OR REPLACE FUNCTION lm_st_xmin(
   p_lon double precision[]
)
RETURNS double precision
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   n integer := coalesce(array_length(p_lon, 1), 0);
   v double precision;
   result double precision;
   i integer;
BEGIN
   IF n = 0 THEN
      RAISE EXCEPTION 'lm_st_xmin: lon array must not be empty';
   END IF;

   result := p_lon[1];
   FOR i IN 2..n LOOP
      v := p_lon[i];
      IF v < result THEN
         result := v;
      END IF;
   END LOOP;

   RETURN result;
END;
$$;


-- ============================================================
-- 2)  lm_st_xmax  --  PostGIS ST_XMax equivalent
-- ============================================================
--
-- Returns the maximum X (longitude) value from a geometry represented
-- as a longitude array.  Equivalent to ST_XMax on a bounding box or
-- geometry envelope.
--
-- Example:
--   SELECT lm_st_xmax(ARRAY[-112.0, -111.9, -111.9, -112.0]);
--   -- Returns: -111.9
--

CREATE OR REPLACE FUNCTION lm_st_xmax(
   p_lon double precision[]
)
RETURNS double precision
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   n integer := coalesce(array_length(p_lon, 1), 0);
   v double precision;
   result double precision;
   i integer;
BEGIN
   IF n = 0 THEN
      RAISE EXCEPTION 'lm_st_xmax: lon array must not be empty';
   END IF;

   result := p_lon[1];
   FOR i IN 2..n LOOP
      v := p_lon[i];
      IF v > result THEN
         result := v;
      END IF;
   END LOOP;

   RETURN result;
END;
$$;


-- ============================================================
-- 3)  lm_st_ymin  --  PostGIS ST_YMin equivalent (bonus)
-- ============================================================
--
-- Returns the minimum Y (latitude) value from a geometry represented
-- as a latitude array.  Included because ST_XMin / ST_XMax are rarely
-- useful without the Y counterparts.
--
-- Example:
--   SELECT lm_st_ymin(ARRAY[40.5, 40.5, 40.55, 40.55]);
--   -- Returns: 40.5
--

CREATE OR REPLACE FUNCTION lm_st_ymin(
   p_lat double precision[]
)
RETURNS double precision
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   n integer := coalesce(array_length(p_lat, 1), 0);
   v double precision;
   result double precision;
   i integer;
BEGIN
   IF n = 0 THEN
      RAISE EXCEPTION 'lm_st_ymin: lat array must not be empty';
   END IF;

   result := p_lat[1];
   FOR i IN 2..n LOOP
      v := p_lat[i];
      IF v < result THEN
         result := v;
      END IF;
   END LOOP;

   RETURN result;
END;
$$;


-- ============================================================
-- 4)  lm_st_ymax  --  PostGIS ST_YMax equivalent (bonus)
-- ============================================================
--
-- Returns the maximum Y (latitude) value from a geometry represented
-- as a latitude array.
--
-- Example:
--   SELECT lm_st_ymax(ARRAY[40.5, 40.5, 40.55, 40.55]);
--   -- Returns: 40.55
--

CREATE OR REPLACE FUNCTION lm_st_ymax(
   p_lat double precision[]
)
RETURNS double precision
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   n integer := coalesce(array_length(p_lat, 1), 0);
   v double precision;
   result double precision;
   i integer;
BEGIN
   IF n = 0 THEN
      RAISE EXCEPTION 'lm_st_ymax: lat array must not be empty';
   END IF;

   result := p_lat[1];
   FOR i IN 2..n LOOP
      v := p_lat[i];
      IF v > result THEN
         result := v;
      END IF;
   END LOOP;

   RETURN result;
END;
$$;


-- ============================================================
-- 5)  lm_st_translate  --  PostGIS ST_Translate equivalent
-- ============================================================
--
-- Translates (shifts) every vertex of a polygon by the given longitude
-- offset (p_dx) and latitude offset (p_dy).  Returns a two-column
-- result set with the shifted lon[] and lat[] arrays.
--
-- PostGIS signature:  ST_Translate(geometry, deltaX, deltaY)
--   deltaX = longitude offset,  deltaY = latitude offset
--
-- Example:
--   SELECT * FROM lm_st_translate(
--       ARRAY[-112.0, -111.9, -111.9, -112.0],
--       ARRAY[40.5,   40.5,   40.55,  40.55],
--       0.01,   -- shift east  by 0.01 degrees longitude
--       -0.02   -- shift south by 0.02 degrees latitude
--   );
--   -- Returns:
--   --   out_lon = {-111.99,-111.89,-111.89,-111.99}
--   --   out_lat = {40.48,40.48,40.53,40.53}
--

CREATE OR REPLACE FUNCTION lm_st_translate(
   p_lon double precision[],
   p_lat double precision[],
   p_dx  double precision,
   p_dy  double precision
)
RETURNS TABLE(out_lon double precision[], out_lat double precision[])
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   n integer := coalesce(array_length(p_lon, 1), 0);
   i integer;
BEGIN
   IF n = 0 OR n <> coalesce(array_length(p_lat, 1), 0) THEN
      RAISE EXCEPTION 'lm_st_translate: lon[] and lat[] must be same length and non-empty';
   END IF;

   out_lon := ARRAY[]::double precision[];
   out_lat := ARRAY[]::double precision[];

   FOR i IN 1..n LOOP
      out_lon := out_lon || (p_lon[i] + p_dx);
      out_lat := out_lat || (p_lat[i] + p_dy);
   END LOOP;

   RETURN NEXT;
END;
$$;


-- ============================================================
-- 6)  lm_st_intersects  --  PostGIS ST_Intersects equivalent
-- ============================================================
--
-- Tests whether two polygons intersect -- that is, they share at
-- least one point in common.  Two polygons intersect if:
--   (a) any vertex of A lies inside B, or
--   (b) any vertex of B lies inside A, or
--   (c) any edge of A crosses any edge of B.
--
-- This covers all cases including overlapping, touching, and one
-- polygon being entirely inside the other.
--
-- Uses the existing point_in_polygon() function for vertex-in-polygon
-- tests and adds a segment-intersection check for edge crossings.
--
-- Example:
--   SELECT lm_st_intersects(
--       ARRAY[-112.0, -111.9, -111.9, -112.0],   -- polygon A lon
--       ARRAY[40.5,   40.5,   40.55,  40.55],     -- polygon A lat
--       ARRAY[-111.95, -111.85, -111.85, -111.95], -- polygon B lon
--       ARRAY[40.52,   40.52,   40.57,   40.57]    -- polygon B lat
--   );
--   -- Returns: true  (the two rectangles overlap)
--

CREATE OR REPLACE FUNCTION lm_st_intersects(
   p_lon_a double precision[],
   p_lat_a double precision[],
   p_lon_b double precision[],
   p_lat_b double precision[]
)
RETURNS boolean
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   na integer := coalesce(array_length(p_lon_a, 1), 0);
   nb integer := coalesce(array_length(p_lon_b, 1), 0);
   i integer;
   j integer;
   i2 integer;
   j2 integer;
BEGIN
   IF na < 1 OR na <> coalesce(array_length(p_lat_a, 1), 0) THEN
      RAISE EXCEPTION 'lm_st_intersects: polygon A lon/lat arrays must be same length and non-empty';
   END IF;
   IF nb < 1 OR nb <> coalesce(array_length(p_lat_b, 1), 0) THEN
      RAISE EXCEPTION 'lm_st_intersects: polygon B lon/lat arrays must be same length and non-empty';
   END IF;

   -- Single-point cases: just test containment
   IF na = 1 AND nb = 1 THEN
      RETURN (p_lon_a[1] = p_lon_b[1] AND p_lat_a[1] = p_lat_b[1]);
   END IF;

   IF na = 1 THEN
      RETURN point_in_polygon(p_lon_a[1], p_lat_a[1], p_lon_b, p_lat_b);
   END IF;

   IF nb = 1 THEN
      RETURN point_in_polygon(p_lon_b[1], p_lat_b[1], p_lon_a, p_lat_a);
   END IF;

   -- Check (a): any vertex of A inside B
   FOR i IN 1..na LOOP
      IF point_in_polygon(p_lon_a[i], p_lat_a[i], p_lon_b, p_lat_b) THEN
         RETURN true;
      END IF;
   END LOOP;

   -- Check (b): any vertex of B inside A
   FOR i IN 1..nb LOOP
      IF point_in_polygon(p_lon_b[i], p_lat_b[i], p_lon_a, p_lat_a) THEN
         RETURN true;
      END IF;
   END LOOP;

   -- Check (c): any edge of A crosses any edge of B
   -- Uses the standard cross-product segment intersection test.
   FOR i IN 1..na LOOP
      i2 := CASE WHEN i = na THEN 1 ELSE i + 1 END;

      FOR j IN 1..nb LOOP
         j2 := CASE WHEN j = nb THEN 1 ELSE j + 1 END;

         IF lm__segments_cross(
               p_lon_a[i],  p_lat_a[i],  p_lon_a[i2], p_lat_a[i2],
               p_lon_b[j],  p_lat_b[j],  p_lon_b[j2], p_lat_b[j2]
            )
         THEN
            RETURN true;
         END IF;
      END LOOP;
   END LOOP;

   RETURN false;
END;
$$;


-- ------------------------------------------------------------
-- Internal helper: do two line segments properly cross?
-- Uses the orientation / cross-product method.
-- ------------------------------------------------------------
--
-- Returns true if segment (ax1,ay1)-(ax2,ay2) properly intersects
-- segment (bx1,by1)-(bx2,by2).  "Properly" includes the case where
-- an endpoint of one segment lies exactly on the other segment
-- (collinear touching), which matches PostGIS ST_Intersects behavior.
--

CREATE OR REPLACE FUNCTION lm__segments_cross(
   ax1 double precision, ay1 double precision,
   ax2 double precision, ay2 double precision,
   bx1 double precision, by1 double precision,
   bx2 double precision, by2 double precision
)
RETURNS boolean
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   d1 double precision;
   d2 double precision;
   d3 double precision;
   d4 double precision;
BEGIN
   -- Cross products to determine orientation.
   -- d = (bx-ax)*(cy-ay) - (by-ay)*(cx-ax)
   --
   d1 := (bx2 - bx1) * (ay1 - by1) - (by2 - by1) * (ax1 - bx1);
   d2 := (bx2 - bx1) * (ay2 - by1) - (by2 - by1) * (ax2 - bx1);
   d3 := (ax2 - ax1) * (by1 - ay1) - (ay2 - ay1) * (bx1 - ax1);
   d4 := (ax2 - ax1) * (by2 - ay1) - (ay2 - ay1) * (bx2 - ax1);

   -- Standard test: segments straddle each other
   IF ((d1 > 0 AND d2 < 0) OR (d1 < 0 AND d2 > 0))
      AND
      ((d3 > 0 AND d4 < 0) OR (d3 < 0 AND d4 > 0))
   THEN
      RETURN true;
   END IF;

   -- Collinear / endpoint-on-segment cases
   IF d1 = 0 AND lm__on_segment(bx1, by1, bx2, by2, ax1, ay1) THEN RETURN true; END IF;
   IF d2 = 0 AND lm__on_segment(bx1, by1, bx2, by2, ax2, ay2) THEN RETURN true; END IF;
   IF d3 = 0 AND lm__on_segment(ax1, ay1, ax2, ay2, bx1, by1) THEN RETURN true; END IF;
   IF d4 = 0 AND lm__on_segment(ax1, ay1, ax2, ay2, bx2, by2) THEN RETURN true; END IF;

   RETURN false;
END;
$$;


-- ------------------------------------------------------------
-- Internal helper: is point (px,py) on segment (sx1,sy1)-(sx2,sy2)?
-- Assumes the three points are already known to be collinear.
-- ------------------------------------------------------------

CREATE OR REPLACE FUNCTION lm__on_segment(
   sx1 double precision, sy1 double precision,
   sx2 double precision, sy2 double precision,
   px  double precision, py  double precision
)
RETURNS boolean
LANGUAGE plpgsql
IMMUTABLE
AS $$
BEGIN
   RETURN px >= least(sx1, sx2)
      AND px <= greatest(sx1, sx2)
      AND py >= least(sy1, sy2)
      AND py <= greatest(sy1, sy2);
END;
$$;


-- ============================================================
-- 7)  lm_st_contains  --  PostGIS ST_Contains equivalent
-- ============================================================
--
-- Tests whether polygon A fully contains polygon B.  Every vertex of
-- B must lie inside A, AND the interiors must share at least one point
-- (i.e., B cannot be entirely on the boundary of A -- matching PostGIS
-- semantics).
--
-- For most practical cases with geohash cells, if all vertices of B
-- are inside A then the interior condition is automatically satisfied.
-- The boundary-only edge case is handled by checking that at least one
-- vertex of B is strictly interior to A (not on any edge).
--
-- Example:
--   -- Does the large rectangle contain the small one?
--   SELECT lm_st_contains(
--       ARRAY[-112.0, -111.8, -111.8, -112.0],   -- A (outer) lon
--       ARRAY[40.4,   40.4,   40.6,   40.6],      -- A (outer) lat
--       ARRAY[-111.95, -111.85, -111.85, -111.95], -- B (inner) lon
--       ARRAY[40.45,   40.45,   40.55,   40.55]    -- B (inner) lat
--   );
--   -- Returns: true
--

CREATE OR REPLACE FUNCTION lm_st_contains(
   p_lon_a double precision[],
   p_lat_a double precision[],
   p_lon_b double precision[],
   p_lat_b double precision[]
)
RETURNS boolean
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   na integer := coalesce(array_length(p_lon_a, 1), 0);
   nb integer := coalesce(array_length(p_lon_b, 1), 0);
   i  integer;
   j  integer;
   i2 integer;

   -- for interior-point check
   on_boundary boolean;
   has_interior boolean := false;

   -- edge vars
   ex1 double precision; ey1 double precision;
   ex2 double precision; ey2 double precision;
   px  double precision; py  double precision;
   t   double precision;
   dist double precision;
   epsilon constant double precision := 1e-12;
BEGIN
   IF na < 3 OR na <> coalesce(array_length(p_lat_a, 1), 0) THEN
      RAISE EXCEPTION 'lm_st_contains: polygon A must have >= 3 vertices with matching lon/lat lengths';
   END IF;
   IF nb < 1 OR nb <> coalesce(array_length(p_lat_b, 1), 0) THEN
      RAISE EXCEPTION 'lm_st_contains: polygon B lon/lat arrays must be same length and non-empty';
   END IF;

   -- Every vertex of B must be inside A (or on its boundary).
   FOR i IN 1..nb LOOP
      IF NOT point_in_polygon(p_lon_b[i], p_lat_b[i], p_lon_a, p_lat_a) THEN
         RETURN false;
      END IF;
   END LOOP;

   -- Additionally, at least one point of B must be strictly in the
   -- interior of A (not on any edge of A).  This satisfies the
   -- PostGIS requirement that the interiors share a point.
   --
   FOR i IN 1..nb LOOP
      px := p_lon_b[i];
      py := p_lat_b[i];
      on_boundary := false;

      FOR j IN 1..na LOOP
         i2 := CASE WHEN j = na THEN 1 ELSE j + 1 END;
         ex1 := p_lon_a[j];   ey1 := p_lat_a[j];
         ex2 := p_lon_a[i2];  ey2 := p_lat_a[i2];

         -- Parameterize: closest point on segment to (px,py)
         t := ((px - ex1) * (ex2 - ex1) + (py - ey1) * (ey2 - ey1))
            / nullif(((ex2 - ex1) * (ex2 - ex1) + (ey2 - ey1) * (ey2 - ey1)), 0.0);

         IF t IS NULL THEN
            -- degenerate edge (zero length); check if point matches
            dist := sqrt((px - ex1)*(px - ex1) + (py - ey1)*(py - ey1));
         ELSE
            t := greatest(0.0, least(1.0, t));
            dist := sqrt(
               (px - (ex1 + t * (ex2 - ex1))) * (px - (ex1 + t * (ex2 - ex1)))
             + (py - (ey1 + t * (ey2 - ey1))) * (py - (ey1 + t * (ey2 - ey1)))
            );
         END IF;

         IF dist < epsilon THEN
            on_boundary := true;
            EXIT;  -- no need to check other edges for this vertex
         END IF;
      END LOOP;

      IF NOT on_boundary THEN
         has_interior := true;
         EXIT;  -- found at least one interior point; done
      END IF;
   END LOOP;

   RETURN has_interior;
END;
$$;


-- ============================================================
-- Example calls
-- ============================================================

-- ------- lm_st_xmin / lm_st_xmax / lm_st_ymin / lm_st_ymax -------
--
-- SELECT lm_st_xmin(ARRAY[-112.0, -111.9, -111.9, -112.0]);
-- -- Returns: -112.0
--
-- SELECT lm_st_xmax(ARRAY[-112.0, -111.9, -111.9, -112.0]);
-- -- Returns: -111.9
--
-- SELECT lm_st_ymin(ARRAY[40.5, 40.5, 40.55, 40.55]);
-- -- Returns: 40.5
--
-- SELECT lm_st_ymax(ARRAY[40.5, 40.5, 40.55, 40.55]);
-- -- Returns: 40.55

-- ------- lm_st_translate -------
--
-- Shift a rectangle 0.01 deg east and 0.02 deg south:
--
-- SELECT * FROM lm_st_translate(
--     ARRAY[-112.0, -111.9, -111.9, -112.0],
--     ARRAY[40.5,   40.5,   40.55,  40.55],
--     0.01,
--     -0.02
-- );
-- -- out_lon = {-111.99,-111.89,-111.89,-111.99}
-- -- out_lat = {40.48,40.48,40.53,40.53}

-- ------- lm_st_intersects -------
--
-- Two overlapping rectangles:
--
-- SELECT lm_st_intersects(
--     ARRAY[-112.0, -111.9, -111.9, -112.0],
--     ARRAY[40.5,   40.5,   40.55,  40.55],
--     ARRAY[-111.95, -111.85, -111.85, -111.95],
--     ARRAY[40.52,   40.52,   40.57,   40.57]
-- );
-- -- Returns: true
--
-- Two non-overlapping rectangles:
--
-- SELECT lm_st_intersects(
--     ARRAY[-112.0, -111.9, -111.9, -112.0],
--     ARRAY[40.5,   40.5,   40.55,  40.55],
--     ARRAY[-111.0, -110.9, -110.9, -111.0],
--     ARRAY[41.0,   41.0,   41.05,  41.05]
-- );
-- -- Returns: false

-- ------- lm_st_contains -------
--
-- Large rectangle fully contains small rectangle:
--
-- SELECT lm_st_contains(
--     ARRAY[-112.0, -111.8, -111.8, -112.0],
--     ARRAY[40.4,   40.4,   40.6,   40.6],
--     ARRAY[-111.95, -111.85, -111.85, -111.95],
--     ARRAY[40.45,   40.45,   40.55,   40.55]
-- );
-- -- Returns: true
--
-- Small does NOT contain large:
--
-- SELECT lm_st_contains(
--     ARRAY[-111.95, -111.85, -111.85, -111.95],
--     ARRAY[40.45,   40.45,   40.55,   40.55],
--     ARRAY[-112.0, -111.8, -111.8, -112.0],
--     ARRAY[40.4,   40.4,   40.6,   40.6]
-- );
-- -- Returns: false

-- ------- Using with geohash bounding boxes -------
--
-- You can combine these with geohash_decode_bbox to work on geohash
-- cells directly.  For example, to test if one geohash cell contains
-- another:
--
-- WITH
--    a AS (SELECT * FROM geohash_decode_bbox('9x0qs0')),
--    b AS (SELECT * FROM geohash_decode_bbox('9x0qs0fd'))
-- SELECT lm_st_contains(
--     ARRAY[a.lon_min, a.lon_max, a.lon_max, a.lon_min],
--     ARRAY[a.lat_min, a.lat_min, a.lat_max, a.lat_max],
--     ARRAY[b.lon_min, b.lon_max, b.lon_max, b.lon_min],
--     ARRAY[b.lat_min, b.lat_min, b.lat_max, b.lat_max]
-- )
-- FROM a, b;
-- -- Returns: true  (precision-8 cell is inside its parent precision-6 cell)




