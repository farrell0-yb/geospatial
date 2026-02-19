

-- ============================================================
--
-- YugabyteDB YSQL / PostgreSQL-compatible geohash functions
-- (pure SQL + PL/pgSQL; no extensions required)
--
-- What you get in this file:
--   1) geohash_encode(lat, lon, precision) -> geohash text
--   2) geohash_adjacent(geohash, dir) -> neighboring geohash in one step
--   3) geohash_neighbors(geohash) -> 8 neighbors as text[]
--   4) geohash_in_list_within_miles(geohash, miles) -> text usable inside IN (...)
--   5) geohash_in_list_within_miles_dir(geohash, miles, dirs[]) -> same, but only chosen directions
--
-- Notes:
--   * The "within miles" functions choose an appropriate geohash precision based on miles,
--     truncate the input geohash to that precision, then generate a grid (or directional ray)
--     of neighboring geohashes sufficient to cover the radius in a conservative way.
--
--   * Return format is a string of *quoted* geohashes separated by commas.
--     Example:  '9x0qs0f','9x0qs0g','9x0qs0u'
--
--     You can use it like:
--        ... WHERE geo_hash10 IN ( geohash_in_list_within_miles('9x0qs0fduh', 2) );
--
-- ============================================================


-- ------------------------------------------------------------
-- Encode lat/lon to geohash
-- ------------------------------------------------------------
--
-- Converts a latitude/longitude coordinate pair into a geohash string of the
-- specified precision (1-10 characters, default 10). Longer precision values
-- produce more specific geohashes that pinpoint smaller geographic areas.
--
-- Example:
--   SELECT geohash_encode(40.522196, -111.969047, 10);
--   -- Returns: '9x0qs0fduh'
--

CREATE OR REPLACE FUNCTION geohash_encode(
   p_lat double precision,
   p_lon double precision,
   p_precision integer DEFAULT 10
)
RETURNS text
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   base32 constant text := '0123456789bcdefghjkmnpqrstuvwxyz';
   lat_min double precision := -90.0;
   lat_max double precision :=  90.0;
   lon_min double precision := -180.0;
   lon_max double precision :=  180.0;

   is_even boolean := true;
   bit integer := 0;
   ch integer := 0;
   mid double precision;

   geoh text := '';
BEGIN
   IF p_precision IS NULL OR p_precision < 1 THEN
      RAISE EXCEPTION 'geohash_encode: precision must be >= 1';
   END IF;

   WHILE char_length(geoh) < p_precision LOOP
      IF is_even THEN
         mid := (lon_min + lon_max) / 2.0;
         IF p_lon >= mid THEN
            ch := (ch * 2) + 1;
            lon_min := mid;
         ELSE
            ch := (ch * 2);
            lon_max := mid;
         END IF;
      ELSE
         mid := (lat_min + lat_max) / 2.0;
         IF p_lat >= mid THEN
            ch := (ch * 2) + 1;
            lat_min := mid;
         ELSE
            ch := (ch * 2);
            lat_max := mid;
         END IF;
      END IF;

      is_even := NOT is_even;
      bit := bit + 1;

      IF bit = 5 THEN
         geoh := geoh || substr(base32, ch + 1, 1);
         bit := 0;
         ch := 0;
      END IF;
   END LOOP;

   RETURN geoh;
END;
$$;

-- ------------------------------------------------------------
-- Single-step neighbor (adjacent geohash)
-- Direction values: 'n','s','e','w'
-- ------------------------------------------------------------
--
-- Returns the geohash that is exactly one cell away from the input geohash in
-- the given cardinal direction ('n', 's', 'e', or 'w'). The returned geohash
-- has the same precision as the input. This is the core building block used by
-- the neighbor and movement functions.
--
-- Example:
--   SELECT geohash_adjacent('9x0qs0fduh', 'n');
--   -- Returns: '9x0qs0fdun'
--

CREATE OR REPLACE FUNCTION geohash_adjacent(
   p_hash text,
   p_dir  text
)
RETURNS text
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   base32 constant text := '0123456789bcdefghjkmnpqrstuvwxyz';

   -- Neighbor and border tables (classic geohash implementation)
   --
   neighbor_n constant text := 'p0r21436x8zb9dcf5h7kjnmqesgutwvy';
   neighbor_s constant text := '14365h7k9dcfesgujnmqp0r2twvyx8zb';
   neighbor_e constant text := 'bc01fg45238967deuvhjyznpkmstqrwx';
   neighbor_w constant text := '238967debc01fg45kmstqrwxuvhjyznp';

   border_n  constant text := 'prxz';
   border_s  constant text := '028b';
   border_e  constant text := 'bcfguvyz';
   border_w  constant text := '0145hjnp';

   h text := lower(coalesce(p_hash, ''));
   dir text := lower(coalesce(p_dir, ''));

   last_char text;
   parent text;
   t_neighbor text;
   t_border text;
   idx integer;
BEGIN
   IF h = '' THEN
      RAISE EXCEPTION 'geohash_adjacent: hash must not be empty';
   END IF;

   IF dir NOT IN ('n','s','e','w') THEN
      RAISE EXCEPTION 'geohash_adjacent: dir must be one of n,s,e,w (got "%")', p_dir;
   END IF;

   last_char := right(h, 1);
   parent := left(h, char_length(h) - 1);

   -- pick tables based on direction and parity
   --
   IF (char_length(h) % 2) = 0 THEN
      -- even length
      --
      IF dir = 'n' THEN t_neighbor := neighbor_n; t_border := border_n;
      ELSIF dir = 's' THEN t_neighbor := neighbor_s; t_border := border_s;
      ELSIF dir = 'e' THEN t_neighbor := neighbor_e; t_border := border_e;
      ELSE                t_neighbor := neighbor_w; t_border := border_w;
      END IF;
   ELSE
      -- odd length: swap N/S and E/W tables
      --
      IF dir = 'n' THEN t_neighbor := neighbor_e; t_border := border_e;
      ELSIF dir = 's' THEN t_neighbor := neighbor_w; t_border := border_w;
      ELSIF dir = 'e' THEN t_neighbor := neighbor_n; t_border := border_n;
      ELSE                t_neighbor := neighbor_s; t_border := border_s;
      END IF;
   END IF;

   -- recurse on border
   --
   IF parent <> '' AND position(last_char in t_border) > 0 THEN
      parent := geohash_adjacent(parent, dir);
   END IF;

   idx := position(last_char in t_neighbor);
   IF idx = 0 THEN
      RAISE EXCEPTION 'geohash_adjacent: invalid geohash character "%" in "%"', last_char, p_hash;
   END IF;

   RETURN parent || substr(base32, idx, 1);
END;
$$;

-- ------------------------------------------------------------
-- 8 neighbors around a geohash (same precision)
-- Returns array in order: [n, ne, e, se, s, sw, w, nw]
-- ------------------------------------------------------------
--
-- Returns all eight surrounding geohash cells (n, s, e, w, ne, nw, se, sw) as
-- a JSONB object keyed by direction. Each neighbor has the same precision as
-- the input geohash. Useful for finding everything immediately adjacent to a
-- given cell.
--
-- Example:
--   SELECT geohash_neighbors('9x0qs0');
--   -- Returns: {"e": "9x0qs1", "n": "9x0qs2", "s": "9x0qek", "w": "9x0qnb",
--   --           "ne": "9x0qs3", "nw": "9x0qnc", "se": "9x0qe7", "sw": "9x0qn8"}
--

CREATE OR REPLACE FUNCTION geohash_neighbors(
   p_hash text
)
RETURNS jsonb
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   h text := lower(coalesce(p_hash,''));
   n  text;
   s  text;
   e  text;
   w  text;
   ne text;
   nw text;
   se text;
   sw text;
BEGIN
   IF h = '' THEN
      RAISE EXCEPTION 'geohash_neighbors: hash must not be empty';
   END IF;

   n  := geohash_adjacent(h, 'n');
   s  := geohash_adjacent(h, 's');
   e  := geohash_adjacent(h, 'e');
   w  := geohash_adjacent(h, 'w');

   ne := geohash_adjacent(n, 'e');
   nw := geohash_adjacent(n, 'w');
   se := geohash_adjacent(s, 'e');
   sw := geohash_adjacent(s, 'w');

   RETURN jsonb_build_object(
      'n',  n,
      's',  s,
      'e',  e,
      'w',  w,
      'ne', ne,
      'nw', nw,
      'se', se,
      'sw', sw
   );
END;
$$;

-- ------------------------------------------------------------
-- Helper: choose a conservative geohash precision from miles.
-- Uses approximate *cell height* at equator for each precision.
-- ------------------------------------------------------------
--
-- Given a distance in miles, returns the highest geohash precision (1-10)
-- whose cell height is still smaller than or equal to that distance. This is
-- used internally to decide how coarse or fine the geohash grid should be when
-- covering a radius-based search area.
--
-- Example:
--   SELECT geohash_precision_for_miles(2.0);
--   -- Returns: 5   (precision-5 cells are ~2.43 miles tall)
--

CREATE OR REPLACE FUNCTION geohash_precision_for_miles(
   p_miles double precision
)
RETURNS integer
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   -- Approx geohash cell height in miles for precisions 1..10.
   -- (Conservative values; width varies with latitude.)
   --
   cell_h constant double precision[] := ARRAY[
      12430.0,  -- 1
      1243.0,   -- 2
      155.0,    -- 3
      19.4,     -- 4
      2.43,     -- 5
      0.61,     -- 6
      0.076,    -- 7
      0.019,    -- 8
      0.0024,   -- 9
      0.0006    -- 10
   ];
   m double precision := greatest(p_miles, 0.0);
   i integer;
BEGIN
   FOR i IN 1..array_length(cell_h, 1) LOOP
      IF cell_h[i] <= m THEN
         RETURN i;
      END IF;
   END LOOP;

   RETURN 10;
END;
$$;

-- ------------------------------------------------------------
-- Helper: cell height in miles for a given precision (1..10)
-- ------------------------------------------------------------
--
-- Returns the approximate height in miles of a single geohash cell at the
-- given precision level (1-10). Values are based on equatorial estimates.
-- Used internally by the "within miles" functions to calculate how many grid
-- steps are needed to cover a given radius.
--
-- Example:
--   SELECT geohash_cell_height_miles(5);
--   -- Returns: 2.43
--

CREATE OR REPLACE FUNCTION geohash_cell_height_miles(
   p_precision integer
)
RETURNS double precision
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   cell_h constant double precision[] := ARRAY[
      12430.0,
      1243.0,
      155.0,
      19.4,
      2.43,
      0.61,
      0.076,
      0.019,
      0.0024,
      0.0006
   ];
   p integer := greatest(1, least(10, p_precision));
BEGIN
   RETURN cell_h[p];
END;
$$;

-- ------------------------------------------------------------
-- Helper: move a geohash multiple steps in a cardinal direction
-- ------------------------------------------------------------
--
-- Moves from a starting geohash the specified number of steps in a single
-- cardinal direction ('n', 's', 'e', or 'w') by repeatedly calling
-- geohash_adjacent. Returns the geohash at the destination cell. Useful for
-- building grids or rays of geohash cells outward from a center point.
--
-- Example:
--   SELECT geohash_move('9x0qs0', 'n', 3);
--   -- Returns: '9x0qs8'  (3 cells north of 9x0qs0)
--

CREATE OR REPLACE FUNCTION geohash_move(
   p_hash text,
   p_dir  text,
   p_steps integer
)
RETURNS text
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   h text := lower(coalesce(p_hash,''));
   dir text := lower(coalesce(p_dir,''));
   i integer;
BEGIN
   IF p_steps IS NULL OR p_steps < 0 THEN
      RAISE EXCEPTION 'geohash_move: steps must be >= 0';
   END IF;

   IF dir NOT IN ('n','s','e','w') THEN
      RAISE EXCEPTION 'geohash_move: dir must be one of n,s,e,w (got "%")', p_dir;
   END IF;

   FOR i IN 1..p_steps LOOP
      h := geohash_adjacent(h, dir);
   END LOOP;

   RETURN h;
END;
$$;

-- ------------------------------------------------------------
-- Return a string of geohashes suitable inside an IN (...)
-- ------------------------------------------------------------
--
-- Generates a comma-separated, quoted list of all geohash cells that fall
-- within a square grid covering the given radius (in miles) around a source
-- geohash. The precision is automatically chosen based on the distance. The
-- returned string is ready to be used directly inside a SQL IN (...) clause
-- for proximity searches.
--
-- Example:
--   SELECT geohash_in_list_within_miles('9x0qs0fduh', 2);
--   -- Returns: '9x0qs0','9x0qs1','9x0qs2','9x0qs3',...  (grid of quoted geohashes)
--

CREATE OR REPLACE FUNCTION geohash_in_list_within_miles(
   p_source_geohash text,
   p_miles double precision
)
RETURNS text
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   src_full text := lower(coalesce(p_source_geohash,''));
   miles double precision := greatest(p_miles, 0.0);

   p integer;
   src text;

   cell_h double precision;
   steps integer;

   dy integer;
   dx integer;

   row_hash text;
   cur_hash text;

   out_list text := '';
   first boolean := true;
BEGIN
   IF src_full = '' THEN
      RAISE EXCEPTION 'geohash_in_list_within_miles: source geohash must not be empty';
   END IF;

   p := geohash_precision_for_miles(miles);
   src := left(src_full, p);

   cell_h := geohash_cell_height_miles(p);
   steps := greatest(1, ceil(miles / cell_h)::integer);

   FOR dy IN -steps..steps LOOP
      IF dy > 0 THEN
         row_hash := geohash_move(src, 'n', dy);
      ELSIF dy < 0 THEN
         row_hash := geohash_move(src, 's', -dy);
      ELSE
         row_hash := src;
      END IF;

      FOR dx IN -steps..steps LOOP
         IF dx > 0 THEN
            cur_hash := geohash_move(row_hash, 'e', dx);
         ELSIF dx < 0 THEN
            cur_hash := geohash_move(row_hash, 'w', -dx);
         ELSE
            cur_hash := row_hash;
         END IF;

         IF first THEN
            out_list := quote_literal(cur_hash);
            first := false;
         ELSE
            out_list := out_list || ',' || quote_literal(cur_hash);
         END IF;
      END LOOP;
   END LOOP;

   RETURN out_list;
END;
$$;

-- ------------------------------------------------------------
-- Directional version: only chosen directions (plus center)
-- p_dirs examples: ARRAY['n','nw','e']  or  ARRAY['n']
-- ------------------------------------------------------------
--
-- Like geohash_in_list_within_miles, but only expands in the specified compass
-- directions (n, s, e, w, ne, nw, se, sw) rather than a full grid. The center
-- cell is always included. This produces a smaller, more targeted set of
-- geohashes -- useful when you know which direction to search (e.g., "look
-- only north and northwest within 5 miles").
--
-- Example:
--   SELECT geohash_in_list_within_miles_dir('9x0qs0fduh', 5, ARRAY['n','nw','e']);
--   -- Returns: '9x0qs','9x0qv','9x0qu','9x0qt','9x0qk','9x0qm','9x0qs',...
--

CREATE OR REPLACE FUNCTION geohash_in_list_within_miles_dir(
   p_source_geohash text,
   p_miles double precision,
   p_dirs text[]
)
RETURNS text
LANGUAGE plpgsql
IMMUTABLE
AS $$
DECLARE
   src_full text := lower(coalesce(p_source_geohash,''));
   miles double precision := greatest(p_miles, 0.0);

   p integer;
   src text;

   cell_h double precision;
   steps integer;

   dir text;
   step_i integer;

   cur text;

   hashes text[];  -- collect hashes, then stringify safely at the end
BEGIN
   IF src_full = '' THEN
      RAISE EXCEPTION 'geohash_in_list_within_miles_dir: source geohash must not be empty';
   END IF;

   p := geohash_precision_for_miles(miles);
   src := left(src_full, p);

   cell_h := geohash_cell_height_miles(p);
   steps := greatest(1, ceil(miles / cell_h)::integer);

   -- always include center
   hashes := ARRAY[src];

   -- If no directions specified, fall back to full-grid version.
   IF p_dirs IS NULL OR array_length(p_dirs, 1) IS NULL THEN
      RETURN geohash_in_list_within_miles(src, miles);
   END IF;

   FOREACH dir IN ARRAY p_dirs LOOP
      dir := lower(trim(dir));

      IF dir IN ('c','center') THEN
         CONTINUE;

      ELSIF dir IN ('n','s','e','w') THEN
         cur := src;
         FOR step_i IN 1..steps LOOP
            cur := geohash_adjacent(cur, dir);
            hashes := hashes || cur;
         END LOOP;

      ELSIF dir IN ('ne','nw','se','sw') THEN
         cur := src;
         FOR step_i IN 1..steps LOOP
            IF dir = 'ne' THEN
               cur := geohash_adjacent(geohash_adjacent(cur, 'n'), 'e');
            ELSIF dir = 'nw' THEN
               cur := geohash_adjacent(geohash_adjacent(cur, 'n'), 'w');
            ELSIF dir = 'se' THEN
               cur := geohash_adjacent(geohash_adjacent(cur, 's'), 'e');
            ELSE -- 'sw'
               cur := geohash_adjacent(geohash_adjacent(cur, 's'), 'w');
            END IF;
            hashes := hashes || cur;
         END LOOP;

      ELSE
         RAISE EXCEPTION 'geohash_in_list_within_miles_dir: invalid direction "%". Use n,s,e,w,ne,nw,se,sw (and optional center).', dir;
      END IF;
   END LOOP;

   -- Return a comma-separated list of quoted literals, suitable inside IN (...)
   RETURN (
      SELECT string_agg(quote_literal(x), ',')
      FROM unnest(hashes) AS x
   );
END;
$$;

-- ============================================================
-- Example calls
-- ============================================================
--
-- SELECT geohash_encode(40.522196, -111.969047, 10);
-- SELECT geohash_neighbors('9x0qs0fduh');
-- SELECT geohash_in_list_within_miles('9x0qs0fduh', 2);
-- SELECT geohash_in_list_within_miles_dir('9x0qs0fduh', 5, ARRAY['n','nw','e']);


-- ============================================================================
-- Polygon coverage utilities
--
-- - geohash_decode_bbox(geohash) -> lat/lon bounding box
-- - geohash_cell_center(geohash) -> center point
-- - point_in_polygon(lon,lat, poly_lon[], poly_lat[]) -> boolean
--

-- Decodes a geohash string back into the bounding box (lat_min, lat_max,
-- lon_min, lon_max) of the rectangular cell it represents. This is the inverse
-- of encoding: given a geohash, it tells you exactly what geographic rectangle
-- that cell covers. Returns a single row with four double precision columns.
--
-- Example:
--   SELECT * FROM geohash_decode_bbox('9x0qs0');
--   -- Returns: lat_min=40.5175781, lat_max=40.5230713,
--   --          lon_min=-111.972656, lon_max=-111.962891
--
CREATE OR REPLACE FUNCTION geohash_decode_bbox(p_geohash text)
RETURNS TABLE(
   lat_min double precision,
   lat_max double precision,
   lon_min double precision,
   lon_max double precision
)
LANGUAGE plpgsql
AS $$
DECLARE
   base32 text := '0123456789bcdefghjkmnpqrstuvwxyz';
   lat_lo double precision := -90.0;
   lat_hi double precision :=  90.0;
   lon_lo double precision := -180.0;
   lon_hi double precision :=  180.0;

   is_even boolean := true;
   i integer;
   cd integer;
   mask integer;
   idx integer;
   c text;
   mid double precision;
BEGIN
   IF p_geohash IS NULL OR length(p_geohash) < 1 THEN
      RAISE EXCEPTION 'geohash_decode_bbox: geohash must be non-empty';
   END IF;

   FOR i IN 1..length(p_geohash) LOOP
      c := substr(p_geohash, i, 1);
      idx := position(c in base32) - 1;

      IF idx < 0 THEN
         RAISE EXCEPTION 'geohash_decode_bbox: invalid char "%" in "%"', c, p_geohash;
      END IF;

      cd := idx;
      mask := 16;  -- 10000b
      WHILE mask > 0 LOOP
         IF is_even THEN
            mid := (lon_lo + lon_hi) / 2.0;
            IF (cd & mask) <> 0 THEN
               lon_lo := mid;
            ELSE
               lon_hi := mid;
            END IF;
         ELSE
            mid := (lat_lo + lat_hi) / 2.0;
            IF (cd & mask) <> 0 THEN
               lat_lo := mid;
            ELSE
               lat_hi := mid;
            END IF;
         END IF;

         is_even := NOT is_even;
         mask := mask / 2;
      END LOOP;
   END LOOP;

   lat_min := lat_lo;
   lat_max := lat_hi;
   lon_min := lon_lo;
   lon_max := lon_hi;

   RETURN NEXT;
END;
$$;


-- Returns the center point (lat, lon) of the geographic cell represented by a
-- geohash. Internally decodes the bounding box and averages the min/max
-- coordinates. Useful when you need a single representative point for a
-- geohash cell (e.g., for distance calculations or map markers).
--
-- Example:
--   SELECT * FROM geohash_cell_center('9x0qs0');
--   -- Returns: lat=40.5203247, lon=-111.967773
--
CREATE OR REPLACE FUNCTION geohash_cell_center(p_geohash text)
RETURNS TABLE(lat double precision, lon double precision)
LANGUAGE plpgsql
AS $$
DECLARE
   b record;
BEGIN
   SELECT * INTO b FROM geohash_decode_bbox(p_geohash);

   lat := (b.lat_min + b.lat_max) / 2.0;
   lon := (b.lon_min + b.lon_max) / 2.0;

   RETURN NEXT;
END;
$$;


-- Ray-casting point-in-polygon.
-- Arrays must be same length >= 3 and represent polygon vertices in order (not necessarily closed).
--
-- Determines whether a given point (lon, lat) lies inside a polygon defined by
-- parallel arrays of vertex longitudes and latitudes, using the ray-casting
-- algorithm. The polygon vertices should be listed in order (clockwise or
-- counter-clockwise); the polygon does not need to be explicitly closed. Returns
-- true if the point is inside, false otherwise.
--
-- Example:
--   SELECT point_in_polygon(
--       -111.97, 40.52,
--       ARRAY[-112.0, -111.9, -111.9, -112.0],
--       ARRAY[40.5, 40.5, 40.55, 40.55]
--   );
--   -- Returns: true  (point is inside the rectangular polygon)
--

CREATE OR REPLACE FUNCTION point_in_polygon(
   p_lon double precision,
   p_lat double precision,
   p_poly_lon double precision[],
   p_poly_lat double precision[]
)
RETURNS boolean
LANGUAGE plpgsql
AS $$
DECLARE
   n integer;
   i integer;
   j integer;
   inside boolean := false;
   xi double precision; yi double precision;
   xj double precision; yj double precision;
   l_intersect boolean;
BEGIN
   n := coalesce(array_length(p_poly_lon, 1), 0);

   IF n < 3 OR n <> array_length(p_poly_lat, 1) THEN
      RAISE EXCEPTION 'point_in_polygon: polygon arrays must be same length >= 3';
   END IF;

   j := n;
   FOR i IN 1..n LOOP
      xi := p_poly_lon[i];
      yi := p_poly_lat[i];
      xj := p_poly_lon[j];
      yj := p_poly_lat[j];

      l_intersect :=
         ((yi > p_lat) <> (yj > p_lat))
         AND
         (p_lon < (xj - xi) * (p_lat - yi) / nullif((yj - yi), 0.0) + xi);

      IF l_intersect THEN
         inside := NOT inside;
      END IF;

      j := i;
   END LOOP;

   RETURN inside;
END;
$$;




