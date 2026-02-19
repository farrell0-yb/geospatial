# Geospatial Queries with YugabyteDB using Geohash

This repository demonstrates the use of geohash on top of YugabyteDB as a means to serve geospatial queries using YSQL only.

---

## Table of Contents

- [Overview](#overview)
- [First Group: Web-Based GPS Demonstration](#first-group-web-based-gps-demonstration)
- [Second Group: Server-Side Functionality](#second-group-server-side-functionality)
- [Third Group: Polygon Queries](#third-group-polygon-queries)
- [Fourth Group: PostGIS-Compatible Functions](#fourth-group-postgis-compatible-functions)

---

## Overview

The contents of this repository are organized into four groups, each demonstrating different aspects of geospatial query capabilities with YugabyteDB.

---

## First Group: Web-Based GPS Demonstration

Delivers a web-based demonstration program similar to a GPS display in an automobile navigation system.

### Files

- **File 10\***: Creates a single base table
- **File 15\***: Loads the table
- **File 20\***: Contains 344,000 rows of data for the US state of Colorado

### Query Examples

#### Q001: Single Geohash Lookup

```sql
EXPLAIN ANALYZE 
SELECT md_lat, md_lng, md_name, md_mysource, md_phone 
FROM my_mapdata 
WHERE left(geo_hash10, 6) = '9xjr16'  -- Match the index expression
LIMIT 50;
```

**Performance:**
```
Limit  (cost=0.00..6.26 rows=50 width=160) (actual time=3.855..3.865 rows=14 loops=1)
  ->  Index Scan using ix_mapdata4 on my_mapdata
      Index Cond: ("left"(geo_hash10, 6) = '9xjr16'::text)
Planning Time: 0.061 ms
Execution Time: 3.906 ms
```

#### Q002: Neighbor Geohash Query

```sql
EXPLAIN ANALYZE 
WITH nbrs AS (
   SELECT 'n' AS dir, '9xjr0' AS gh
   UNION ALL SELECT 's' AS dir, '9xjq8' AS gh
   UNION ALL SELECT 'e' AS dir, '9xjqc' AS gh 
   UNION ALL SELECT 'w' AS dir, '9xjnz' AS gh
   UNION ALL SELECT 'ne' AS dir, '9xjr1' AS gh
   UNION ALL SELECT 'nw' AS dir, '9xjpp' AS gh
   UNION ALL SELECT 'se' AS dir, '9xjq9' AS gh
   UNION ALL SELECT 'sw' AS dir, '9xjnx' AS gh
)
SELECT m.md_lat, m.md_lng, m.md_name, m.md_mysource, m.md_phone, n.dir
FROM my_mapdata m JOIN nbrs n ON left(m.geo_hash10, 5) = n.gh
LIMIT 50;
```

**Performance:**
```
Limit  (cost=0.00..1689.52 rows=50 width=192) (actual time=3.457..3.490 rows=50 loops=1)
  ->  Nested Loop  (cost=0.00..35851.69 rows=1061 width=192)
      ->  Append  (cost=0.00..0.12 rows=8 width=64)
      ->  Index Scan using ix_mapdata3 on my_mapdata m
Planning Time: 0.147 ms
Execution Time: 3.574 ms
```

---

## Second Group: Server-Side Functionality

Adds more server-side functionality to reduce client-side complexity and eliminate the need for client-side geohash libraries.

### Files

- **File 30\***: Creates stored procedures that eliminate the need for client-side geohash libraries
- **File 31\* and 34\***: Store geohash-encoded data as CHAR(08) for exact matching without trailing wildcards

### Benefits

- Reduces application complexity
- Centralizes geohash logic on the server
- Enables exact equality comparisons instead of pattern matching

### Query Examples

#### Q003: Simplified Neighbor Query (Initial Approach)

```sql
EXPLAIN ANALYZE
WITH nbrs AS (    
   SELECT key AS dir, value AS gh    
   FROM jsonb_each_text(geohash_neighbors('9xjr1')) 
) 
SELECT m.md_lat, m.md_lng, m.md_name, m.md_mysource, m.md_phone, n.dir 
FROM my_mapdata m 
JOIN nbrs n ON m.geo_hash5 = n.gh  -- Use indexed column instead of left()
LIMIT 50;
```

**Note:** The optimizer chooses hash join due to 8 compass points. Q004 provides an alternative approach.

**Performance:**
```
Limit  (cost=2.25..138.04 rows=50 width=192) (actual time=4.234..12.208 rows=50 loops=1)
  ->  Hash Join  (cost=2.25..35895.80 rows=13217 width=192)
      Hash Cond: (m.geo_hash5 = jsonb_each_text.value)
      ->  Seq Scan on my_mapdata m  (cost=0.00..34468.80 rows=344688 width=192)
Planning Time: 0.251 ms
Execution Time: 12.257 ms
```

#### Q004: Optimized Neighbor Query (Using ANY)

```sql
EXPLAIN ANALYZE
SELECT m.md_lat, m.md_lng, m.md_name, m.md_mysource, m.md_phone
FROM my_mapdata m
WHERE m.geo_hash5 = ANY(ARRAY(
   SELECT value 
   FROM jsonb_each_text(geohash_neighbors('9xjr1'))
))
LIMIT 50;
```

**Performance:**
```
Limit  (cost=1.00..7.69 rows=50 width=160) (actual time=4.589..4.642 rows=50 loops=1)
  InitPlan 1 (returns $0)
    ->  Function Scan on jsonb_each_text
  ->  Index Scan using my_mapdata_geo_hash5_idx on my_mapdata m
      Index Cond: (geo_hash5 = ANY ($0))
Planning Time: 0.590 ms
Execution Time: 4.684 ms
```

---

## Third Group: Polygon Queries

Demonstrates polygon queries without new server-side stored procedures—just different query patterns.

### Query Examples

#### Q005: Generate Bounding Boxes Within Polygon

Helper method that generates geohash bounding boxes contained within a polygon. This method is deterministic.

```sql
EXPLAIN ANALYZE
SELECT geohash8_fully_within_polygon(
   '9xj6hj07xg', 
   '9xj65t471y', 
   '9xj65db1vc', 
   '9xj6h50h9c'
) AS gh8;
```

**Performance:**
```
ProjectSet  (cost=0.00..5.27 rows=1000 width=32) 
            (actual time=686.670..686.798 rows=3938 loops=1)
  ->  Result *RESULT*  (cost=0.00..0.01 rows=1 width=0)
Planning Time: 0.020 ms
Execution Time: 687.277 ms
```

#### Q006: Query Using Generated Bounding Boxes

Uses the output from the method above to query actual data.

```sql
EXPLAIN ANALYZE
SELECT md_lat, md_lng, md_name 
FROM my_mapdata
WHERE geo_hash8 = ANY(ARRAY[
   '9xj65db7','9xj65dbe','9xj65dbg','9xj65dc5'
])
LIMIT 50;
```

**Performance:**
```
Limit  (cost=0.00..6.31 rows=50 width=96) (actual time=9.570..9.576 rows=1 loops=1)
  ->  Index Scan using my_mapdata_geo_hash8_idx on my_mapdata
      Index Cond: (geo_hash8 = ANY ('{9xj65db7,9xj65dbe,9xj65dbg,9xj65dc5}'::text[]))
Planning Time: 0.059 ms
Execution Time: 9.621 ms
```

---

## Fourth Group: PostGIS-Compatible Functions

Implements PostGIS functions requested by customers, delivered as server-level stored procedures.

### Files

- **File 40\***: Creates stored procedures to deliver PostGIS-compatible methods at the server level

### Query Examples

#### Q007: ST_Contains

Tests if one polygon contains another.

```sql
EXPLAIN ANALYZE
SELECT lm_st_contains(
   ARRAY[-112.0, -111.8, -111.8, -112.0],
   ARRAY[40.4,   40.4,   40.6,   40.6],
   ARRAY[-111.95, -111.85, -111.85, -111.95],
   ARRAY[40.45,   40.45,   40.55,   40.55]
);
```

**Performance:**
```
Result *RESULT*  (cost=0.00..0.01 rows=1 width=1) 
                 (actual time=0.000..0.000 rows=1 loops=1)
Planning Time: 4.759 ms
Execution Time: 0.012 ms
```

#### Q008: ST_XMax

Returns the maximum X coordinate of a polygon.

```sql
EXPLAIN ANALYZE
SELECT lm_st_xmax(ARRAY[-112.0, -111.9, -111.9, -112.0]);
```

**Performance:**
```
Result *RESULT*  (cost=0.00..0.01 rows=1 width=8) 
                 (actual time=0.000..0.001 rows=1 loops=1)
Planning Time: 0.234 ms
Execution Time: 0.077 ms
```

#### Q009: ST_Translate

Translates (shifts) a polygon by given X and Y offsets.

```sql
EXPLAIN ANALYZE
SELECT * FROM lm_st_translate(
   ARRAY[-112.0, -111.9, -111.9, -112.0],
   ARRAY[40.5,   40.5,   40.55,  40.55],
   0.01,
   -0.02
);
```

**Performance:**
```
Function Scan on lm_st_translate  (cost=0.25..10.25 rows=1000 width=64) 
                                  (actual time=0.235..0.236 rows=1 loops=1)
Planning Time: 0.045 ms
Execution Time: 0.257 ms
```

#### Q010: ST_Intersects

Tests if two polygons intersect.

```sql
EXPLAIN ANALYZE
SELECT lm_st_intersects(
   ARRAY[-112.0, -111.9, -111.9, -112.0],
   ARRAY[40.5,   40.5,   40.55,  40.55],
   ARRAY[-111.95, -111.85, -111.85, -111.95],
   ARRAY[40.52,   40.52,   40.57,   40.57]
);
```

**Performance:**
```
Result *RESULT*  (cost=0.00..0.01 rows=1 width=1) 
                 (actual time=0.000..0.000 rows=1 loops=1)
Planning Time: 0.368 ms
Execution Time: 0.014 ms
```

---

## Performance Summary

| Query | Description | Execution Time | Notes |
|-------|-------------|----------------|-------|
| Q001 | Single geohash lookup | 3.906 ms | Uses index scan |
| Q002 | 8-neighbor search | 3.574 ms | Nested loop with index |
| Q003 | Server-side neighbors (initial) | 12.257 ms | Hash join, seq scan |
| Q004 | Server-side neighbors (optimized) | 4.684 ms | Index scan with ANY |
| Q005 | Generate polygon bounding boxes | 687.277 ms | Returns 3,938 geohashes |
| Q006 | Query with bounding boxes | 9.621 ms | Index scan |
| Q007 | ST_Contains | 0.012 ms | Geometric calculation |
| Q008 | ST_XMax | 0.077 ms | Coordinate extraction |
| Q009 | ST_Translate | 0.257 ms | Polygon transformation |
| Q010 | ST_Intersects | 0.014 ms | Intersection test |

---

## Key Takeaways

- **Geohash precision** matters: Use appropriate precision levels (5, 6, 8, 10) for different use cases
- **Index strategy**: Create indexes on computed geohash columns for better performance
- **Query optimization**: Use `ANY(ARRAY(...))` pattern instead of JOIN for better index utilization
- **Server-side functions**: Centralize geospatial logic on the server to reduce client complexity
- **PostGIS compatibility**: Custom stored procedures can provide PostGIS-like functionality

---

## License

[Add your license here]

## Contributing

[Add contribution guidelines here]

## Contact

[Add contact information here]
