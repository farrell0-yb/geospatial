

DELETE FROM my_mapdata;

--  This command must be on a single line
--
\copy my_mapdata FROM '20_mapData.pipe' WITH (FORMAT csv, DELIMITER '|', HEADER true, ROWS_PER_TRANSACTION 100);


ANALYZE;






