-- Insert Users (update the password hash accordingly)
INSERT INTO users (name, email, password, created_at, last_login)
VALUES ('Virginia Rose', 'v', '$2a$12$NTXfW4ncAcMcVwA9NhEaL.LmqSjVWEmMVa8p.im4WjEGMfR2nVLHe', 
        '2025-12-27 21:32:47', '2026-01-04 11:41:25');

-- Insert Movies/Series
INSERT INTO movies_series (id, title, media_type, genre, year, watch_status, rating, review, 
    director, cast_members, plot_summary, poster_url, total_episodes, date_added, date_watched, 
    date_modified, total_duration, watched_duration, total_seasons, current_season, 
    episodes_current_season, current_episode, total_episodes_watched, tmdb_id)
VALUES 
(1, 'Interstellar', 'Movie', 'Adventure, Drama, Science Fiction', 2014, 'Watched', 5, 'Wow', 
 'Christopher Nolan', 'Matthew McConaughey, Anne Hathaway, Michael Caine, Jessica Chastain, Casey Affleck, Wes Bentley, Topher Grace, Mackenzie Foy, Ellen Burstyn, John Lithgow', 
 'The adventures of a group of explorers who make use of a newly discovered wormhole to surpass the limitations on human space travel and conquer the vast distances involved in an interstellar voyage.', 
 'https://image.tmdb.org/t/p/w500/gEU2QniE6E77NI6lCU6MxlNBvIx.jpg', 0, '2026-01-01 20:33:35', '2026-01-02', 
 '2026-01-01 20:33:35', 169, 169, 0, 0, 0, 0, 0, NULL),
 
(2, 'Friends', 'TV Series', 'Comedy', 1994, 'Watched', 5, 'Pivot', 
 'Marta Kauffman, David Crane', 'Jennifer Aniston, Courteney Cox, Lisa Kudrow, Matt LeBlanc, Matthew Perry, David Schwimmer', 
 'Six young people from New York City, on their own and struggling to survive in the real world, find the companionship, comfort and support they get from each other to be the perfect antidote to the pressures of life.', 
 'https://image.tmdb.org/t/p/w500/f496cm9enuEsZkSPzCwnTESEK5s.jpg', 228, '2026-01-01 20:34:26', '2026-01-02', 
 '2026-01-01 20:34:26', 0, 0, 10, 10, 24, 24, 228, NULL),
 
(4, 'The Great Flood', 'Movie', 'Science Fiction, Adventure, Drama', 2025, 'Unwatched', 0, '', 
 'Kim Byung-woo', 'Kim Da-mi, Park Hae-soo, Kwon Eun-seong, Jeon Hye-jin, Park Byung-eun, Lee Hak-ju, Yuna, Park Mi-hyeon, Lee Dong-chan, Kwon Min-kyung', 
 'When a raging flood traps a researcher and her young son, a call to a crucial mission puts their escape — and the future of humanity — on the line.', 
 'https://image.tmdb.org/t/p/w500/1tUOZQDgZaGqZtrB21MieiXARL2.jpg', 0, '2026-01-01 20:34:50', NULL, 
 '2026-01-01 20:34:50', 109, 0, 0, 0, 0, 0, 0, NULL),
 
(7, 'The Wild Robot', 'Movie', 'Adventure, Animation, Drama, Family, Science Fiction', 2024, 'Watched', 5, 'a', 
 'Chris Sanders', 'Lupita Nyong''o, Pedro Pascal, Kit Connor, Bill Nighy, Stephanie Hsu, Matt Berry, Ving Rhames, Mark Hamill, Catherine O''Hara, Boone Storm', 
 'After a shipwreck, an intelligent robot called Roz is stranded on an uninhabited island. To survive the harsh environment, Roz bonds with the island''s animals and cares for an orphaned baby goose.', 
 'https://image.tmdb.org/t/p/w500/wTnV3PCVW5O92JMrFvvrRcV39RU.jpg', 0, '2026-01-01 20:56:04', '2026-01-02', 
 '2026-01-01 23:44:53', 102, 102, 0, 0, 0, 0, 0, NULL),
 
(8, 'The Martian', 'Movie', 'Drama, Adventure, Science Fiction', 2015, 'Unwatched', 0, '', 
 'Ridley Scott', 'Matt Damon, Jessica Chastain, Kristen Wiig, Jeff Daniels, Michael Peña, Sean Bean, Kate Mara, Sebastian Stan, Aksel Hennie, Chiwetel Ejiofor', 
 'During a manned mission to Mars, Astronaut Mark Watney is presumed dead after a fierce storm and left behind by his crew. But Watney has survived and finds himself stranded and alone on the hostile planet. With only meager supplies, he must draw upon his ingenuity, wit and spirit to subsist and find a way to signal to Earth that he is alive.', 
 'https://image.tmdb.org/t/p/w500/3ndAx3weG6KDkJIRMCi5vXX6Dyb.jpg', 0, '2026-01-01 20:56:22', NULL, 
 '2026-01-01 20:56:22', 141, 0, 0, 0, 0, 0, 0, NULL),
 
(11, 'Sex and the City', 'TV Series', 'Drama, Comedy', 1998, 'Unwatched', 0, '', 
 'Darren Star', 'Sarah Jessica Parker, Kim Cattrall, Cynthia Nixon, Kristin Davis', 
 'Based on the bestselling book by Candace Bushnell, Sex and the City tells the story of four best friends, all single and in their late thirties, as they pursue their careers and talk about their sex lives, all while trying to survive the New York social scene.', 
 'https://image.tmdb.org/t/p/w500/jfLp8gTfdi9d8onEFJ60kp1Bl1e.jpg', 94, '2026-01-01 21:14:37', NULL, 
 '2026-01-01 21:14:37', 0, 0, 6, 0, 1, 0, 0, NULL),
 
(12, 'How I Met Your Mother', 'TV Series', 'Comedy', 2005, 'Unwatched', 0, '', 
 'Craig Thomas, Carter Bays', 'Josh Radnor, Neil Patrick Harris, Jason Segel, Alyson Hannigan, Cobie Smulders, Cristin Milioti', 
 'A father recounts to his children - through a series of flashbacks - the journey he and his four best friends took leading up to him meeting their mother.', 
 'https://image.tmdb.org/t/p/w500/b34jPzmB0wZy7EjUZoleXOl2RRI.jpg', 208, '2026-01-01 21:15:11', NULL, 
 '2026-01-01 21:15:11', 0, 0, 9, 0, 1, 0, 0, NULL),
 
(18, 'W-A-L-K', 'Movie', 'Drama, Comedy', 2015, 'Unwatched', 0, '', 
 'Anna Sikorski', 'Madison McAleer, Atif Y. Siddiqi, Brittany Teo, Paul-Eric Hausknost', 
 'During a lazy Montreal summer day, preteen Clara decides to master the art of walking in high-heel shoes. It would probably be an easier undertaking if she didn''t have to take the dog, Eddie, along for the ride.', 
 'https://image.tmdb.org/t/p/w500/kKCCeY8UBc5qA4Vj0Wh5ZZzBPVw.jpg', 0, '2026-01-04 11:33:28', NULL, 
 '2026-01-04 11:33:28', 11, 0, 0, 0, 0, 0, 0, NULL),
 
(19, 'The Walking Dead', 'TV Series', 'Drama', 2010, 'Unwatched', 0, '', 
 'Frank Darabont', 'Lauren Cohan, Norman Reedus, Jeffrey Dean Morgan, Melissa McBride, Christian Serratos, Seth Gilliam, Ross Marquand, Josh McDermitt, Khary Payton, Cooper Andrews', 
 'Sheriff''s deputy Rick Grimes awakens from a coma to find a post-apocalyptic world dominated by flesh-eating zombies. He sets out to find his family and encounters many other survivors along the way.', 
 'https://image.tmdb.org/t/p/w500/ng3cMtxYKt1OSQYqFlnKWnVsqNO.jpg', 177, '2026-01-04 11:34:01', NULL, 
 '2026-01-04 11:34:01', 0, 0, 11, 0, 6, 0, 0, NULL);

-- Insert Watch History
INSERT INTO watch_history (movie_id, watch_date, created_at)
VALUES 
(1, '2026-01-02', '2026-01-01 20:33:35'),
(2, '2026-01-02', '2026-01-01 20:34:26'),
(7, '2026-01-02', '2026-01-01 23:44:53');

-- Reset sequences to correct values
SELECT setval('users_id_seq', (SELECT MAX(id) FROM users));
SELECT setval('movies_series_id_seq', (SELECT MAX(id) FROM movies_series));
SELECT setval('watch_history_id_seq', (SELECT MAX(id) FROM watch_history));