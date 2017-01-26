/*
DROP SEQUENCE word_id_seq;
DROP TABLE props;
DROP TABLE words;
*/

CREATE TABLE words 
	( id int
	, lang char check (lang in ('r','e','h'))
	, word text not null
	, PRIMARY KEY (id,lang)
	);
CREATE TABLE props 
	( word_id int 
	, lang char
	, name text
	, val text not null
	, PRIMARY KEY (word_id, lang, name)
	, FOREIGN KEY (word_id, lang) REFERENCES words (id, lang) ON DELETE CASCADE
	);

CREATE SEQUENCE word_id_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
