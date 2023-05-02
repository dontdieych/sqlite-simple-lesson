drop table if exists checkedout;

drop table if exists tools;

drop table if exists users;

create table
  users (id integer PRIMARY KEY, username text);

create table
  tools (
    id integer PRIMARY KEY,
    name text,
    description text,
    lastReturned text,
    timesBorrowed integer
  );

CREATE TABLE
  checkedout (user_id integer, tool_id integer);

INSERT INTO
  users (username)
VALUES
  ('willkurt');

insert into
  tools (name, description, lastReturned, timesBorrowed)
values
  ('hammer', 'hits stuff', '2017-01-01', 0);

insert into
  tools (name, description, lastReturned, timesBorrowed)
values
  ('saw', 'cuts stuff', '2017-01-01', 0);
