create or replace function create_user(username text, pwdhash text, email text, ts timestamptz,
                                       token text, token_name text, token_lifetime text)
returns boolean as $$
  with
    -- create user
    insert_user as (
      insert into users(username, pwdhash, email, created_at, updated_at) values
        ($1, $2, $3, $4, $4) returning user_id),
    -- create public channels of each type
    insert_channels as (
      insert into channels(channel_mode, channel_type)
        values ('public', 'posts'), ('public', 'comments'), ('public', 'reactions'), ('public', 'tags')
          returning channel_id),
    -- insert new records into user_channels
    insert_user_channels as (
      insert into user_channels(user_id, channel_id)
        select user_id, channel_id from insert_user, insert_channels
          returning *),
    -- create home feed
    insert_feed as (
      insert into feeds(user_id, feed_name)
        select user_id, 'home' as feed_name from insert_user
          returning feed_id),
    -- insert new records into feed_channels
    insert_feed_channels as (
      insert into feed_channels(feed_id, channel_id)
        select feed_id, channel_id from insert_feed, insert_channels
          returning *),
    insert_token as (
      insert into tokens (user_id, token, token_name, created_at, valid_until)
        select user_id, $5, $6, $4, $4::timestamptz + $7::interval from insert_user
          returning token_id)
    select (select count(*) = 1 from insert_user) and
           (select count(*) = 4 from insert_channels) and
           (select count(*) = 4 from insert_user_channels) and
           (select count(*) = 1 from insert_feed) and
           (select count(*) = 4 from insert_feed_channels) and
           (select count(*) = 1 from insert_token);
$$ language sql;

drop type post cascade;
create type post as (username text, permalink text, body text,
                     created_at timestamptz, updated_at timestamptz, rating bigint,
                     comments json, reactions json);
create or replace function get_post(postid integer)
returns post as $$
  select u.username, p.permalink, p.body, p.created_at, p.updated_at, p.rating,
    json_agg(c.*) as comments, json_agg(r.*) as reactions
    from posts p join users u using(user_id)
    left join comments c using (post_id)
    left join reactions r using(post_id)
    where p.post_id = $1 group by p.post_id, u.username;
$$ language sql stable;

create or replace function get_post(username text, permalink text)
returns post as $$
  select u.username, p.permalink, p.body, p.created_at, p.updated_at, p.rating,
    json_agg(c.*) as comments, json_agg(r.*) as reactions
    from  users u join posts p using (user_id)
    left join comments c using (post_id)
    left join reactions r using(post_id)
    where u.username = $1 and p.permalink = $2 group by p.post_id, u.username;
$$ language sql stable;

create or replace function create_post(userid integer, permalink text, body text, createts timestamptz, rating bigint)
returns post as $$
  with
    channel as (
        select channel_id from user_channels
          join users using (user_id)
          join channels using(channel_id)
          where user_id = $1 and channel_type = 'posts'),
    insert_post as (
      insert into posts (user_id, permalink, body, created_at, updated_at, rating)
        values ($1, $2, $3, $4, $4, $5) returning post_id),
    insert_post_channels as (
      insert into post_channels (post_id, channel_id)
        select post_id, channel_id from insert_post, channel returning post_id)
  select u.username, $2 as permalink, $3 as body, $4 as created_at, $4 as updated_at,
    $5 as rating, '{}'::json as comments, '{}'::json as reactions from users u where user_id = $1;
$$ language sql;

create or replace function is_post_public(username text, permalink text)
returns boolean as $$
  select count(post_channels.channel_id) > 0
    from posts
    join users using (user_id)
    join post_channels using (post_id)
    join channels using (channel_id)
  where users.username = $1 and posts.permalink = $2 and channel_mode = 'public';
$$ language sql stable;

create or replace function can_read_post(token text, username text, permalink text, ts timestamptz)
returns boolean as $$
  with
    this_user as (select user_id from tokens where token = $1 and valid_until > $4),
    this_post_channels as (select channel_id from posts join users using (user_id)
      join post_channels using (post_id) where username = $2 and permalink = $3 group by channel_id)
  select is_post_public(username, permalink) or
    count(channel_id) > 0 from this_user join feeds using (user_id)
    join feed_channels using (feed_id)
    join this_post_channels using (channel_id) where user_id = this_user.user_id;
$$ language sql stable;

drop type comment cascade;
create type comment as (username text, body text, created_at timestamptz);
create or replace function create_comment(token text, username text, post_permalink text,
                                          body text, ts timestamptz, rating bigint)
returns comment as $$
  with
    this_user as (select user_id from tokens where token = $1),
    post as (
      select post_id from posts
        join users using (user_id)
        join post_channels using (post_id)
        where users.username = $2 and posts.permalink = $3),
    channel as (
        select channel_id from user_channels
          join this_user using (user_id)
          join channels using(channel_id)
          where channel_type = 'comments'),
    insert_comment as (
      insert into comments (user_id, post_id, body, created_at, updated_at)
        select user_id, post_id, $4 as body, $5 as created_at, $5 as updated_at
          from this_user, post returning comment_id),
    insert_post_channels as (
      insert into post_channels (post_id, channel_id)
        select post_id, channel_id from post, channel
        where not exists (select post_id, channel_id from post_channels)),
    insert_post_activity_hist as (
      insert into post_activity_hist (post_id, post_activity, user_id, created_at)
        select post_id, 'comment', user_id, $5 from this_user, post),
    -- TODO: decide whether to update or not update post rating depending on what's in post_activity_hist
    -- for now it's always updated
    update_post_rating as (
      update posts set rating = $6 where post_id in (select post_id from post))
    select $1 as username, $2 as body, $5 as created_at;
$$ language sql;

create or replace function subscribe_on(userid integer, username text)
returns boolean as $$
  -- TODO: feed name as argument
  with
    user_feed as (select feed_id from feeds where user_id = $1 and feed_name = 'home'),
    target_channels as (select channel_id from users join user_channels using (user_id) where username = $2),
    insert_feed_channels as (
      insert into feed_channels (feed_id, channel_id)
        select feed_id, channel_id from user_feed, target_channels returning *)
    select count(*) > 0 from insert_feed_channels;
$$ language sql;

create or replace function get_feed(userid integer, feedname text, feedlimit integer)
returns setof post as $$
  with
    post_ids as (
      select post_id from feeds f
        join feed_channels using (feed_id)
        join post_channels using (channel_id)
        join posts using (post_id)
        where f.user_id = $1 and f.feed_name = $2
        order by rating desc limit $3)
  select u.username, p.permalink, p.body, p.created_at, p.updated_at, p.rating,
    json_agg(c.*) as comments, json_agg(r.*) as reactions
    from posts p join users u using(user_id)
    left join comments c using(post_id)
    left join reactions r using(post_id)
    where post_id in (select post_id from post_ids)
    group by p.post_id, u.username
    order by rating desc;
$$ language sql stable;
