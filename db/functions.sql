create type comment as (username text, body text, created_at timestamptz);
create type reaction as (username text, reaction_type reaction_type, created_at timestamptz);
create type post as (username text, permalink text, body text,
                     created_at timestamptz, updated_at timestamptz, post_rating int8,
                     comments json, reactions json);

create or replace function get_post(post_id int8)
returns post as $$
  select u.username, p.permalink, p.body, p.created_at, p.updated_at, p.post_rating,
     json_agg(distinct ((select username from users where user_id = c.user_id), c.body, c.created_at)::comment) as comments,
     json_agg(distinct ((select username from users where user_id = r.user_id), r.reaction_type, r.created_at)::reaction) as reactions
     from posts p join users u using(user_id)
     left join comments c using (post_id)
     left join reactions r using(post_id)
     where p.post_id = $1 group by p.post_id, u.username;
$$ language sql stable;

create or replace function is_post_public(username text, permalink text)
returns table (post_id int8) as $$
  select post_channels.post_id
    from posts
    join users using (user_id)
    join post_channels using (post_id)
    join channels using (channel_id)
  where users.username = $1 and posts.permalink = $2 and channel_mode = 'public';
$$ language sql stable;

create or replace function can_read_post(user_id int8, username text, permalink text)
returns table (post_id int8) as $$
  with
    this_post_channels as (
      select channel_id, post_id from posts
        join users using (user_id)
        join post_channels using (post_id)
        where username = $2 and permalink = $3 group by channel_id, post_id)
  select post_id from feeds f
    join feed_channels using (feed_id)
    join this_post_channels using (channel_id)
    where f.user_id = $1
  union all
  select * from is_post_public($2, $3);
$$ language sql stable;

create or replace function subscribe_on(userid int8, feedname text, username text)
returns table (feed_id int8, channel_id int8) as $$
  with
    user_feed as (select feed_id from feeds where user_id = $1 and feed_name = $2),
    target_channels as (select channel_id from users join user_channels using (user_id) where username = $3)
    insert into feed_channels (feed_id, channel_id)
      select feed_id, channel_id from user_feed, target_channels returning *
$$ language sql;

create or replace function get_feed(userid int8, feedname text, feedlimit integer, feedoffset integer)
returns setof post as $$
  with
    post_ids as (
      select post_id from feeds f
        join feed_channels using (feed_id)
        join post_channels using (channel_id)
        join posts using (post_id)
        where f.user_id = userid and f.feed_name = feedname
        order by post_rating desc limit feedlimit offset feedoffset)
  select u.username, p.permalink, p.body, p.created_at, p.updated_at, p.post_rating,
    json_agg(c.* order by c.created_at asc) as comments, json_agg(distinct r.*) as reactions
    from posts p join users u using(user_id)
    left join comments c using(post_id)
    left join reactions r using(post_id)
    where post_id in (select post_id from post_ids)
    group by p.post_id, u.username;
$$ language sql stable;

drop type subscription cascade;
create type subscription as (username text, channel_mode channel_mode, channel_type channel_type);
create or replace function get_subscriptions(username text, feedname text)
returns setof subscription as $$
  with channel_ids as (
    select channel_id from users u join feeds using(user_id) join feed_channels using(feed_id)
    where u.username = $1 and feed_name = $2)
  select u.username, channel_mode, channel_type from
    channel_ids join channels using(channel_id)
                join user_channels using(channel_id)
                join users u using(user_id);
$$ language sql stable;
