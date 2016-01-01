drop type comment cascade;
drop type reaction cascade;
drop type post cascade;
create type comment as (username text, body text, created_at text);
create type reaction as (username text, reaction_type reaction_type, created_at text);
create type post as (username text, permalink text, body text,
                     created_at text, updated_at text, post_rating int8,
                     comments json, reactions json);

create or replace function get_post(post_id int8)
returns post as $$
  select u.username, p.permalink, p.body, p.created_at::text, p.updated_at::text, p.post_rating,
     json_agg(distinct ((select username from users where user_id = c.user_id), c.body, c.created_at::text)::comment) as comments,
     json_agg(distinct ((select username from users where user_id = r.user_id), r.reaction_type, r.created_at::text)::reaction) as reactions
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
    join channels ch using (channel_id)
  where users.username = $1 and posts.permalink = $2 and ch.channel_mode = 'public'
  group by post_channels.post_id;
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
  union
  select post_id from is_post_public($2, $3);
$$ language sql stable;

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
  select u.username, p.permalink, p.body, p.created_at::text, p.updated_at::text, p.post_rating,
     json_agg(distinct ((select username from users where user_id = c.user_id), c.body, c.created_at::text)::comment) as comments,
     json_agg(distinct ((select username from users where user_id = r.user_id), r.reaction_type, r.created_at::text)::reaction) as reactions
    from posts p join users u using(user_id)
    left join comments c using(post_id)
    left join reactions r using(post_id)
    where post_id in (select post_id from post_ids)
    group by p.post_id, u.username
    order by p.post_rating desc;
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

create or replace function get_comment(comment_id int8)
returns comment as $$
  select u.username, c.body, c.created_at::text
     from comments c join users u using(user_id)
     where c.comment_id = $1;
$$ language sql stable;

create or replace function get_user_open_channels(username text)
returns table (channel_id int8) as $$
  select channel_id from user_channels
    join users using (user_id)
    join channels using (channel_id)
    where username = $1 and channel_mode <> 'private';
$$ language sql stable;

create or replace function get_user_private_channels(username text)
returns table (channel_id int8) as $$
  select channel_id from user_channels
    join users using (user_id)
    join channels using (channel_id)
    where username = $1 and channel_mode = 'private';
$$ language sql stable;

