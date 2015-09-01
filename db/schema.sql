-- df English: word=/etc/dictionaries-common/words

create sequence users_user_id_seq;
create table users ( -- df: mult=1.0
    user_id integer primary key not null default nextval('users_user_id_seq'::regclass),
    username text unique not null, -- df: length=12 lenvar=5
    screen_name text,
    pwdhash text not null,
    email text not null,
    is_private boolean not null default false,
    created_at timestamptz not null,
    updated_at timestamptz not null
);

create sequence tokens_token_id_seq;
create table tokens ( -- df: mult=1.0
    token_id integer primary key not null default nextval('tokens_token_id_seq'::regclass),
    user_id integer not null references users on delete cascade,  -- df: sub=serand
    token text unique not null, -- df: length=20
    token_name text not null,
    created_at timestamptz not null,
    valid_until timestamptz not null
);

create sequence groups_group_id_seq;
create table groups ( -- df: mult=0.5
    group_id integer primary key not null default nextval('groups_group_id_seq'::regclass),
    group_name text unique not null, -- df: length=12 lenvar=5
    group_screen_name text,
    group_description text,
    is_private boolean,
    created_at timestamptz not null,
    updated_at timestamptz not null
);

create sequence users_groups_user_group_id_seq;
create table users_groups ( -- df: mult=1.0
    user_group_id integer primary key not null default nextval('users_groups_user_group_id_seq'::regclass),
    user_id integer not null references users on delete cascade,  -- df: sub=serand
    group_id integer not null references groups on delete cascade,  -- df: sub=serand
    joined_on timestamptz not null,
    unique(user_id, group_id)
);
create index users_groups_user_id on users_groups(user_id);
create index users_groups_group_id on users_groups(group_id);

create sequence posts_post_id_seq;
create table posts ( -- df: mult=100.0
    post_id integer primary key not null default nextval('posts_post_id_seq'::regclass),
    user_id integer not null references users on delete cascade,  -- df: sub=serand
    body text not null, -- df: text=English length=30 lenvar=20
    permalink text not null, -- df: length=20
    created_at timestamptz not null,
    updated_at timestamptz not null,
    rating bigint not null,
    unique(user_id, permalink)
);
create index posts_user_id on posts(user_id);
create index posts_rating on posts(rating);

-- create sequence post_activities_post_activity_id_seq;
-- create table post_activities (
--     post_activity_id integer primary key not null default nextval('post_activities_post_activity_id_seq'::regclass),
--     post_activity text not null
-- );
-- insert into post_activities(post_activity) values ('comment'), ('reaction'), ('edit');

create type post_activity as enum('comment', 'reaction', 'edit');
create table post_activity_hist (
    post_id integer not null references posts on delete cascade, -- df: sub=serand
    user_id integer not null references users on delete cascade, -- df: sub=serand
    post_activity post_activity not null,
    created_at timestamptz not null
);
-- create index post_activity_hist_post_id on post_activity_hist(post_id);
-- create index post_activity_hist_user_id on post_activity_hist(user_id);

create sequence comments_comment_id_seq;
create table comments ( -- df: mult=1000.0
    comment_id integer primary key not null default nextval('comments_comment_id_seq'::regclass),
    user_id integer not null references users on delete cascade, -- df: sub=serand
    post_id integer not null references posts on delete cascade, -- df: sub=serand
    body text, -- df: text=English length=15 lenvar=14
    created_at timestamptz not null,
    updated_at timestamptz not null
);
create index comments_user_id on comments(user_id);
create index comments_post_id on comments(post_id);

create sequence tags_tag_id_seq;
create table tags ( -- df: mult=100.0
    tag_id integer primary key not null default nextval('tags_tag_id_seq'::regclass),
    tag text not null -- df: prefix=# length=10 lenvar=3
);
create index tags_text on tags(tag);

create table post_tags (
    post_id integer not null references posts on delete cascade, -- df: sub=serand
    tag_id integer not null references tags on delete cascade, -- df: sub=serand
    primary key (post_id, tag_id)
);
create index tags_tag_id on post_tags(tag_id);
create index tags_post_id on post_tags(post_id);

-- create sequence reaction_types_reaction_type_id_seq;
-- create table reaction_types ( -- df: size=1 nogen
--     reaction_type_id integer primary key not null default nextval('reaction_types_reaction_type_id_seq'::regclass),
--     reaction_type text
-- );
-- insert into reaction_types(reaction_type) values ('like');

create type reaction_type as enum('like');
create sequence reactions_reaction_id_seq;
create table reactions ( -- df: mult=100
    reaction_id integer primary key not null default nextval('reactions_reaction_id_seq'::regclass),
    user_id integer not null references users on delete cascade, -- df: sub=serand
    post_id integer not null references posts on delete cascade, -- df: sub=serand
    reaction_type reaction_type not null default 'like',
    created_at timestamptz not null,
    unique(user_id, post_id)
);
create index reactions_user_id on reactions(user_id);
create index reactions_post_id on reactions(post_id);

-- create sequence channel_types_channel_type_id_seq;
-- create table channel_types ( -- df: size=4 nogen
--     channel_type_id integer primary key not null default nextval('channel_types_channel_type_id_seq'::regclass),
--     channel_type text not null
-- );
-- insert into channel_types(channel_type) values ('posts'), ('comments'), ('reactions'), ('tags');

-- create sequence channel_modes_channel_mode_id_seq;
-- create table channel_modes ( -- df: size=3 nogen
--     channel_mode_id integer primary key not null default nextval('channel_modes_channel_mode_id_seq'::regclass),
--     channel_mode text not null
-- );
-- insert into channel_modes(mode) values ('public'), ('protected'), ('private');

create type channel_mode as enum('public', 'protected', 'private');
create type channel_type as enum('posts', 'comments', 'reactions', 'tags');
create sequence channels_channel_id_seq;
create table channels ( -- df: mult=3.0
    channel_id integer primary key not null default nextval('channels_channel_id_seq'::regclass),
    channel_mode channel_mode not null,
    channel_type channel_type not null
);

create table user_channels ( -- df: mult=3.0
    user_id integer not null references users on delete cascade, -- df: sub=serand
    channel_id integer not null references channels on delete cascade, -- df: sub=serand
    primary key (user_id, channel_id)
);
create index user_channels_channel_id on user_channels(channel_id);
create index user_channels_user_id on user_channels(user_id);

create table group_channels (
    group_id integer not null references groups on delete cascade, -- df: sub=serand
    channel_id integer not null references channels on delete cascade, -- df: sub=serand
    primary key (group_id, channel_id)
);
create index group_channels_channel_id on group_channels(channel_id);
create index group_channels_group_id on group_channels(group_id);

create table post_channels ( -- df: mult=300.0
    post_id integer not null references posts on delete cascade, -- df: sub=serand
    channel_id integer not null references channels on delete cascade, -- df: sub=serand
    primary key (post_id, channel_id)
);
create index post_channels_post_id on post_channels(post_id);
create index post_channels_channel_id on post_channels(channel_id);

create sequence feeds_feed_id_seq;
create table feeds ( -- df: mult=2.0
    feed_id integer primary key not null default nextval('feeds_feed_id_seq'::regclass),
    user_id integer not null references users on delete cascade, -- df: sub=serand
    feed_name text not null,  -- df: length=12 lenvar=4
    unique(feed_id, user_id)
);
create index feeds_user_id on feeds(user_id);

create table feed_channels ( -- df: mult=4.0
    feed_id integer not null references feeds on delete cascade, -- df: sub=serand
    channel_id integer not null references channels on delete cascade, -- df: sub=serand
    primary key (feed_id, channel_id)
);
create index feed_channels_feed_id on feed_channels(feed_id);
create index feed_channels_user_id on feed_channels(channel_id);

alter table users owner to ffengine;
alter table groups owner to ffengine;
alter table users_groups owner to ffengine;
alter table posts owner to ffengine;
alter table comments owner to ffengine;
alter table tags owner to ffengine;
alter table post_tags owner to ffengine;
-- alter table reaction_types owner to ffengine;
alter table reactions owner to ffengine;
-- alter table channel_types owner to ffengine;
-- alter table channel_modes owner to ffengine;
alter table channels owner to ffengine;
alter table user_channels owner to ffengine;
alter table group_channels owner to ffengine;
alter table post_channels owner to ffengine;
alter table feeds owner to ffengine;
alter table feed_channels owner to ffengine;

