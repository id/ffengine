# Initializing

    cd db
    psql -d ffengine -h localhost -U ffengine -f roles.sql
    psql -d ffengine -h localhost -U ffengine -f schema.sql
    psql -d ffengine -h localhost -U ffengine -f functions.sql

# Generating test data
Size indicates how many users you want in a test database.
Other tables will scale proportionally.

    cd db
    wget https://www.cri.ensmp.fr/people/coelho/datafiller
    chmod +x datafiller
    sudo apt-get install wamerican
    ./datafiller --size=10 schema.sql > test_data.sql
    psql -d ffengine -h localhost -U ffengine -f test_data.sql

# Terminology
## Channels
Channels are containers for posts, for posts commented by, with reactions from, tagged as, posts with a comment tagged as.

A channel can belong to a user (user's posts and direct messages) or to a group.

## Feeds
Feeds are aggregators for channels. A feed can be subscribed to one or more channels.

A user has one feed by default, home feed.

In home feed user gets hers own posts, direct messages, posts created by, commented by or having reactions from users the user subscriebed on.

## Users
Users own channels (where they can post to) and feeds (where they can read from).

## Posts
Only users can create posts.

## Comments
Users create comments for posts.
A comment generates an entry in post_channels table for channel with type 'comments' which belong to the user.

## Reactions
Users react on posts via reactions
A reaction generates an entry in post_channels table for channel with type 'reactions' which belong to the user.

## Tags
Tag is a word preceeded by #. Tags can be in a post body as well as in a comment body. But in both cases tags belong to posts.
A tag generates an entry in post_channels table for channel with type 'tags' which belong to the user.

