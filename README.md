# ffengine
Yet another FriendFeed clone.

Under development.

See also db/

# Quick start

    vagrant up
    vagrant ssh
    cd /vagrant
    cat db/roles.sql | sudo su - postgres -c "psql"
    echo "localhost:5432:ffengine:ffengine:ffengine" >> ~/.pgpass
    chmod 0600 ~/.pgpass
    psql -d ffengine -h localhost -U ffengine -f db/schema.sql
    psql -d ffengine -h localhost -U ffengine -f db/functions.sql
    make run

From another terminal:

    for i in 1 2 3; do curl -X POST -d "username=user$i&password=pwd$i&email=email$i" localhost:3000/v1/users/create; done
    for i in 1 2 3; do curl -i -X POST -d "username=user$i&password=pwd$i" localhost:3000/v1/sessions; done

Save authToken values as `export TOKEN1=...` etc.
Subscribe user1 on user2 and user3, user2 on user3 and user3 on user1:

    curl -X POST -H "X-Authentication-Token: $TOKEN1" -d "" localhost:3000/v1/users/subscribe/user2
    curl -X POST -H "X-Authentication-Token: $TOKEN1" -d "" localhost:3000/v1/users/subscribe/user3
    curl -X POST -H "X-Authentication-Token: $TOKEN2" -d "" localhost:3000/v1/users/subscribe/user3
    curl -X POST -H "X-Authentication-Token: $TOKEN3" -d "" localhost:3000/v1/users/subscribe/user1

user1 posts something:

    curl -X POST -H "X-Authentication-Token: $TOKEN1" -d "body=post1 from user1" localhost:3000/v1/posts

You'll get a permalink back, use it in the next request:

    curl localhost:3000/v1/posts/user1/PERMALINK1

Now check out user1's feed:

    curl -H "X-Authentication-Token: $TOKEN1" localhost:3000/v1/feeds/home

Create one more post:

    curl -X POST -H "X-Authentication-Token: $TOKEN1" -d "body=post2 from user1" localhost:3000/v1/posts

post2 must be the first one in the feed:

    curl -H "X-Authentication-Token: $TOKEN1" localhost:3000/v1/feeds/home

Add a comment from user2 to post1:

    curl -X POST -H "X-Authentication-Token: $TOKEN2" -d "body=comment1 to post1 from user2" localhost:3000/v1/comments/user1/PERMALINK1

Now post1 should be the first one:

    curl -H "X-Authentication-Token: $TOKEN1" localhost:3000/v1/feeds/home
