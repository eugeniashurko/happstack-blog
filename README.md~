Simple blog Web-application using HappStack
===========================================

Happstack is the Haskell application server stack. It is
primarily used for web application development, but it can be
used for any type of application server.

### Requirements
  - happstack
  - BlazeHtml
  - acid-state

You can install it using The Haskell Cabal

    $ cabal update
    $ cabal install happstack-server

#### Templating for HTML

Templating library _BlazeHtml_ used. It is fast, has automatic
escaping, output is always well-formed and able to use the power
of Haskell in your templates.

#### Database

_acid-state_ is a NoSQL, RAM-cloud, persistent data store. One of
attractive feature is that it's designed to store arbitrary Haskell datatypes
and queries are written using plain old Haskell code. We write pure
functions that query that value or which return an updated value.

Implemented application has following functionality:
- create new post and add it to our persistent data storage
- delete and do respective things with this storage
- edit existing post
- save post as draft to be edited and published later
- show messages about unsuccessful requests to posts or pages


Server listens to request from port 8000.

Functional Programming course, UW, 2012
