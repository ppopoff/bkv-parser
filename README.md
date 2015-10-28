# Bkv parser

The Parser for configuration file format. Implemented in Parboiled2 for my article for Habrahabr and
upcoming scala-talk.

The BKV (Block Key Value) format looks like this:

    server.name = "webserver"
    server {
      port = "8080"
      address = "192.168.88.88"

      settings {
        greeting_message = "Hello!\n It's me!"
      }
    }
    
All Strings must be quoted. Escapings are identical to C-like languages. Lines are separated with Newline.
Keys and Block names may be alphanumeric strings with underscore and dot allowed.
