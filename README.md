Magic Wormhole for Haskell
==========================

Library and command-line tools for interacting with [Magic
Wormhole](https://github.com/warner/magic-wormhole/) in Haskell

Magic Wormhole lets you get things from one computer to another, safely.

## Running tests

Install the python dependencies for running integration tests.

```
virtualenv venv
source venv/bin/activate
pip install -r requirements.txt
```

Now run the tests:

```
cabal test
```
