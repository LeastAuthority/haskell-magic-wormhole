======================
haskell-magic-wormhole
======================
----------------------
Internal documentation
----------------------

- our best attempt to understand the server protocol is available as a message sequence chart in ``server-protocol.diag``
- running ``make`` will render that into an SVG
- there are at least two protocols
  - "server", which is how clients talk to the rendezvous relay server
  - "client", which is tunnelled through the server protocol, and is how clients communicate to each other
- the server protocol is roughly symmetric wrt "sender" and "receiver"
  - each client communicates with the server in much the same way
  - they negotiate a shared mailbox
  - then the sender sends stuff to the mailbox
- there's a log of messages sent & received by ``wormhole send`` in ``send.log``
- likewise, a log of messages to & from ``wormhole receive`` in ``receive.log``
- format is
  - sent messages are between ``>>>`` lines
  - received messages are between ``<<<`` lines
  - stuff not between those are from the console
- note that the two logs are from different sessions
- the main thing is that JSON messages are sent & received over a binary websocket connection to the rendezvous server (port 4000)
- the server sends an 'ack' after every message
