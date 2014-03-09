# Gratuitous Resource Gathering
###### Simple web-based game prototype to try out some FBP principles

GRG is implemented using the [Daimio language/framework](https://github.com/dxnn/daimio) and served up using the [House](https://github.com/Inaimathi/house) Common Lisp web-server.

Its aim is to be a very simple resource gathering/civilization building game. We'll see how far along that goal we manage to get...

### Installation

1. Install a [Common Lisp](http://www.cliki.net/Common+Lisp+Implementation) and [quicklisp](http://www.quicklisp.org/beta/)
2. Clone [this repository](https://github.com/Inaimathi/gratuitous-resource-gathering) and [house](https://github.com/Inaimathi/house) into your `~/quicklisp/local-projects/` directory
3. Run your lisp and evaluate `(ql:quickload :grg)`. This should start a server on port `4242` (change that by changing the port number in `grg.lisp`)

### How to play

1. Click on things
2. That's it

### Notes/TODO

- Implement non-building upgrades
- Implement workers (re-think basic buildings to make this make some sense? Workers should consume some amount of resources, but also let you do some automated gathering. Make them re-assignable to different resources)
- Upgrades that decrese tick time? (Flavor wise: make your civ more efficient. Ex: roads, trade-routes, transportation tech)
- Buildings should only appear as prereqs are met
- Explicit research resource?
- De-comission resources as we go?
- What does end-game look like?
