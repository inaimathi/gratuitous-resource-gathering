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
###### Notes
- There are two things: **resources** and **technologies**.
- A **resource** is a thing you can gather. Each resource has
	- A name
	- A requirement (What technologies you need before you can gather any)
	- A balance (How much of it you have)
	- A capacity (The maximum balance you can have)
	- A skill (How much of it you gather per click)
	- An income (How much of it you gather per tick)
	- An upkeep (How much of another resource it consumes per tick. If you don't have enough of the required resource, you instead lose one of the upkept resource)
- A **technology** is a thing you can build. Each technology has
	- A name
	- A cost (How many and what types of resources you need to buy it)
	- A prerequisite (What technologies you need before you can build this one)
	- An upgrade (What buying this technology gets you)

- You start each game as a primitive human tribe gathering food and clay. Using these resources, you can expand your gathering skill, capacity, population and technologies to work up into a post-industrial civilization.

**Technology according to Civ...**

- Agriculture - Fire - Pottery - Animal Husbandry - Archery - Mining
- Writing - Sailing - Calendar - Trapping - The Wheel - Masonry - Bronze Working - Geometry
- Optics - Philosophy - Horseback Riding - Mathematics - Construction - Iron Working
- Theology - Civil Service - Currency - Engineering - Metal Casting
- Compass - Education - Chivalry - Machinery - Physics - Steel
- Astronomy - Acoustics - Banking - Printing Press - Gunpowder
- Navigation - Economics - Chemistry - Metallurgy
- Archaeology - Scientific Theory - Military Science - Fertilizer - Rifling
- Biology - Steam Power - Dynamite
- Electricity - Replaceable Parts - Railroad
- Refrigeration - Telegraph - Radio - Flight - Combustion
- Penicilin - Plastics - Electronics - Mass Media - Radar - Atomic Theory
- Ecology - Digital Computers - Rocketry - Lasers - Nuclear Fission
- Globalization - Robotics - Satellites - Stealth - Advanced Ball Bearings
- Particle Physics - Nuclear Fusion
- Nanotechnology

(Totally add

    Artificial Intelligence - Mind Uploading - P=NP
    The Singularity

at the end there)

**...And according to Wikipedia/Ryan North:**

(Still needs more detail, and progress up to modern day)

- Basic stone tools
- Fire
- Stone tools
- Language
- Ceramics
- Domestication
- Bow/Sling
- Copper
- Agriculture/Plough
- The Wheel
- Gnomon (central part of the sundial)
- Writing
- Bronze
- Salt
- Chariot
- Iron
- Sundial
- Glass
- Catapult
- Horseshoe
- Stirrup

###### TODO
- Implement one-time upgrades
- Implement resources that need upkeep
- Upgrades that decrese tick time? (Flavor wise: make your civ more efficient. Ex: roads, trade-routes, transportation tech)
- What does end-game look like?

