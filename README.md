# Gratuitous Resource Gathering
###### Simple web-based game prototype to try out some FBP principles

GRG is implemented using the [Elm language](http://elm-lang.org/).

Its aim is to be a very simple resource gathering/civilization building game. We'll see how far along that goal we manage to get...

### How to play

1. Click on things (resources to gather them, -/+ buttons to assign workers, technologies to research/build them)
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
	- Assigned workers (how many of your workers are devoted to gathering this resource)
- A **technology** is a thing you can build. Each technology has
	- A name
	- A cost (How many and what types of resources you need to buy it)
	- A prerequisite (What technologies you need before you can build this one)
	- An upgrade (What buying this technology gets you)

- You start each game as a primitive human tribe gathering food and clay. Using these resources, you can expand your gathering skill, capacity, population and technologies to work up into a post-industrial civilization.

###### TODO
- Implement a gathering rate other than 1:1 on (workers assigned):(resource gathered per tick/click)
- More uniform gathering rate, cost, decay functions (they should probably all just be either functions or constants)
