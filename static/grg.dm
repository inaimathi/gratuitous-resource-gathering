outer
	@resource-click dom-on-click .resource
	@building-click dom-on-click .building

	@timer every 500

	$buildings { "Lumber Yard" : { "cost": { "wood": 50, "stone": 10 }, "upgrade": { "wood": 1 } }
						 , "Quarry"      : { "cost": { "wood": 20, "stone": 50 }, "upgrade": { "stone": 1 } }
						 , "Farm"        : { "cost": { "wood": 30, "stone": 30 }, "upgrade": { "food": 1 } }
						 , "Mine" 			 : { "cost": { "wood": 50, "stone": 20, "food": 30}, "upgrade": { "ore": 1}, "requires": ["Lumber Yard", "Quarry", "Farm"]}
						 , "Gold Mine"   : { "cost": { "wood": 50, "stone": 20, "food": 30}, "upgrade": { "gold": 1}, "requires": ["Mine"]}
						 , "Blacksmith"  : { "cost": { "wood": 60, "stone": 50 }, "upgrade": {}, "requires": ["Mine"]}}

	$building { "cost": { "wood": 50, "stone": 10 }, "upgrade": { "wood": 10 } }
	$click-increment { "wood": 1, "stone": 1, "food": 1}
	$tick-increment {}
	$balance {}

	inc-balance 
		{ __ | each block "{_value | add $balance.{_key} | >$balance.{_key}}" }
	dec-balance 
		{ __ | each block "{_value | subtract value _value from $balance.{_key} | >$balance.{_key}}" }

	upgrade 
		{ __ | each block "{_value | add $tick-increment.{_key} | >$tick-increment.{_key}}" }

		
	can-build
		{ __ | >bld | $buildings.{__}.cost | map block "{$balance.{_key} | less than _value | not}" | and | then _bld }
		
	@resource-click -> { __ | * (__ $click-increment.{__}) } -> inc-balance -> show-balance

	@building-click -> can-build -> { __ | then $buildings.{__}.cost } -> dec-balance -> show-balance
										 can-build -> { __ | then $buildings.{__}.upgrade } -> upgrade -> show-income

	@timer -> {__ | $tick-increment } -> inc-balance -> show-balance

	@show-balance dom-set-html balance-display
	show-balance {__}
	show-balance -> { $balance | list remove by_key :0 } -> @show-balance

	@show-income dom-set-html income-display
	show-income {__}
	show-income -> { $tick-increment | list remove by_key :0 } -> @show-income

	@show-skills dom-set-html skills-display
	show-skills {__}
	show-skills -> { $click-increment | list remove by_key :0 } -> @show-skills	