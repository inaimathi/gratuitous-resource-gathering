outer
	@resource-click dom-on-click .resource
	@tech-click dom-on-click .technology

	@resource-html dom-set-html resources
	@technology-html dom-set-html technologies

	resource
	technology

	@timer every 500
	@init on-startup
	
	@init -> { __ | $resources | map block "{* (:name _key)}" } -> resource -> @resource-html
	@init -> { __ | $techs | map block "{* (:name _key)}" } -> technology -> @technology-html


	$techs { "Clay Pit"    : { "cost": { "wood": 30 }, "upgrade": { "clay": 1 }, "requires": [] }
				 , "Pottery"     : { "cost": { "wood": 25, "clay": 25 }, "upgrade": {}, "requires": ["Clay Pit"]}
				 , "Lumber Yard" : { "cost": { "wood": 50, "stone": 10 }, "upgrade": { "wood": 1 }, "requires": [] }
				 , "Quarry"      : { "cost": { "wood": 20, "stone": 50 }, "upgrade": { "stone": 1 }, "requires": ["Lumber Yard"] }
				 , "Farm"        : { "cost": { "wood": 30, "stone": 30 }, "upgrade": { "food": 1 }, "requires": [] }
				 , "Mine" 			 : { "cost": { "wood": 50, "stone": 20, "food": 30}, "upgrade": { "ore": 1, "salt": 1 }, "requires": ["Lumber Yard", "Quarry", "Farm"] }
				 , "Gold Mine"   : { "cost": { "wood": 50, "stone": 20, "food": 30}, "upgrade": { "gold": 1}, "requires": ["Mine"] }
				 , "Blacksmith"  : { "cost": { "wood": 60, "stone": 50 }, "upgrade": {}, "requires": ["Mine"] }}

	$resources { "wood" : [], "stone" : [],  "food" : [], "clay": []
						 , "ore": ["Mine"], "salt": ["Mine"], "gold": ["Gold Mine"]
						 , "money": ["Printing Press", "Mint"], "reputation": ["Printing Press"]
						 , "trade": ["Shipyard"] 
						 , "knowledge": ["Philosophy"] }

	$click-increment { "wood": 1, "stone": 1, "food": 1}
	$capacity { "wood": 50, "stone": 100, "food": 50, "salt": 50, "clay": 50 }
	$built {}
	$tick-increment {}
	$balance { "workers": 2, "food": 10 }

	inc-balance { __ | each block "{_value | add $balance.{_key} | min $capacity.{_key} | >$balance.{_key}}" }
	dec-balance { __ | each block "{_value | subtract value _value from $balance.{_key} | >$balance.{_key}}" }

	upgrade { __ | each block "{_value | add $tick-increment.{_key} | >$tick-increment.{_key}}" }

	have-prereqs
		{ __ | >tch | $techs.{_tch}.requires | >reqs | $built | list keys | (__ _reqs) | list intersect | eq _reqs | then _tch }
	can-afford
		{ __ | >tch | $techs.{_tch}.cost | map block "{$balance.{_key} | less than _value | not}" | and | then _tch }
		
	@resource-click -> { __ | * (__ $click-increment.{__}) } -> inc-balance -> show-balance

	@tech-click -> have-prereqs -> can-afford -> { $techs.{__}.cost } -> dec-balance -> show-balance
								 								 can-afford -> { $built.{__} | add 1 | >$built.{__} } -> show-built
																 can-afford -> { $techs.{__}.upgrade } -> upgrade -> show-income

	@timer -> {__ | $tick-increment } -> inc-balance -> show-balance

	@show-balance dom-set-html balance-display
	show-balance {__}
	show-balance -> { $balance | list remove by_key :0 } -> @show-balance

	@show-income dom-set-html income-display
	show-income {__}
	show-income -> { $tick-increment | list remove by_key :0 } -> @show-income

	@show-built dom-set-html tech-display
	show-built {__}
	show-built -> { $built | list remove by_key "" } -> @show-built

	@show-skills dom-set-html skills-display
	show-skills {__}
	show-skills -> { $click-increment | list remove by_key :0 } -> @show-skills	