outer
	@wood-click dom-on-click lumberjack
	@stone-click dom-on-click quarry
	@food-click dom-on-click forage

	@building-click dom-on-click .building
	@show-balance dom-set-html balance-display

	@timer every 500

	$buildings { "Lumber Yard" : { "cost": { "wood": 50, "stone": 10 }, "upgrade": { "wood": 1 } }
						 , "Quarry"      : { "cost": { "wood": 20, "stone": 50 }, "upgrade": { "stone": 1 } }
						 , "Farm"        : { "cost": { "wood": 30, "stone": 30 }, "upgrade": { "food": 1 } }}

	$building { "cost": { "wood": 50, "stone": 10 }, "upgrade": { "wood": 10 } }
	$click-increment 1
	$tick-increment {}
	$balance { }

	inc-balance 
		{ __ | each block "{_value | add $balance.{_key} | >$balance.{_key}}" || $balance }
	dec-balance 
		{ __ | each block "{_value | subtract value _value from $balance.{_key} | >$balance.{_key}}" || $balance }

	upgrade 
		{ __ | each block "{_value | add $tick-increment.{_key} | >$tick-increment.{_key}}" || }

	can-afford
		{ __ | $building.cost | list keys | map block "{__ | $building.cost.{_value} | subtract 1 | less value __ than $balance.{_value} }" | and }
		
	@wood-click -> { __ | * (:wood $click-increment) } -> inc-balance -> @show-balance
	@stone-click -> { __ | * (:stone $click-increment) } -> inc-balance -> @show-balance
	@food-click -> { __ | * (:food $click-increment) } -> inc-balance -> @show-balance

	@building-click -> {__ | tap } -> can-afford -> { __ | then $building.cost } -> dec-balance -> @show-balance
										 		 	 		 	 		can-afford -> { __ | then $building.upgrade } -> upgrade

	@timer -> {__ | $tick-increment } -> inc-balance -> @show