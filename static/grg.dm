outer
	@wood-click dom-on-click lumberjack
	@stone-click dom-on-click quarry
	@food-click dom-on-click forage
	
	@building-click dom-on-click building
	@show dom-set-html display

	@timer every 500

	$building-cost 50
	$click-increment 1
	$tick-increment {}
	$balance {}

	inc-balance 
		{ __ | each block "{_value | add $balance.{_key} | >$balance.{_key}}" || $balance }
	dec-balance 
		{ __ | each block "{_value | subtract value __ from $balance.{_key} | >$balance.{_key}}" || $balance }

	upgrade 
		{ __ | each block "{_value | add $tick-increment.{_key} | >$tick-increment.{_key}}" || $tick-increment }

	can-afford
		{ __ | ($building-cost $balance.wood) | max | eq $balance.wood }

	@wood-click -> {__ | * (:wood $click-increment) } -> inc-balance -> @show
	@stone-click -> { __ | * (:stone $click-increment) } -> inc-balance -> @show
	@food-click -> { __ | * (:food $click-increment) } -> inc-balance -> @show


	@building-click -> can-afford -> { __ | then $building-cost else 0 | * (:wood __)} -> dec-balance -> @show
			   can-afford -> { __ | then 10 else 0 | * (:wood __) } -> upgrade
			   can-afford -> { __ | then "Buying..." | tap }

	@timer -> {__ | $tick-increment } -> inc-balance -> @show