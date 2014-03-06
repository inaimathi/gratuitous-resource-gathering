outer
	@resource-click dom-on-click resource
	@building-click dom-on-click building
	@show dom-set-html display

	@timer every-half-second

	$building-cost 5
	$click-increment 1
	$tick-increment 0
	$balance 0

	inc-balance 
		{ __ | add $balance | >$balance }
	dec-balance 
		{ __ | subtract value __ from $balance | >$balance }

	can-afford
		{ __ | ($building-cost $balance) | max | eq $balance }
	
	
	@resource-click -> {__ | $click-increment } -> inc-balance -> @show
	@building-click -> can-afford -> { __ | then $building-cost else 0} -> dec-balance -> @show
			   can-afford -> { __ | then 10 else 0 | add $tick-increment | >$tick-increment }
			   can-afford -> { __ | then "Buying..." | tap }

	@timer -> {__ | $tick-increment } -> inc-balance -> { __ | $balance } ->  @show