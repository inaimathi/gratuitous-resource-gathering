outer
	@resource-click dom-on-click resource
	@building-click dom-on-click building
	@show dom-set-html display

	@timer every-half-second

	$building-cost 5
	$click-increment 1
	$tick-increment 0
	$balance 0

	inc-balance { __ | add $balance | >$balance }
	dec-balance { __ | subtract value __ from $balance | >$balance }

	gt-balance {__ | ($building-cost $balance) | max | eq $balance }

	@resource-click -> {__ | $click-increment } -> inc-balance -> @show
	@building-click -> gt-balance -> { __ | then "Buying..." else "You can't afford it!" } -> { __ | tap }

	@timer -> {__ | $tick-increment } -> dec-balance -> { __ | $balance } ->  @show