D.import_port_flavour('dom-on-drag', {
    dir: 'in',
    outside_add: function() {
	var self = this
	D.track_event('drag', this.settings.thing, 
		      function(value) { 
			  console.log (value) 
		      })
    }
})

D.import_port_flavour('xhr-send', {
    dir: 'out',
    outside_exit: function (ship) {
	xhr_get(ship, D.noop)
    }
})

D.import_port_flavour('sse-receive', {
    dir: 'in',
    outside_add: function () {
	var channel = new EventSource(this.settings.thing)
	var self = this;
	channel.onmessage = function (e) { 
	    self.enter(e.data)
	}
    }
})

function xhr_get(target, callback) {
    var xhr = new XMLHttpRequest()
    xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) {
            callback(xhr.responseText)
        }
    }
    xhr.open('GET', target, true)
    xhr.send(null)
}

D.import_models({
    string: {
	methods: {
	    "uri-decode": {
		desc: "Decode a URI",
		params: [{ key: 'value', desc: 'A URI encoded string', type: 'string'}],
		fun: function (value) {
		    return decodeURIComponent(value)
		}
	    }
	}
    }
})

document.addEventListener('DOMContentLoaded', function() {
    xhr_get('/static/daim_go.dm', function(data) {
	var outerseed = D.make_some_space(data, D.get_templates());
	OuterSpace = new D.Space(outerseed);
    });
});
