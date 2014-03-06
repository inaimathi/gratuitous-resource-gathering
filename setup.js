var game = false;

function select (selector) {
    return document.querySelectorAll(selector);
}
function select0 (selector) {
    return select(selector)[0];
}

function clickedWood() {
    console.log("Clicked wood!");
    game.ports.wood.send(true);
}

function building() { 
    game.ports.building.send("building");
}

window.onload = function () {
    game = Elm.embed(Elm.Test, select0("#counter"), 
		     { wood: false, 
		       building: "" });
}

// var getter = Elm.worker(Elm.DictRequest);
// var minibuffer = Elm.embed(Elm.Autocomplete, byId("minibuffer")document.getElementById("minibuffer"), { wordList : [] });
// var display = Elm.embed(Elm.Display, document.getElementById("display"), { input: "" });

// getter.ports.output.subscribe(function (msg) { minibuffer.ports.wordList.send(msg)});
// minibuffer.ports.output.subscribe(function (msg) { display.ports.input.send(msg) });
