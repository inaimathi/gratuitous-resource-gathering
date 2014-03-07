var game = false;

function select (selector) { return document.querySelectorAll(selector); }
function select0 (selector) { return select(selector)[0]; }

function gather() { game.ports.gather.send(true); }

function build() { game.ports.build.send("something"); }

window.onload = function () {
    game = Elm.embed(Elm.Test, select0("#game"), { gather: false,  build: "" });
}
