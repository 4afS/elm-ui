require('../sass/style.scss');
const { Elm } = require('../src/Main.elm');

var app = Elm.Main.init({
  node: document.getElementById('elm')
});

