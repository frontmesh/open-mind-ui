import '../css/styles.css';
import { Elm } from '../elm/Main.elm';

var storageKey = "store";
var storedData = localStorage.getItem(storageKey);
var flags = storedData ? JSON.parse(storedData) : null;

// Start the Elm application.
var app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: flags
});

app.ports.setStorage.subscribe(function(val) {

  if (val === null) {
    localStorage.removeItem(storageKey);
  } else {
    localStorage.setItem(storageKey, JSON.stringify(val));
  }

  // Report that the new session was stored successfully.
  setTimeout(function() { app.ports.onStoreChange.send(val); }, 0);
});

// Whenever localStorage changes in another tab, report it if necessary.
window.addEventListener("storage", function(event) {
  if (event.storageArea === localStorage && event.key === storageKey) {
    app.ports.onStoreChange.send(event.newValue);
  }
}, false);
