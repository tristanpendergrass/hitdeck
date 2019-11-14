import { Elm } from "./Main.elm";

const localStorageData = JSON.parse(localStorage.getItem("hitdeck"));

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: localStorageData
});

app.ports.sendToLocalStorage.subscribe(data => {
  localStorage.setItem("hitdeck", JSON.stringify(data));
});
