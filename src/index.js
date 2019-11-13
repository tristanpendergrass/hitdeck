import { Elm } from "./Main.elm";

const initialName = localStorage.getItem("initialName");

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: initialName || "Default"
});

app.ports.sendName.subscribe(name => {
  localStorage.setItem("initialName", name);
});
