import { CounterState, test } from 'wasm';

const counter_state = CounterState.new();

document.getElementById("counter").textContent = counter_state.get_counter();

document.getElementById("increment").addEventListener("click", () => {
    document.getElementById("counter").textContent = counter_state.increment_counter();
});
document.getElementById("decrement").addEventListener("click", () => {
    document.getElementById("counter").textContent = counter_state.decrement_counter();
});

let arrayOfObjects = [
  {name: "hello world", content: "99", },
  {name: "hello world2", content: "88",},
  {name: "hello world3", content: "77",}
]

console.log(test);
console.log(test( arrayOfObjects ));
