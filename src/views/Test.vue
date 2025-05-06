<script setup>
import { ref } from 'vue'

import { AndGate } from "@/logic/components/AndGate.js";
import { OrGate } from "@/logic/components/OrGate.js";
import { NotGate } from "@/logic/components/NotGate.js";
import {CircuitLogic} from "@/logic/CircuitLogic.js";
import {useCircuitStore} from "@/store/CircuitStore"

function testConnect(){
    let circuitLogic = CircuitLogic.getInstance();
    const circuitStore = useCircuitStore();
    // 创建逻辑门实例
    circuitStore.addComponent("And", [0,0]);
    circuitStore.addComponent("Or", [0,0]);
    circuitStore.addComponent("Not", [0,0]);
    circuitStore.addComponent("And", [0,0]);
    let c1 = circuitStore.getComponent(0);
    let c2 = circuitStore.getComponent(1);
    let c3 = circuitStore.getComponent(2);

    c1.changeInputPinCount(2);
    c2.changeInputPinCount(2);
    c3.changeInputPinCount(1);
    c1.changeInput(0, 1);
    c1.changeInput(1, 1);

    console.log("AndGate instance:", c1);
    console.log("OrGate instance:", c2);
    console.log("NotGate instance:", c3);
    
    
    // 测试连接
    console.log("test connect");
    circuitLogic.connect(0, 2, 1, 0);
    circuitLogic.connect(1, 2, 2, 0);

    console.log("AndGate instance:", c1);
    console.log("OrGate instance:", c2);
    console.log("NotGate instance:", c3);  

    // 当连接发生变化
    let c4 = circuitStore.getComponent(3);
    c4.changeInputPinCount(2);
    c4.changeInput(0, 0);
    
    circuitLogic.connect(3,2,0,1);

    console.log("andGate一个输入为0，andGate输出给orGate，orGate输出给Not")
    console.log("AndGate instance:", c1);
    console.log("OrGate instance:", c2);
    console.log("NotGate instance:", c3);  

}

function handleClick() {
    testConnect();
}
</script>

<template>
  <div>
    <h1>测试页面</h1>
    <button @click="handleClick">点击我</button>
  </div>
</template>



<style scoped>
h1 {
  color: #42b983;
}
button {
  padding: 10px 20px;
  background-color: #42b983;
  color: white;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}
button:hover {
  background-color: #369870;
}
</style>