<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue'

import { AndGate } from "@/logic/components/AndGate.js";
import { OrGate } from "@/logic/components/OrGate.js";
import { NotGate } from "@/logic/components/NotGate.js";
import {CircuitLogic} from "@/logic/CircuitLogic.js";
import {useCircuitStore} from "@/store/CircuitStore"


    let circuitLogic = CircuitLogic.getInstance();
    const circuitStore = useCircuitStore();
    // 创建逻辑门实例
    circuitStore.addComponent("And", [0,0]);
    circuitStore.addComponent("Or", [0,0]);
    circuitStore.addComponent("Not", [0,0]);
    circuitStore.addComponent("And", [0,0]);
    circuitStore.addComponent("Clock", [0,0]);

    let and1 = circuitStore.getComponent(0);
    let or1 = circuitStore.getComponent(1);
    let not1 = circuitStore.getComponent(2);

    and1.changeInputPinCount(2);
    or1.changeInputPinCount(2);
    not1.changeInputPinCount(1);
    and1.changeInput(0, 1);
    and1.changeInput(1, 1);

    console.log("AndGate instance:", and1);
    console.log("OrGate instance:", or1);
    console.log("NotGate instance:", not1);
    
    
    // 测试连接
    console.log("test connect");
    circuitLogic.connect(0, 2, 1, 0);
    circuitLogic.connect(1, 2, 2, 0);

    console.log("AndGate instance:", and1);
    console.log("OrGate instance:", or1);
    console.log("NotGate instance:", not1);  

    // 当连接发生变化
    let and2 = circuitStore.getComponent(3);
    and2.changeInputPinCount(2);
    and2.changeInput(0, 0);
    
    circuitLogic.connect(3,2,0,1);

    let clock = circuitStore.getComponent(4);
    circuitLogic.connect(4, 0, 0, 0);

    console.log("andGate一个输入为0，andGate输出给orGate，orGate输出给Not")
    console.log("AndGate instance:", and1);
    console.log("OrGate instance:", or1);
    console.log("NotGate instance:", not1);  


const outputs = ref({
  and2: and2.getOutputs()[0],
  clock: clock.getOutputs()[0],

  and1: and1.getOutputs()[0],
  
  or1: or1.getOutputs()[0],
  not1: not1.getOutputs()[0],
  
})
let timer: any = null;
onMounted(() => {
  timer = setInterval(() => {
    outputs.value.and1 = and1.getOutputs()[0];
    outputs.value.and2 = and2.getOutputs()[0];
    outputs.value.or1 = or1.getOutputs()[0];
    outputs.value.not1 = not1.getOutputs()[0];
    outputs.value.clock = clock.getOutputs()[0];

    console.log(and1.getOutputs()[0]);
  }, 100); // 100ms刷新一次
});
onUnmounted(() => {
  clearInterval(timer);
});
// function handleClick() {
//     testConnect();
// }

</script>

<template>
  <div>
    <h1>测试页面</h1>
     {{ "与门2   " + outputs.and2 }}
    <br>
    {{ "时钟" + outputs.clock }}
    <br>
    {{ "与门：" + outputs.and1 }}
    <br>
    {{ "或门" + outputs.or1 }}
    <br>
    {{ "非门" + outputs.not1 }}

    <br>

   


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