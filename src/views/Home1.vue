<template>
  <div>
    <h1>数字电路实验</h1>
    <!-- <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <OrGate :id="orGateId"/>
    </svg> -->

    <!-- <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <InputPin :id="inputPinId"/>
    </svg> -->

    <svg
      viewBox="-300 -300 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <CustomizeComoponent :id="CustomizeComponentId"/>
    </svg>

    <!-- <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <Tunnel :id="tunnelId"/>
    </svg> -->

    <!-- <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <AndGate :id="andGateId"/>
    </svg> -->

    <!-- <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <Constant :id="constantId"/>
    </svg> -->

    <!-- <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <Clock :id="clockId"/>
    </svg> -->

    <!-- <svg
      viewBox="-300 -300 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <OutputPin :id="outputpinId"/>
    </svg> -->

    <!-- <svg
      viewBox="-300 -300 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <andGate />
    </svg> -->

    <!-- <Splitter /> -->
    <button @click="toggleInputPin"> 改InputPin的位宽 </button>
  </div>
</template>

<script setup>
import AndGate from '@/components/Gates/AndGate.vue'
import NotGate from '@/components/Gates/NotGate.vue'
import OrGate from '@/components/Gates/OrGate.vue'
import NorGate from '@/components/Gates/NorGate.vue'
import Tunnel from '@/components/Wiring/Tunnel.vue'
import InputPin from '@/components/Wiring/InputPin.vue'
import OutputPin from '@/components/Wiring/OutputPin.vue'
import Constant from '@/components/Wiring/Constant.vue'
import Clock from '@/components/Wiring/Clock.vue'
import Splitter from '@/components/Wiring/Splitter.vue'
import CustomizeComoponent from '@/components/CustomizeComponent.vue'
import andGate from '@/components/preview/andGate.vue'

import { useCircuitStore } from '@/store/CircuitStore.js'
const circuitStore = useCircuitStore();

// 预先添加组件并保存 ID
const andGateId = circuitStore.addComponent('AND');
const tunnelId = circuitStore.addComponent('TUNNEL');
const orGateId = circuitStore.addComponent('OR');
const inputPinId = circuitStore.addComponent('INPUT');
const outputpinId = circuitStore.addComponent('OUTPUT');
const constantId = circuitStore.addComponent('CONSTANT');
const clockId = circuitStore.addComponent('CLOCK');
// const splitterId = circuitStore.addComponent('SPLITTER')
const CustomizeComponentId = circuitStore.addComponent('AND')

function toggleInputPin()
{
  // circuitStore.getComponent(inputPinId).setBitCount(32);
  circuitStore.getComponent(CustomizeComponentId).inputCount = 8;
}
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style>