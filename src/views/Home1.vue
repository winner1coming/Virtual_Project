<template>
  <div>
    <h1>数字电路实验</h1>
    <AndGate :id="andGateId"/>
    <Tunnel :id="tunnelId"/>
    <OrGate :id="orGateId"/>
    <InputPin :id="inputPinId"/>
    <Constant :id="constantId"/>
    <Clock :id="clockId"/>
  </div>
</template>

<script setup>
import AndGate from '@/components/Gates/AndGate.vue'
import NotGate from '@/components/Gates/NotGate.vue'
import OrGate from '@/components/Gates/OrGate.vue'
import NorGate from '@/components/Gates/NorGate.vue'
import Tunnel from '@/components/Wiring/Tunnel.vue'
import InputPin from '@/components/Wiring/InputPin.vue'
import Constant from '@/components/Wiring/Constant.vue'
import Clock from '@/components/Wiring/Clock.vue'

import { useCircuitStore } from '@/store/CircuitStore.js'
const circuitStore = useCircuitStore();

// 预先添加组件并保存 ID
const andGateId = circuitStore.addComponent('AND');
const tunnelId = circuitStore.addComponent('TUNNEL');
const orGateId = circuitStore.addComponent('OR');
const inputPinId = circuitStore.addComponent('INPUT');
const constantId = circuitStore.addComponent('CONSTANT');
const clockId = circuitStore.addComponent('CLOCK');
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style>