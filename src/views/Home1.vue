<template>
  <div>
    <h1>数字电路实验</h1>
    <AndGate :id="circuitStore.addComponent('AND')"/>
    <Tunnel :id="circuitStore.addComponent('TUNNEL')"/>
    <OrGate :id="circuitStore.addComponent('OR')"/>
    <InputPin :id="circuitStore.addComponent('INPUT')"/>
    <Constant :id="circuitStore.addComponent('CONSTANT')"/>
    <Clock :id="circuitStore.addComponent('CLOCK')"/>
    <button @click=""/>
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
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style>