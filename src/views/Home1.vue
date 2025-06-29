<template>
  <div>
    <h1>数字电路实验</h1>
    <AndGate />
    <Tunnel />
    <OrGate />
    <InputPin />
    <Constant />
    <button @click=""/>
  </div>
</template>

<script setup>
import AndGate from '@/components/AndGate.vue'
import NotGate from '@/components/NotGate.vue'
import OrGate from '@/components/OrGate.vue'
import NorGate from '@/components/NorGate.vue'
import Tunnel from '@/components/Tunnel.vue'
import InputPin from '@/components/InputPin.vue'
import Constant from '@/components/Constant.vue'
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style>