<template>
  <div>
    <h1>数字电路实验</h1>
    <AndGate />
    <NotGate />
    <OrGate />
    <NorGate />
    <button @click=""/>
  </div>
</template>

<script setup>
import AndGate from '@/components/AndGate.vue'
import NotGate from '@/components/NotGate.vue'
import OrGate from '@/components/OrGate.vue'
import NorGate from '@/components/NorGate.vue'
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style>