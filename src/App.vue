<template>
<div id="app">
  <n-config-provider>
    <n-message-provider>
      <router-view></router-view>
    </n-message-provider>
  </n-config-provider>
  <div id="app">
    <Builder />
  </div>
</div>
</template>

<script setup>
import { NMessageProvider, NConfigProvider } from 'naive-ui';
import Builder from './components/CanvasEditor.vue'
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style>
