<template>
<div id="app">
  <n-config-provider>
    <n-message-provider>
      <keep-alive>
        <router-view />
      </keep-alive>
    </n-message-provider>
  </n-config-provider>
</div>
</template>

<script setup>
import { NMessageProvider, NConfigProvider } from 'naive-ui';
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style> 