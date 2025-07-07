<template>
<div id="app">
  <n-config-provider>
    <n-message-provider>
        <router-view />
    </n-message-provider>
  </n-config-provider>
</div>
</template>

<script setup lang="ts">
import { NMessageProvider, NConfigProvider} from 'naive-ui';
import { onMounted, onBeforeUnmount } from 'vue';
onMounted(() => {
  window.addEventListener('beforeunload', handleBeforeUnload);
});

onBeforeUnmount(() => {
  window.removeEventListener('beforeunload', handleBeforeUnload);
});

function handleBeforeUnload(event: Event) {
  // 清理缓存中以 "circuit" 开头的项
  Object.keys(localStorage).forEach(key => {
    if (key.startsWith('circuit')) {
      localStorage.removeItem(key);
    }
  });
  console.log('已清理以 "circuit" 开头的缓存项');

}
</script>

<style>
body {
  margin: 0;
  font-family: sans-serif;
}
</style> 