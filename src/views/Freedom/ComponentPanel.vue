<template>
  <n-split
    direction="vertical"
    class="split-container"
    :default-sizes="[50, 50]" 
    style="height: 100%;"
    :max="0.8"
    :min="0.2"
  >
    <!-- 上半部分：电子元件列表 -->
    <template #1>
      <ComponentList />
    </template>

    <!-- 下半部分：元件属性 -->
    <template #2>
      <Properties />
    </template>
  </n-split>
</template>

<script setup>
import { NButton, NSplit } from 'naive-ui';
import Properties from '@/components/ComponentPanel/Properties.vue';
import ComponentList from '@/components/ComponentPanel/ComponentList.vue';


</script>

<style scoped>
.split-container {
  height: 100vh;
  background-color: transparent;
}



.component-item {
  padding: 10px;
  margin: 8px 0;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: move;
}

.component-item:hover {
  background-color: #f0f5ff;
}


</style>