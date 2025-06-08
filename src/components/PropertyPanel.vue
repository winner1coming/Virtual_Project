<!-- components/PropertyPanel.vue -->
<template>
    <div v-if="selectedComponent" class="w-64 bg-white shadow p-4">
      <h2 class="text-lg font-bold mb-4">属性编辑</h2>
  
      <div class="mb-4">
        <label class="block text-sm font-medium mb-1">ID</label>
        <input type="text" v-model="localComponent.id" class="w-full border rounded px-2 py-1" disabled />
      </div>
  
      <div class="mb-4">
        <label class="block text-sm font-medium mb-1">引脚数</label>
        <input type="number" v-model.number="localComponent.pinCount" min="1" class="w-full border rounded px-2 py-1" />
      </div>
  
      <div class="mb-4">
        <label class="block text-sm font-medium mb-1">方向</label>
        <select v-model="localComponent.direction" class="w-full border rounded px-2 py-1">
          <option value="up">向上</option>
          <option value="right">向右</option>
          <option value="down">向下</option>
          <option value="left">向左</option>
        </select>
      </div>
  
      <button @click="applyChanges" class="bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600">
        应用修改
      </button>
    </div>
  </template>
  
  <script>
  export default {
    props: {
      selectedComponent: Object, // 父组件传入选中的元件
    },
    data() {
      return {
        localComponent: null, // 本地副本用于编辑
      };
    },
    watch: {
      selectedComponent: {
        immediate: true,
        handler(newVal) {
          this.localComponent = newVal ? { ...newVal } : null;
        },
      },
    },
    methods: {
      applyChanges() {
        this.$emit('update-component', this.localComponent);
      },
    },
  };
  </script>
  
  <style scoped>
  /* 可加样式 */
  </style>
  