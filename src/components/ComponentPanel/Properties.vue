<template>
  <div class="component-properties">
    <h3>元件属性</h3>
    <div v-if="selectedComponent">
      <!-- 修改名字 -->
      <div class="property-item">
        <label for="name">名字：</label>
        <input
          id="name"
          type="text"
          v-model="selectedComponent.name"
          placeholder="请输入名字"
        />
      </div>

      <!-- 修改朝向 -->
      <div class="property-item">
        <label for="orientation">朝向：</label>
        <select
          id="orientation"
          v-model="selectedComponent.direction"  
          @change="updateDirection"
        >
          <option value="east">东</option>
          <option value="west">西</option>
          <option value="north">北</option>
          <option value="south">南</option>
        </select>
      </div>

      <!-- 修改数据位宽 -->
      <div class="property-item">
        <label for="bitWidth">数据位宽：</label>
        <select
          id="bitWidth"
          v-model="selectedComponent.bitCount"
        >
          <option v-for="width in bitWidthOptions" :key="width" :value="width">
            {{ width }} 位
          </option>
        </select>
      </div>
    </div>
    <div v-else>
      <p>未选中任何元件</p>
    </div>
  </div>
</template>

<script setup>
import { ref, watch } from 'vue';
import { useCircuitStore } from '@/store/CircuitStore';
import eventBus from '@/modules/useEventBus';

const circuitStore = useCircuitStore();

// 选中的元件
const selectedComponent = circuitStore.selectedComponent;
// const selectedComponent = ref({
//     name: '',
//     orientation: 'up',
//     bitCount: 1
// });
// 数据位宽选项
const bitWidthOptions = ref([1, 2, 4, 8, 16, 32, 64]);

function updateDirection() {
  // 更新元件的方向
  eventBus.emit('updateComponentDirection');
}

</script>

<style scoped>
.component-properties {
  padding: 10px;
  background-color: transparent;
}

.property-item {
  margin-bottom: 10px;
  display: flex;
  flex-direction: row;
}

.property-item label {
  display: inline-block;
  width: 80px;
  font-weight: bold;
}

.property-item input,
.property-item select {
  padding: 5px;
  border: 1px solid #ddd;
  border-radius: 4px;
  width: calc(100% - 90px);
}
</style>