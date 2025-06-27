<template>
  <div class="component-properties">
    <h3>元件属性</h3>
    <div v-if="circuitStore.selectedId !== -1">
      <!-- 修改名字 -->
      <div class="property-item">
        <label for="name">名字：</label>
        <input
          id="name"
          type="text"
          v-model="circuitStore.getComponent(circuitStore.selectedId).name"
          placeholder=""
        />
      </div>

      <!-- 修改朝向 -->
      <div class="property-item">
        <label for="orientation">朝向：</label>
        <n-select
          id="orientation"
          v-model:value="circuitStore.getComponent(circuitStore.selectedId).direction.toString"  
          :options="[
            { label: '东', value: 'east' },
            { label: '西', value: 'west' },
            { label: '北', value: 'north' },
            { label: '南', value: 'south' }
          ]"
          @update:value="updateDirection"
        >
        </n-select>
      </div>

      <!-- 修改数据位宽 -->
      <div class="property-item">
        <label for="bitWidth">数据位宽：</label>
        <n-select
          id="bitWidth"
          v-model:value="circuitStore.getComponent(circuitStore.selectedId).bitCount"
          :options="bitWidthOptions.map(width => ({ label: `${width} 位`, value: width }))"
        >
        </n-select>
      </div>

      <!-- 修改引脚数量-->
      <div class="property-item" v-show="showPinCountOptions">
        <label for="pinCount">引脚数量：</label>
        <select
          id="pinCount"
          v-model="circuitStore.getComponent(circuitStore.selectedId).inputCount"
        >
          <option v-for="count in pinCountOptions" :key="count" :value="count">
            {{ count }} 个
          </option>
        </select>
       </div>
    </div>
    <div v-else>
      <p>未选中任何元件</p>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed, ref, watch } from 'vue';
import { useCircuitStore } from '@/store/CircuitStore';
import eventBus from '@/modules/useEventBus';
import { NSelect } from 'naive-ui';
import type { SelectOption } from 'naive-ui'

const circuitStore = useCircuitStore();

// 选中的元件
// const selectedComponent = ref({
//     name: '',
//     orientation: 'up',
//     bitCount: 1
// });

// 数据位宽选项
const bitWidthOptions = ref([1, 2, 4, 8, 16, 32, 64]);
// 引脚数量选项
const pinCountOptions = ref([2,3,4,5,6,7,8]);
// 判断是否显示修改引脚数量的选项
const showPinCountOptions = computed(() => {
  const selectedComponent = circuitStore.getComponent(circuitStore.selectedId);
  return selectedComponent.type !== 'Not' && selectedComponent.type !== 'Clock' &&
        selectedComponent.type !== 'Input' && selectedComponent.type !== 'Output' &&
        selectedComponent.type !== '7SegmentDisplay' && selectedComponent.type !== 'Tunnel' &&
         selectedComponent.type !== 'Power' && selectedComponent.type !== 'Ground';
});

function updateDirection(value: string, option: SelectOption) {
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