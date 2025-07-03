<template>
  <div class="component-properties">
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
          v-model:value="circuitStore.getComponent(circuitStore.selectedId).direction"  
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
          :value="circuitStore.getComponent(circuitStore.selectedId).bitWidth"
          :options="bitWidthOptions.map(width => ({ label: `${width} 位`, value: width }))"
          @update:value="updateBitWidth"
        >
        </n-select>
      </div>

      <!-- 修改引脚数量-->
      <div class="property-item" v-show="showPinCountOptions">
        <label for="pinCount">引脚数量：</label>
        <n-select
          id="pinCount"
          v-model:value="circuitStore.getComponent(circuitStore.selectedId).inputCount"
          :options="pinCountOptions.map(count => ({ label: `${count} 个`, value: count }))"
          @update:value="updateInputCount"
        >
        </n-select>
       </div>

      <!-- 修改scale-->
      <div class="property-item">
        <label for="scale">大小：</label>
        <n-select
          id="scale"
          v-model:value="circuitStore.getComponent(circuitStore.selectedId).scale"
          :options="[
            { label: '小', value: 0.25 },
            { label: '中', value: 0.5 },
            { label: '大', value: 1 },
          ]"
        >
        </n-select>
      </div>

      <!-- 支持输入反转-->
      <div class="invert-container" v-show="showInvertInputOption">
        <div v-for="(invert, index) in circuitStore.getComponent(circuitStore.selectedId).inputInverted" :key="index" class="invert-item">
          <label>反转输入 {{ index}}:</label>
          <n-select
            :value="circuitStore.getComponent(circuitStore.selectedId).inputInverted[index]? 1 : 0"
            :options="[
              { label: '否', value: 0 },
              { label: '是', value: 1 }
            ]"
            @update:value="(value, option) => {
              circuitStore.getComponent(circuitStore.selectedId).inputInverted[index] = value;
            }"
          >
          </n-select>
        </div>
      </div>

    </div>
    <div v-else>
      <p align="center">选中元件以查看属性</p>
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

// 数据位宽选项
const bitWidthOptions = ref([1, 2, 4, 8, 16, 32, 64]);
// 引脚数量选项
const pinCountOptions = ref([2,3,4,5,6,7,8]);
// 判断是否显示修改引脚数量的选项
const showPinCountOptions = computed(() => {
  const selectedComponent = circuitStore.getComponent(circuitStore.selectedId);
  return selectedComponent.type !== 'NOT' && selectedComponent.type !== 'CLOCK' &&
        selectedComponent.type !== 'INPUT' && selectedComponent.type !== 'OUTPUT' &&
        selectedComponent.type !== 'SegmentDisplay' && selectedComponent.type !== 'TUNNEL' &&
         selectedComponent.type !== 'POWER' && selectedComponent.type !== 'GROUND';
});
// 判断是否显示输入反转的选项
const showInvertInputOption = computed(() => {
  const selectedComponent = circuitStore.getComponent(circuitStore.selectedId);
  return selectedComponent.type !== 'NOT' && selectedComponent.type !== 'CLOCK' &&
         selectedComponent.type !== 'INPUT' && selectedComponent.type !== 'OUTPUT' &&
         selectedComponent.type !== 'SegmentDisplay' && selectedComponent.type !== 'TUNNEL' &&
         selectedComponent.type !== 'POWER' && selectedComponent.type !== 'GROUND';
});

function updateDirection(value: string, option: SelectOption) {
  // 更新元件的方向
  eventBus.emit('updateComponentDirection');
}

function updateBitWidth(value: number, option: SelectOption) {
  // 更新元件的数据位宽
  // eventBus.emit('updateComponentBitWidth');
  circuitStore.getComponent(circuitStore.selectedId).setBitWidth(value);
}

function updateInputCount(value: number) {
  circuitStore.getComponent(circuitStore.selectedId).changeInputPinCount(value);
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

.invert-container {
  margin-bottom: 10px;
  display: flex;
  flex-direction: column;
}
.invert-container label {
  display: inline-block;
  font-weight: bold;
  width: 80px;
}
.invert-item {
  display: flex;
  align-items: center;
  margin-bottom: 5px;
}
.invert-item span {
  margin-right: 10px;
}
</style>