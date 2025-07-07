<template>
  <div class="component-properties">
    <div v-if="circuitStore.selectedId !== -1">
      <!-- 修改名字 -->
      <div class="property-item">
        <label for="name">名字：</label>
        <n-input
          id="name"
          type="text"
          v-model:value="name"
          placeholder=""
          @blur="changeName()"
          @keyup.enter="changeName()"
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
      <div class="property-item" v-show="showBitWidthOptions">
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
          :value="circuitStore.getComponent(circuitStore.selectedId).inputCount"
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
          :value="circuitStore.getComponent(circuitStore.selectedId).scale"
          :options="[
            { label: '小', value: 0.25 },
            { label: '中', value: 0.5 },
            { label: '大', value: 1 },
          ]"
          @update:value="(value, option) => {
            if(value !== circuitStore.getComponent(circuitStore.selectedId).scale) {
              circuitStore.getComponent(circuitStore.selectedId).setScale(value);
            }
          }"
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
              if(value !== invert) {
                circuitStore.getComponent(circuitStore.selectedId).changeInputInverted(index);
              }
            }"
          >
          </n-select>
        </div>
      </div>

      <!-- 时钟-->
      <!-- 修改时钟周期-->
      <div class="property-item" v-if="circuitStore.getComponent(circuitStore.selectedId).type === 'CLOCK'">
        <label for="period">时钟周期(s)：</label>
        <n-input
          id="period"
          v-model:value="period"
          placeholder="请输入时钟周期（秒）"
          @blur="changePeriod(period)"
        />
      </div>
      <!-- 停止时钟-->
      <div class="property-item" v-if="circuitStore.getComponent(circuitStore.selectedId).type === 'CLOCK'">
        <label for="stopClock">停止时钟：</label>
        <n-select
          id="stopClock"
          :options="[
            { label: '是', value: 1 },
            { label: '否', value: 0 }
          ]"
          :value="(circuitStore.getComponent(circuitStore.selectedId) as Clock).isStopped ? 1 : 0"
          @update:value="(value, option) => {
            if(value === 1) {
              (circuitStore.getComponent(circuitStore.selectedId) as Clock).stop();
            } else {
              (circuitStore.getComponent(circuitStore.selectedId) as Clock).start();
            }
          }"
        >
        </n-select>
      </div>
      
      <!-- 修改数值-->
      <div class="property-item" v-if="circuitStore.getComponent(circuitStore.selectedId).type === 'CONSTANT'">
        <label for="value">数值：</label>
        <n-input
          id="value"
          v-model:value="constantValue"
          placeholder="请输入数值"
          @blur="changeValue()"
          @keyup.enter="changeValue()"
        />
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
import { NSelect, NInput } from 'naive-ui';
import type { SelectOption } from 'naive-ui'
import { ConstantInput } from '@/logic/components/ConstantInput';
import { Clock } from '@/logic/components/Clock';

const circuitStore = useCircuitStore();
const name = ref('');
const period = ref('1');
const constantValue = ref('0');
watch(() => circuitStore.selectedId, (newId) => {
  const component = circuitStore.getComponent(newId);
  if (component) {
    name.value = component.name;
    period.value = component.type === 'CLOCK' ? (component as Clock).period.toString() : '1';
    constantValue.value = component.type === 'CONSTANT' ? component.outputs[0].toString() : '0';
  }
});

// 引脚数量选项
const pinCountOptions = ref([2,3,4,5,6,7,8]);
// 判断是否显示修改引脚数量的选项
const showPinCountOptions = computed(() => {
  const selectedComponent = circuitStore.getComponent(circuitStore.selectedId);
  return selectedComponent.type !== 'NOT' && selectedComponent.type !== 'CLOCK' &&
        selectedComponent.type !== 'INPUT' && selectedComponent.type !== 'OUTPUT' &&
        selectedComponent.type !== 'SegmentDisplay' && selectedComponent.type !== 'TUNNEL' &&
         selectedComponent.type !== 'POWER' && selectedComponent.type !== 'GROUND'
         && selectedComponent.type !== 'SPLITTER' && selectedComponent.type !== 'COMBINER' &&
          selectedComponent.type !== 'CONSTANT' && selectedComponent.type !== 'HEX_DISPLAY' &&
        selectedComponent.type !== 'REGISTER' && selectedComponent.type !== 'D_FLIP_FLOP';
});
// 判断是否显示输入反转的选项
const showInvertInputOption = computed(() => {
  const selectedComponent = circuitStore.getComponent(circuitStore.selectedId);
  return selectedComponent.type !== 'NOT' && selectedComponent.type !== 'CLOCK' &&
         selectedComponent.type !== 'INPUT' && selectedComponent.type !== 'OUTPUT' &&
         selectedComponent.type !== 'SegmentDisplay' && selectedComponent.type !== 'TUNNEL' &&
         selectedComponent.type !== 'POWER' && selectedComponent.type !== 'GROUND';
})
;
// 名字
function changeName(){
  const component = circuitStore.getComponent(circuitStore.selectedId);
  if (!component) {
    return;
  }
  const newName = name.value.trim();
  if (newName === component.name) {
    return; // 如果名字没有变化，则不处理
  }
  component.setName(newName);
}
// 位置------------------------------
function updateDirection(value: string, option: SelectOption) {
  circuitStore.getComponent(circuitStore.selectedId).setDirection(value);
}

// 数据位宽选项
const bitWidthOptions = ref([1, 2, 4, 8, 16, 32, 64]);
// 位宽
const showBitWidthOptions = computed(() => {
  const selectedComponent = circuitStore.getComponent(circuitStore.selectedId);
  return selectedComponent.type !== 'CLOCK' && selectedComponent.type !== 'SegmentDisplay'  &&
         selectedComponent.type !== 'REGISTER' && selectedComponent.type !== 'D_FLIP_FLOP';;
});
function updateBitWidth(value: number, option: SelectOption) {
  // 更新元件的数据位宽
  // eventBus.emit('updateComponentBitWidth');
  if(value === circuitStore.getComponent(circuitStore.selectedId).bitWidth) {
    return;
  }
  circuitStore.getComponent(circuitStore.selectedId).setBitWidth(value);
}

function updateInputCount(value: number) {
  if(value === circuitStore.getComponent(circuitStore.selectedId).inputCount) {
    return;
  }
  circuitStore.getComponent(circuitStore.selectedId).changeInputPinCount(value);
}
// 常量输入
function changeValue(){
  const component = circuitStore.getComponent(circuitStore.selectedId) as ConstantInput;
  if (!component || component.type !== 'CONSTANT') {
    return;
  }
  if (constantValue.value === '') {
    constantValue.value = component.outputs[0].toString();
    return; 
  }

  const v = parseInt(constantValue.value);
  if (!isNaN(v) && v >= 0 && v < Math.pow(2, component.bitWidth) && v !== component.outputs[0]) {
    component.changeInput(0,v)
  } 
  constantValue.value = component.outputs[0].toString();
}

// 时钟
function changePeriod(period: string) {
  const component = circuitStore.getComponent(circuitStore.selectedId) as Clock;
  if (!component || component.type !== 'CLOCK') {
    return;
  }
  

  const v = parseFloat(period);
  if (!isNaN(v) && v > 0) {
    if(v===component.period) return;
    component.stop(); 
    component.period = v;
    component.start();
  } else {
    // 回退到当前实际输出值
    period = (component.period).toString();
  }
}

</script>

<style scoped>
.component-properties {
  padding: 10px;
  background-color: transparent;
  height: 100%;
  overflow-y: auto;
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