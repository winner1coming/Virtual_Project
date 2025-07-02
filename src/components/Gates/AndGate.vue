<template>
    <g :transform="`translate(${andGate.position[0]}, ${andGate.position[1]}) scale(${andGate.scale})`" cursor="move">
      <!-- AND 门图形 -->
       <!--左侧竖线-->
      <path
        stroke="black"
        stroke-width="12"
        :d="minY<181? `M149 ${minY-6}L149 ${maxY+6}`: `M149 175L149 401`"
      />
      <path stroke="black" stroke-width="12" d="M149 395L372 395" />
      <path stroke="black" stroke-width="12" d="M149 181L372 181" />
      <path
        fill="transparent"
        stroke="black"
        stroke-width="12"
        d="M366.5 180C507 259.301 410.965 399.972 366.5 395"
      />
      <path stroke="black" stroke-width="12" d="M440 288L497 288" />
      
      <!--选中方框-->
      <SelectedBox :x="82" :y="minY<181? minY-12: 175" :width="424" :height="minY<181? (maxY-minY+24): 226" :visible="true"/>
      
      <!-- 输入引脚 -->
      <template v-for="(input, index) in andGate.inputs" :key="index">
        <circle
          v-if="andGate.inputInverted[index]"
          :cx="149 - 26"
          :cy="andGate.inputPinPosition[index][1]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path v-if="!andGate.inputInverted[index]":d="`M92 ${andGate.inputPinPosition[index][1]}L149 ${andGate.inputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
        <InputPort :cx="92" :cy="andGate.inputPinPosition[index][1]" :active="input" :bitWidth="andGate.bitWidth" @toggle="() => handleToggleInput(index)"/>
      </template>

      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="andGate.outputs[0]" />

    </g>
</template>

<script setup>
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { defineProps } from 'vue'

import { useCircuitStore } from '@/store/CircuitStore'

const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: Number,
    required: true
  }
})
// const id = circuitStore.addComponent('And', [0,0]);  // debug

// console.log('AndGate.vue props.id:', props.id); // debug
const andGate = computed(() => {
  // return circuitStore.getComponent(id);   // debug
  return circuitStore.getComponent(props.id);  
});

let minY = computed(()=>Math.min(...andGate.value.inputPinPosition.map(pin => pin[1])));
let maxY = computed(()=>Math.max(...andGate.value.inputPinPosition.map(pin => pin[1])));

// function setInputCount()
// {
//   // 更新输入引脚的布局
//   inputYs = useGateLayout(andGate.value.inputCount)

//   minY = Math.min(...inputYs.value);
//   maxY = Math.max(...inputYs.value);

//   // 更新引脚位置
//   andGate.value.inputPinPosition = andGate.value.inputPinPosition.map((pin, index) => {
//     return [
//       andGate.value.position[0] + 92 * andGate.value.scale,
//       andGate.value.position[1] + inputYs.value[index] * andGate.value.scale,
//     ];
//   });
// }

// const unwatchInputCount  = watchComponentChanges(andGate, setInputCount);

// onUnmounted(() => {
//   unwatchInputCount(); // 清理监听
// });


// 以下为调试用代码，后期可删除---------------------------------------

// 测试用函数，后期删掉  todo
function handleToggleInput(index) {
  //toggleInput(andGate, index, updateOutput)
  // todotodo
  setInputCount(10);
  if(andGate.value.inputs[index] === 0){
    andGate.value.changeInput(index, 1);
  }else{
    andGate.value.changeInput(index, 0);
  }

  // test
  // this.setInputCount(4); // 更新输入引脚布局
  andGate.value.changeInputInverted(index, !andGate.value.inputInverted[index]); // 切换输入引脚的反相状态
}

function handleSetScale(newScale){
  andGate.value.scale = newScale;
}

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
