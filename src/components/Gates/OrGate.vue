<template>
    <g :transform="`translate(${orGate.position[0]}, ${orGate.position[1]}) scale(${orGate.scale})`" cursor="move">
      <!-- OR 门图形 -->
      <path stroke="black" stroke-width="12" d="M145.999 181L315.999 181" /> <!--上下两条横线-->
      <path stroke="black" stroke-width="12" d="M146 395.115L316 395.115" />
      <path fill="none" stroke="black" stroke-width="12" d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739" /> <!--右上角的小曲线-->
      <path fill="none" stroke="black" stroke-width="12" d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375" /> <!--右下角的小曲线-->
      <path stroke="black" stroke-width="12" d="M440 288L497 288" /> <!--输出端的横线-->
      <path stroke="black" stroke-width="12" d="M368.226 198.885L450.774 292.115" /> <!--右上角的斜直线-->
      <path stroke="black" stroke-width="12" d="M368.227 377.23L450.775 284" /> <!--右下角的斜直线-->
      <path fill="none" stroke="black" stroke-width="12" d="M149 179.5C188.3 212.65 273.5 295 149 397" /> <!--左侧曲线-->
      <!--填充透明区域-->
      <!-- <path
        fill="transparent"
        d="
          M145.999 181
          L315.999 181
          C313.833 179.216 358.755 182.869 373.209 204.739
          L450.774 292.115
          L450.775 284
          L368.227 377.23
          C358.756 393.246 313.834 396.899 303.27 394.502
          L146 395.115
          C273.5 295 188.3 212.65 149 179.5
          Z"
      /> -->

      <!-- 上方引脚竖线 -->
      <path
        v-if="minY < bezierYMin"
        :d="`M149 ${minY-6}L149 ${bezierYMin+6}`"
        stroke="black"
        stroke-width="12"
      />
      <!-- 下方引脚竖线 -->
      <path
        v-if="maxY > bezierYMax"
        :d="`M149 ${bezierYMax-6}L149 ${maxY+6}`"
        stroke="black"
        stroke-width="12"
      />

      <!--选中方框-->
      <SelectedBox :x="82" :y="minY<181? minY-12: 175" :width="424" :height="minY<181? (maxY-minY+24): 226" :visible="true"/>

      <!-- 输入引脚 -->
      <template v-for="(input, index) in orGate.inputs" :key="input.id">
        <circle
          v-if="orGate.inputInverted[index]"
          :cx="`${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2-26}`"
          :cy="inputYs[index]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path
          v-if="orGate.inputInverted[index]"
          :d="`M${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x1} ${inputYs[index]}L${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2-36} ${inputYs[index]}`"
          stroke="black"
          stroke-width="12"
          />
        <path
          v-if="!orGate.inputInverted[index]"
          :d="`M${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x1} ${inputYs[index]}L${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2} ${inputYs[index]}`"
          stroke="black"
          stroke-width="12"
        />
        <InputPort
          :cx="92"
          :cy="inputYs[index]"
          :active="input"
          :bitWidth="orGate.width"
          @toggle="() => handleToggleInput(index)"
        />
      </template>



      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="orGate.outputs[0]" />
    </g>
</template>

<script setup>
// import {
//   createInputs,
//   setInputValue,
//   toggleInput,
//   setInputInverted,
//   setScale
// } from '@/logic/usegates/useLogicGates'
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { defineProps } from 'vue'

import { useGateLayout, getInputLine } from '@/logic/usegates/useGateLayout'
import { useCircuitStore } from '@/store/CircuitStore'
import {watchComponentChanges} from '@/modules/useComponentsWatchers'


//const inputCount = 3
//console.log(computedLineBottom, computedLineTop)

// const orGate = reactive({
//   x: 0,
//   y: 0,
//   scale: 1,
//   inputCount: 8,
//   inputs: [],
//   output: false,
// })
const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: Number,
    required: true
  }
})
// const id = circuitStore.addComponent('Or', [0,0]);  // debug

const orGate = computed(() => {
  // return circuitStore.getComponent(id);   //debug
  return circuitStore.getComponent(props.id);  
});

let inputYs = useGateLayout(orGate.value.inputCount)
const bezierYMin = 179.5;
const bezierYMax = 397;
let minY = Math.min(...inputYs.value);
let maxY = Math.max(...inputYs.value);

function setInputCount(newCount)
{
  orGate.value.changeInputPinCount(newCount);
  inputYs = useGateLayout(orGate.value.inputCount)

  minY = Math.min(...inputYs.value);
  maxY = Math.max(...inputYs.value);
}

const {unwatchInputCount } = watchComponentChanges(orGate, setInputCount);

onUnmounted(() => {
  unwatchInputCount; // 清理监听
});


// 以下调试用，后期删除  todo -----------------------------------------------------------

// handleSetInputInverted(1, true);
// handleSetScale(0.5)

function handleToggleInput(index) {
  // todotodo
  setInputCount(8);
  if(orGate.value.inputs[index] === 0){
    orGate.value.changeInput(index, 1);
  }else{
    orGate.value.changeInput(index, 0);
  }
}

function handleSetInputInverted(index, inverted) {
  setInputInverted(orGate, index, inverted, updateOutput)
}

function handleSetScale(newscale)
{
  setScale(orGate, newscale)
}

function updateOutput() {
  orGate.output = orGate.inputs.some(input =>
    input.inverted ? !input : input
  )
}

// onMounted(()=>{
//  orGate.inputs = computed(()=>createInputs(orGate.inputCount));
// })

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
