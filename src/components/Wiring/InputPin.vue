<template>
  <g :transform="`translate(${inputPin.offset[0]*inputPin.scale}, ${inputPin.offset[1]*inputPin.scale}) scale(${inputPin.scale})`" cursor="move">
    <!-- 背景矩形 -->
    <rect
      x="0"
      y="0"
      :width="svgWidth"
      :height="svgHeight"
      fill=none
      stroke="black"
      stroke-width="8"
    />
    
    <!--填充透明区域以便选中-->
    <rect
      x="0"
      y="0"
      :width="svgWidth"
      :height="svgHeight"
      fill=transparent
    />

    <!--显示名字-->
    <text
      :x="svgWidth/2" 
      :y="svgHeight+40" 
      text-anchor="middle"
      font-size="36"
      fill="#333"
    >
    {{inputPin.name}}
    </text>

    <!--选中方框-->
    <SelectedBox :x="-6" :y="-6" :width="svgWidth+12" :height="svgHeight+12" :visible="circuitStore.selectedId===props.id"/>

    <!-- 每个 bit 位 -->
    <g
      v-for="(bit, index) in inputPin.getBits()"
      :key="index"
      @click="toggleBit(index)"
      style="cursor: pointer;"
    >
      <text
        :x="bitX(index)"
        :y="bitY(index)"
        font-size="48"
        text-anchor="middle"
        alignment-baseline="middle"
      >
        {{ bit === -1 ? 'x' : bit }}
      </text>
    </g>
    <!-- 输出 -->
    <OutputPort :cx="inputPin.outputPinPosition[0][0]" :cy="inputPin.outputPinPosition[0][1]" :active="inputPin.outputs[0]" :bitWidth="inputPin.bitWidth" />
  </g>
</template>
  
<script setup>
import { computed } from 'vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
  
import { useCircuitStore } from '@/store/CircuitStore'

const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: Number,
    required: true
  },
})

const inputPin = computed(() => {
  return circuitStore.getComponent(props.id);  
});

// 排布相关参数
const colMax = 8 // 每行最多8列
const cellWidth = 40
const cellHeight = 60
const padding = 40

// 点击切换
function toggleBit(index) {
  inputPin.value.toggleBit(index);
}

// 计算坐标
function bitX(index) {
  const col = index % colMax
  return padding + col * cellWidth
}

function bitY(index) {
  const row = Math.floor(index / colMax)
  return padding + row * cellHeight
}

// 宽高
const svgWidth = computed(() => {
  const cols = Math.min(inputPin.value.bitWidth, colMax)
  return cols * cellWidth + padding
})

const svgHeight = computed(() => {
  const rows = Math.ceil(inputPin.value.bitWidth / colMax)
  return rows * cellHeight + padding/2
})
</script>
  