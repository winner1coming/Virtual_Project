<template>
  <g :transform="`translate(${outputPin.offset[0]*outputPin.scale}, ${outputPin.offset[1]*outputPin.scale}) scale(${outputPin.scale})`" cursor="move">
    <!-- 背景矩形 -->
    <rect
      x="0"
      y="0"
      :width="svgWidth"
      :height="svgHeight"
      fill=none
      stroke="black"
      stroke-width="8"
      rx="30"
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
    {{outputPin.name}}
    </text>

    <!--选中方框-->
    <SelectedBox :x="-6" :y="-6" :width="svgWidth+12" :height="svgHeight+12" :visible="circuitStore.selectedId===props.id"/>

    <!-- 每个 bit 位 -->
    <g
      v-for="(bit, index) in outputPin.getBits()"
      :key="index"
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
    <!-- 输入 -->
    <InputPort :cx="outputPin.inputPinPosition[0][0]" :cy="outputPin.inputPinPosition[0][1]" :active="outputPin.inputs[0]" :bitWidth="outputPin.bitWidth" />
  </g>
</template>

<script setup>
import { computed } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'

import { useCircuitStore } from '@/store/CircuitStore'

const circuitStore = useCircuitStore();
const props = defineProps({
id: {
  type: Number,
  required: true
},
})

const outputPin = computed(() => {
return circuitStore.getComponent(props.id);  
});

// 排布相关参数
const colMax = 8 // 每行最多8列
const cellWidth = 40
const cellHeight = 60
const padding = 40

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
const cols = Math.min(outputPin.value.bitWidth, colMax)
return cols * cellWidth + padding
})

const svgHeight = computed(() => {
const rows = Math.ceil(outputPin.value.bitWidth / colMax)
return rows * cellHeight + padding/2
})
</script>
