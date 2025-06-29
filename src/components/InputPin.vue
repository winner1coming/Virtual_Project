<template>
    <svg
        viewBox="0 0 600 600"
        width="300"
        height="300"
        style="overflow: visible;"
    >
      <g :transform="`translate(0, 0) scale(1)`" cursor="move">
      <!-- 背景矩形 -->
      <rect
        x="0"
        y="0"
        :width="svgWidth"
        :height="svgHeight"
        fill=none
        stroke="black"
        stroke-width="8"
        rx="6"
      />
  
      <!-- 每个 bit 位 -->
      <g
        v-for="(bit, index) in bits"
        :key="index"
        @click="toggleBit(index)"
        style="cursor: pointer;"
      >
        <text
          :x="bitX(index)"
          :y="bitY(index)"
          font-size="36"
          text-anchor="middle"
          alignment-baseline="middle"
        >
          {{ bit }}
        </text>
      </g>
      <!-- 输出 -->
      <OutputPort :cx="svgWidth" :cy="svgHeight/2" active=1 />
      </g>
    </svg>
  </template>
  
  <script setup>
  import { ref, computed, watch } from 'vue'
  import OutputPort from './OutputPort.vue'
  
  const props = defineProps({
    bitWidth: {
      type: Number,
      default: 32,
      validator: (v) => v >= 1 && v <= 32
    },
    bits: Array
  })
  
  // 排布相关参数
  const colMax = 8 // 每行最多8列
  const cellWidth = 40
  const cellHeight = 60
  const padding = 40
  
  // bits 状态
  const bits = ref(Array(props.bitWidth).fill(0))
  
  watch(() => props.bitWidth, (newWidth) => {
    bits.value = Array(newWidth).fill(0)
  })
  
  // 点击切换
  function toggleBit(index) {
    bits.value[index] = bits.value[index] === 0 ? 1 : 0
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
    const cols = Math.min(props.bitWidth, colMax)
    return cols * cellWidth + padding
  })
  
  const svgHeight = computed(() => {
    const rows = Math.ceil(props.bitWidth / colMax)
    return rows * cellHeight + padding/2
  })
  </script>
  