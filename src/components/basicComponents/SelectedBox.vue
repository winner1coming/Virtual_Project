<template>
    <g v-if="visible" :transform="`translate(${x}, ${y})`" cursor="move">
      <!-- 主矩形 -->
      <rect
        x="0"
        y="0"
        :width="width"
        :height="height"
        :stroke="strokeColor"
        stroke-width="3"
        fill="transparent"
      />
  
      <!-- 四个顶点锚点 -->
      <rect
        :x="0 - handleSize"
        :y="0 - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
      <rect
        :x="0 - handleSize"
        :y="0 + height - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
      <rect
        :x="0 + width - handleSize"
        :y="0 - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
      <rect
        :x="0 + width - handleSize"
        :y="0 + height - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
    </g>
</template>
  
  <script setup>
  const props = defineProps({
    x: Number,
    y: Number,
    width: Number,
    height: Number,
    visible: {
      type: Boolean,
      default: false,
    },
  })
  
  const handleSize = 20
  const strokeColor = 'rgba(179, 179, 179, 1)'

  </script>
  