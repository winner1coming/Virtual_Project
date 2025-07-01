<!-- <template>
    <rect x="92" y="175" width="393" height="251" stroke="rgba(179, 179, 179, 1)" stroke-width="3"   >
    </rect>
    <rect x="82" y="165" width="20" height="20" stroke="rgba(179, 179, 179, 1)" stroke-width="3"      fill="#FFFFFF" >
    </rect>
    <rect x="82" y="416" width="20" height="20" stroke="rgba(179, 179, 179, 1)" stroke-width="3"      fill="#FFFFFF" >
    </rect>
    <rect x="475" y="165" width="20" height="20" stroke="rgba(179, 179, 179, 1)" stroke-width="3"      fill="#FFFFFF" >
    </rect>
    <rect x="475" y="416" width="20" height="20" stroke="rgba(179, 179, 179, 1)" stroke-width="3"      fill="#FFFFFF" >
    </rect>
</template> -->
<template>
  <svg
    viewBox="0 0 600 600"
    width="300"
    height="300"
    style="overflow: visible;"
    
  >
    <g :transform="`translate(${andGate.position[0]}, ${andGate.position[1]}) scale(${andGate.scale})`" cursor="move">
      <!-- 主矩形 -->
      <rect
        :x="x"
        :y="y"
        :width="width"
        :height="height"
        stroke="rgba(179, 179, 179, 1)"
        stroke-width="3"
        fill="none"
      />
  
      <!-- 四个顶点锚点 -->
      <rect
        :x="x - handleSize"
        :y="y - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
      <rect
        :x="x - handleSize"
        :y="y + height - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
      <rect
        :x="x + width - handleSize"
        :y="y - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
      <rect
        :x="x + width - handleSize"
        :y="y + height - handleSize"
        :width="handleSize * 2"
        :height="handleSize * 2"
        :stroke="strokeColor"
        :stroke-width="3"
        fill="#FFFFFF"
      />
    </g>
  </svg>
</template>
  
  <script setup>
  const props = defineProps({
    x: Number,
    y: Number,
    width: Number,
    height: Number,
  })
  
  const handleSize = 10
  const strokeColor = 'rgba(179, 179, 179, 1)'
  </script>
  