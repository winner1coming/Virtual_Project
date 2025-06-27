<template>
    <g @click.stop="toggle">
      <circle
        :cx="cx"
        :cy="cy"
        r="12"
        fill="lightblue"
        @click.stop="toggle"
      />
      <text
        :x="cx"
        :y="cy + 4"
        text-anchor="middle"
        font-size="12"
        fill="black"
      >
        {{ displayValue }}
      </text>
    </g>
  </template>
  
  <script setup>
  import {computed} from 'vue'
  
  const props = defineProps({
    cx: Number,
    cy: Number,
    value: Number,        // 支持多位输入的值
    bitWidth: {
      type: Number,
      default: 1          // 默认为 1 位输入
    },
  })
  const emit = defineEmits(['change', 'toggle']) 
  
  function toggle() {
    emit('toggle')
  }
  
  // 切换函数（这里只是 demo，实际可能需外部输入）
//   function toggle() {
//     const newValue = (props.value + 1) % (2 ** props.bitWidth)
//     emit('change', newValue)
//   }
  
  // 格式化显示（如二进制/十六进制）
  // const displayValue = computed(() => {
  //   if (props.bitWidth <= 4) {
  //     return props.value.toString(2).padStart(props.bitWidth, '0')
  //   } else {
  //     return '0x' + props.value.toString(16).toUpperCase()
  //   }
  // })
  </script>
  