<template>
  <g :transform="transform" cursor="move">
      <!-- 图形 -->
      <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M223.65 362.58L183.982 308.585"><!--左下方斜线-->
      </path>
      <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M223.71 261L184.042 314.995"><!--左上方斜线-->
      </path>
      <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    :d="`M219.061 262.579L${x_right+6} 262.579`"><!--上方横线-->
      </path>
      <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    :d="`M${x_right} 362.579L${x_right} 262.579`"><!--右方竖线-->
      </path>
      <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    :d="`M218.999 361L${x_right+6} 361`"><!--下方横线-->
      </path>

      <!--选中方框-->
      <SelectedBox :x="175" :y="255" :width="x_right-178+12" :height="112" :visible="circuitStore.selectedId===props.id"/>

      <!-- 输出引脚 -->
      <OutputPort :cx="183.98" :cy="310" :active="tunnel.outputs[0]" :bitWidth="tunnel.bitWidth" />
      <!--填充透明区域-->
      <path
        fill="transparent"
        :d="`M223.65 362.58 L183.982 308.585 L223.71 261 L${x_right+6} 262.579 L${x_right+6} 362.579 L218.999 361 Z`"
      />
  </g>
  <g :transform="`translate(${tunnel.offset[0]*tunnel.scale}, ${tunnel.offset[1]*tunnel.scale}) scale(${tunnel.scale})`" cursor="move">
      <!-- 文本 -->
      <text
          ref="labelTextBox"
          :x="tunnel.direction==='west'? `${(2*184)-x_right+30}`:234"
          :y="285 + 48"
          font-family="Arial"
          :font-size="48"
      >
      {{ tunnel.name }}
      </text>
  </g>
</template>
  
<script setup>
import { ref, reactive, computed, onMounted, onUnmounted, watch, nextTick } from 'vue'
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
});

const tunnel = computed(() => {
    return circuitStore.getComponent(props.id);  
});

const labelTextBox = ref(null);
// let label = ref('LBL')
const tunnelName = computed(() => tunnel.value.name);
const x_right = ref(313);

// const directionToAngle = {
//   east: 0,
//   south: 90,
//   west: 180,
//   north: 270
// }

function updateXRight() {
  if (labelTextBox.value && tunnel.value.name) {
    const bbox = labelTextBox.value.getBBox()
    x_right.value = bbox.x + bbox.width + 20
  }
}

// 监听 label 变化，并等待 DOM 更新
watch(tunnelName, async () => {
  await nextTick()
  updateXRight()
})

// 初始化时也调用一次
onMounted(async () => {
  await nextTick()
  updateXRight()
})

// 添加翻转 transform
const transform = computed(() => {
  const [x, y] = tunnel.value.offset;
  const scale = tunnel.value.scale;
  const cx = 183.98;

  if (tunnel.value.direction === 'west') {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
      translate(${cx}, 0)
      scale(-1, 1)
      translate(${-cx}, 0)
    `;
  } else {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
    `;
  }
});
// const textTransform = computed(() => {
//   if (tunnel.value.direction === 'west') {
//     // 中心点 cx = 183.98，文字原始 x = 234
//     // 镜像后的 x 位置为：(2 * cx - 原始 x)
//     const mirroredX = 2 * 183.98 - 234;

//     return ``; 
//     // 解释：
//     // 1. 先平移到正确镜像位置（相对于整个翻转 g）
//     // 2. 然后在文字内部反转回来，保持文字不被镜像
//   }
//   return '';
// });
</script>

<style scoped>
svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
}
</style>
  