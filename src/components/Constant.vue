<template>
    <svg
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 600 600"
        width="300"
        height="300"
        style="overflow: visible;" 
    >
        <g :transform="`translate(${constant.position[0]}, ${constant.position[1]}) scale(${constant.scale})`" cursor="move">
            <rect
                x="218"
                y="262"
                :width="rect_width"
                height="100"
                fill="#D3D3D3"
            />
            <!-- 文本 -->
            <text
                :key="constant_value"
                ref="constantTextBox"
                :x="234"
                :y="285 + 48"
                font-family="Arial"
                :font-size="48"
                letter-spacing="5"
            >
            {{ constant_value }}
            </text>
            <!-- 输出引脚 -->
            <OutputPort :cx="218+rect_width" :cy="310" :active=true />
            <InputPort :cx="200" :cy="310" :active=0 @toggle="() => handleToggleInput()"/><!--调试用，记得删这行 todo-->
        </g>
    </svg>
</template>
  
<script setup>
import { ref, reactive, computed, onMounted, onUnmounted, watch, nextTick } from 'vue'
import OutputPort from './OutputPort.vue'
import InputPort from './InputPort.vue'
import { defineProps } from 'vue'

import { useCircuitStore } from '@/store/CircuitStore'
import {watchComponentChanges} from '@/modules/useComponentsWatchers'

const circuitStore = useCircuitStore();

const id = circuitStore.addComponent('And', [0,0]);  // debug

const constant = computed(() => {
    return circuitStore.getComponent(id);   // debug
    // return circuitStore.getComponent(props.id);  
});

const constantTextBox = ref(null);
let constant_value = ref('1f');
const rect_width = ref(95);

function updateRect() {
  if (constantTextBox.value) {
    const bbox = constantTextBox.value.getBBox()
    rect_width.value = bbox.width + 40
  }
}

// 监听 constant_value 变化，并等待 DOM 更新
watch(constant_value, async () => {
  await nextTick()
  updateRect()
})

// 初始化时也调用一次
onMounted(async () => {
  await nextTick()
  updateRect()
})

//调试用，要删 todo
function handleToggleInput() {
    constant_value.value = "3fffffff";
}

</script>

<style scoped>
svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
}
</style>
  