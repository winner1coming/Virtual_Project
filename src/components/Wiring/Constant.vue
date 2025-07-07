<template>
    <g :transform="`translate(${constant.offset[0]*constant.scale}, ${constant.offset[1]*constant.scale}) scale(${constant.scale})`" cursor="move">
        <rect
            x="0"
            y="0"
            :width="rect_width"
            height="100"
            fill="#D3D3D3"
        />
        <!-- 文本 -->
        <text
            :key="constant.getValue()"
            ref="constantTextBox"
            :x="16"
            :y="71"
            font-family="Arial"
            :font-size="48"
            letter-spacing="5"
        >
        {{ constant.getValue() }}
        </text>
        
        <!--选中方框-->
        <SelectedBox :x="0" :y="0" :width="rect_width+12" :height="100" :visible="circuitStore.selectedId===props.id"/>

        <!-- 输出引脚 -->
        <OutputPort :cx="constant.outputPinPosition[0][0]" :cy="constant.outputPinPosition[0][1]" :active="1" />
    </g>
</template>
  
<script setup>
import { ref, reactive, computed, onMounted, onUnmounted, watch, nextTick } from 'vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import InputPort from '@/components/Ports/InputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { defineProps } from 'vue'

import { useCircuitStore } from '@/store/CircuitStore'
import {watchComponentChanges} from '@/modules/useComponentsWatchers'

const circuitStore = useCircuitStore();

const props = defineProps({
    id: {
        type: Number,
        required: true
    }
});
// const id = circuitStore.addComponent('And', [0,0]);  // debug

const constant = computed(() => {
    // return circuitStore.getComponent(id);   // debug
    return circuitStore.getComponent(props.id);  
});

const constantTextBox = ref(null);
// let constant_value = ref('1f');
const constant_value = computed(()=>constant.value.getValue());
const rect_width = ref(95);

function updateRect() {
  if (constantTextBox.value) {
    const bbox = constantTextBox.value.getBBox()
    rect_width.value = bbox.width + 40
    constant.value.rectWidth = rect_width.value;
    constant.value.updatePinPosition();
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


</script>

<style scoped>
svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
}
</style>
  