<template>
    <g :transform="`translate(${register.offset[0]*register.scale}, ${register.offset[1]*register.scale}) scale(${register.scale})`" cursor="move">
        <!-- 图形 -->
        <rect x="143" y="174" width="227" height="227" stroke="rgba(0, 0, 0, 1)" stroke-width="12"      fill="#FFFFFF" >
        </rect>

        <!--选中方框-->
        <SelectedBox :x="86" :y="168" :width="346" :height="264" :visible="circuitStore.selectedId===props.id"/>
  
        <!--左边两个输入-->
        <path    stroke="black" stroke-width="12"    d="M92 235L149 235">
        </path>
        <InputPort :cx="92" :cy="235" :active="register.inputs[0]" :bitWidth="register.bitWidth" />
        <!--todo-->
        <path    stroke="black" stroke-width="12"    d="M92 300L149 300">
        </path>
        <InputPort :cx="92" :cy="300" :active="register.inputs[1]" :bitWidth="register.bitWidth" />
        <!--右边的输出-->
        <path    stroke="black" stroke-width="12"    d="M365 235L422 235">
        </path>
        <OutputPort :cx="422" :cy="235" :active="register.outputs[0]" />
        <!--下面两个输入-->
        <path    stroke="black" stroke-width="12"    d="M203 401L203 426">
        </path>
        <InputPort :cx="203" :cy="426" :active="register.inputs[2]" :bitWidth="register.bitWidth" />
        <path    stroke="black" stroke-width="12"    d="M329 401L329 426">
        </path>
        <InputPort :cx="329" :cy="426" :active="register.inputs[3]" :bitWidth="register.bitWidth" />

        <!--字母-->
        <g >
        <path     fill="black"  d="M188.976 381.304C192.48 381.304 195.264 380.152 197.52 378.664L195.984 375.736C194.016 377.032 192 377.8 189.456 377.8C184.512 377.8 181.104 374.248 180.816 368.68L198.384 368.68C198.48 368.008 198.576 367.144 198.576 366.184C198.576 358.744 194.832 353.944 188.16 353.944C182.208 353.944 176.496 359.176 176.496 367.672C176.496 376.264 182.016 381.304 188.976 381.304ZM180.768 365.56C181.296 360.376 184.56 357.448 188.256 357.448C192.336 357.448 194.736 360.28 194.736 365.56L180.768 365.56ZM205.008 380.68L209.424 380.68L209.424 361.768C212.016 359.128 213.84 357.784 216.528 357.784C219.984 357.784 221.472 359.848 221.472 364.744L221.472 380.68L225.84 380.68L225.84 364.168C225.84 357.544 223.344 353.944 217.872 353.944C214.32 353.944 211.584 355.912 209.136 358.408L209.04 358.408L208.608 354.616L205.008 354.616L205.008 380.68Z">
        </path>
        </g>
        <g >
        <path     fill="black"  d="M308.416 380.68L312.832 380.68L312.832 363.928C314.56 359.512 317.2 357.88 319.36 357.88C320.464 357.88 321.04 358.024 321.904 358.312L322.72 354.52C321.904 354.088 321.088 353.944 319.936 353.944C317.056 353.944 314.368 356.056 312.544 359.368L312.448 359.368L312.016 354.616L308.416 354.616L308.416 380.68ZM337.6 381.304C341.104 381.304 343.888 380.152 346.144 378.664L344.608 375.736C342.64 377.032 340.624 377.8 338.08 377.8C333.136 377.8 329.728 374.248 329.44 368.68L347.008 368.68C347.104 368.008 347.2 367.144 347.2 366.184C347.2 358.744 343.456 353.944 336.784 353.944C330.832 353.944 325.12 359.176 325.12 367.672C325.12 376.264 330.64 381.304 337.6 381.304ZM329.392 365.56C329.92 360.376 333.184 357.448 336.88 357.448C340.96 357.448 343.36 360.28 343.36 365.56L329.392 365.56Z">
        </path>
        </g>
        <g >
        <path     fill="black"  d="M326.26 253.76C318.34 253.76 313.18 246.68 313.18 235.46C313.18 224.48 318.34 217.7 326.26 217.7C334.18 217.7 339.34 224.48 339.34 235.46C339.34 246.68 334.18 253.76 326.26 253.76ZM339.7 268.64C342.34 268.64 344.68 268.22 346 267.62L344.92 263.36C343.78 263.72 342.28 264.02 340.3 264.02C335.56 264.02 331.48 262.04 329.5 258.14C338.8 256.52 345.04 248.12 345.04 235.46C345.04 221.36 337.3 212.84 326.26 212.84C315.22 212.84 307.48 221.36 307.48 235.46C307.48 248.36 313.96 256.88 323.56 258.2C326.02 264.2 331.6 268.64 339.7 268.64Z">
        </path>
        </g>
        <g >
        <path     fill="black"  d="M189.62 323.38C195.32 323.38 199.64 321.1 203.12 317.08L200.06 313.54C197.24 316.66 194.06 318.52 189.86 318.52C181.46 318.52 176.18 311.56 176.18 300.46C176.18 289.48 181.76 282.7 190.04 282.7C193.82 282.7 196.7 284.38 199.04 286.84L202.04 283.24C199.52 280.42 195.32 277.84 189.98 277.84C178.82 277.84 170.48 286.42 170.48 300.64C170.48 314.92 178.64 323.38 189.62 323.38ZM211.34 322.6L236.12 322.6L236.12 317.86L216.86 317.86L216.86 278.62L211.34 278.62L211.34 322.6ZM243.92 322.6L249.44 322.6L249.44 308.68L257 299.68L270.2 322.6L276.38 322.6L260.48 295.3L274.28 278.62L267.98 278.62L249.56 300.7L249.44 300.7L249.44 278.62L243.92 278.62L243.92 322.6Z">
        </path>
        </g>
        <g >
        <path     fill="black"  d="M173.06 257.6L184.28 257.6C197.54 257.6 204.74 249.38 204.74 235.46C204.74 221.42 197.54 213.62 184.04 213.62L173.06 213.62L173.06 257.6ZM178.58 253.04L178.58 218.12L183.56 218.12C193.94 218.12 199.04 224.3 199.04 235.46C199.04 246.56 193.94 253.04 183.56 253.04L178.58 253.04Z">
        </path>
        </g>
    </g>
</template>

<script setup>
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
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
})
// const id = circuitStore.addComponent('And', [0,0]);  // debug

// console.log('AndGate.vue props.id:', props.id); // debug
const register = computed(() => {
  // return circuitStore.getComponent(id);   // debug
  return circuitStore.getComponent(props.id);  
});
</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
