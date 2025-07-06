<template v-if="steps.length > 0">
  <div class="screenshot-tutorial">
    <div class="tutorial-content">
      <!-- 按钮先放上面 -->
      <div class="tutorial-buttons top-buttons">
        <n-button 
        type="primary" 
        quaternary
        style=
        "background: #aadaed; 
        color: black;
        border-radius: 8px;
        padding: 0.5rem 1.5rem;
        font-size: 16px;"
        @click="nextStep"
        v-if="!isLastStep">
          下一步
        </n-button>

        <n-button 
        type="success" 
          quaternary
          style=
          "background: #aadaed; 
          color: black;
          border-radius: 8px;
          padding: 0.5rem 1.5rem;
          font-size: 16px;"
        @click="finishTutorial" 
        v-else>
          完成教学
        </n-button>

        <n-button 
        type="error" 
        @click="finishTutorial" 
        style=
        "background: #b5d9e8b9; 
        color: black;
        margin-left: 8px;
        border-radius: 8px;
        padding: 0.5rem 1.5rem;
        font-size: 16px;">
          退出教学
        </n-button>
      </div>
      <img 
      v-if="currentStep"
        :src="currentStep.image"
        class="tutorial-image-full"
        alt="教学步骤截图"
      />
      <p v-if="currentStep" class="tutorial-text">{{ currentStep.text }}</p>
    </div>
  </div>
</template>


<script setup>
import { ref, computed } from 'vue'
import { useRouter,useRoute } from 'vue-router'

const router = useRouter()
const route = useRoute()
const experiment = computed(() => route.query?.experiment ?? 'default')

console.log('experiment:', experiment.value)

const stepsMap = {
  adder8bit: [
      {
        text: '本次实验的目标是完成一个8位可控加法器。' +
          '在动手搭建之前，请先打开资料文件，详细学习一位全加器的工作原理，因为它是8位加法器的基础组件。',
        image: new URL('@/assets/teaching/adder8bit/step1.png', import.meta.url).href
      },
      {
        text: '第2步：进入电子元件选择区域，' +
          '将所需的输入和输出元件拖放到画布上。',
        image: new URL('@/assets/teaching/adder8bit/step2.png', import.meta.url).href
      },
      {
        text: '第3步：选择与门、或门和异或门等逻辑器件，' +
          '根据一位全加器的逻辑表达式正确连接它们的输入输出，完成基本电路搭建。',
        image: new URL('@/assets/teaching/adder8bit/step3.png', import.meta.url).href
      },
      {
        text: '第4步：切换至资料文件，' +
          '查找与8位可控加法器设计相关的参考资料，' +
          '在项目文件中创建一个新的电路文件，' +
          '这里提供了已封装的一位全加器模块，可直接复用进行组合。',
        image: new URL('@/assets/teaching/adder8bit/step4.png', import.meta.url).href
      },
      {
        text: '第5步：学习8位可控加法器的电路结构和原理，' +
          '该电路包含8×2+1个输入端和8+1个输出端，' +
          '请按照示意图正确连接所有输入输出引脚，确保逻辑关系无误。',
        image: new URL('@/assets/teaching/adder8bit/step5.png', import.meta.url).href
      },
      {
        text: '最后：验证实验结果的正确性，' +
          '通过不断调整输入信号，观察输出结果是否与预期一致，' +
          '确保电路设计达到设计目标。',
        image: new URL('@/assets/teaching/adder8bit/step6.png', import.meta.url).href
      },
  ],
  carrylookahead4bit:[
    {
      text: '四位并行进位加法器 - 第1步：打开学习资料，学习先行进位得原理',
      image: new URL('@/assets/teaching/carrylookahead4bit/step1.png', import.meta.url).href
    },
    {
      text: '四位并行进位加法器 - 第2步：理解进位生成和进位传递',
      image: new URL('@/assets/teaching/carrylookahead4bit/step2.png', import.meta.url).href
    },
    {
      text: '四位并行进位加法器 - 第3步：拖拽输入输出元件...得到进位生成G和进位传递P的输入',
      image: new URL('@/assets/teaching/carrylookahead4bit/step3.png', import.meta.url).href
    },
    {
      text: '四位并行进位加法器 - 第4步：根据原理公式绘制C1-C4的电路图',
      image: new URL('@/assets/teaching/carrylookahead4bit/step4.png', import.meta.url).href
    },
    {
      text: '四位并行进位加法器 - 第5步：P*的输出逻辑',
      image: new URL('@/assets/teaching/carrylookahead4bit/step5.png', import.meta.url).href
    },
    {
      text: '四位并行进位加法器 - 最后：G*的输出逻辑——完成最后的电路',
      image: new URL('@/assets/teaching/carrylookahead4bit/step6.png', import.meta.url).href
    },
  ],
  default:[{
    text:"你访问的界面不存在",
    image:'@/assets/teaching/test4.png'
  }]
}

const steps = computed(() => stepsMap[experiment.value] || stepsMap.default)

const currentStepIndex = ref(0)
const currentStep = computed(() => steps.value[currentStepIndex.value])
const isLastStep = computed(() => currentStepIndex.value === steps.value.length - 1)


const nextStep = () => {
  if (currentStepIndex.value < steps.value.length - 1) {
    currentStepIndex.value++
  }
}

const finishTutorial = () => {
  router.push({
    path: '/workspace',
    query: {mode :'tutorial'}
  })
}
</script>

<style scoped>
.screenshot-tutorial {
  position: fixed;
  top: 60px; 
  left: 0;
  width: 100%;
  height: calc(100% - 60px);
  background: rgba(214, 211, 211, 0.71);
  z-index: 2000;
  display: flex;
  justify-content: center;
  align-items: center;
}

.tutorial-content {
  background: transparent;
  padding: 0;
  border-radius: 0;
  box-shadow: none;
  width: 100%;
  height: 100%;
  text-align: center;
  position: relative;
}

.tutorial-image-full {
  max-width: 100%;
  max-height: 100%;
  width: auto;
  height: auto;
  margin: auto;
}

.tutorial-buttons.top-buttons {
  position: absolute;
  top: 20px;
  left: 50%;
  transform: translateX(-50%);
  display: flex;
  gap: 1.0rem;
  z-index: 10;
}

.tutorial-text {
  position: absolute;
  bottom: 0px;
  left: 50%;
  transform: translateX(-50%);
  background: rgba(193, 105, 105, 0.645);
  color: #000000;
  padding: 0.6rem 1rem;
  border-radius: 4px;
  font-size: 1rem;
}

</style>
