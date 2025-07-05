<template>
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
        :src="currentStep.image"
        class="tutorial-image-full"
        alt="教学步骤截图"
      />
      <p class="tutorial-text">{{ currentStep.text }}</p>
    </div>
  </div>
</template>


<script setup>
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'

const router = useRouter()

// 所有步骤（换成你自己的图片路径）
const steps = [
  {
    text: '这是第一步，先认识界面布局',
    image: new URL('@/assets/teaching/test4.png', import.meta.url).href
  },
  {
    text: '这是第二步，如何拖拽与门',
    //image: new URL('@/assets/teaching/test2.jpg', import.meta.url).href
  },
  {
    text: '这是第三步，拖拽输入引脚',
    //image: new URL('@/assets/teaching/test3.jpg', import.meta.url).href
  }
]

const currentStepIndex = ref(0)
const currentStep = computed(() => steps[currentStepIndex.value])
const isLastStep = computed(() => currentStepIndex.value === steps.length - 1)

const nextStep = () => {
  if (currentStepIndex.value < steps.length - 1) {
    currentStepIndex.value++
  }
}

const finishTutorial = () => {
  router.push('/')
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
  bottom: 20px;
  left: 50%;
  transform: translateX(-50%);
  background: rgba(252, 252, 252, 0);
  color: #000000;
  padding: 0.6rem 1rem;
  border-radius: 4px;
  font-size: 1rem;
}

</style>
