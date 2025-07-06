<template>
  <div class="selector-container">
    <h2>请选择你要学习的实验</h2>
    <div class="experiment-cards">
      <div 
        v-for="exp in experiments" 
        :key="exp.id" 
        class="experiment-card"
        @click="selectExperiment(exp.id)"
      >
        <img :src="exp.image" class="experiment-image" />
        <h3>{{ exp.title }}</h3>
        <p>{{ exp.description }}</p>
      </div>
    </div>
  </div>
</template>

<script setup>
import { useRouter, useRoute } from 'vue-router'

const router = useRouter()
const route = useRoute()

const experiments = [
  {
    id: 'adder8bit',
    title: '8位可控加法器',
    description: '通过组合一位全加器实现8位二进制加法',
    image: new URL('@/assets/teaching/adder8bit/image.png', import.meta.url).href
  },
  {
    id: 'carrylookahead4bit',
    title: '4位先行进位加法器',
    description:'实现先行进位的加法器，减少运算的延时',
    image: new URL('@/assets/teaching/carrylookahead4bit/image1.png', import.meta.url).href
  }
  // 可以在这里继续扩展更多实验
]

const selectExperiment = (experimentId) => {
  router.push({
    path: '/workspace',
    query: {
      mode: 'tutorial',
      experiment: experimentId
    }
  })
}
</script>

<style scoped>
.selector-container {
  width: 100%;
  height: calc(100vh - 60px); /* 如果你的navbar大约60px高 */
  background: #f5f5f5;
  padding: 2rem;
  box-sizing: border-box;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: flex-start;
}

.experiment-cards {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 2rem;
  margin-top: 2rem;
}
.experiment-card {
  background: #fff;
  border-radius: 8px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.1);
  width: 240px;
  cursor: pointer;
  transition: transform 0.2s;
  text-align: center;
  padding: 1rem;
}
.experiment-card:hover {
  transform: translateY(-5px);
}
.experiment-image {
  width: 100%;
  height: 140px;
  object-fit: cover;
  border-radius: 4px;
}
</style>
