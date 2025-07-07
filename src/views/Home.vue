<template>
  <div class="home-container">
    <h1>电路模拟实验室</h1>
    <div class="mode-cards">
      <div 
        v-for="mode in modes"
        :key="mode.id"
        class="mode-card"
        @click="navigateToWorkspace(mode.id)"
      >
        <h2>{{ mode.title }}</h2>
        <p>{{ mode.description }}</p>
      </div>
    </div>
  </div>
</template>

<script setup>
import { useRouter } from 'vue-router'

const modes = [
  { id: 'practice', title: '自由练习模式', description: '无限制的电路设计与实验环境' },
  { id: 'challenge', title: '闯关模式', description: '完成特定电路设计挑战' },
  { id: 'tutorial', title: '教学模式', description: '循序渐进学习数字电路知识' }
]

import { useCircuitStore } from '@/store/CircuitStore'
import { useProjectStore } from '@/store/ProjectStore'
const projectStore = useProjectStore()
const circuitStore = useCircuitStore()
const router = useRouter()
const navigateToWorkspace = (mode) => {
  circuitStore.currentMode = mode 
  if(mode !== 'tutorial') {
    for(const project of projectStore.getAllProjects()) {
      if(project.mode === mode) {
        projectStore.loadProject(project.projectId)
        break
      }
    }
  }
  router.push({ path: '/workspace', query: { mode } })
}
</script>

<style scoped>
.home-container {
    width: 100vw;
    height: 100vh;
}

.mode-cards {
  margin: 2px;
  height: 60%;
  display: flex;
  align-items: center;
  justify-content: space-around;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 2rem;
  margin-top: 3rem;
}

.mode-card {
  padding: 2rem;
  border-radius: 12px;
  background: #fff;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  cursor: pointer;
  transition: transform 0.2s;
  min-width: 250px;
  text-align: center;
}

.mode-card:hover {
  transform: translateY(-5px);
}
</style>