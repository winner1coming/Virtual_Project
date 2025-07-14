import { createRouter, createWebHistory } from 'vue-router'

// 定义路由
const routes = [
  { path: '/', name: 'Home', component: ()=> import('@/views/Home.vue') }, // 默认路由
  { path: '/workspace', name: 'Workspace', component: ()=> import('@/views/Layout.vue'),props: route=> ({mode : route.query.mode}) },
]

// 创建路由实例
const router = createRouter({
  history: createWebHistory(),
  routes,
})

export default router