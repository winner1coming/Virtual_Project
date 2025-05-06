import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import { fileURLToPath, URL } from 'node:url'

// https://vite.dev/config/
export default defineConfig({
  plugins: [vue()],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)) // 使用 @ 代替 src 目录
    },
    extensions: ['.js', '.vue', '.json'] // 导入时可忽略的文件扩展名
  },
  assetsInclude: ['./src/assets'],
  build: {
    outDir: 'dist', // 确保与 tsconfig.json 的 outDir 一致
  },
})