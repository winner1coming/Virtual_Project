<script setup lang="ts">
import { 
  NBreadcrumb, 
  NBreadcrumbItem, 
  NButton, 
  NButtonGroup, 
  NIcon, 
  NTooltip,
} from 'naive-ui'
import { 
  SaveOutline as SaveIcon,
  ArrowBackOutline as ArrowBackIcon,
  ArrowForwardOutline as ArrowForwardIcon,
  TrashOutline as TrashIcon,
  Play as PlayIcon,
  Pause as PauseIcon,
  Stop as StopIcon,
  HomeOutline as home,
  BowlingBallOutline as Bo,
  CaretForwardCircleOutline as forwardIcon,
  CaretDownCircleOutline as downIcon,
  CloudUploadOutline as UploadIcon,
  Book
} from '@vicons/ionicons5'
import { ref, computed, onMounted, watch, useTemplateRef, nextTick } from 'vue'
import { useRouter,useRoute } from 'vue-router'
const props = defineProps<{
  mode: 'practice' | 'challenge' | 'tutorial',
  editorRef: number
}>()
import { useCircuitStore } from '@/store/CircuitStore'
const circuitStore = useCircuitStore();

// #region 导航栏相关方法
// 添加新状态
const showToolbar = ref(false)
const router = useRouter()


const modeLabels = {
  practice: '自由练习模式',
  challenge: '闯关模式',
  tutorial: '教学模式'
}

const modeLabel = computed(() => modeLabels[props.mode] || '自由练习模式')

const goHome = () => {
  router.push({name : 'Home'})
}

const handleClick = () => {
  showToolbar.value = !showToolbar.value
}


// #region 历史记录
import {useHistory} from '@/modules/useHistory'
const {redo, undo, clearAll} = useHistory(); 
const prevStep = () => {
  undo();
}
const nextStep = () => {
  redo();
}
const clearWorkspace = () => {
  clearAll();
}
// #endregion 历史记录

// #region 模拟器
// 模拟器控制相关
const isSimulatorStarted = ref(true);
const isSimulatorPaused = ref(false);

// 开启模拟器
const startSimulator = () => {
  if(isSimulatorPaused.value) {
    circuitStore.resumeSimulator();
    isSimulatorPaused.value = false; // 恢复暂停状态
    return; // 如果模拟器已经暂停，则不执行任何操作
  }
  if(isSimulatorStarted.value) {
    return; // 如果模拟器已经启动，则不执行任何操作
  }
  circuitStore.enableSimulator();
  circuitStore.resumeSimulator();
  isSimulatorStarted.value = true;
}
// 关闭模拟器
const stopSimulator = () => {
  if(!isSimulatorStarted.value) {
    return; // 如果模拟器没有启动，则不执行任何操作
  }
  isSimulatorPaused.value = false; 
  circuitStore.disableSimulator();
  isSimulatorStarted.value = false;
}
// 暂停模拟器
const pauseSimulator = () => {
  if(isSimulatorPaused.value || !isSimulatorStarted.value) {
    return; // 如果模拟器已经暂停，则不执行任何操作
  }
  circuitStore.pauseSimulator();
  isSimulatorPaused.value = true;
} 
// 恢复模拟器
const resumeSimulator = () => {
  if(!isSimulatorPaused.value || !isSimulatorStarted.value) {
    return; // 如果模拟器没有暂停，则不执行任何操作
  }
  circuitStore.resumeSimulator();
  isSimulatorPaused.value = false;
}
// 单步运行模拟器 todo
// #endregion 模拟器

// #region 项目
import { useProjectStore } from '@/store/ProjectStore'
import {loadProject, exportProject, importProjectFromFile} from '@/modules/useImport'
import eventBus from '@/modules/useEventBus';
const projectStore = useProjectStore()

// 另存项目
const saveProject = () => {
  exportProject(projectStore.getCurrentProject());
}
const fileInput = useTemplateRef('fileInput');
// 上传项目
const uploadProject = () => {
  if (fileInput.value) {
	fileInput.value.click();
  }
}
const handleFileUpload = (event: Event) => {
  projectStore.createProject('new project');
  nextTick(() => {
    const canvasRef = props.editorRef;
    importProjectFromFile(event, canvasRef);
  });
  eventBus.emit('freshProject'); // 刷新项目
}



// #endregion 项目
// #region 测试真值表
// 加载闯关模式关卡
import { loadChallengesOnStartup} from '@/config/init'
nextTick(async() => {
  if(projectStore.nextProjectId === 1){
    // 获取文件列表
    const fileNames = ['1.一位全加器.json', '一位全加器_答案.json'];

    for (const fileName of fileNames) {
      projectStore.createProject('new project', 'challenge');
      const response = await fetch(`/assets/challenges/${fileName}`);
      if (!response.ok) {
        throw new Error(`无法加载文件: ${fileName}`);
      }
      const module = await response.json();
      console.log(`加载关卡文件: ${fileName}`);
      const canvasRef = props.editorRef;
      await loadProject(module, canvasRef); 
      await nextTick();
    }
    console.log('所有关卡加载完成！');
    if(props.mode === 'practice'){
      projectStore.loadProject(0); 
    }
    eventBus.emit('freshProject'); // 刷新项目
    
  }
});
// 答案
// 一位全加器（sum, cout)
const oneBitFullAdder = [
  [0, 0], // 输入组合 000 的输出
  [1, 0], // 输入组合 001 的输出
  [1, 0], // 输入组合 010 的输出
  [0, 1],  // 输入组合 011 的输出
  [1, 0], // 输入组合 100 的输出
  [0, 1], // 输入组合 101 的输出
  [0, 1], // 输入组合 110 的输出
  [1, 1]  // 输入组合 111 的输出
]

const answer: { [key: string]: number[][] } = {
  "unnamed":[], 
  "1.一位全加器":oneBitFullAdder,
  "一位全加器_答案": oneBitFullAdder,
};
import { calculateTruthTable } from '@/modules/useTruthTable'

const testTruthTable = () => {
  const projectId = projectStore.selectedProjectId; // 获取当前选中的项目ID
  if (!projectId) {
    return;
  }
  const truthTable = calculateTruthTable(projectId);
  console.log('测试真值表:', truthTable);
  console.log('答案:', answer[projectStore.getCurrentProject().name]);
  if (JSON.stringify(truthTable) === JSON.stringify(answer[projectStore.getCurrentProject().name])) {
    message.success('测试通过！');
  } else {
    message.error('测试失败，请检查电路设计。');
  }
}

// 消息显示
import { useMessage } from "naive-ui";
const message = useMessage();
// #endregion 测试真值表
// #endregion 导航栏相关方法
</script>

<!-- 导航栏-->
<template>
	<div class="navbar-container">
	  <nav class="navbar">
		<!-- 左侧面包屑 -->
		<div class="nav-left">
		  <n-breadcrumb>
			<n-breadcrumb-item @click="goHome">
			  <n-icon :component="home"/>首页
			</n-breadcrumb-item>
			<n-breadcrumb-item>
			  <n-icon :component="Bo"/>{{ modeLabel }}
			</n-breadcrumb-item>
		  </n-breadcrumb>
		</div>

		<!-- 右侧菜单 -->
		<div class="nav-right">
			<n-button 
			:bordered="false"
			:focusable="false"
			quaternary circle @click="handleClick">
			  <template #icon>
				<n-icon >
				  <component :is = "showToolbar ? downIcon : forwardIcon"/>
				</n-icon>
			  </template>
			</n-button>
		</div>
	  </nav>
	  
	  <!-- 工具栏 - 展开后显示 -->
	  <div v-if="showToolbar" class="toolbar">
		<n-button-group class = "buttongroup">
		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="saveProject" >
				<template #icon>
				  <n-icon><save-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			保存
		  </n-tooltip>
		  
		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="prevStep">
				<template #icon>
				  <n-icon><arrow-back-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			上一步
		  </n-tooltip>
		  
		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="nextStep" >
				<template #icon>
				  <n-icon><arrow-forward-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			下一步
		  </n-tooltip>
		  
		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="clearWorkspace">
				<template #icon>
				  <n-icon><trash-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			清空
		  </n-tooltip>

		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="startSimulator" >
				<template #icon>
				  <n-icon :color="isSimulatorStarted && !isSimulatorPaused ? 'green' : 'black'"><play-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			启动模拟器
		  </n-tooltip>

		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="pauseSimulator">
				<template #icon>
				  <n-icon :color="isSimulatorPaused ? 'red' : 'black'"><pause-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			暂停模拟器
		  </n-tooltip>

		  <!-- <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="stopSimulator">
				<template #icon>
				  <n-icon><stop-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			停止模拟器
		  </n-tooltip> -->

		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="saveProject">
				<template #icon>
				  <n-icon><save-icon /></n-icon>
				</template>
			  </n-button>
			</template>
			保存项目
		  </n-tooltip>

		  <n-tooltip trigger="hover">
			<template #trigger>
			  <n-button quaternary @click="uploadProject">
				<template #icon>
				  <n-icon><UploadIcon /></n-icon>
				</template>
			  </n-button>
			</template>
			上传项目
		  </n-tooltip>

		  <!-- 测试按钮-->
      <div v-show="props.mode === 'challenge'">
		  <n-tooltip trigger="hover" >
        <template #trigger>
          <n-button quaternary @click="testTruthTable">
          <template #icon>
            <n-icon><Book /></n-icon>
          </template>
          </n-button>
        </template>
        测试
		  </n-tooltip>
      </div>

		  <!-- 隐藏的文件输入框 -->
		  <input 
			type="file" 
			ref="fileInput" 
			style="display: none;" 
			@change="handleFileUpload"
		  />

		</n-button-group>
	  </div>
	</div>
</template>

<style scoped>
/* 导航栏容器 */
.navbar-container {
  background: #ffffff;
  border-bottom: 1px solid #E0E6ED;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  color: white;
}

.navbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.8rem 1.2rem;
}

.nav-left {
  display: flex;
  align-items: center;
}

.nav-right {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.mode-label {
  font-size: 0.9rem;
}
/* 工具栏 */
.toolbar {
  display: flex;
  justify-content: flex-start;
  padding: 0.6rem 1.2rem;
  background: #F6F8FA;
  border-top: 1px solid #E0E6ED;
}
.slide-fade-enter-active,
.slide-fade-leave-active {
  transition: all 0.3s ease;
}
.slide-fade-enter-from,
.slide-fade-leave-to {
  transform: translateX(-20px);
  opacity: 0;
}
</style>