import { loadProject } from '@/modules/useImport';
import { useProjectStore } from '@/store/ProjectStore';
import { useCircuitStore } from '@/store/CircuitStore';
import { nextTick } from 'vue';

export async function loadChallengesOnStartup(canvasEditorRefs: any) {
  try {
    
    const projectStore = useProjectStore();
    const circuitStore = useCircuitStore();
    projectStore.createProject('new project');
    nextTick(async() => {
    const canvasRef = canvasEditorRefs.get(projectStore.selectedProjectId);
    

    // 获取文件列表（需要手动维护文件名列表）
    const fileNames = ['1.一位全加器.json', '一位全加器_答案.json'];

    for (const fileName of fileNames) {
        const response = await fetch(`/assets/challenges/${fileName}`);
        if (!response.ok) {
          throw new Error(`无法加载文件: ${fileName}`);
        }
        const module = await response.json();
        console.log(`加载关卡文件: ${fileName}`);
        await loadProject(module, canvasRef); 
        projectStore.createProject('new project');
        await nextTick();
      }
    });
    console.log('所有关卡加载完成！');
  } catch (error) {
    console.error('加载关卡文件失败:', error);
  }
}