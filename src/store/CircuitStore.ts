import { reactive, computed } from 'vue';
import { defineStore } from 'pinia'
import { BaseComponent } from '@/logic/BaseComponent.js';
import { EventDrivenSimulator } from '@/logic/Simulator';

import { useProjectStore } from './ProjectStore';

import {createComponentByType} from '@/modules/useComponentType';


export const useCircuitStore = defineStore('circuit', {
  state: () => ({
    components: new Map<number, BaseComponent>(),
    selectedId: -1,   // 选中的组件ID，-1表示没有选中任何组件
    currentId: 0,

    undoStack:[] as any[],
    redoStack:[] as any[],
    simulator: EventDrivenSimulator.getInstance(),
    // 获取项目管理器实例
    projectStore: useProjectStore(), 
    currentMode: 'practice',
  }),

  actions: {
    // #region 组件相关操作
    // getComponent返回的对象并不是响应式的，返回时需要用compute手动包装
    // 用法：const A = computed(() => store.components.get(1));
    getComponent(id: number): BaseComponent{
      const component = this.components.get(id)!;
      return component;
    },

    getComponentOutputs(id:number): number[]{
      return this.getComponent(id).getOutputs();
    },

    // 返回当前项目的所有组件
    getAllCurrentComponents(): BaseComponent[] {
      const project = this.projectStore.getCurrentProject();
      return project.componentsId.map(id => this.components.get(id)!);
    },

    // 添加一个组件，返回id
    addComponent(type: string, position: [number, number]=[0,0], name: string ="", projectId: number): number {
      useProjectStore().getCurrentProject().hasChanged = true;
      const id = this.currentId++;
      this.components.set(id, reactive(createComponentByType(id, type, position, name, projectId)));
      this.projectStore.getCurrentProject().componentsId.push(id);
      if(type === "INPUT"){
        this.projectStore.getCurrentProject().inputPins.push(id);
      }else if(type === "OUTPUT"){
        this.projectStore.getCurrentProject().outputPins.push(id);
      } else if(type === "CLOCK"){
        this.projectStore.getCurrentProject().clockIds.push(id);
      }
      return id;
    },
    // 移除一个组件
    // 注意：如果组件有连接的电线，需要先删除电线
    removeComponent(id: number) {
      useProjectStore().getCurrentProject().hasChanged = true; // 标记项目已更改
      const component = this.components.get(id);
      if (!component) {
        throw new Error(`Component with id ${id} not found`);
      }
      // 删除组件时，先删除其所有连接的电线  
      // 删除组件的连线
      this.simulator.removeComponent(id);
      // 删除元件
      this.components.delete(id);
      // 如果删除的组件是当前选中的组件，则取消选中
      if (this.selectedId === id) {
        this.selectedId = -1;
      }
      const project = this.projectStore.getCurrentProject();
      const index = project.componentsId.indexOf(id);

      if (index !== -1) {
        project.componentsId.splice(index, 1);
      }

      if (project.inputPins.includes(id)) {
        const inputIndex = project.inputPins.indexOf(id);
        if (inputIndex !== -1) {
          project.inputPins.splice(inputIndex, 1);
        }
      }else if (project.outputPins.includes(id)) {
        const outputIndex = project.outputPins.indexOf(id);
        if (outputIndex !== -1) {
          project.outputPins.splice(outputIndex, 1);
        }
      }else if (project.clockIds.includes(id)) {
        const clockIndex = project.clockIds.indexOf(id);
        if (clockIndex !== -1) {
          project.clockIds.splice(clockIndex, 1);
        }
      }
    },

    // 移动一个组件
    moveComponent(id: number, newPosition: [number, number]) {
      const component = this.components.get(id);
      if (!component) {
        throw new Error(`Component with id ${id} not found`);
      }
      component.setPosition(newPosition);
      // 更新组件位置后，可能需要更新电线的位置 todo
    },

    // 选择组件
    selectComponent(id: number) {
      if (this.components.has(id)) {
        this.selectedId = id;
      } else {
        throw new Error(`Component with id ${id} not found`);
      }
    },
    unselectComponent() {
      this.selectedId = -1;
    },
    // #endregion 组件相关操作
    // #region 连线相关操作
    connect(id1: number, idx1:number, id2: number, idx2:number) {
      useProjectStore().getCurrentProject().hasChanged = true; // 标记项目已更改
      this.simulator.connect(id1, idx1, id2, idx2);
    },

    disconnect(id1: number, idx1:number, id2: number, idx2:number) {
      useProjectStore().getCurrentProject().hasChanged = true; // 标记项目已更改
      this.simulator.disconnect(id1, idx1, id2, idx2);
    },
    // #endregion 连线相关操作
    // #region 模拟器逻辑
    // 启用模拟器
    enableSimulator() {
      this.simulator.enable();
      this.simulator.resumeSimulator();
    },

    disableSimulator() {
      this.simulator.disable();
    },

    pauseSimulator(){
      this.simulator.pauseSimulator();
    },

    resumeSimulator(){
      this.simulator.resumeSimulator();
    },
    simulateCircuit() {},
    // #endregion 模拟器逻辑
    // #region 项目管理
    // 获取当前项目
    getCurrentProject() {
      return this.projectStore.getCurrentProject();
    },

    getCurrentProjectId(): number {
      return this.projectStore.selectedProjectId;
    },
    
    changeProject(projectId: number) {
      this.unselectComponent(); // 取消选中任何组件
      this.simulator.changeProject(projectId);
    }
    // // 创建新项目
    // createProject(name: string) {
    //   return this.projectManager.createProject(name);
    // },
    // // 选中项目
    // selectProject(projectId: number) {
    //   const project = this.projectManager.getAllProjects().find(p => p.projectId === projectId);
    //   if (project) {
    //     this.projectManager.loadProject(project);
    //   } else {
    //     throw new Error(`Project with id ${projectId} not found`);
    //   }
    // },
  }
});