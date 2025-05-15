import { defineStore } from 'pinia'
import { BaseComponent } from '@/logic/BaseComponent.js';
import {AndGate} from '@/logic/components/AndGate';
import {OrGate} from '@/logic/components/OrGate';
import {NotGate} from '@/logic/components/NotGate';
import {NandGate} from '@/logic/components/NandGate';
import {NorGate} from '@/logic/components/NorGate';
import {XorGate} from '@/logic/components/XorGate';
import {Clock} from '@/logic/components/Clock';
export const useCircuitStore = defineStore('circuit', {
  state: () => ({
    components: new Map<number, BaseComponent>(),
    // wires: new Map<string, Wire>(),
    selectedGateId: null,
    currentId: 0,
  }),
  actions: {
    getComponent(id: number): BaseComponent{
      const component = this.components.get(id);
      if(!component){
        throw new Error(`Component with id ${id} not found`);
      } 
      return component;
    },
    getComponentOutputs(id:number): number[]{
      return this.getComponent(id).getOutputs();
    },
    // 添加一个组件，返回id
    addComponent(type: String, position: [number, number]): number {
      const id = this.currentId++;
      // const logic = createGate(type, id);
      if(type === "And"){
        this.components.set(id, new AndGate(id, type, position));
      }else if(type === "Or"){
        this.components.set(id, new OrGate(id, type, position));
      }else if(type === "Not"){
        this.components.set(id, new NotGate(id, type, position));
      }else if(type === "Nand"){
        this.components.set(id, new NandGate(id, type, position));
      }else if(type === "Nor"){
        this.components.set(id, new NorGate(id, type, position));
      }else if(type === "Xor"){
        this.components.set(id, new XorGate(id, type, position));
      }else if(type === "Clock"){
        this.components.set(id, new Clock(id, type, position));
      }
      return id;
    },
    // addWire(from, to) { },
    simulateCircuit() {},
  }
});
