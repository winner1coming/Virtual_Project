import { BaseComponent } from "./BaseComponent";
import {ConnectionManager} from "./ConnectionManager";

export class CircuitLogic{
    constructor(){
        this.connectionManager = new ConnectionManager();
    }
    // 连线
    connect(component1, pinIndex1, component2, pinIndex2){
        // 组件的引脚从0开始编号，前n位为输入
        let inputComponent, outputComponent, inputIndex, outputIndex;
        // component1 为电线输入端，即其端口为输出端
        if(pinIndex1 >= component1.getInputPinCount()){
            inputComponent = component1;
            // 那么component2应为输出
            if(pinIndex2 >= component2.getInputPinCount()){
                // 非法
                // 标记连线为非法  todo
                
            }else{
                // 合法
                inputIndex = pinIndex1 - inputComponent.getInputPinCount();
                outputComponent = component2;
                outputIndex = pinIndex2;
                this.connectionManager.addConnection(inputComponent, inputIndex, outputComponent, outputIndex, true);
            }
        }else{
            outputComponent = component1;
            if(pinIndex2 < component2.getInputPinCount()){
                // 非法
                // 标记连线为非法  todo
            }else{
                // 合法
                outputIndex = pinIndex1;
                inputComponent = component2;
                inputIndex = pinIndex2 - inputComponent.getInputPinCount();  
                this.connectionManager.addConnection(inputComponent, inputIndex, outputComponent, outputIndex, true);
            }
        }
        // 正常的情况 
        let workList = [];  // 数组元素为[component, idx, value]
        let pinMap;
        if(outputComponent.changeIndex(outputIndex)){
            // 将与输出组件的输出端相连的所有组件加入workList
            pinMap = this.connectionManager.getOutputPinMap(outputComponent)
            for(const idx in pinMap){
                workList.push([pinMap[idx].component, pinMap[idx].index, outputComponent.getOutput()]);
            }
        }
        while(workList.length !== 0){
            let tmp = workList.shift();  // 删除第一个元素
            if(tmp[0].changeIndex(tmp[1], tmp[2])){
                pinMap = this.connectionManager.getOutputPinMap(tmp[0])
                for(const idx in pinMap){
                    workList.push([pinMap[idx].component, pinMap[idx].index, outputComponent.getOutput()]);
                }
            }
        }
    }
}
