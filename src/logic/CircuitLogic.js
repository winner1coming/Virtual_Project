import { BaseComponent } from "./BaseComponent.js";
import {ConnectionManager} from "./ConnectionManager.js";

export class CircuitLogic{
    constructor(){
        // 设置为单例模式
        if(CircuitLogic.instance){
            return CircuitLogic.instance;
        }
        CircuitLogic.instance = this;
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
        // console.log(inputComponent, outputComponent);  //
        let workList = [];  // 数组元素为[component, idx, value]
        let pinMap, oldOutput;
        oldOutput = outputComponent.getOutput();
        if(oldOutput !== outputComponent.changeInput(outputIndex, inputComponent.getOutput())){
            // 将与输出组件的输出端相连的所有组件加入workList
            pinMap = this.connectionManager.getOutputPinMap(outputComponent)
            if(pinMap){
                //console.log(pinMap);  //
                for(const connection of pinMap.values()){
                    //console.log(pinMap);  //
                    workList.push([connection.component, connection.index, outputComponent.getOutput()]);
                }
            }
            
        }
        while(workList.length !== 0){
            let tmp = workList.shift();  // 删除第一个元素
            //console.log(tmp[0]);
            oldOutput = tmp[0].getOutput();
            if(oldOutput !== tmp[0].changeInput(tmp[1], tmp[2])){
                pinMap = this.connectionManager.getOutputPinMap(tmp[0]);
                if(pinMap){
                    for(const connection of pinMap.values()){
                        workList.push([connection.component, connection.index, outputComponent.getOutput()]);
                    }
                }
                
            }
        }
    }
}
