import { BaseComponent } from "./BaseComponent.js";
import {useCircuitStore} from "@/store/CircuitStore"
import {ConnectionManager, PinMap, Conn} from "./ConnectionManager.js";

export class CircuitLogic{
    connectionManager: ConnectionManager;
    private static instance: CircuitLogic|null = null;
    private circuitStore;
    // 设置为单例模式
    static getInstance(): CircuitLogic{
        if(!CircuitLogic.instance){
            CircuitLogic.instance = new CircuitLogic();
        }
        return CircuitLogic.instance;
    }
    private constructor(){
        this.connectionManager = new ConnectionManager();
        this.circuitStore = useCircuitStore();
    }
    // 连线
    connect(id1, pinIndex1, id2, pinIndex2){
        // 组件的引脚从0开始编号，前n位为输入
        let inputId, outputId, inputIndex, outputIndex;
        // component1 为电线输入端，即其端口为输出端
        if(pinIndex1 >= this.circuitStore.getComponent(id1).getInputPinCount()){
            inputId = id1;
            // 那么component2应为输出
            if(pinIndex2 >= this.circuitStore.getComponent(id2).getInputPinCount()){
                // 非法
                // 标记连线为非法  todo

            }else{
                // 合法
                inputIndex = pinIndex1 - this.circuitStore.getComponent(inputId).getInputPinCount();
                outputId = id2;
                outputIndex = pinIndex2;
                this.connectionManager.addConnection(inputId, inputIndex, outputId, outputIndex, true);
            }
        }else{
            outputId = id1;
            if(pinIndex2 < this.circuitStore.getComponent(id2).getInputPinCount()){
                // 非法
                // 标记连线为非法  todo
            }else{
                // 合法
                outputIndex = pinIndex1;
                inputId = id2;
                inputIndex = pinIndex2 - this.circuitStore.getComponent(inputId).getInputPinCount();  
                this.connectionManager.addConnection(inputId, inputIndex, outputId, outputIndex, true);
            }
        }
        // 正常的情况 
        // console.log(inputComponent, outputComponent);  //
        let workList = [];  // 数组元素为[component, idx, value]
        let pinMap: PinMap, oldOutput;
        oldOutput = this.circuitStore.getComponent(outputId).getOutput();
        if(oldOutput !== this.circuitStore.getComponent(outputId)
                .changeInput(outputIndex, this.circuitStore.getComponent(inputId).getOutput())){
            // 将与输出组件的输出端相连的所有组件加入workList
            pinMap = this.connectionManager.getOutputPinMap(outputId)
            if(pinMap){
                //console.log(pinMap);  //
                for(const connection of pinMap.values()){
                    //console.log(pinMap);  //
                    workList.push([connection.id, connection.idx, this.circuitStore.getComponent(outputId).getOutput()]);
                }
            }
            
        }
        while(workList.length !== 0){
            let tmp = workList.shift();  // 删除第一个元素
            //console.log(tmp[0]);
            oldOutput = this.circuitStore.getComponentOutput(tmp[0]);
            if(oldOutput !== this.circuitStore.getComponent(tmp[0]).changeInput(tmp[1], tmp[2])){
                pinMap = this.connectionManager.getOutputPinMap(tmp[0]);
                if(pinMap){
                    for(const connection of pinMap.values()){
                        workList.push([connection.id, connection.idx, this.circuitStore.getComponentOutput(tmp[0])]);
                    }
                }
                
            }
        }
    }
}
