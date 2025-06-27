import { BaseComponent } from "./BaseComponent";
import {useCircuitStore} from "@/store/CircuitStore"
import {ConnectionManager, PinMap, Conn} from "./ConnectionManager";

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
    connect(id1: number, pinIndex1: number, id2: number, pinIndex2: number){
        // 组件的引脚从0开始编号，前n位为输入
        let inputId: number, outputId: number, inputIndex: number, outputIndex: number;
        // inputId 对应电线输入端，即其实际上为该元件的输出引脚
        if(pinIndex1 >= this.circuitStore.getComponent(id1).getInputPinCount()){
            inputId = id1;
            // 那么component2应为输出
            if(pinIndex2 >= this.circuitStore.getComponent(id2).getInputPinCount()){
                // 非法
                // 标记连线为非法  todo
                outputId = id2;
                outputIndex = pinIndex1;
                inputIndex = pinIndex2 - this.circuitStore.getComponent(inputId).getInputPinCount();    
                this.connectionManager.addConnection(inputId, inputIndex, outputId, outputIndex, false);
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
                inputId = id2;
                inputIndex = pinIndex1 - this.circuitStore.getComponent(inputId).getInputPinCount();
                outputIndex = pinIndex2;
                this.connectionManager.addConnection(inputId, inputIndex, outputId, outputIndex, false);
            }else{
                // 合法
                outputIndex = pinIndex1;
                inputId = id2;
                inputIndex = pinIndex2 - this.circuitStore.getComponent(inputId).getInputPinCount();  
                this.connectionManager.addConnection(inputId, inputIndex, outputId, outputIndex, true);
            }
        }
        // 正常的情况 
        // console.log(this.circuitStore.getComponent(inputId), this.circuitStore.getComponent(outputId));  //
        interface workListItem{
            id: number;
            idx: number;
            value: number;
        }
        let workList: workListItem[] = [];  
        let oldOutputs: number[];
        let pinMap: PinMap;
        
        oldOutputs = JSON.parse(JSON.stringify(this.circuitStore.getComponent(outputId).getOutputs()));
        let newOutputs = this.circuitStore.getComponent(outputId).changeInput(outputIndex, this.circuitStore.getComponent(inputId).getOutputs()[inputIndex]);
        if(oldOutputs !== newOutputs){
            // 将与电线输出组件的输出端相连的所有组件加入workList
            pinMap = this.connectionManager.getOutputPinMap(outputId)!;
            if(pinMap){
                //console.log(pinMap);  //
                for(const pinIdx of pinMap.keys()){
                    for(const connection of pinMap.get(pinIdx) as Conn[]){
                        //console.log(pinMap);  //
                        // 要将id组件的idx索引改为value
                        workList.push({ id: connection.id, idx: connection.idx, value: newOutputs[pinIdx] });
                    }
                }
            }else{
                // 没有引脚图 todo 不该出现的情况
            }
            
        }
        while(workList.length !== 0){
            let tmp: workListItem = workList.shift()!; // 删除第一个元素
            //console.log(tmp[0]);
            oldOutputs = JSON.parse(JSON.stringify(this.circuitStore.getComponentOutputs(tmp.id)));
            newOutputs = this.circuitStore.getComponent(tmp.id).changeInput(tmp.idx, tmp.value);
            if(oldOutputs !== newOutputs){
                pinMap = this.connectionManager.getOutputPinMap(tmp.id)!;
                if(pinMap){
                    for(const pinIdx of pinMap.keys()){
                        for(const connection of pinMap.get(pinIdx) as Conn[]){
                            workList.push({ id: connection.id, idx: connection.idx, value: this.circuitStore.getComponentOutputs(tmp.id)[pinIdx] });
                        }
                    }
                }
                
            }
        }
    }
}
