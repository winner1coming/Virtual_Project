import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";
import { SubCircuitComponent } from "./SubCircuitComponent";

export class Clock extends BaseComponent {
    private intervalId: any = null;
    public period: number; // 时钟周期，单位毫秒
    private parent: SubCircuitComponent | null = null; 
    public isStopped: boolean = false; // 是否停止时钟

    constructor(id: number, 
            type: string, 
            position: [number, number] = [0, 0], 
            simulator: any = null,
            ) {
        super(id, type, position);
        this.offset=[-120,-120];
        if(!simulator) {
			this.simulator = EventDrivenSimulator.getInstance(); 
		}else {
			this.simulator = simulator; 
		}
        this.initInputPin(0); // 时钟没有输入引脚
        this.period = 1000;
        this.updatePinPosition();
        this.start(); // 启动时钟
    }


    setParent(parent: SubCircuitComponent) {
        this.parent = parent;
    }

    // 启动时钟
    start() {
        this.isStopped = false;
        if (this.intervalId !== null) return;
        this.intervalId = setInterval(() => {
            this.outputs[0] = this.outputs[0] === 0 ? 1 : 0;
            this.simulator.processOutputChange(this.id, 0,this.outputs[0]);
            if(this.parent) {
                this.parent.updateOutputs();
            }
            
        }, this.period);
    }

    // 停止时钟
    stop() {
        this.isStopped = true;
        if (this.intervalId !== null) {
            clearInterval(this.intervalId);
            this.intervalId = null;
        }
    }

    // 销毁时钟，释放定时器的资源
    destroy() {
        this.stop(); // 停止定时器
    }

    compute(): number[] {
        // 时钟输出由定时器自动切换，compute 只返回当前输出
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        throw new Error("shouldn't be here")
    }
    // 更新引脚位置
    updatePinPosition(): void{
        this.outputPinPosition.splice(0, this.outputPinPosition.length, [235, 118]);
        if(this.direction === 'east')
        {
            this.outputPinPosition.splice(0, this.outputPinPosition.length, [235, 118]);
        }
        else if(this.direction === 'west')
        {
            this.outputPinPosition.splice(0, this.outputPinPosition.length, [0, 118]);
        }
        else if(this.direction === 'north')
        {
            this.outputPinPosition.splice(0, this.outputPinPosition.length, [117.5, 0]);
        }
        else if(this.direction === 'south')
        {
            this.outputPinPosition.splice(0, this.outputPinPosition.length, [117.5, 236]);
        }
    }
}