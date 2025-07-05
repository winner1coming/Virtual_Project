import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class Clock extends BaseComponent {
    private intervalId: any = null;

    constructor(id: number, 
            type: String, 
            position: [number, number] = [0, 0], 
            period: number = 1000,
            simulator: any = null) {
        super(id, type, position);
        if(!simulator) {
			this.simulator = EventDrivenSimulator.getInstance(); 
		}else {
			this.simulator = simulator; 
		}
        this.initInputPin(0); // 时钟没有输入引脚
        this.period = period;
        this.updatePinPosition();
        this.start(); // 启动时钟
    }

    // 启动时钟
    start() {
        if (this.intervalId !== null) return;
        this.intervalId = setInterval(() => {
            this.outputs[0] = this.outputs[0] === 0 ? 1 : 0;
            this.simulator.processOutputChange(this.id, 0,this.outputs[0]);
        }, this.period);
    }

    // 停止时钟
    stop() {
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
    }
}