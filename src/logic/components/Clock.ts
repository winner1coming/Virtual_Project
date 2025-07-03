import { BaseComponent } from "../BaseComponent";

export class Clock extends BaseComponent {
    private intervalId: any = null;
    private period: number; // 时钟周期，单位ms

    constructor(id: number, type: String, position: [number, number] = [0, 0], period: number = 1000) {
        super(id, type, position);
        this.changeInputPinCount(0); // 时钟没有输入引脚
        this.period = period;

        this.start(); // 启动时钟
    }

    // 启动时钟
    start() {
        if (this.intervalId !== null) return;
        this.intervalId = setInterval(() => {
            this.outputs[0] = this.outputs[0] === 0 ? 1 : 0;
            // todo 增加一个逻辑，可以调用全局的输入改变函数，以通知时钟输出改变
            //console.log("click", this.outputs[0]);
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
}