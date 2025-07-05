import eventBus from "@/modules/useEventBus";
import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class InputPin extends BaseComponent {
    constructor(
        id: number, 
        type: String = "InputPin", 
        position: [number, number] = [0, 0], 
        simulator: any = null 
    ) {
        super(id, type, position);
        if(!simulator) {
			this.simulator = EventDrivenSimulator.getInstance(); 
		}else {
			this.simulator = simulator; 
		}
        this.initInputPin(0); // InputPin没有输入引脚
        this.outputs.splice(0, this.outputs.length, 0); 
        this.bitWidth = 1; // 默认为1位
        // 修改输入
        this.inputPinPosition.splice(0, this.inputPinPosition.length);
    }

    // 更新引脚位置
    updatePinPosition(): void{
        // 排布相关参数
        const colMax = 8;
        const cellWidth = 40;
        const cellHeight = 60;
        const padding = 40;
        const cols = Math.min(this.bitWidth, colMax);
        const rows = Math.ceil(this.bitWidth / colMax);
        // 宽高
        const svgWidth = cols * cellWidth + padding;
        const svgHeight = rows * cellHeight + padding/2;
        // 修改输出
        this.outputPinPosition.splice(0, this.outputPinPosition.length, [svgWidth, svgHeight/2]);
    }

    // 切换某一位的值 （index为高位到低位（1101从左往右数），index从0开始）
    toggleBit(index: number): void {
        if (index >= 0 && index < this.bitWidth) {
            let value = this.outputs[0]; 
            if (value === -1 || value === -2) {
                return;
            }
            index = this.bitWidth - 1 - index; // 将index转换为低位到高位的顺序
            // 计算当前位的掩码
            const mask = 1 << index;

            this.outputs[0] = value ^ mask; // 使用按位异或操作切换位

            // 输入引脚的改变会导致电路的改变
            EventDrivenSimulator.getInstance().enqueue(this.id, 0, this.outputs[0]);
            EventDrivenSimulator.getInstance().processQueue();
        }
    }

    // 返回bits数组
    getBits(): number[] {
        if(this.outputs.length === 0 || this.outputs[0] === -1 || this.outputs[0] === -2) {
            return []; 
        }
        const bits: number[] = [];
        if (this.bitWidth <= 0) {
            return bits; // 如果位宽为 0，返回空数组
        }

        let value = this.outputs[0]; 

        for (let i = 0; i < this.bitWidth; i++) {
            // 提取每一位（从高位到低位）
            bits.unshift(value & 1); // 取最低位
            value = value >> 1; // 右移 1 位
        }
        return bits;
    }

    compute(): number[] {
        
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        // 该函数是在有外部输入时调用
        this.outputs.splice(0, this.outputs.length, v); // 更新输出为输入值
        return this.outputs;
    }

    setBitWidth(bitWidth: number): void {
        if (bitWidth !== this.bitWidth) {
            this.bitWidth = bitWidth;
            // 保留原有的有效值，其余填充-1
            const newOutputs = new Array(bitWidth).fill(-1);
            const copyLength = Math.min(bitWidth, this.outputs.length);
            
            for (let i = 0; i < copyLength; i++) {
                if (this.outputs[i] !== undefined) {
                    newOutputs[i] = this.outputs[i];
                }
            }
            
            ////is.outputs = newOutputs;
            this.outputs.splice(0, this.outputs.length, ...newOutputs); // 替换outputs的值

            this.simulator.checkComponentConnections(this.id); // 检查连线
            this.updatePinPosition(); // 更新引脚位置
            eventBus.emit('updatePinPosition', {id: this.id}); 
        }

        
    }

    // 获取当前值（十进制表示，适用于多bit）
    getDecimalValue(): number {
        if (this.bitWidth === 1) {
            return this.outputs[0];
        }
        
        // 多bit转换为十进制
        return this.outputs.reduce((acc, val, index) => {
            if (val === 1) {
                return acc + Math.pow(2, this.bitWidth - 1 - index);
            }
            return acc;
        }, 0);
    }
}