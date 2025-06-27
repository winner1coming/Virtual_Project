import { BaseComponent } from "../BaseComponent";

export class InputPin extends BaseComponent {
    constructor(
        id: number, 
        type: String = "InputPin", 
        position: [number, number] = [0, 0], 
        pinPosition = []
    ) {
        super(id, type, position, pinPosition);
        // this.inputs = []; // InputPin不需要输入引脚
        this.inputs.splice(0, this.inputs.length); // 清空输入引脚
        this.inputCount = 0; // 输入引脚数量为0
        this.inputInverted.splice(0, this.inputInverted.length);
        //this.outputs = [-1]; // 初始输出为未连接状态
        this.outputs.splice(0, this.outputs.length, -1); 
        this.bitCount = 1; // 默认为1位
    }

    // 切换某一位的值 (0变1，1变0)
    toggleBit(index: number): void {
        if (index >= 0 && index < this.bitCount) {
            if (this.bitCount === 1) {
                // this.outputs[0] = this.outputs[0] === 0 ? 1 : 
                //                  this.outputs[0] === 1 ? 0 : 
                //                  this.outputs[0]; // 保持-1或-2不变
                this.outputs.splice(0, 1, this.outputs[0] === 0 ? 1 : 
                                      this.outputs[0] === 1 ? 0 :
                                        this.outputs[0]); // 保持-1或-2不变
            } else {
                if (this.outputs.length !== this.bitCount) {
                    //this.outputs = new Array(this.bitCount).fill(-1);
                    this.outputs.splice(0, this.outputs.length, ...Array(this.bitCount).fill(-1));
                }
                // this.outputs[index] = this.outputs[index] === 0 ? 1 : 
                //                       this.outputs[index] === 1 ? 0 : 
                //                       this.outputs[index];
                this.outputs.splice(index, 1, this.outputs[index] === 0 ? 1 : 
                                              this.outputs[index] === 1 ? 0 : 
                                              this.outputs[index]); // 保持-1或-2不变
            }
        }
    }

    compute(): number[] {
        // InputPin的计算逻辑简单，主要是确保输出状态正确
        if (this.bitCount > 1) {
            // 检查所有输出是否有效
            for (const val of this.outputs) {
                if (val === -2) {
                    return this.outputs; // 保持错误状态
                }
            }
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        // InputPin不应该有输入引脚，所以这里返回当前输出
        return this.outputs;
    }

    setBitCount(bitCount: number): void {
        if (bitCount !== this.bitCount) {
            this.bitCount = bitCount;
            // 保留原有的有效值，其余填充-1
            const newOutputs = new Array(bitCount).fill(-1);
            const copyLength = Math.min(bitCount, this.outputs.length);
            
            for (let i = 0; i < copyLength; i++) {
                if (this.outputs[i] !== undefined) {
                    newOutputs[i] = this.outputs[i];
                }
            }
            
            ////is.outputs = newOutputs;
            this.outputs.splice(0, this.outputs.length, ...newOutputs); // 替换outputs的值
        }
    }

    // 获取当前值（十进制表示，适用于多bit）
    getDecimalValue(): number {
        if (this.bitCount === 1) {
            return this.outputs[0];
        }
        
        // 多bit转换为十进制
        return this.outputs.reduce((acc, val, index) => {
            if (val === 1) {
                return acc + Math.pow(2, this.bitCount - 1 - index);
            }
            return acc;
        }, 0);
    }
}