import { reactive } from "vue";
import { calcInputYs } from "@/logic/utils/useGateLayout";

// 电路传输整型，-1表示未连接，-2表示错误
export abstract class BaseComponent{
	id: number;
	type: String;
	name: String;
	inputs: number[];
	inputCount: number; // 输入引脚数量
	inputInverted: boolean[]; // 输入引脚是否取反   todo 内部逻辑未实现!
	outputs: number[];
	bitWidth: number;
	height: number;
	width: number;
	scale: number; // 缩放比例
	position: [number, number];
	inputPinPosition: Array<[number, number]>;   // todo! 默认为2，部分特殊文件中的这个还没改
  //inputYs: number[]; // 输入引脚的y坐标
	outputPinPosition: Array<[number, number]>;  // todo! 默认为2，部分特殊文件中的这个还没改
	direction: string; // 组件的方向，'east', 'west', 'north', 'south'

	constructor(id: number, type: String, position:[number, number] = [0,0],  inputPinPosition = []) {
		this.id = id;
		this.type = type;
		this.name = "";    // todo
		
		this.inputs = reactive([-1, -1]);     // 默认2个输入，如果不是，子类需要在构造函数中初始化
		this.inputCount = 2; // 默认2个输入
		this.inputInverted = reactive([false, false]);   // 默认两个引脚

		this.outputs = reactive([-1]);  // 输出初始值为-1 未连接
		this.bitWidth = 1;
		this.height = 1;   // todo
		this.width = 1;
		this.scale = 1;    
		this.position = reactive(position); // 将 position 包装为 reactive
		this.inputPinPosition =  reactive([[0,0], [0,0]]);  // 默认只有两个输入引脚
		this.outputPinPosition = reactive([[0,0]]); // 默认只有一个输出引脚
		this.direction = 'east';  // 默认方向为东

    this.changeInputPinCount(2); // 初始化输入引脚数量为2  todo! 这里需要在子类中调用
    // this.inputYs = calcInputYs(this.inputCount); // 计算输入引脚的y坐标
	};

	abstract compute(): number[];   // 调用后返回outputs
	abstract changeInput(idx: number, v: number): number[];  // 改变某一个引脚的电平，返回outputs
	// // 取反（只给位宽为1的输入引脚用）
	// invertInput(idx: number): void {
	//     this.inputs[idx] = this.inputs[idx] === 1 ? 0 : 1;
	//     this.compute();  // 更新outputs
	// } 

	setName(name: String){
		this.name = name;
	}

	setBitWidth(bitWidth: number){
		this.bitWidth = bitWidth;
	}
	setPosition(position: [number, number]) {
		this.position[0] = position[0]; 
		this.position[1] = position[1];
		// const [baseX, baseY] = position;

		// // 🟢 更新输入引脚坐标
		// for (let i = 0; i < this.inputCount; i++) {
		// 	this.inputPinPosition[i] = [baseX - 20, baseY]; 
		// }

		// // 🟢 更新输出引脚坐标
		// for (let i = 0; i < this.outputs.length; i++) {
		// 	this.outputPinPosition[i] = [baseX + 80, baseY]; 
		// }

    this.updatePinPosition(); // 更新引脚位置
	}
  setScale(scale: number) {
    this.scale = scale;
    this.updatePinPosition(); 
  }

  updatePinPosition(): void{} // 更新引脚位置

	// 会清空输入与引脚的取反状态
	changeInputPinCount(num: number){
		this.inputCount = num;
		this.inputs.splice(0, this.inputs.length, ...Array(num).fill(-1));    // 将输入全部置-1
		this.inputInverted.splice(0, this.inputInverted.length, ...Array(num).fill(false)); // 初始化输入取反状态

    // 修改引脚位置
    const inputYs = calcInputYs(num);

    this.inputPinPosition.splice(0, this.inputPinPosition.length,
      ...inputYs.map((pin, index): [number, number] => {
        // return [
        //   this.position[0] + 92 * this.scale,
        //   this.position[1] + inputYs[index] * this.scale,
        // ];
		return [
			0 + 92 * this.scale,
			0 + pin * this.scale,
		];
    }));

	}

	changeInputInverted(idx: number){
		if(idx < 0 || idx >= this.inputCount){
			throw new Error(`Input index ${idx} out of bounds for component ${this.type}`);
		}
		this.inputInverted.splice(idx, 1, !this.inputInverted[idx]); // 切换输入取反状态
	}

	getInputPinCount(): number{
		return this.inputs.length;
	}
	getOutputs(): number[]{
		return this.outputs;
	}


	getAllPorts(){
		let result = {
			id: this.id,			
      ports:[] as Array<{
				id: number,
				x: number,
				y: number
			}>
		};
		for(let i = 0; i < this.getInputPinCount(); i++){
			result.ports.push({
				id: i,
				x: this.inputPinPosition[i][0],
				y: this.inputPinPosition[i][1],
			});
		}  

		for(let i = 0; i < this.outputs.length; i++){
			result.ports.push({
				id: i + this.getInputPinCount(),
				x: this.outputPinPosition[i][0],
				y: this.outputPinPosition[i][1],
			});
		}
		return result;
	}
}