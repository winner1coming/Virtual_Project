class ConnectionManager {
    constructor() {
        this.connections = new Map(); // 存储连线关系
        // 连线关系的结构如下：
        // {
        //   component1: Map {   // component1 一般为输入端（对电线来说）
        //       index1: { component: component2, index: index2, legal },
        //       ...
        //   },
        //   ...
        // }
    }

    // 添加连线
    addConnection(component1, index1, component2, index2, legal) {
        if (!this.connections.has(component1)) {
            this.connections.set(component1, new Map());
        }
        this.connections.get(component1).set(index1, { component: component2, index: index2, legal: legal });
    }

    // 删除连线
    removeConnection(component1, index1) {
        if (this.connections.has(component1)) {
            const pinMap = this.connections.get(component1);
            pinMap.delete(index1); // 删除指定引脚的连线
            return true;
        }
        return false;
    }

    // 查找连线
    getConnection(component1, index1) {
        if (this.connections.has(component1)) {
            return this.connections.get(component1).get(index1) || null;
        }
        return null;
    }

    // 获取与某组件的输出端相连的pinMap
    getOutputPinMap(component1){
        if(this.connections.has(component1)){
            return this.connections.get(component1);
        }
        return null;
    }

    // 获取所有连线
    getAllConnections() {
        const result = [];
        for (const [component1, pinMap] of this.connections) {
            for (const [index1, { component, index }] of pinMap) {
                result.push({ component1, index1, component2: component, index2: index });
            }
        }
        return result;
    }
}