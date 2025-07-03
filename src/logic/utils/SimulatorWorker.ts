// 算法改过了，如果还要用这个，要更新里面的算法

self.onmessage = (event) => {
  const { workQueue, connectionManager, circuitStore } = event.data;

  while (workQueue.length > 0) {
    const { id, idx, value } = workQueue.shift();
    const component = circuitStore.getComponent(id);
    if (!component) continue;

    const oldOutputs = [...component.getOutputs()];
    const newOutputs = component.changeInput(idx, value);

    if (oldOutputs.toString() !== newOutputs.toString()) {
      const pinMap = connectionManager.getOutputPinMap(id);
      if (!pinMap) continue;

      for (const pinIdx of pinMap.keys()) {
        for (const conn of pinMap.get(pinIdx) || []) {
          if (conn.legal) {
            const targetComponent = circuitStore.getComponent(conn.id);
            if (!targetComponent) continue;

            workQueue.push({ id: conn.id, idx: conn.idx, value: newOutputs[pinIdx] });
          }
        }
      }
    }
  }

  // self.postMessage({ success: true });
};