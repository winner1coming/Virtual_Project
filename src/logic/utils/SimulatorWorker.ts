// import { EventDrivenSimulator } from '../Simulator';

// const simulator = EventDrivenSimulator.getInstance();

// self.onmessage = (event) => {
//   const { type, payload } = event.data;

//   switch (type) {
//     case 'init': {
//       simulator.changeProject(payload.projectId);
//       break;
//     }

//     case 'enqueue': {
//       simulator.workerEnqueue(payload.id, payload.idx, payload.value);
//       break;
//     }

//     case 'process': {
//       simulator.workerProcess();
//       break;
//     }

//     case 'connect': {
//       simulator.connect(payload.id1, payload.pinIdx1, payload.id2, payload.pinIdx2);
//       break;
//     }

//     case 'disconnect': {
//       simulator.disconnect(payload.id1, payload.pinIdx1, payload.id2, payload.pinIdx2);
//       break;
//     }

//     default:
//       console.warn('Unknown message type:', type);
//   }

//   // 计算完成后，返回最新状态
//   self.postMessage({
//     type: 'done',
//     payload: {
      
//     },
//   });
// };
