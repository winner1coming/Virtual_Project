// src/composables/useLogicGate.js
import { ref, reactive } from 'vue'

export function createInputs(count) {
  return Array.from({ length: count }, (_, i) =>
    reactive({
      id: `in${i}`,
      value: false,
      inverted: false,
    })
  )
}

export function createOutputs(count) {
  return Array.from({ length: count }, (_, i) =>
    reactive({
      id: `out${i}`,
      value: false,
    })
  )
}

export function setInputValue(gateRef, index, value, updateOutputFn) {
  if (index < 0 || index >= gateRef.inputs.length) return
  gateRef.inputs[index].value = value
  updateOutputFn()
}

export function toggleInput(gate, index, updateOutputFn) {
  gate.inputs[index].value = !gate.inputs[index].value
  updateOutputFn()
}

export function setInputInverted(gateRef, index, inverted, updateOutputFn) {
  if (index < 0 || index >= gateRef.inputs.length) return
  gateRef.inputs[index].inverted = inverted
  updateOutputFn()
}

export function setScale(gateReactive, newScale)
{
  gateReactive.scale = newScale
}