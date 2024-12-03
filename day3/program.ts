function mul(a: number, b: number) {
  return a * b
}

const mulRegex = /mul\(\d+,\d+\)/g
const mulEnableRegex = /mul\(\d+,\d+\)|do\(\)|don't\(\)/g

function evalMatch(match: string): number {
  return Function('mul', `"use strict";return (${match});`)(mul)
}

function sumMuls(input: string, withEnable = false) {
  let enabled = true

  return input
    .matchAll(withEnable ? mulEnableRegex : mulRegex)
    .reduce((acc, matchArr) => {
      const match = matchArr[0]
      if (match === 'do()') {
        enabled = true
      } else if (match === `don't()`) {
        enabled = false
      } else if (enabled) {
        acc += evalMatch(match)
      }

      return acc
    }, 0)
}

async function readStdin() {
  let text = ''
  const decoder = new TextDecoder()
  for await (const chunk of Deno.stdin.readable) {
    text += decoder.decode(chunk)
  }
  return text
}

const input = await readStdin()
console.log('mul sum: ', sumMuls(input))
console.log('mul sum enabled: ', sumMuls(input, true))
