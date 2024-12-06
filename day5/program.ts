type Graph = Map<number, Set<number>>

// returns a map of successors to their predecessors
export function makeRules(rules: string[]) {
  const ret: Graph = new Map()
  for (const r of rules) {
    const [pred, succ] = r.split('|').map(Number)
    let existing = ret.get(succ)
    if (!existing) {
      existing = new Set<number>()
      ret.set(succ, existing)
    }
    existing.add(pred)
  }

  return addMissingRules(ret)
}

// add the possible starting points - the predecessors which have no successors
export function addMissingRules(rules: Graph) {
  const startingKeys = new Set(rules.values().flatMap(i => i)).difference(rules)

  // and add to the rules
  for (const k of startingKeys) {
    rules.set(k, new Set())
  }

  return rules
}

// returns a map of successors to their predecessors
export function makeUpdate(update: string) {
  const ret: Graph = new Map()
  const updateArr = update.split(',').map(Number)
  updateArr.forEach((u, i) => {
    ret.set(u, new Set(updateArr.slice(0, i)))
  })
  return ret
}

export function makeData(input: string) {
  const [ruleStrs, updateStrs] = input
    .split('\n\n')
    .map(r => r.split('\n').filter(l => !!l))

  const rules = makeRules(ruleStrs)
  const updates = updateStrs.map(makeUpdate)

  return { rules, updates }
}

// check that for all of the items in the update map
// its predecessor set is a subset of the rule's predecessor set
export const validOrder = (update: Graph, rules: Graph) =>
  update
    .entries()
    .every(([k, preds]) => preds.isSubsetOf(rules.get(k) ?? new Set()))

// removes a node from the graph
export const removeNode = (g: Graph, node: number): Graph =>
  new Map(
    g
      .entries()
      .flatMap(([k, preds]): [number, Set<number>][] =>
        k === node ? [] : [[k, new Set(preds.values().filter(p => p !== node))]]
      )
  )

// generates all topological sorts of the given DAG
export function topoSorts(g: Graph): number[][] {
  if (g.size === 0) {
    return [[]]
  }

  return [
    ...g
      .entries()
      // get the starting keys - keys which have no predecessors
      .filter(([_, prev]) => prev.size === 0)
      .flatMap(([node]) =>
        topoSorts(removeNode(g, node)).map(s => [node].concat(s))
      ),
  ]
}

// remove from the rules the numbers which are not present in this update
export function filterRules(updateKeys: number[], rules: Graph): Graph {
  rules = new Map(
    rules.entries().flatMap(([k, set]) => {
      if (!updateKeys.includes(k)) {
        return []
      }

      const nextSet = new Set(set.values().filter(s => updateKeys.includes(s)))
      return [[k, nextSet]]
    })
  )

  return addMissingRules(rules)
}

export function reorderUpdate(updateKeys: number[], rules: Graph): number[] {
  return topoSorts(filterRules(updateKeys, rules))[0]
}

export function getOrderedSums(
  rules: Graph,
  updates: Graph[]
): { correct: number; reordered: number } {
  return updates.reduce(
    (acc, update) => {
      let key: 'correct' | 'reordered' = 'correct'
      let updateKeys = [...update.keys()]

      if (!validOrder(update, rules)) {
        key = 'reordered'
        // reorder the update map
        updateKeys = reorderUpdate(updateKeys, rules)
      }

      // get the middle and add
      acc[key] += updateKeys[Math.floor(updateKeys.length / 2)] ?? 0
      return acc
    },
    { correct: 0, reordered: 0 }
  )
}

function main(input: string) {
  const { rules, updates } = makeData(input)

  const { correct, reordered } = getOrderedSums(rules, updates)
  console.log('correctly-ordered sum: ', correct)
  console.log('re-ordered sum: ', reordered)
}

async function readStdin() {
  let text = ''
  const decoder = new TextDecoder()
  for await (const chunk of Deno.stdin.readable) {
    text += decoder.decode(chunk)
  }
  return text
}

if (import.meta.main) {
  main(await readStdin())
}
