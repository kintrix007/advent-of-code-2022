import yaml from "yaml";
import * as fs from "fs";

type Operator = "+" | "-" | "*" | "/"

type Operation = {
    a: string;
    b: string;
    op: Operator;
}

type Monkeys = {
    [monkey: string]: number | Operation;
}

function main(): void {
    const monkeys = parse("input");

    const p1 = part1(monkeys);
    console.log(`Part 1: ${p1}`);

    monkeys["humn"] = NaN;
    const chain = getChainToHuman(monkeys, "root");
    const root = monkeys[chain.pop()!]!;
    
    if (typeof root === "number") throw new Error("Root just has a number");

    const target = calcNumber(monkeys, (root.a === chain[chain.length-1]! ? root.b : root.a));
    const p2 = part2(monkeys, chain, chain.pop()!, target);
    console.log(`Part 2: ${p2}`);
}

function part2(monkeys: Monkeys, chain: string[], monkeyName: string, target: number): number {
    if (monkeyName === "humn") return target;
    
    const m = monkeys[monkeyName];
    if (m === undefined) throw new Error(`Monkey '${monkeyName}' does not exist`);
    if (typeof m === "number") throw new Error(`Monkey '${monkeyName}' has a number instead of an operation`);


    const nextName = chain.pop()!;
    const a = calcNumber(monkeys, m.a);
    const b = calcNumber(monkeys, m.b);
    // console.log({a, op: m.op, b, target});
    const newTarget = getNewTarget(a, m.op, b, target);
    return part2(monkeys, chain, nextName, newTarget);
}

function getNewTarget(a: number, op: Operator, b: number, res: number): number {
    switch (op) {
        case "+":
            if (isNaN(a))      return res - b;
            else if (isNaN(b)) return res - a;
            throw new Error(`Something went wrong doing the inverse of '${op}'`);
        case "-":
            if (isNaN(a))      return res + b;
            else if (isNaN(b)) return a - res;
            return getNewTarget(-a, "+", b, res);
        case "*":
            if (isNaN(a))      return res / b;
            else if (isNaN(b)) return res / a;
            throw new Error(`Something went wrong doing the inverse of '${op}'`);
        case "/":
            if (isNaN(a))      return res * b;
            else if (isNaN(b)) return a / res;
            throw new Error(`Something went wrong doing the inverse of '${op}'`);
        default:
            throw new Error(`Something went wrong doing the inverse of '${op}'`);
    }
}

function part1(monkeys: Monkeys): number {
    return calcNumber(monkeys, "root");
}

function calcNumber(monkeys: Monkeys, monkeyName: string): number {
    const m = monkeys[monkeyName];
    if (m === undefined) throw new Error(`Monkey '${monkeyName}' does not exist`);
    if (typeof m === "number") return m;

    const a = calcNumber(monkeys, m.a);
    const b = calcNumber(monkeys, m.b);
    return doOperation(a, m.op, b);
}

function getChainToHuman(monkeys: Monkeys, monkeyName: string): string[] {
    if (monkeyName === "humn") return ["humn"]

    const m = monkeys[monkeyName];
    if (m === undefined) throw new Error(`Monkey '${monkeyName}' does not exist`);
    if (typeof m === "number") return [];
    
    const l = getChainToHuman(monkeys, m.a);
    const r = getChainToHuman(monkeys, m.b);

    if (l[0] === "humn") {
        l.push(monkeyName);
        return l;
    }
    if (r[0] === "humn") {
        r.push(monkeyName);
        return r;
    }
    return [];
}

function doOperation(a: number, op: Operator, b: number) {
    return <number>eval(`${a} ${op} ${b}`);
}


function parse(filename: string): Monkeys {
    const cont: {} = yaml.parse(fs.readFileSync("input").toString());

    const result: Monkeys = Object.fromEntries(Object.entries(cont).map(([k, v]) => {
        switch (typeof v) {
            case "number":
                return [k, v] as const;
            case "string":
                return [k, parseOperation(v)] as const;
            default:
                throw new Error(`Invalid value: '${v}'`);
        }
    }));
    
    return result;
}

function parseOperation(operationStr: string): Operation {
    const [a, op, b] = operationStr.split(" ");
    if (a === undefined || op === undefined || b === undefined) {
        throw new Error(`Invalid operation '${operationStr}'`);
    }

    switch (op) {
        case "+":
        case "-":
        case "*":
        case "/":
            return {a, op, b}
        default:
            throw new Error(`Invalid operator: '${op}'`);
    }
}


main();
