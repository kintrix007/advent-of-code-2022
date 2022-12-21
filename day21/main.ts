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
}

function part1(monkeys: Monkeys): number {
    return calculateNumber(monkeys, "root");
}

function calculateNumber(monkeys: Monkeys, monkeyName: string): number {
    const m = monkeys[monkeyName];
    if (m == undefined) throw new Error(`Monkey '${monkeyName}' does not exist`);
    if (typeof m == "number") return m;
    
    const a = calculateNumber(monkeys, m.a);
    const b = calculateNumber(monkeys, m.b);
    return doOperation(a, m.op, b);
}

function doOperation(a: number, op: Operator, b: number) {
    return <number>eval(`${a} ${op} ${b}`);
}

function parse(filename: string): {} {
    const cont: {} = yaml.parse(fs.readFileSync("input").toString());

    const result: Monkeys = Object.fromEntries(Object.entries(cont).map(([k, v]) => {
        switch (typeof v) {
            case "number":
                return [k, v] as const;
            case "string": {
                return [k, parseOperation(v)] as const;
            }
            default:
                throw new Error(`Invalid value: '${v}'`);
        }
    }));
    
    return result;
}

function parseOperation(operationStr: string): Operation {
    const [a, op, b] = operationStr.split(" ");
    if (a == undefined || op == undefined || b == undefined) {
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
