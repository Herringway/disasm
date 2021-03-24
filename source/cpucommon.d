module cpucommon;

import std.format;

import std.stdio;

ExecutionResults execute(CPU)(CPU cpu) {
	import std.algorithm.iteration : filter, map;
	import std.algorithm.sorting : sort;
	import std.range : enumerate;
	ExecutionResults output;
	bool stopExecution;
	auto cpuInstructions = cpu.map!(x => cast(Instruction)x);
	output.labels[cpuInstructions.front.fullAddress] = Label(format("UNKNOWN_"~cpuInstructions.front.offsetFormat, cpuInstructions.front.offset), cpuInstructions.front.fullAddress);
	outer: foreach (instruction; cpuInstructions) {
		if (instruction.isBranch) {
			const branchAddress = instruction.target.address.get;
			output.labels.require(branchAddress, Label("", branchAddress));
		}
		output.instructions ~= instruction;
	}
	foreach (id, labelAddr; output.labels.keys.sort.filter!(x => output.labels[x].name == "").enumerate) {
		output.labels[labelAddr].name = format!"@UNKNOWN%d"(id);
	}
	return output;
}

struct Label {
	string name;
	ulong offset;
}

struct ExecutionResults {
	Instruction[] instructions;
	Label[ulong] labels;
	void toString(Writer)(Writer sink, const FormatSpec!char fmt) const {
		import std.format : format, formattedWrite;
		import std.range : put;
		foreach (instruction; instructions.dup) {
			if (instruction.fullAddress in labels) {
				sink.formattedWrite!"%s:\n"(labels[instruction.fullAddress].name);
			}
			if (fmt.spec == 'r') {
				if (instruction.hasBank) {
					sink.formattedWrite(instruction.bankFormat, instruction.bank);
				}
				sink.formattedWrite(instruction.offsetFormat, instruction.offset);
				sink.formattedWrite!" % -12s"(format!"%(%02X %)"(instruction.raw));
			} else {
				put(sink, "\t");
			}
			if (instruction.isBranch && !instruction.target.address.isNull && (instruction.target.address.get in labels)) {
				instruction.target.name = labels[instruction.target.address.get].name;
			}
			sink.formattedWrite!"%s %s"(instruction.mnemonic, instruction.target);
			if (!instruction.comment.isNull) {
				sink.formattedWrite!"; %s"(instruction.comment.get);
			}
			put(sink, "\n");
		}
	}
}

struct Instruction {
	import std.typecons : Nullable;
	struct Target {
		Nullable!ulong address;
		Nullable!(string,"") name;
		ulong memorySpace;
		string argument;
		string format = "%s";
		string toString() const {
			import std.string : format;
			if (name.isNull) {
				return format(this.format, argument);
			}
			return format(this.format, name);
		}
	}
	ulong fullAddress;
	ulong offset;
	string offsetFormat;
	bool hasBank;
	ulong bank;
	string bankFormat;
	const(ubyte)[] raw;
	string mnemonic;
	Target target;
	bool isBranch;
	Nullable!(string,"") comment;
}

void x() {
	import std.array;
	const instr = ExecutionResults();
	Appender!(char[]) appender;
	instr.toString(appender, FormatSpec!char.init);
}