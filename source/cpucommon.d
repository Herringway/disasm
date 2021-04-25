module cpucommon;

import std.format;

import std.stdio;

ExecutionResults execute(CPU)(ref CPU cpu) @safe pure {
	import std.algorithm.iteration : filter, map;
	import std.algorithm.sorting : sort;
	import std.array : array;
	import std.range : enumerate, front, popFrontN;
	ExecutionResults output;
	bool stopExecution;
	auto cpuInstructions = cpu.map!(x => cast(Instruction)x).array;
	output.labels[cpuInstructions.front.fullAddress] = Label(format("UNKNOWN_"~cpuInstructions.front.offsetFormat, cpuInstructions.front.offset), cpuInstructions.front.fullAddress);
	foreach (instruction; cpuInstructions) {
		if (instruction.isBranch) {
			const branchAddress = instruction.target.address.get;
			output.labels.require(branchAddress, Label("", branchAddress));
		}
		output.instructions ~= instruction;
	}
	foreach (id, labelAddr; output.labels.byKey.array.sort.filter!(x => output.labels[x].name == "").enumerate) {
		output.labels[labelAddr].name = format!"@UNKNOWN%d"(id);
	}
	cpu.popFrontN(cpuInstructions.length);
	output.nextOffset = cpu.fullAddr();
	return output;
}

struct Label {
	string name;
	ulong offset;
}

struct ExecutionResults {
	Instruction[] instructions;
	Label[ulong] labels;
	size_t nextOffset;
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
		bool opEquals(const Target other) const @safe {
			return ((this.name.isNull || other.name.isNull)
				? ((this.address.isNull || other.address.isNull)
					? (this.argument == other.argument)
					: (this.address == other.address))
				: (this.name == other.name)) &&
				this.format == other.format;
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
	string addressingMode;
	void toString(Writer)(Writer sink, const FormatSpec!char fmt) const {
		import std.format : format, formattedWrite;
		import std.range : put;
		if (fmt.spec == 'r') {
			if (hasBank) {
				sink.formattedWrite(bankFormat, bank);
			}
			sink.formattedWrite(offsetFormat, offset);
			sink.formattedWrite!" % -12s"(format!"%(%02X %)"(raw));
		} else {
			put(sink, "              ");
		}
		sink.formattedWrite!"%s %s"(mnemonic, target);
	}
	bool opEquals(const Instruction other) const @safe {
		return this.mnemonic == other.mnemonic &&
			this.addressingMode == other.addressingMode &&
			this.target == other.target;
	}
}
