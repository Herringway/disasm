
import cpu6502;
import cpu65816;
import cpucommon;

import gameyaml;

import std.algorithm;
import std.conv;
import std.file;
import std.format;
import std.getopt;
import std.stdio;
import std.range;

enum CPU {
	wd65816,
	mos6502,
}

struct Options {
	string[string] special;
	string yamlfile;
	ulong overrideSize;
	bool showRaw;
	uint memorySpace;
	CPU cpu;
}

int main(string[] args) {
	import std.string : split;
	Options options;
	auto getoptInfo = getopt(args, config.caseSensitive,
		"cpuid|C", &options.memorySpace,
		"cpu|c", &options.cpu,
		"flag|f", &options.special,
		"raw|r", &options.showRaw,
		"size|s", &options.overrideSize,
		"yaml|y", &options.yamlfile);
	if (getoptInfo.helpWanted || args.length < 2) {
		defaultGetoptPrinter("Usage: disasm <romfile> <offset>.", getoptInfo.options);
		return 1;
	}
	ExecutionResults[] results;
	foreach (chunk; args[1 ..  $].chunks(2)) {
		auto path = chunk[0];
		auto offsetStr = chunk[1];
		auto data = cast(ubyte[])read(path);
		ulong offset;
		if (args.length > 2) {
			try {
				offset = parseOffset(offsetStr);
			} catch (Exception) {
				writefln("%s is not a valid offset!", offsetStr);
				return 1;
			}
		}
		results ~= fixup(disassemble(options, data, offset));
	}
	foreach (idx; 0 .. results.map!(x => x.instructions.length).maxElement) {
		size_t labelsWritten;
		foreach (count, result; results) {
			if (idx >= result.instructions.length) {
				continue;
			}
			const address = result.instructions[idx].fullAddress;
			if (address in result.labels) {
				foreach (space; 0 .. count - labelsWritten) {
					writef("% 60s", "");
				}
				labelsWritten++;
				writef!"% -60s"(result.labels[address].name~":");
			}
		}
		if (labelsWritten > 0) {
			writeln();
		}
		bool differences;
		foreach (result; results[1 .. $]) {
			if ((idx >= result.instructions.length) || (idx >= results[0].instructions.length)) {
				differences = true;
				break;
			}
			const baseline = results[0].instructions[idx];
			const instruction = result.instructions[idx];
			if (baseline != instruction) {
				differences = true;
			}
		}
		if (differences) {
			write("\u001b[31m");
		} else {
			write("\u001b[32m");
		}
		foreach (resId, result; results) {
			if (idx >= result.instructions.length) {
				writef("% 60s", "");
				continue;
			}
			auto instruction = result.instructions[idx];
			if (options.showRaw) {
				writef!"% -60s"(format!"    %r"(instruction));
			} else {
				writef!"% -60s"(format!"    %s"(instruction));
			}
		}
		writeln("\u001b[0m");
	}

	return 0;
}
auto disassemble(Options options, ubyte[] rom, ulong addr) {
	auto disassembleCPU(CPUType)() @safe pure {
		auto cpu = CPUType(rom, cast(uint)CPUType.translate(cast(uint)addr));
		if (options.overrideSize > 0) {
			//cpu.stopExecutionAt = options.overrideSize;
		}
		foreach (flag, val; options.special) {
			cpu.setSpecial(flag, val);
		}
		cpu.popFront();
		return cpu.execute();
	}
	GameData yaml;
	if (options.yamlfile != ""){
		yaml = loadGameFiles(options.yamlfile);
	}
	rom = rom[addr..$];
	final switch (options.cpu) {
		case CPU.wd65816:
			return disassembleCPU!(CPU_65816);
		case CPU.mos6502:
			return disassembleCPU!(CPU_6502);
	}
}

auto fixup(ExecutionResults result) {
	foreach (ref instruction; result.instructions) {
		if (instruction.isBranch && !instruction.target.address.isNull && (instruction.target.address.get in result.labels)) {
	        instruction.target.name = result.labels[instruction.target.address.get].name;
		}
	}
	return result;
}

ulong parseOffset(string arg) {
	ulong offset;
	if (arg[1] == 'x') {
		formattedRead(arg, "0x%x", &offset);
	} else {
		formattedRead(arg, "%s", &offset);
	}
	return offset;
}