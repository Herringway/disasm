
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
	auto data = cast(ubyte[])read(args[1]);
	ulong offset;
	if (args.length > 2) {
		try {
			offset = parseOffset(args[2]);
		} catch (Exception) {
			writefln("%s is not a valid offset!", args[2]);
			return 1;
		}
	}
	//auto finalOffset = CPU.translate(offset);
	//writefln!"Offset: %08X"(finalOffset);
	//if (overrideSize > 0) {
	//	size = overrideSize;
	//}

	const result = disassemble(options, data, offset);
	if (options.showRaw) {
		writefln!"%r"(result);
	} else {
		writefln!"%s"(result);
	}
	//size = 0.reduce!((x,y) => x+y.raw.length)(instructions);
	return 0;
}
auto disassemble(Options options, ubyte[] rom, ulong addr) {
	auto disassembleCPU(CPUType)() {
		auto cpu = CPUType(rom, cast(uint)CPUType.translate(cast(uint)addr));
		if (options.overrideSize > 0) {
			//cpu.stopExecutionAt = options.overrideSize;
		}
		foreach (flag, val; options.special) {
			cpu.setSpecial(flag, val);
		}

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

ulong parseOffset(string arg) {
	ulong offset;
	if (arg[1] == 'x') {
		formattedRead(arg, "0x%x", &offset);
	} else {
		formattedRead(arg, "%s", &offset);
	}
	return offset;
}