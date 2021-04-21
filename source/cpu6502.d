module cpu6502;

import cpucommon;

import std.conv;
import std.string;
import std.stdio;
import std.algorithm;
import std.range;


enum defaultFlags = [true, true, true, true, true, true, true, true, true];
enum defaultStackLocation = 0x01FF;
enum Flags65816 { Emulation, Negative, Overflow, AccumWidth, IndexWidth, Decimal, IRQDisable, Zero, Carry };
struct CPU_6502 {
	ubyte[] memory;
	bool[] processorFlags = defaultFlags;
	ushort Accum,IndexX,IndexY = 0;
	ushort StackPointer = defaultStackLocation;
	bool stopAtLongJump = false;
	Instruction6502 front;
	uint _PC;
	bool stopExecution;
	bool executionEnded;
	@disable this();
	this(ubyte[] mem, uint initialPC) @safe pure {
		memory = mem;
		_PC = initialPC;
	}
	void reset() @safe pure {
		processorFlags = defaultFlags;
		IndexX = IndexY = Accum = 0;
		StackPointer = defaultStackLocation;
	}
	static ulong translate(ulong addr) @safe pure {
		return ((addr >> 13) << 16) + (addr&0x1FFF) - 0x10;
	}
	void setSpecial(string flag, string val) @safe pure {
		switch (flag) {
			case "longjmpstop", "Stop at Long Jump": stopAtLongJump = ((val != "0") && (val != "false")); break;
			default: throw new UnsupportedFlagException(flag);
		}
	}
	string getSpecial(string flag) @safe pure {
		switch (flag) {
			case "longjmpstop", "Stop at Long Jump": return stopAtLongJump.text;
			default: throw new UnsupportedFlagException(flag);
		}
	}
	string addressFormat() @safe pure {return "%06X"; }
	ulong offset(uint addr) pure {
		return _PC = addr;
	}
	bool empty() @safe pure {
		return executionEnded;
	}
	uint fullAddr() @safe pure {
		return _PC;
	}
	void popFront() @safe pure {
		if (stopExecution) {
			executionEnded = true;
			return;
		}
		const op = memory.front;
		const addressing = opAddressing[op];
		const instructionLength = addressingInfo[addressing].len + 1;

		front = Instruction6502(_PC, memory.take(instructionLength).array);
		_PC += instructionLength;
		stopExecution = front.endOfRoutine || (stopAtLongJump && (front.mnemonic == Mnemonic.JMP));
		memory.popFrontN(instructionLength);

	}
}

struct Instruction6502 {
	Mnemonic mnemonic;
	const(ubyte)[] raw;
	Addressing addressing;
	uint programCounter;

	uint targetAddress;
	this(uint programCounter, const(ubyte)[] data) pure @safe {
		mnemonic = mnemonics[data[0]];
		addressing = opAddressing[data[0]];
		this.programCounter = programCounter;
		raw = data;
		if (addressing.hasAddress) {
			targetAddress = getAddress();
		}
	}
	bool endOfRoutine() const @safe pure {
		return addressing == Addressing.Return;
	}
	bool isBranch() const @safe pure {
		return addressing == Addressing.RELATIVE;
	}
	T opCast(T : Instruction)() const {
		auto output = Instruction();
		output.hasBank = true;
		output.fullAddress = programCounter;
		output.bank = programCounter>>16;
		output.bankFormat = "$%02X:";
		output.offset = programCounter&0xFFFF;
		output.offsetFormat = "%04X";
		output.raw = raw;
		output.isBranch = isBranch;
		output.mnemonic = mnemonic.text;
		if (addressingInfo[addressing].hasValue) {
			output.target.format = addressingInfo[addressing].format;
			output.target.argument = format(addressingInfo[addressing].valformat, targetAddress);
			if (addressing.hasAddress) {
				output.target.address = fullAddress;
			}
		}
		return output;
	}
	size_t instructionLength() const @safe pure nothrow @nogc {
		return addressingInfo[addressing].len;
	}
	uint getAddress() const pure @safe {
		uint addr;
		if (raw.length > 1) {
			foreach (i,v; raw[1..$]) {
				addr += v<<(i*8);
			}
		}
		with(Addressing) final switch(addressing) {
			case None:
			case Return:
				throw new AddressException("No address");
			case SmallConst:
			case Const:
			case DirectPage:
			case DPINDIRECT:
			case DPINDIRECTLONG:
			case DPINDIRECTINDEXEDY:
			case DPINDIRECTLONGINDEXEDY:
			case DPINDEXEDX:
			case DPINDEXEDY:
			case DPINDEXEDINDIRECTX:
			case Absolute:
			case ABSOLUTEINDEXEDX:
			case ABSOLUTEINDEXEDY:
			case ABSOLUTEJMP:
			case ABSOLUTEINDIRECT:
			case ABSOLUTEINDEXEDINDIRECT:
				return addr;
			case RELATIVE:
				return cast(uint)(programCounter + raw.length + cast(byte)addr) & 0xFFFF;
		}
	}
	uint fullAddress() const pure @safe {
		return (getAddress()&0xFFFF) + (programCounter&0xFF0000);
	}
}
enum Mnemonic : ubyte {
	ERR,
	RTS, RTI,
	NOP, BRK,
	PHA, PHP,
	PLA, PLP,
	STA, STX, STY,
	LDX, LDA, LDY,
	JMP, JSR,
	DEC, INC, INY, DEY, INX, DEX,
	CMP, CPY, CPX,
	ADC, SBC,
	BEQ, BNE, BPL, BMI, BCC, BVS, BVC, BCS,
	ASL, LSR, ROL, ROR,
	AND, ORA, EOR, BIT,
	TAY, TYA, TAX, TXA, TXS, TSX,
	SEI, SED, SEC, CLC, CLD, CLV, CLI,
};
enum Addressing {
	None, Return, DirectPage, Absolute, DPINDIRECTINDEXEDY, DPINDEXEDINDIRECTX, RELATIVE,
	DPINDIRECTLONG, ABSOLUTEJMP, DPINDIRECT, DPINDEXEDX, DPINDEXEDY, ABSOLUTEINDEXEDX,
	ABSOLUTEINDEXEDY, Const, DPINDIRECTLONGINDEXEDY,
	ABSOLUTEINDIRECT, SmallConst, ABSOLUTEINDEXEDINDIRECT
}
bool hasAddress(Addressing addr) pure @safe {
	return !addr.among(Addressing.None, Addressing.Return, Addressing.SmallConst);
}
bool hasBaseAddress(Addressing addr) pure @safe {
	return !addr.among(Addressing.None, Addressing.Return, Addressing.Const, Addressing.SmallConst);
}
struct AddressingInfo {
	bool hasValue = true;
	string format = "%s";
	string valformat;
	bool relative;
	byte len;
}
immutable AddressingInfo[] addressingInfo;
immutable Mnemonic[] mnemonics;
immutable Addressing[] opAddressing;
shared static this() {
	addressingInfo = new AddressingInfo[](Addressing.max+1);
	foreach (index, ref addrstruct; addressingInfo) {
		final switch(cast(Addressing)index) {
			case Addressing.Return:
				addrstruct.hasValue = false;
				addrstruct.len = 0;
				break;
			case Addressing.DPINDIRECTINDEXEDY:
				addrstruct.len = 1;
				addrstruct.format = "(%s),Y";
				addrstruct.valformat = "$%02X";
				break;
			case Addressing.DPINDEXEDINDIRECTX:
				addrstruct.len = 1;
				addrstruct.format = "(%s,X)";
				addrstruct.valformat = "$%02X";
				break;
			case Addressing.ABSOLUTEJMP:
				addrstruct.valformat = "$%04X";
				addrstruct.len = 2;
				break;
			case Addressing.None:
				addrstruct.hasValue = false;
				break;
			case Addressing.RELATIVE:
				addrstruct.valformat = "$%02X";
				addrstruct.relative = true;
				addrstruct.len = 1;
				break;
			case Addressing.Const:
				addrstruct.valformat = "#$%02X";
				addrstruct.len = 1;
				break;
			case Addressing.DPINDIRECTLONG:
				addrstruct.len = 1;
				addrstruct.format = "[%s]";
				addrstruct.valformat = "$%02X";
				break;
			case Addressing.DPINDIRECT:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.SmallConst:
				addrstruct.hasValue = false;
				break;
			case Addressing.ABSOLUTEINDIRECT:
				addrstruct.hasValue = true;
				addrstruct.format = "(%s)";
				addrstruct.valformat = "$%04X";
				addrstruct.len = 2;
				break;
			case Addressing.DPINDIRECTLONGINDEXEDY:
				addrstruct.len = 1;
				addrstruct.format = "[%s],Y";
				addrstruct.valformat = "$%02X";
				break;
			case Addressing.ABSOLUTEINDEXEDINDIRECT:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.DirectPage:
				addrstruct.len = 1;
				addrstruct.valformat = "$%02X";
				break;
			case Addressing.Absolute:
				addrstruct.valformat = "$%04X";
				addrstruct.len = 2;
				break;
			case Addressing.DPINDEXEDX:
				addrstruct.hasValue = true;
				addrstruct.len = 1;
				addrstruct.valformat = "$%02X";
				addrstruct.format = "%s,X";
				break;
			case Addressing.DPINDEXEDY:
				addrstruct.hasValue = false;
				break;
			case Addressing.ABSOLUTEINDEXEDX:
				addrstruct.format = "%s,X";
				addrstruct.valformat = "$%04X";
				addrstruct.len = 2;
				break;
			case Addressing.ABSOLUTEINDEXEDY:
				addrstruct.format = "%s,Y";
				addrstruct.valformat = "$%04X";
				addrstruct.len = 2;
				break;
		}
		//writeln(addressingInfo);
	}
	with(Mnemonic) mnemonics = [
		//x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xA  xB  xC  xD  xE  xF
		BRK,ORA,ERR,ERR,ERR,ORA,ASL,ERR,PHP,ORA,ASL,ERR,ERR,ORA,ASL,ERR, //0x
		BPL,ORA,ERR,ERR,ERR,ORA,ASL,ERR,CLC,ORA,ERR,ERR,ERR,ORA,ASL,ERR, //1x
		JSR,AND,ERR,ERR,BIT,AND,ROL,ERR,PLP,AND,ROL,ERR,BIT,AND,ROL,ERR, //2x
		BMI,AND,ERR,ERR,ERR,AND,ROL,ERR,SEC,AND,ERR,ERR,ERR,AND,ROL,ERR, //3x
		RTI,EOR,ERR,ERR,ERR,EOR,LSR,ERR,PHA,EOR,LSR,ERR,JMP,EOR,LSR,ERR, //4x
		BVC,EOR,ERR,ERR,ERR,EOR,LSR,ERR,CLI,EOR,ERR,ERR,ERR,EOR,LSR,ERR, //5x
		RTS,ADC,ERR,ERR,ERR,ADC,ROR,ERR,PLA,ADC,ROR,ERR,JMP,ADC,ROR,ERR, //6x
		BVS,ADC,ERR,ERR,ERR,ADC,ROR,ERR,SEI,ADC,ERR,ERR,ERR,ADC,ROR,ERR, //7x
		ERR,STA,ERR,ERR,STY,STA,STX,ERR,DEY,ERR,TXA,ERR,STY,STA,STX,ERR, //8x
		BCC,STA,ERR,ERR,STY,STA,STX,ERR,TYA,STA,TXS,ERR,ERR,STA,ERR,ERR, //9x
		LDY,LDA,LDX,ERR,LDY,LDA,LDX,ERR,TAY,LDA,TAX,ERR,LDY,LDA,LDX,ERR, //Ax
		BCS,LDA,ERR,ERR,LDY,LDA,LDX,ERR,CLV,LDA,TSX,ERR,LDY,LDA,LDX,ERR, //Bx
		CPY,CMP,ERR,ERR,CPY,CMP,DEC,ERR,INY,CMP,DEX,ERR,CPY,CMP,DEC,ERR, //Cx
		BNE,CMP,ERR,ERR,ERR,CMP,DEC,CMP,CLD,CMP,ERR,ERR,ERR,CMP,DEC,ERR, //Dx
		CPX,SBC,ERR,ERR,CPX,SBC,INC,ERR,INX,SBC,NOP,ERR,CPX,SBC,INC,ERR, //Ex
		BEQ,SBC,ERR,ERR,ERR,SBC,INC,ERR,SED,SBC,ERR,ERR,ERR,SBC,INC,ERR //Fx
	];
	with(Addressing) opAddressing = [
		None, DPINDEXEDINDIRECTX, None, None, None, DirectPage, DirectPage, None, None, Const, None, None, Absolute, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, DPINDIRECT, None, DirectPage, DPINDEXEDX, DPINDEXEDX, None, None, ABSOLUTEINDEXEDY, None, None, Absolute, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, None,
		ABSOLUTEJMP, DPINDEXEDINDIRECTX, None, None, DirectPage, DirectPage, DirectPage, None, None, Const, None, None, Absolute, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, DPINDIRECT, None, DPINDEXEDX, DPINDEXEDX, DPINDEXEDX, None, None, ABSOLUTEINDEXEDY, None, None, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, None,
		Return, DPINDEXEDINDIRECTX, None, None, None, DirectPage, DirectPage, None, None, Const, None, None, ABSOLUTEJMP, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, DPINDIRECT, None, None , DPINDEXEDX, DPINDEXEDX, None, None, ABSOLUTEINDEXEDY, None, None, None, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, None,
		Return, DPINDEXEDINDIRECTX, None, None, DirectPage, DirectPage, DirectPage, None, None, Const, None, Return, ABSOLUTEINDIRECT, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, DPINDIRECT, None, DPINDEXEDX, DPINDEXEDX, DPINDEXEDX, None, None, ABSOLUTEINDEXEDY, None, None, ABSOLUTEINDEXEDINDIRECT, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, None,
		RELATIVE, DPINDEXEDINDIRECTX, None, None, DirectPage, DirectPage, DirectPage, None, None, Const, None, None, Absolute, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, DPINDIRECT, None, DPINDEXEDX, DPINDEXEDX, DPINDEXEDY, None, None, ABSOLUTEINDEXEDY, None, None, Absolute, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, None,
		Const, DPINDEXEDINDIRECTX, Const, None, DirectPage, DirectPage, DirectPage, None, None, Const, None, None, Absolute, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, None, None, DPINDEXEDX, DPINDEXEDX, DPINDEXEDY, None, None, ABSOLUTEINDEXEDY, None, None, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDY, None,
		Const , DPINDEXEDINDIRECTX, None, None, DirectPage, DirectPage, DirectPage, None, None, Const, None, None, Absolute, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, None, None, SmallConst, DPINDEXEDX, DPINDEXEDX, None, None, ABSOLUTEINDEXEDY, None, None, None, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, None,
		Const , DPINDEXEDINDIRECTX, None, None, DirectPage, DirectPage, DirectPage, None, None, Const, None, None, Absolute, Absolute, Absolute, None,
		RELATIVE, DPINDIRECTINDEXEDY, None, None, Const, DPINDEXEDX, DPINDEXEDX, None, None, ABSOLUTEINDEXEDY, None, None, ABSOLUTEINDEXEDINDIRECT, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, None
	];
}
class AddressException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) pure @safe {
		super (msg, file, line);
	}
}
class UnsupportedFlagException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) pure @safe {
		super (msg, file, line);
	}
}
