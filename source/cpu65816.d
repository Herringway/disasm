module cpu65816;

import cpucommon;
import std.conv, std.string, std.stdio, std.algorithm, std.range;

class UnsupportedFlagException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) pure @safe {
		super (msg, file, line);
	}
}

enum ACCUM_DEPENDENT = -1;
enum INDEX_DEPENDENT = -2;
enum defaultFlags = [true, true, true, true, true, true, true, true, true];
enum defaultStackLocation = 0x01FF;
enum Flags65816 { Emulation, Negative, Overflow, AccumWidth, IndexWidth, Decimal, IRQDisable, Zero, Carry };
struct CPU_65816 {
	ubyte[] memory;
	bool[] processorFlags = defaultFlags;
	ushort Accum,IndexX,IndexY = 0;
	ubyte ProgramBank = 0x00;
	ubyte DataBank = 0x00;
	ushort DirectPage = 0x0000;
	ushort StackPointer = defaultStackLocation;
	bool stopAtLongJump = false;
	Instruction65816 front;
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
		ProgramBank = 0x00;
		DataBank = 0x00;
		DirectPage = 0x0000;
		StackPointer = defaultStackLocation;
	}
	static ulong translate(ulong addr) @safe pure {
		if ((addr >= 0xC00000) && (addr <= 0xFFFFFF)) {
			return addr - 0xC00000;
		}
		return addr;
	}
	void setSpecial(string flag, string val) @safe pure {
		switch (flag) {
			case "accum", "M", "8 Bit Accum": accum8 = ((val != "0") && (val != "false")); break;
			case "index", "X", "8 Bit Index": index8 = ((val != "0") && (val != "false")); break;
			case "longjmpstop", "Stop at Long Jump": stopAtLongJump = ((val != "0") && (val != "false")); break;
			default: throw new UnsupportedFlagException(flag);
		}
	}
	string getSpecial(string flag) @safe pure {
		switch (flag) {
			case "accum", "M", "8 Bit Accum": return accum8.text;
			case "index", "X", "8 Bit Index": return index8.text;
			case "longjmpstop", "Stop at Long Jump": return stopAtLongJump.text;
			default: throw new UnsupportedFlagException(flag);
		}
	}
	ref bool accum8() @safe pure {
		return processorFlags[Flags65816.AccumWidth];
	}
	ref bool index8() @safe pure {
		return processorFlags[Flags65816.IndexWidth];
	}
	string addressFormat() @safe pure {return "%06X"; }
	ulong offset(uint addr) pure {
		DataBank = cast(ubyte)((addr&0xFF0000)>>16);
		return _PC = (addr & 0xFFFF);
	}
	uint fullAddr() @safe pure {
		return (DataBank << 16) + _PC;
	}
	bool empty() @safe pure {
		return executionEnded;
	}
	void popFront() @safe pure {
		if (stopExecution) {
			executionEnded = true;
			return;
		}
		const op = memory.front;
		const addressing = opAddressing[op];
		const accumWidth = accum8 ? 1 : 2;
		const indexWidth = index8 ? 1 : 2;
		const instructionLength = addressingInfo[addressing].length(accumWidth, indexWidth) + 1;

		front = Instruction65816(_PC, processorFlags[1 .. 9], memory.take(instructionLength).array);
		_PC += instructionLength;
		stopExecution = front.endOfRoutine || (stopAtLongJump && (front.mnemonic == Mnemonic.JMP));
		if (front.flagsChanged) {
			processorFlags[1 .. 9] = front.processorFlags;
		}
		memory.popFrontN(instructionLength);
	}
}

unittest {
	{
		auto result = CPU_65816!(ubyte[])([0x6B]).array;
		assert(result[0].mnemonic == Mnemonic.RTL);
		assert(result[0].raw == [0x6B]);
		assert(result[0].addressing == Addressing.Return);
	}
	{
		auto result = CPU_65816!(ubyte[])([0xA9, 0xFF, 0x6B]).array;
		assert(result[0].mnemonic == Mnemonic.LDA);
		assert(result[0].raw == [0xA9, 0xFF]);
		assert(result[0].addressing == Addressing.AccumConst);
		assert(result[1].mnemonic == Mnemonic.RTL);
		assert(result[1].raw == [0x6B]);
		assert(result[1].addressing == Addressing.Return);
	}
	{
		auto result = CPU_65816!(ubyte[])([0xC2, 0x20, 0xA9, 0xFF, 0xFF, 0x6B]).array;
		assert(result[0].mnemonic == Mnemonic.REP);
		assert(result[0].raw == [0xC2, 0x20]);
		assert(result[0].addressing == Addressing.ProcConst);
		assert(result[1].mnemonic == Mnemonic.LDA);
		assert(result[1].raw == [0xA9, 0xFF, 0xFF]);
		assert(result[1].addressing == Addressing.AccumConst);
		assert(result[2].mnemonic == Mnemonic.RTL);
		assert(result[2].raw == [0x6B]);
		assert(result[2].addressing == Addressing.Return);
	}
}
enum RelativeTo {
	stack,
	directPage,
	memory
}
struct Instruction65816 {
	Mnemonic mnemonic;
	const(ubyte)[] raw;
	Addressing addressing;
	uint programCounter;

	uint targetAddress;

	bool[8] processorFlags;
	bool flagsChanged;
	this(uint programCounter, bool[8] flags, const(ubyte)[] data) pure @safe {
		mnemonic = mnemonics[data[0]];
		addressing = opAddressing[data[0]];
		processorFlags = flags;
		this.programCounter = programCounter;
		raw = data;
		if (mnemonic.among(Mnemonic.SEP, Mnemonic.REP)) {
			foreach (i; 0..8) {
				bool flag = ((raw[1]>>i)&1) == 1;
				if (mnemonic == Mnemonic.REP) {
					flag = !flag;
				}
				processorFlags[7-i] = flag;
			}
			flagsChanged = true;
		}
		if (addressing.hasAddress) {
			targetAddress = getAddress();
		}
	}

	T opCast(T : Instruction)() {
		auto output = Instruction();
		output.offset = programCounter;
		output.fullAddress = programCounter;
		output.offset = programCounter;
		output.offsetFormat = "%06X";
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
	bool isBranch() const @safe pure {
		return addressing == Addressing.RELATIVE;
	}
	bool endOfRoutine() const @safe pure {
		return addressing == Addressing.Return;
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
			case BlockMove:
				throw new AddressException("No address");
			case STACKRELATIVE:
			case SRINDIRECTINDEXEDY:
				return 0;//StackPointer;
			case AccumConst:
			case IndexConst:
			case ProcConst:
			case Const:
			case SmallConst:
			case ABSOLUTELONG:
			case ABSOLUTELONGJMP:
			case ABSOLUTELONGINDEXED:
				return addr;
			case DirectPage:
			case DPINDIRECT:
			case DPINDIRECTLONG:
			case DPINDIRECTINDEXEDY:
			case DPINDIRECTLONGINDEXEDY:
			case DPINDEXEDX:
			case DPINDEXEDY:
			case DPINDEXEDINDIRECTX:
				return addr;
			case Absolute:
			case ABSOLUTEINDEXEDX:
			case ABSOLUTEINDEXEDY:
			case ABSOLUTEJMP:
			case ABSOLUTEINDIRECT:
			case ABSOLUTEINDEXEDINDIRECT:
			case ABSOLUTEINDIRECTLONG:
				//return (programCounter&0xFF0000) + addr;
				return addr;
			case RELATIVE:
				return (programCounter + raw.length + cast(byte)addr)&0xFFFFFF;
			case RELATIVELONG:
				return (programCounter + raw.length + cast(short)addr)&0xFFFFFF;
		}
	}
	uint fullAddress() const pure @safe {
		return getAddress();
	}
}
enum Mnemonic : ubyte {
	ERR, WDM,
	RTL, RTS, RTI,
	NOP, BRK, WAI, STP,
	REP, SEP,
	PEA,
	PHD, PHA, PHP, PHY, PHB, PHX,
	PLD, PLA, PLP, PLY, PLB, PLX,
	STA, STX, STY, STZ,
	LDX, LDA, LDY,
	JMP, JSL, JSR, JML,
	TCD, TDC,
	DEC, INC, INY, DEY, INX, DEX,
	CLC,
	CMP, CPY, CPX,
	ADC, SBC,
	BRA, BEQ, BNE, BPL, BMI, BCC, BVS, BVC, BRL, BCS,
	ASL, LSR, ROL, ROR,
	AND, ORA, EOR, BIT,
	TAY, TYA, TAX, TXA, TXS, TSX, TXY, TYX, XCE, XBA,
	TRB, TSB, TCS, TSC,
	MVN, MVP,
	SED, COP, SEC, CLD, CLV, CLI,
	SEI, PEI,
	PHK, PER
};
enum Addressing {
	None, Return, DirectPage, Absolute, DPINDIRECTINDEXEDY, DPINDEXEDINDIRECTX, STACKRELATIVE, RELATIVE, SRINDIRECTINDEXEDY,
	DPINDIRECTLONG, ABSOLUTEJMP, AccumConst, IndexConst, DPINDIRECT, ABSOLUTELONG, DPINDEXEDX, DPINDEXEDY, ABSOLUTEINDEXEDX,
	ABSOLUTEINDEXEDY, ABSOLUTELONGINDEXED, BlockMove, ProcConst, ABSOLUTELONGJMP, Const, DPINDIRECTLONGINDEXEDY,
	ABSOLUTEINDIRECT, ABSOLUTEINDIRECTLONG, SmallConst, ABSOLUTEINDEXEDINDIRECT, RELATIVELONG
}
bool hasAddress(Addressing addr) pure @safe {
	return !addr.among(Addressing.None, Addressing.Return, Addressing.BlockMove);
}
struct AddressingInfo {
	bool hasValue = true;
	string format = "%s";
	string valformat = "%s";
	string valformat8 = "%s";
	bool relative;
	byte len;
	ubyte length(size_t accumWidth, size_t indexWidth) const pure @safe {
		if (len == ACCUM_DEPENDENT) {
			return cast(ubyte)accumWidth;
		} else if (len == INDEX_DEPENDENT) {
			return cast(ubyte)indexWidth;
		}
		return len;
	}
}
immutable AddressingInfo[] addressingInfo;
immutable Addressing[] opAddressing;
immutable Mnemonic[] mnemonics;
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
				addrstruct.valformat8 = addrstruct.valformat = "$%02X";
				break;
			case Addressing.DPINDEXEDINDIRECTX:
				addrstruct.len = 1;
				addrstruct.format = "(%s,X)";
				addrstruct.valformat8 = addrstruct.valformat = "$%02X";
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
			case Addressing.STACKRELATIVE:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.Const:
				addrstruct.valformat = "#$%04X";
				addrstruct.len = 2;
				break;
			case Addressing.AccumConst:
				addrstruct.valformat = "#$%04X";
				addrstruct.valformat8 = "#$%02X";
				addrstruct.len = ACCUM_DEPENDENT;
				break;
			case Addressing.IndexConst:
				addrstruct.valformat = "#$%04X";
				addrstruct.valformat8 = "#$%02X";
				addrstruct.len = INDEX_DEPENDENT;
				break;
			case Addressing.BlockMove:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.SRINDIRECTINDEXEDY:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.DPINDIRECTLONG:
				addrstruct.len = 1;
				addrstruct.format = "[%s]";
				addrstruct.valformat8 = addrstruct.valformat = "$%02X";
				break;
			case Addressing.ProcConst:
				addrstruct.len = 1;
				addrstruct.valformat8 = addrstruct.valformat = "#%%%08b";
				break;
			case Addressing.DPINDIRECT:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.RELATIVELONG:
				addrstruct.len = 2;
				addrstruct.valformat = "$%04X";
				addrstruct.relative = true;
				break;
			case Addressing.SmallConst:
				break;
			case Addressing.ABSOLUTELONG:
				addrstruct.valformat = "$%06X";
				addrstruct.len = 3;
				break;
			case Addressing.ABSOLUTELONGINDEXED:
				addrstruct.format = "%s,X";
				addrstruct.valformat = "$%06X";
				addrstruct.len = 3;
				break;
			case Addressing.ABSOLUTELONGJMP:
				addrstruct.valformat = "$%06X";
				addrstruct.len = 3;
				break;
			case Addressing.ABSOLUTEINDIRECT:
				break;
			case Addressing.DPINDIRECTLONGINDEXEDY:
				addrstruct.len = 1;
				addrstruct.format = "[%s],Y";
				addrstruct.valformat8 = addrstruct.valformat = "$%02X";
				break;
			case Addressing.ABSOLUTEINDIRECTLONG:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.ABSOLUTEINDEXEDINDIRECT:
				//TODO?
				addrstruct.hasValue = false;
				break;
			case Addressing.DirectPage:
				addrstruct.len = 1;
				addrstruct.valformat8 = addrstruct.valformat = "$%02X";
				break;
			case Addressing.Absolute:
				addrstruct.valformat = "$%04X";
				addrstruct.len = 2;
				break;
			case Addressing.DPINDEXEDX:
				addrstruct.format = "%s,X";
				addrstruct.valformat = "$%02X";
				addrstruct.len = 1;
				break;
			case Addressing.DPINDEXEDY:
				addrstruct.format = "%s,Y";
				addrstruct.valformat = "$%02X";
				addrstruct.len = 1;
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
	  BRK,ORA,COP,ORA,TSB,ORA,ASL,ORA,PHP,ORA,ASL,PHD,TSB,ORA,ASL,ORA, //0x
	  BPL,ORA,ORA,ORA,TRB,ORA,ASL,ORA,CLC,ORA,INC,TCS,TRB,ORA,ASL,ORA, //1x
	  JSR,AND,JSL,AND,BIT,AND,ROL,AND,PLP,AND,ROL,PLD,BIT,AND,ROL,AND, //2x
	  BMI,AND,AND,AND,BIT,AND,ROL,AND,SEC,AND,DEC,TSC,BIT,AND,ROL,AND, //3x
	  RTI,EOR,WDM,EOR,MVP,EOR,LSR,EOR,PHA,EOR,LSR,PHK,JMP,EOR,LSR,EOR, //4x
	  BVC,EOR,EOR,EOR,MVN,EOR,LSR,EOR,CLI,EOR,PHY,TCD,JMP,EOR,LSR,EOR, //5x
	  RTS,ADC,PER,ADC,STZ,ADC,ROR,ADC,PLA,ADC,ROR,RTL,JMP,ADC,ROR,ADC, //6x
	  BVS,ADC,ADC,ADC,STZ,ADC,ROR,ADC,SEI,ADC,PLY,TDC,JMP,ADC,ROR,ADC, //7x
	  BRA,STA,BRL,STA,STY,STA,STX,STA,DEY,BIT,TXA,PHB,STY,STA,STX,STA, //8x
	  BCC,STA,STA,STA,STY,STA,STX,STA,TYA,STA,TXS,TXY,STZ,STA,STZ,STA, //9x
	  LDY,LDA,LDX,LDA,LDY,LDA,LDX,LDA,TAY,LDA,TAX,PLB,LDY,LDA,LDX,LDA, //Ax
	  BCS,LDA,LDA,LDA,LDY,LDA,LDX,LDA,CLV,LDA,TSX,TYX,LDY,LDA,LDX,LDA, //Bx
	  CPY,CMP,REP,CMP,CPY,CMP,DEC,CMP,INY,CMP,DEX,WAI,CPY,CMP,DEC,CMP, //Cx
	  BNE,CMP,CMP,CMP,PEI,CMP,DEC,CMP,CLD,CMP,PHX,STP,JML,CMP,DEC,CMP, //Dx
	  CPX,SBC,SEP,SBC,CPX,SBC,INC,SBC,INX,SBC,NOP,XBA,CPX,SBC,INC,SBC, //Ex
	  BEQ,SBC,SBC,SBC,PEA,SBC,INC,SBC,SED,SBC,PLX,XCE,JSR,SBC,INC,SBC //Fx
	];
	with(Addressing) opAddressing = [
	  None       , DPINDEXEDINDIRECTX, None           , STACKRELATIVE     , DirectPage, DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, None  , Absolute               , Absolute        , Absolute        , ABSOLUTELONG,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, DirectPage, DPINDEXEDX, DPINDEXEDX, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , Absolute               , ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTELONGINDEXED,
	  ABSOLUTEJMP, DPINDEXEDINDIRECTX, ABSOLUTELONGJMP, STACKRELATIVE     , DirectPage, DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, None  , Absolute               , Absolute        , Absolute        , ABSOLUTELONGINDEXED,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, DPINDEXEDX, DPINDEXEDX, DPINDEXEDX, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , ABSOLUTEINDEXEDX       , ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTELONGINDEXED,
	  Return     , DPINDEXEDINDIRECTX, None           , STACKRELATIVE     , BlockMove , DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, None  , ABSOLUTEJMP            , Absolute        , Absolute        , ABSOLUTELONGINDEXED,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, BlockMove , DPINDEXEDX, DPINDEXEDX, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , ABSOLUTELONGJMP        , ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTELONGINDEXED,
	  Return     , DPINDEXEDINDIRECTX, RELATIVELONG   , STACKRELATIVE     , DirectPage, DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, Return, ABSOLUTEINDIRECT       , Absolute        , Absolute        , ABSOLUTELONG,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, DPINDEXEDX, DPINDEXEDX, DPINDEXEDX, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , ABSOLUTEINDEXEDINDIRECT, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTELONGINDEXED,
	  RELATIVE   , DPINDEXEDINDIRECTX, RELATIVELONG   , STACKRELATIVE     , DirectPage, DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, None  , Absolute               , Absolute        , Absolute        , ABSOLUTELONG,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, DPINDEXEDX, DPINDEXEDX, DPINDEXEDY, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , Absolute               , ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTELONGINDEXED,
	  IndexConst , DPINDEXEDINDIRECTX, IndexConst     , STACKRELATIVE     , DirectPage, DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, None  , Absolute               , Absolute        , Absolute        , ABSOLUTELONG,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, DPINDEXEDX, DPINDEXEDX, DPINDEXEDY, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , ABSOLUTEINDEXEDX       , ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDY, ABSOLUTELONGINDEXED,
	  IndexConst , DPINDEXEDINDIRECTX, ProcConst      , STACKRELATIVE     , DirectPage, DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, None  , Absolute               , Absolute        , Absolute        , ABSOLUTELONG,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, SmallConst, DPINDEXEDX, DPINDEXEDX, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , ABSOLUTEINDIRECTLONG   , ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTELONGINDEXED,
	  IndexConst , DPINDEXEDINDIRECTX, ProcConst      , STACKRELATIVE     , DirectPage, DirectPage, DirectPage, DPINDIRECTLONG        , None, AccumConst      , None, None  , Absolute               , Absolute        , Absolute        , ABSOLUTELONGINDEXED,
	  RELATIVE   , DPINDIRECTINDEXEDY, DPINDIRECT     , SRINDIRECTINDEXEDY, Const     , DPINDEXEDX, DPINDEXEDX, DPINDIRECTLONGINDEXEDY, None, ABSOLUTEINDEXEDY, None, None  , ABSOLUTEINDEXEDINDIRECT, ABSOLUTEINDEXEDX, ABSOLUTEINDEXEDX, ABSOLUTELONGINDEXED];
}
class AddressException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) pure @safe {
		super (msg, file, line);
	}
}