         Q   P        ��������榶{VD��.���g����X;            uall: call_x86.masm call_x64.masm

%.masm: %.S
	${CPP} -E -P -DGEN_MASM $< -o $@
