CFLAGS=-O2 --limit=0x7fff
TARGET=minilisp.dsk
TARGET_BAS=$(TARGET:.dsk=.bas)
TARGET_BIN=$(TARGET:.dsk=.bin)
TARGET_C=$(TARGET:.dsk=.c)

MESS_DIR=~/Applications/mame
MESS=$(MESS_DIR)/mess64

$(TARGET) : $(TARGET_BAS) $(TARGET_BIN) fact.lsp fib.lsp life.lsp nqueens.lsp
	rm -f $@
	decb dskini $@
	decb copy $(TARGET_BAS) $@,"*.BAS" -t
	decb attr $@,"*.BAS" -0
	decb copy $(TARGET_BAS) $@,$(shell echo $(TARGET_BAS) | tr a-z A-Z) -t
	decb attr $@,$(shell echo $(TARGET_BAS) | tr a-z A-Z) -0
	decb copy $(TARGET_BIN) $@,$(shell echo $(TARGET_BIN) | tr a-z A-Z)
	decb attr $@,$(shell echo $(TARGET_BIN) | tr a-z A-Z) -2 -b
	decb copy fact.lsp $@,"FACT.LSP"
	decb copy fib.lsp $@,"FIB.LSP"
	decb copy life.lsp $@,"LIFE.LSP"
	decb copy nqueens.lsp $@,"NQUEENS.LSP"


$(TARGET_BIN) : $(TARGET_C)
	cmoc $(CFLAGS) $< 

%.bin : %.c
	cmoc $(CFLAGS) $< 

clean :
	rm -rf $(TARGET) *.bin *.i *.lst *.asm *.hex

run : $(TARGET)
	$(MESS) coco3 -rompath $(MESS_DIR)/roms -window -flop1 $(TARGET) -cfg_directory ../cfgs/rgb

run_cmp : $(TARGET)
	$(MESS) coco3 -rompath $(MESS_DIR)/roms -window -flop1 $(TARGET) -cfg_directory ../cfgs/cmp

debug : $(TARGET)
	$(MESS) coco3 -rompath $(MESS_DIR)/roms -window -flop1 $(TARGET) -cfg_directory ../cfgs/rgb -debug

debug_cmp : $(TARGET)
	$(MESS) coco3 -rompath $(MESS_DIR)/roms -window -flop1 $(TARGET) -cfg_directory ../cfgs/cmp -debug


