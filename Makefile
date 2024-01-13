.PHONY: all gui timing flash formal sim isim wave synth util clean cleanall

TOP ?= Top
SCALADEPS := build.sbt src/**/*.scala src/**/**/*.scala
PNRARGS := --pcf icestick.pcf --freq 60 --hx1k --package tq144

all: out/$(TOP).v

clean:
	rm -rvf simWorkspace out target tmp

cleanall: clean
	rm -rvf .bloop .metals

synth: out/$(TOP).v
	$(EDITOR) $^

gui: out/Top.json
	source /opt/oss-cad-suite/environment && nextpnr-ice40 --json $^ --asc $@ $(PNRARGS) --gui

timing: out/Top.asc
	source /opt/oss-cad-suite/environment && icetime -d hx1k -t $^

flash: out/Top.bin
	@echo '--- Flashing... ---'
	source /opt/oss-cad-suite/environment && iceprog $^
	@echo '--- Done ---'

util: out/Top.json
	source /opt/oss-cad-suite/environment && nextpnr-ice40 --pack-only --json $^ $(PNRARGS)

formal: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain eepyu.$(TOP)Formal'

sim: $(SCALADEPS)
	sbt 'runMain eepyu.$(TOP)Sim --iverilog'

vsim: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain eepyu.$(TOP)Sim'

osim: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain eepyu.$(TOP)Sim --optimise'

wave:
	gtkwave simWorkspace/$(TOP)/test.vcd

out/$(TOP).v: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain eepyu.$(TOP)Verilog'

out/Top.json: out/Top.v out/BlackboxRTL.v
	source /opt/oss-cad-suite/environment && yosys -p "synth_ice40 -top Top -json $@" $^

out/Top.asc: out/Top.json
	source /opt/oss-cad-suite/environment && nextpnr-ice40 --json $^ --asc $@ $(PNRARGS)

out/Top.bin: out/Top.asc
	source /opt/oss-cad-suite/environment && icepack $^ $@
