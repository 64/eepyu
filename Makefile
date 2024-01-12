.PHONY: all gui timing flash formal sim isim wave synth clean

TOP ?= Top
SCALADEPS := build.sbt src/**/*.scala
PNRARGS := --pcf icestick.pcf --freq 60 --hx1k --package tq144

all: out/$(TOP).v

clean:
	rm -rvf simWorkspace out target tmp

synth: out/Top.json

gui: out/Top.json
	source /opt/oss-cad-suite/environment && nextpnr-ice40 --json $^ --asc $@ $(PNRARGS) --gui

timing: out/Top.asc
	source /opt/oss-cad-suite/environment && icetime -d hx1k -t $^

flash: out/Top.bin
	@echo '--- Flashing... ---'
	source /opt/oss-cad-suite/environment && iceprog $^
	@echo '--- Done ---'

formal: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain ice.$(TOP)Formal'

sim: $(SCALADEPS)
	sbt 'runMain ice.$(TOP)Sim --iverilog'

vsim: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain ice.$(TOP)Sim'

osim: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain ice.$(TOP)Sim --optimise'

wave:
	gtkwave simWorkspace/$(TOP)/test.fst

out/Top.v: $(SCALADEPS)
	source /opt/oss-cad-suite/environment && sbt 'runMain ice.$(TOP)Verilog'

out/Top.json: out/Top.v out/BlackboxRTL.v
	source /opt/oss-cad-suite/environment && yosys -p "synth_ice40 -top Top -json $@" $^

out/Top.asc: out/Top.json
	source /opt/oss-cad-suite/environment && nextpnr-ice40 --json $^ --asc $@ $(PNRARGS)

out/Top.bin: out/Top.asc
	source /opt/oss-cad-suite/environment && icepack $^ $@
