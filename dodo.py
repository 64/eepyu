#!/usr/bin/env -S doit -v2 -f

import os
import glob

from doit.action import CmdAction
from doit.tools import Interactive

from functools import partial

DOIT_CONFIG = {"default_tasks": ["elaborate:Top"], "backend": "sqlite3"}

# PNR_ARGS = "--pcf icestick.pcf --freq 60 --hx1k --package tq144"
PNR_ARGS = "--pcf icestick.pcf --freq 12 --hx1k --package tq144"

TOP_BLOCKS = [
    "Alu",
    "AluUart",
    "Core",
    "Decoder",
    "Memory",
    "RegFile",
    "System",
    "Top",
]


def get_scala_sources():
    return glob.glob("./eepyu/src/**/*.scala", recursive=True)


def get_scala_test_sources():
    return glob.glob("./eepyu/**/*.scala", recursive=True)


def get_bin_sources(block):
    return glob.glob(f"./simWorkspace/{block}/{block}.**.bin")


def get_mill_cmd(cmd, verilator=False):
    if verilator:
        os.environ["EEPYU_SIM_OPTS"] = "verilator"
    else:
        os.environ["EEPYU_SIM_OPTS"] = "iverilog"
    return f"./mill {cmd}"


def get_edit_file_cmd(top):
    editor = os.environ.get("EDITOR")
    return f"{editor} out/{top}.v"


def with_oss_cad_suite(cmd):
    return "source /opt/oss-cad-suite/environment && " + cmd


def task_elaborate():
    for top in TOP_BLOCKS:
        yield {
            "name": top,
            "actions": [get_mill_cmd(f"eepyu.runMain eepyu.{top}Verilog")],
            "file_dep": get_scala_sources(),
            "targets": [f"out/{top}.v"],
        }

    yield {"name": None, "task_dep": ["elaborate:Top"]}


def task_verilog():
    for top in TOP_BLOCKS:
        yield {
            "name": top,
            "actions": [Interactive(get_edit_file_cmd(top))],
            "file_dep": [f"out/{top}.v"],
            "uptodate": [False],
        }

    yield {"name": None, "task_dep": ["verilog:Top"]}


def task_sim():
    for top in TOP_BLOCKS:
        yield {
            "name": top,
            "actions": [get_mill_cmd(f"eepyu.runMain eepyu.{top}Sim")],
            "file_dep": get_scala_sources(),
            "uptodate": [False],
        }

    yield {"name": None, "task_dep": ["sim:Top"]}


def task_wave():
    for top in TOP_BLOCKS:
        wave_file = f"simWorkspace/{top}/test.vcd"
        yield {
            "name": top,
            "actions": [Interactive(f"gtkwave {wave_file}")],
            "file_dep": [wave_file],
            "uptodate": [False],
        }

    yield {"name": None, "task_dep": ["wave:Top"]}


def task_test():
    def get_test_cmd(top, sub_test, verilator):
        if verilator:
            cmd = with_oss_cad_suite(
                get_mill_cmd(f"eepyu.test.testOnly eepyu.{top}Tests", True)
            )
        else:
            # For now, we have to run iverilog outside the OSS cad suite.
            cmd = get_mill_cmd(f"eepyu.test.testOnly eepyu.{top}Tests")

        # ScalaTest syntax to run specific commands
        if len(sub_test) > 0:
            cmd += " -- -z " + sub_test

        return cmd

    for top in TOP_BLOCKS:
        yield {
            "name": top,
            "actions": [CmdAction(partial(get_test_cmd, top))],
            "params": [
                {"name": "sub_test", "short": "t", "default": ""},
                {
                    "name": "verilator",
                    "long": "verilator",
                    "type": bool,
                    "default": False,
                },
            ],
            "file_dep": get_scala_sources() + get_scala_test_sources(),
            "uptodate": [False],
            "verbosity": 2,
        }


def task_synthesize():
    return {
        "actions": [
            with_oss_cad_suite(
                'yosys -p "synth_ice40 -top Top -json out/Top.json" out/Top.v out/BlackboxRTL.v'
            )
        ],
        "file_dep": ["out/Top.v", "out/BlackboxRTL.v"] + get_bin_sources("Top"),
        "targets": ["out/Top.json"],
    }


def task_show():
    for top in TOP_BLOCKS:
        yield {
            "name": top,
            "actions": [
                with_oss_cad_suite(
                    f'yosys -p "read_verilog out/Top.v out/BlackboxRTL.v; opt; show {top}"'
                )
            ],
            "file_dep": ["out/Top.v", "out/BlackboxRTL.v"],
            "uptodate": [False],
        }

    yield {"name": None, "task_dep": ["elaborate:Core"]}


def task_pnr():
    return {
        "actions": [
            with_oss_cad_suite(
                f"nextpnr-ice40 --json out/Top.json --asc out/Top.asc {PNR_ARGS}"
            )
        ],
        "file_dep": ["out/Top.json"],
        "targets": ["out/Top.asc"],
    }


def task_gui():
    return {
        "actions": [
            Interactive(
                with_oss_cad_suite(
                    f"nextpnr-ice40 --json out/Top.json {PNR_ARGS} --gui"
                )
            )
        ],
        "file_dep": ["out/Top.json"],
        "uptodate": [False],
    }


def task_pack():
    return {
        "actions": [with_oss_cad_suite("icepack out/Top.asc out/Top.bin")],
        "file_dep": ["out/Top.asc"],
        "targets": ["out/Top.bin"],
    }


def task_flash():
    return {
        "actions": [with_oss_cad_suite("iceprog out/Top.bin")],
        "file_dep": ["out/Top.bin"],
    }


def task_util():
    return {
        "actions": [
            with_oss_cad_suite(
                f"nextpnr-ice40 --pack-only --json out/Top.json {PNR_ARGS}"
            )
        ],
        "file_dep": ["out/Top.json"],
        "uptodate": [False],
    }


def task_timing():
    return {
        "actions": [with_oss_cad_suite("icetime -d hx1k -t out/Top.asc")],
        "file_dep": ["out/Top.asc"],
        "uptodate": [False],
        "verbosity": 2,
    }
