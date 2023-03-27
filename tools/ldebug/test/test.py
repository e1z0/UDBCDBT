#! /usr/bin/env python3

# lDebug tests
#  2021 by C. Masloch
#
# Usage of the works is permitted provided that this
# instrument is retained with the works, so that any entity
# that uses the works is notified of this instrument.
#
# DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

import sys, pty, os, time, subprocess, unittest
import fcntl, signal, pathlib, re, contextlib


SCRIPTDIR = pathlib.Path(__file__).parent
build_name = os.getenv('build_name', 'debug')
booting = int(os.getenv('test_booting', '0'))
machine = os.getenv('DEFAULT_MACHINE', 'qemu')
try:
    if os.path.isfile(SCRIPTDIR / ("../tmp/qemutest/l%s.img" % build_name)):
        os.symlink("../tmp/qemutest/l%s.img" % build_name,
            SCRIPTDIR / ("l%s.img" % build_name))
except FileExistsError:
    pass
try:
    if os.path.isfile(SCRIPTDIR / ("../tmp/bdbgtest/b%s.img" % build_name)):
        os.symlink("../tmp/bdbgtest/b%s.img" % build_name,
            SCRIPTDIR / ("b%s.img" % build_name))
except FileExistsError:
    pass
try:
    if os.path.isfile(SCRIPTDIR / ("../bin/l%s.com" % build_name)):
        os.symlink("../bin/l%s.com" % build_name,
            SCRIPTDIR / ("l%s.com" % build_name))
except FileExistsError:
    pass
if machine == 'dosemu':
    if booting:
        dpmiavailable = 0
    else:
        dpmiavailable = 1
elif machine == 'qemu':
    dpmiavailable = 0
    if not booting:
        if "x" in build_name and os.path.isfile(SCRIPTDIR / "dpmi.img"):
            dpmiavailable = 1


class InvalidVariableException(Exception):
    pass

class ControlCException(Exception):
    pass

class lDebugTest(unittest.TestCase):
    def setUp(self):
        self.debug = int(os.getenv('DEBUG', '0'))
        self.booting = int(os.getenv('test_booting', '0'))
        self.defaultsleepduration = \
            float(os.getenv('test_sleepduration', '0.05'))
        self.addsleepduration = \
            float(os.getenv('test_addsleepduration', '0.0'))
        machine = os.getenv('DEFAULT_MACHINE', 'qemu')
        build_name = os.getenv('build_name', 'debug')
        (self.m, s) = pty.openpty()
        self.sname = os.ttyname(s)
        os.close(s)
        self.printprogress = \
            not self.debug and \
            not (('-v' in sys.argv) or ('--verbose' in sys.argv))
        if self.debug:
            print("")
            print("sname=" + self.sname
                + " machine=" + machine
                + " build_name=" + build_name)
        if machine == 'dosemu':
            executable = os.getenv('DOSEMU', 'dosemu')
            if self.booting:
                self.dpmiavailable = 0
                command = "%s -dumb " % (executable) \
                    + " -I \'floppy { device \"%s/b%s.img\" }\' -A" % (SCRIPTDIR, build_name) \
                    + " -I \'serial { com 2 device %s }\'" % self.sname,
            else:
                self.dpmiavailable = 1
                if "x" in build_name:
                    self.dpmidir = "dpmitest\\"
                    self.dpmiexe = None
                command = "%s -dumb -K \"%s\" -E " % (executable, SCRIPTDIR) \
                    + "\'l%s.com /c=\\\"r dco or= 4000\\\"\'" % build_name \
                    + " -I \'serial { com 2 device %s }\'" % self.sname,
        elif machine == 'qemu':
            dpmiimg = ""
            self.dpmiavailable = 0
            executable = os.getenv('QEMU', 'qemu-system-i386')
            if self.booting:
                command = "%s -fda \"%s/b%s.img\"" % (executable, SCRIPTDIR, build_name) \
                    + " -boot order=a" \
                    + " -display none 2> /dev/null" \
                    + " -chardev serial,id=serial2,path=%s" % self.sname \
                    + " -serial null -serial chardev:serial2",
            else:
                if "x" in build_name and os.path.isfile(SCRIPTDIR / "dpmi.img"):
                    self.dpmidir = "b:"
                    self.dpmiexe = "b:hdpmi32.exe -r"
                    self.dpmiavailable = 1
                    dpmiimg = " -fdb \"%s/dpmi.img\"" % SCRIPTDIR
                command = "%s -fda \"%s/l%s.img\"" % (executable, SCRIPTDIR, build_name) \
                    + dpmiimg \
                    + " -boot order=a" \
                    + " -display none 2> /dev/null" \
                    + " -chardev serial,id=serial2,path=%s" % self.sname \
                    + " -serial null -serial chardev:serial2",
        else:
            os.close(self.m)
            raise InvalidVariableException("Invalid content of environment variable DEFAULT_MACHINE")
        self.vmprocess = subprocess.Popen(
            command,
            shell=True,
            preexec_fn=os.setsid,
            stdin=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL)
        self.buffer = ""
        try:
            waitingseconds = 0
            while "Enter KEEP to confirm.\r" not in self.buffer:
                if self.vmprocess.poll() is not None:
                    print("vm stopped")
                    self.tearDown()
                    raise AssertionError("vm stopped")
                try:
                    self.buffer += os.read(self.m, 1).decode()
                except OSError:
                    waitingseconds += 1
                    if self.debug:
                        print("\rWaiting %s seconds" % waitingseconds, end='')
                    time.sleep(1)
            if self.debug and waitingseconds:
                print("")
            time.sleep(0.2)
            os.write(self.m, b"keep\r")
            fl = fcntl.fcntl(self.m, fcntl.F_GETFL)
            fcntl.fcntl(self.m, fcntl.F_SETFL, fl | os.O_NONBLOCK)
            test_initialise_commands = \
                os.getenv('test_initialise_commands', '')
            if test_initialise_commands != "":
                os.write(self.m, test_initialise_commands
                    .replace(';', '\r').encode() + b'\r')
        except KeyboardInterrupt:
            print("^C (aborting vm)")
            self.tearDown()
            raise ControlCException from None

    def tearDown(self):
        if self.debug:
            print("tear down called")
        self.buffer = ""
        if self.booting:
            os.write(self.m, b"boot quit\r")
        else:
            os.write(self.m, b"q\r")
        time.sleep(0.05)
        if self.debug:
            self.do_read(sleepduration = None)
            print(self.buffer)
        if self.vmprocess and self.vmprocess.poll() is None:
            if self.debug:
                print("tear down terminates children")
            os.killpg(os.getpgid(self.vmprocess.pid), signal.SIGTERM)
            # self.vmprocess.terminate()
            self.vmprocess.wait()
        os.close(self.m)

    @contextlib.contextmanager
    def subtestcontext(self, *args, **kwargs):
        with self.subTest(*args, **kwargs) as var:
            try:
                if self.debug:
                    print("=== subtest: " + kwargs.get("msg", "(no message)"))
                yield var
                if self.printprogress:
                    print("#", end='', file=sys.stderr)
                    sys.stderr.flush()
            except:
                raise

    def do_read_single(self):
        try:
            input = os.read(self.m, 1).decode()
        except BlockingIOError:
            try:
                time.sleep(0.001)
                input = os.read(self.m, 1).decode()
                if self.debug:
                    print("success after sleeping")
                    sys.stdout.flush()
            except BlockingIOError:
                input = None
                pass
            except OSError:
                input = None
                pass
            pass
        except OSError:
            input = None
            pass
        return input

    def do_read(self, sleepduration = 0.0, beepreplace = ''):
        if sleepduration is not None:
            if sleepduration != 0.0:
                time.sleep(sleepduration + self.addsleepduration)
            else:
                time.sleep(self.defaultsleepduration)
        overwrite = 0
        linebuffer = ""
        repeat = 4
        while repeat:
            input = self.do_read_single()
            if input:
                break
            if self.debug:
                print("repeating")
                sys.stdout.flush()
            repeat -= 1
            time.sleep(0.005)
        while input:
            if overwrite:
                if input != '\n':
                    linebuffer = ""
                overwrite = 0
            if input == '\r':
                overwrite = 1
            if beepreplace is None:
                linebuffer += input
            else:
                linebuffer += input.replace('\x07', beepreplace)
            if input == '\n':
                self.buffer += linebuffer
                linebuffer = ""
            input = self.do_read_single()
        self.buffer += linebuffer

    def do_write_long(self, text, sleepduration=0.05, sleepmodulo=8):
        text = re.sub(r"[\r\n]+ +", "\r", text)
        text = re.sub(r"^\r+|\r+$", "", text)
        parts = text.split('\r')
        ii = 0
        while parts:
            ii += 1
            if (ii % sleepmodulo) == 0:
                if self.debug:
                    print("sleeping")
                time.sleep(sleepduration)
            part = parts.pop(0)
            if self.debug:
                print("appending >>%s<<" % part)
            part += '\r'
            os.write(self.m, part.encode())

    def test_beep(self):
        os.write(self.m, b"... invalid command\r")
        self.do_read(beepreplace = None)
        if self.debug:
            print(self.buffer.replace('\x07', "#BEEP#"))
        self.assertRegex(self.buffer,
            r"\r\n *\^ Error\x07\r\n")

    def test_build(self):
        os.write(self.m, b"?build\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertRegex(self.buffer,
            r"lDebugX? (release [0-9]+ )?\(",
            "name and opening parens not found")
        self.assertIn(
            "Source Control Revision ID",
            self.buffer,
            "id label not found")

    def test_rr_status(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
            self.buffer = ""
        os.write(self.m, b"a\r")
        self.do_read()
        self.assertRegex(self.buffer,
            r"(?m)^[0-9A-Fa-f]{4}:[0-9A-Fa-f]{4,8} ",
            "assembly aa prompt not found")
        os.write(self.m, b".\r")
        self.do_read()
        self.buffer = ""
        os.write(self.m, b"r ip := 100\rf cs:ip l 10 90\r")
        self.do_read()
        self.assertNotIn("^ Error", self.buffer)
        rrtests = [
            ("test rr jumping", "jo 100",   "70FE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jno 100",  "71FE", "r efl := 0", "  jumping"),
            ("test rr jumping", "jc 100",   "72FE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jnc 100",  "73FE", "r efl := 0", "  jumping"),
            ("test rr jumping", "je 100",   "74FE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jne 100",  "75FE", "r efl := 0", "  jumping"),
            ("test rr jumping", "jbe 100",  "76FE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jnbe 100", "77FE", "r efl := 0", "  jumping"),
            ("test rr jumping", "js 100",   "78FE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jns 100",  "79FE", "r efl := 0", "  jumping"),
            ("test rr jumping", "jp 100",   "7AFE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jnp 100",  "7BFE", "r efl := 0", "  jumping"),
            ("test rr jumping", "jl 100",   "7CFE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jnl 100",  "7DFE", "r efl := 0", "  jumping"),
            ("test rr jumping", "jle 100",  "7EFE", "r efl := 0", "  not jumping"),
            ("test rr jumping", "jnle 100", "7FFE", "r efl := 0", "  jumping"),
            ("test rr jumping", "jo 100",   "70FE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jno 100",  "71FE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "jc 100",   "72FE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jnc 100",  "73FE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "je 100",   "74FE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jne 100",  "75FE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "jbe 100",  "76FE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jnbe 100", "77FE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "js 100",   "78FE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jns 100",  "79FE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "jp 100",   "7AFE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jnp 100",  "7BFE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "jl 100",   "7CFE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "jnl 100",  "7DFE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jle 100",  "7EFE", "r efl := -1", "  jumping"),
            ("test rr jumping", "jnle 100", "7FFE", "r efl := -1", "  not jumping"),
            ("test rr jumping", "jcxz 100", "E3FE", "r ecx := 1", "  not jumping"),
            ("test rr jumping", "jcxz 100", "E3FE", "r ecx := 0", "  jumping"),
            ("test rr jumping", "jcxz 100", "E3FE", "r ecx := 1_0000", "  jumping"),
            ("test rr jumping", "jecxz 100", "67E3FD", "r ecx := 1", "  not jumping"),
            ("test rr jumping", "jecxz 100", "67E3FD", "r ecx := 0", "  jumping"),
            ("test rr jumping", "jecxz 100", "67E3FD", "r ecx := 1_0000", "  not jumping"),
            ("test rr jumping", "loopz 100", "E1FE", \
                "r ecx := 1\rr efl := -1", "  not jumping"),
            ("test rr jumping", "loopz 100", "E1FE", \
                "r ecx := 0\rr efl := -1", "  jumping"),
            ("test rr jumping", "loopz 100", "E1FE", \
                "r ecx := 1_0001\rr efl := -1", "  not jumping"),
            ("test rr jumping", "loopzd 100", "67E1FD", \
                "r ecx := 1\rr efl := -1", "  not jumping"),
            ("test rr jumping", "loopzd 100", "67E1FD", \
                "r ecx := 0\rr efl := -1", "  jumping"),
            ("test rr jumping", "loopzd 100", "67E1FD", \
                "r ecx := 1_0001\rr efl := -1", "  jumping"),
            ("test rr jumping", "loopnz 100", "E0FE", \
                "r ecx := 1\rr efl := 0", "  not jumping"),
            ("test rr jumping", "loopnz 100", "E0FE", \
                "r ecx := 0\rr efl := 0", "  jumping"),
            ("test rr jumping", "loopnz 100", "E0FE", \
                "r ecx := 1_0001\rr efl := 0", "  not jumping"),
            ("test rr jumping", "loopnzd 100", "67E0FD", \
                "r ecx := 1\rr efl := 0", "  not jumping"),
            ("test rr jumping", "loopnzd 100", "67E0FD", \
                "r ecx := 0\rr efl := 0", "  jumping"),
            ("test rr jumping", "loopnzd 100", "67E0FD", \
                "r ecx := 1_0001\rr efl := 0", "  jumping"),
            ("test rr jumping", "loop 100", "E2FE", \
                "r ecx := 1", "  not jumping"),
            ("test rr jumping", "loop 100", "E2FE", \
                "r ecx := 0", "  jumping"),
            ("test rr jumping", "loop 100", "E2FE", \
                "r ecx := 1_0001", "  not jumping"),
            ("test rr jumping", "loopd 100", "67E2FD", \
                "r ecx := 1", "  not jumping"),
            ("test rr jumping", "loopd 100", "67E2FD", \
                "r ecx := 0", "  jumping"),
            ("test rr jumping", "loopd 100", "67E2FD", \
                "r ecx := 1_0001", "  jumping"),
            ("test rr flags", "nop", "90", \
                "r f NC", " NC"),
            ("test rr flags", "nop", "90", \
                "r f CY", " CY"),
            ("test rr flags", "nop", "90", \
                "r fl and:= ~1", " NC"),
            ("test rr flags", "nop", "90", \
                "r fl or:= 1", " CY"),
            ("test rr flags", "nop", "90", \
                "r f NZ", " NZ "),
            ("test rr flags", "nop", "90", \
                "r f ZR", " ZR "),
            ("test rr flags", "nop", "90", \
                "r fl and:= ~40", " NZ "),
            ("test rr flags", "nop", "90", \
                "r fl or:= 40", " ZR "),
            ("test rr mem", "mov ax, word [cs:100]", "2EA10001", \
                "; none", " CS:0100=A12E\r\n"),
            ("test rr mem", "mov word [cs:100], ax", "2EA30001", \
                "; none", " CS:0100=A32E\r\n"),
            ("test rr 386", "mov eax, eax", "6689C0", \
                "m 0", " [needs 386]"),
            ("test rr 386", "mov eax, dword [cs:100]", "662EA10001", \
                "m 0", " [needs 386]"),
            ]
        for tuple in rrtests:
            msg = tuple[0]
            inst = tuple[1]
            code = tuple[2]
            setup = tuple[3]
            match = tuple[4]
            if len(tuple) >= 6:
                success = tuple[5]
            else:
                success = 1
            if len(tuple) >= 7:
                delay = tuple[6]
            else:
                delay = 0
            with self.subtestcontext(msg = msg + ": " + inst):
                if self.debug:
                    print(self.buffer)
                    print("msg=%s inst=%s code=%s success=%d"
                        % (msg, inst, code, success))
                self.buffer = ""
                actuallysucceeded = 0
                try:
                    os.write(self.m, ("a 100\r%s\r.\r" % inst).encode())
                    os.write(self.m, ("%s\r" % setup).encode())
                    os.write(self.m, b"r\r")
                    if delay:
                        time.sleep(0.05)
                    self.do_read()
                    self.assertNotRegex(self.buffer,
                        r"\^ Error",
                        "error in assembling")
                    self.assertRegex(self.buffer,
                        r"\r\n[0-9A-Fa-f:]{9,13} %s " % code,
                        "code mismatch")
                    self.assertRegex(self.buffer,
                        match,
                        "status mismatch")
                    actuallysucceeded = 1
                except AssertionError as e:
                    if success:
                        raise e
                    else:
                        if self.debug:
                            print("passing on upon AssertionError(\""
                                + str(e) + "\")")
                        pass
                if actuallysucceeded and not success:
                    raise AssertionError("unexpected success")
        if self.debug:
            print(self.buffer)

    def test_aa_basic(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
            self.buffer = ""
        os.write(self.m, b"a\r")
        self.do_read()
        self.assertRegex(self.buffer,
            r"(?m)^[0-9A-Fa-f]{4}:[0-9A-Fa-f]{4,8} ",
            "assembly aa prompt not found")
        os.write(self.m, b".\r")
        self.do_read()
        asmtests = [
            ("test aa", "nop", ".*"),
            ("test aa", "nop", "90"),
            ("test aa", "mov ax, 0", "B80000"),
            ("test aa", "mov ax, 1234", "B83412"),
            ("test aa", "mov ax, (26)", "B82600"),
            ("test aa", "mov ax, (#38)", "B82600"),
            ("test aa", "jmp 100", "EBFE"),
            ("test aa", "jmp near 100", "E9FDFF"),
            ("test aa fail", "jmp near 100", "90", 0),
            ("test aa fail", "jmp xyz", ".*", 0),
            ("test aa", "push +1", "6A01"),
            ("test aa", "pushw +1", "6A01"),
            ("test aa", "pushd +1", "666A01"),
            ("test aa", "push byte +1", "6A01"),
            ("test aa", "pushw byte +1", "6A01"),
            ("test aa", "pushd byte +1", "666A01"),
            ("test aa", "push word +1", "680100"),
            ("test aa", "push dword +1", "666801000000"),
            ("test aa", "pushw word +1", "680100"),
            ("test aa", "pushd dword +1", "666801000000"),
            ("test aa fail", "pushd word +1", ".*", 0),
            ("test aa fail", "pushw dword +1", ".*", 0),
            ("test aa", "push +123", "682301"),
            ("test aa", "pushw +123", "682301"),
            ("test aa", "pushd +123", "666823010000"),
            ("test aa fail", "push byte +123", ".*", 0),
            ("test aa fail", "pushw byte +123", ".*", 0),
            ("test aa fail", "pushd byte +123", ".*", 0),
            ("test aa", "push word +123", "682301"),
            ("test aa", "push dword +123", "666823010000"),
            ("test aa", "pushw word +123", "682301"),
            ("test aa", "pushd dword +123", "666823010000"),
            ("test aa fail", "pushd word +123", ".*", 0),
            ("test aa fail", "pushw dword +123", ".*", 0),
            ("test aa fail", "add [100], 12", ".*", 0),
            ("test aa", "loop 100", "E2FE"),
            ("test aa", "loopz 100", "E1FE"),
            ("test aa", "loopnz 100", "E0FE"),
            ("test aa", "loopw 100", "E2FE"),
            ("test aa", "loopzw 100", "E1FE"),
            ("test aa", "loopnzw 100", "E0FE"),
            ("test aa", "loopd 100", "67E2FD"),
            ("test aa", "loopzd 100", "67E1FD"),
            ("test aa", "loopnzd 100", "67E0FD"),
            ("test aa", "loop 100, cx", "E2FE"),
            ("test aa", "loopz 100, cx", "E1FE"),
            ("test aa", "loopnz 100, cx", "E0FE"),
            ("test aa fajl", "loopw 100, cx", ".*", 0),
            ("test aa fail", "loopzw 100, cx", ".*", 0),
            ("test aa fail", "loopnzw 100, cx", ".*", 0),
            ("test aa fail", "loopd 100, cx", ".*", 0),
            ("test aa fail", "loopzd 100, cx", ".*", 0),
            ("test aa fail", "loopnzd 100, cx", ".*", 0),
            ("test aa", "loop 100, ecx", "67E2FD"),
            ("test aa", "loopz 100, ecx", "67E1FD"),
            ("test aa", "loopnz 100, ecx", "67E0FD"),
            ("test aa fail", "loopw 100, ecx", ".*", 0),
            ("test aa fail", "loopzw 100, ecx", ".*", 0),
            ("test aa fail", "loopnzw 100, ecx", ".*", 0),
            ("test aa fail", "loopd 100, ecx", ".*", 0),
            ("test aa fail", "loopzd 100, ecx", ".*", 0),
            ("test aa fail", "loopnzd 100, ecx", ".*", 0),
            ("test aa", "int3", "CC"),
            ("test aa", "int 3", "CC"),
            ("test aa", "int byte 3", "CD03"),
            ("test aa", "ret", "C3"),
            ("test aa", "retn", "C3"),
            ("test aa", "retf", "CB"),
            ("test aa", "ret 8", "C20800"),
            ("test aa", "retn 8", "C20800"),
            ("test aa", "retf 8", "CA0800"),
            ("test aa", "retw", "C3"),
            ("test aa", "retnw", "C3"),
            ("test aa", "retfw", "CB"),
            ("test aa", "retw 8", "C20800"),
            ("test aa", "retnw 8", "C20800"),
            ("test aa", "retfw 8", "CA0800"),
            ("test aa", "retd", "66C3"),
            ("test aa", "retnd", "66C3"),
            ("test aa", "retfd", "66CB"),
            ("test aa", "retd 8", "66C20800"),
            ("test aa", "retnd 8", "66C20800"),
            ("test aa", "retfd 8", "66CA0800"),
            ("test aa fail", "ret 10000", ".*", 0),
            ("test aa fail", "retn 10000", ".*", 0),
            ("test aa fail", "retf 10000", ".*", 0),
            ("test aa fail", "retw 10000", ".*", 0),
            ("test aa fail", "retnw 10000", ".*", 0),
            ("test aa fail", "retfw 10000", ".*", 0),
            ("test aa fail", "retd 10000", ".*", 0),
            ("test aa fail", "retnd 10000", ".*", 0),
            ("test aa fail", "retfd 10000", ".*", 0),
            ("test aa", "mov ax, 1_00", "B80001"),
            ]
        for tuple in asmtests:
            msg = tuple[0]
            inst = tuple[1]
            code = tuple[2]
            if len(tuple) >= 4:
                success = tuple[3]
            else:
                success = 1
            if len(tuple) >= 5:
                delay = tuple[4]
            else:
                delay = 0
            with self.subtestcontext(msg = msg + ": " + inst):
                if self.debug:
                    print(self.buffer)
                    print("msg=%s inst=%s code=%s success=%d"
                        % (msg, inst, code, success))
                self.buffer = ""
                actuallysucceeded = 0
                try:
                    os.write(self.m, ("a 100\r%s\r.\r" % inst).encode())
                    os.write(self.m, b"u 100 l 1\r")
                    if delay:
                        time.sleep(0.05)
                    self.do_read()
                    self.assertNotRegex(self.buffer,
                        r"\^ Error",
                        "error in assembling")
                    self.assertRegex(self.buffer,
                        r"\r\n[0-9A-Fa-f:]{9,13} %s " % code,
                        "code mismatch")
                    actuallysucceeded = 1
                except AssertionError as e:
                    if success:
                        raise e
                    else:
                        if self.debug:
                            print("passing on upon AssertionError(\""
                                + str(e) + "\")")
                        pass
                if actuallysucceeded and not success:
                    raise AssertionError("unexpected success")
        if self.debug:
            print(self.buffer)

    def test_rr_basic(self):
        self.do_read()
        rrtests = [
            ("test rr", "r ax := 1", "ax", ".*"),
            ("test rr", "r ax := 1", "ax", "1"),
            ("test rr", "r ax +:= 1", "ax", "2"),
            ("test rr", "r word [cs:ip] := 9090", "word [cs:ip]", "9090"),
            ("test rr fail", "r ax := 0", "ax", "FFFF", 0),
            ("test rr fail", "r ax := baz", "ax", ".*", 0),
            ("test rr double 1", "r bxcx := 1234_5678", "cx", "5678"),
            ("test rr double 2", "; nothing", "bx", "1234"),
            ("test expr basic", "r v0 := 2 * 3 + 4", "v0", "A"),
            ("test expr parens", "r v0 := 2 * (3 + 4)", "v0", "E"),
            ("test expr bitmirror", "r v0 := AA >< 8", "v0", "55"),
            ("test expr value in", "r v0 :="
                + " VALUE 26 IN 26, 26, FROM 20 TO 26,"
                + " FROM 26 LENGTH 1, 38, AA55", "v0", "4", 1, 2),
            ("test expr division error", "r v0 := 1 / 0", "v0", ".*", 0),
            ("test expr mismatch", "r v0 := 1", "v0", "0", 0),
            ("test expr overflow", "r v0 := #4294967296", "v0", ".*", 0),
            ("test expr conditional", "r v0 := 1 ?? 4 :: 8", "v0", "4"),
            ("test expr conditional", "r v0 := 0 ?? 4 :: 8", "v0", "8"),
            ("test expr conditional", "r v0 := 0 ?? 4 :: 1 ?? 8 :: 10", "v0", "8"),
            ("test expr conditional", "r v0 := 0 ?? 4 :: 0 ?? 8 :: 10", "v0", "10"),
            ("test expr conditional", "r v0 := 1 ?? 0 ?? 8 :: 10 :: 20", "v0", "10"),
            ("test expr conditional", "r v0 := 1 ?? 1 ?? 8 :: 10 :: 20", "v0", "8"),
            ("test rr", "r ax := #'A1'", "ax", "3141"),
            ("test rr", 'r ax := #"A1"', "ax", "3141"),
            ("test rr", 'r eax := #"321"', "eax", "00313233"),
            ("test rr fail", 'r eax := #"1', "eax", ".*", 0),
            ("test rr fail", 'r eax := #""', "eax", "0+"),
            ("test rr", "r dword [cs:ip] := 90909090", "dword [cs:ip]", "90909090"),
            ("test rr", "r 3byte [cs:ip] := 112233", "3byte [cs:ip]", "112233"),
            ("test rr", "; nothing", "byte [cs:ip + 3]", "90"),
            ("test rr", "r word [cs:ip] := 1122", "word [cs:ip]", "1122"),
            ("test rr", "; nothing", "word [cs:ip + 2]", "9011"),
            ("test rr", "r byte [cs:ip] := 11", "byte [cs:ip]", "11"),
            ("test rr", "; nothing", "3byte [cs:ip + 1]", "901111"),
            ]
        for tuple in rrtests:
            msg = tuple[0]
            cmd = tuple[1]
            var = tuple[2]
            value = tuple[3]
            if len(tuple) >= 5:
                success = tuple[4]
            else:
                success = 1
            if len(tuple) >= 6:
                delay = tuple[5]
            else:
                delay = 0
            with self.subtestcontext(msg = msg + ": " + cmd):
                if self.debug:
                    print(self.buffer)
                    print("msg=%s cmd=%s var=%s value=%s success=%d"
                        % (msg, cmd, var, value, success))
                self.buffer = ""
                actuallysucceeded = 0
                try:
                    os.write(self.m, ("%s\r" % cmd).encode())
                    os.write(self.m, ("r %s .\r" % var).encode())
                    if delay:
                        time.sleep(0.05 * delay)
                    self.do_read()
                    self.assertNotRegex(self.buffer,
                        r"\^ Error",
                        "error in command")
                    self.assertRegex(self.buffer,
                        r"\r\n[\]\[: A-Za-z0-9]+ 0*%s\r\n" % value,
                        "value mismatch")
                    actuallysucceeded = 1
                except AssertionError as e:
                    if success:
                        raise e
                    else:
                        if self.debug:
                            print("passing on upon AssertionError(\""
                                + str(e) + "\")")
                        pass
                if actuallysucceeded and not success:
                    raise AssertionError("unexpected success")
        if self.debug:
            print(self.buffer)

    def test_misc(self):
        self.do_read()
        misctests = [
            ("ss", "f 100 l 80 0\r"
                + "s 100 l 10 0", "\r\n0010 matches\r\n"),
            ("post ss", "r src .", "\r\nSRC 0*10\r\n"),
            ("post ss", "r sro .", "\r\nSRO 0*100\r\n"),
            ("post ss", "r sro0 .", "\r\nSRO0 0*100\r\n"),
            ("post ss", "r sro1 .", "\r\nSRO1 0*101\r\n"),
            ("post ss", "h srs == ds", "\r\n0*1  decimal:.*\r\n"),
            ("setup for dd", "r dx 100", "."),
            ("dd", "d 100 l 10", "\r\n[0-9A-Fa-f]{4}:0100  (00[- ]){16}.{16}\r\n"),
            ("dd", "d $ds:100 l 10", "\r\n[0-9A-Fa-f]{4}:0100  (00[- ]){16}.{16}\r\n"),
            ("dd", "d 100 ; comment", "\r\n([0-9A-Fa-f]{4}:01[0-7]0  (00[- ]){16}.{16}\r\n){8}"),
            ("dd", "d ptr dsdx l 10", "\r\n[0-9A-Fa-f]{4}:0100  (00[- ]){16}.{16}\r\n"),
            ("dd", "d pointer dsdx l 10", "\r\n[0-9A-Fa-f]{4}:0100  (00[- ]){16}.{16}\r\n"),
            ("dd", "d ptr dsdx", "\r\n([0-9A-Fa-f]{4}:01[0-7]0  (00[- ]){16}.{16}\r\n){8}"),
            ("dd", "d $ ptr dsdx l 10", "\r\n[0-9A-Fa-f]{4}:0100  (00[- ]){16}.{16}\r\n"),
            ("dd", "d $ pointer dsdx l 10", "\r\n[0-9A-Fa-f]{4}:0100  (00[- ]){16}.{16}\r\n"),
            ("dd", "d $ ptr dsdx", "\r\n([0-9A-Fa-f]{4}:01[0-7]0  (00[- ]){16}.{16}\r\n){8}"),
            ("dd", "d ds:ptr dsdx", ".", 0),
            ("dd", "d ds:ptr dx", ".", 0),
            ("dd", "d ds:dx ptr 10F", ".", 0),
            ("dd", "d ds:pointer dsdx", ".", 0),
            ("dd", "d ds:pointer dx", ".", 0),
            ("dd", "d ds:dx pointer 10F", ".", 0),
            ("vars", "r dco .", "."),
            ("vars", "r dco0 .", ".", 0),
            ("vars", "r dco1 .", "."),
            ("vars", "r dco2 .", "."),
            ("vars", "r dco3 .", "."),
            ("vars", "r dco4 .", "."),
            ("vars", "r dco5 .", "."),
            ("vars", "r dco6 .", "."),
            ("vars", "r dco7 .", ".", 0),
            ("vars", "r dif .", "."),
            ("vars", "r dif0 .", ".", 0),
            ("vars", "r dif1 .", "."),
            ("vars", "r dif2 .", "."),
            ("vars", "r dif3 .", "."),
            ("vars", "r dif4 .", "."),
            ("vars", "r dif5 .", "."),
            ("vars", "r dif6 .", "."),
            ("vars", "r dif7 .", ".", 0),
            ("hh", "h base=#10 18", "\r\n24\r\n"),
            ("hh", "h base=#10 group=3 400", "\r\n1_024\r\n"),
            ("hh", "h base=#2 group=4 AA55", "\r\n1010_1010_0101_0101\r\n"),
            ("hh", "h base=#2 group=4 width=10 1138", "\r\n0001_0001_0011_1000\r\n"),
            ("hh", "h linear $1234:5", "\r\n0*12345 "),
            ("hh", "h linear $1000:2345", "\r\n0*12345 "),
            ("hh", "h 2 +1", "\r\n0*3  decimal:"),
            ("hh", "h 2, +1", "\r\n0*3  0*1\r\n"),
            ("hh", "h 2,, +1", ".", 0),
            ("hh", "h base=10 2, +1", ".", 0),
            ("hh", "h ri2Do == word [$0:2D*4]", "\r\n0*1  decimal:"),
            ("hh", "h ri2Ds == word [$0:2D*4 + 2]", "\r\n0*1  decimal:"),
            ("hh", "h ri2Dl == linear $word [$0:2D*4 + 2]:word [$0:2D*4]", "\r\n0*1  decimal:", 1, 1),
            ("hh", "h ri2Dl == linear $ri2Ds:ri2Do", "\r\n0*1  decimal:", 1, 1),
            ("psp var", "h segment ppi == dpr || dif & 4000", "\r\n0*1  decimal:"),
            ("psp var", "h ppr == dpr || dif & 4000", "\r\n0*1  decimal:"),
            ("psp var", "h psp != 0 || dif & 4000", "\r\n0*1  decimal:"),
            ("psp var", "h (linear psps:0) == (psp << 4) || dif & 4000", "\r\n0*1  decimal:", 1, 1),
            ("psp var", "h ppi == 0 || !(dif & 4000)", "\r\n0*1  decimal:"),
            ("psp var", "h ppr == 0 || !(dif & 4000)", "\r\n0*1  decimal:"),
            ("psp var", "h psp == 0 || !(dif & 4000)", "\r\n0*1  decimal:"),
            ("reg var", "h al", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ah", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ax", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h eax", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h bl", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h bh", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h bx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ebx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h cl", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ch", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h cx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ecx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h dl", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h dh", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h dx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h edx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h di", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h edi", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h si", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h esi", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h bp", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ebp", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h sp", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h esp", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ip", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h eip", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h fl", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h efl", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h cs", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ds", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h es", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h ss", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h fs", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h gs", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h dxax", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h bxcx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h csip", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h sssp", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h dssi", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h esdi", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h dsdx", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var", "h sidi", "\r\n[0-9A-Fa-f]*  decimal:"),
            ("reg var equal", "h dxax == dx * 10000 + ax", "\r\n0*1  decimal:"),
            ("reg var equal", "h bxcx == bx * 10000 + cx", "\r\n0*1  decimal:"),
            ("reg var equal", "h csip == cs * 10000 + ip", "\r\n0*1  decimal:"),
            ("reg var equal", "h sssp == ss * 10000 + sp", "\r\n0*1  decimal:"),
            ("reg var equal", "h dssi == ds * 10000 + si", "\r\n0*1  decimal:"),
            ("reg var equal", "h esdi == es * 10000 + di", "\r\n0*1  decimal:"),
            ("reg var equal", "h dsdx == ds * 10000 + dx", "\r\n0*1  decimal:"),
            ("reg var equal", "h sidi == si * 10000 + di", "\r\n0*1  decimal:"),
            ("rr limit", "r ax 0", "."),
            ("rr limit", "r ax 1", "."),
            ("rr limit", "r ax -1", "."),
            ("rr limit", "r ax 7FFF", "."),
            ("rr limit", "r ax FFFF", "."),
            ("rr limit", "r ax 10000", ".", 0),
            ("rr limit", "r ax -8000", "."),
            ("rr limit", "r ax -8001", ".", 0),
            ("rr limit", "r ax FFFF_FFFF", ".", 0),
            ("rr limit", "r word [F0] 0", "."),
            ("rr limit", "r word [F0] 1", "."),
            ("rr limit", "r word [F0] -1", "."),
            ("rr limit", "r word [F0] 7FFF", "."),
            ("rr limit", "r word [F0] FFFF", "."),
            ("rr limit", "r word [F0] 10000", ".", 0),
            ("rr limit", "r word [F0] -8000", "."),
            ("rr limit", "r word [F0] -8001", ".", 0),
            ("rr limit", "r word [F0] FFFF_FFFF", ".", 0),
            ]
        for tuple in misctests:
            msg = tuple[0]
            cmd = tuple[1]
            match = tuple[2]
            if len(tuple) >= 4:
                success = tuple[3]
            else:
                success = 1
            if len(tuple) >= 5:
                delay = tuple[4]
            else:
                delay = 0
            with self.subtestcontext(msg = msg + ": " + cmd.replace("\r","##")):
                if self.debug:
                    print(self.buffer)
                    print("msg=%s cmd=%s match=%s success=%d"
                        % (msg, cmd.replace("\r","##"),
                            match.replace("\r","##").replace("\n","%%"),
                            success))
                self.buffer = ""
                actuallysucceeded = 0
                try:
                    os.write(self.m, ("%s\r" % cmd).encode())
                    if delay:
                        time.sleep(0.05)
                    self.do_read()
                    self.assertNotRegex(self.buffer,
                        r"\^ Error",
                        "error in command")
                    self.assertRegex(self.buffer,
                        match,
                        "match mismatch")
                    actuallysucceeded = 1
                except AssertionError as e:
                    if success:
                        raise e
                    else:
                        if self.debug:
                            print("passing on upon AssertionError(\""
                                + str(e) + "\")")
                        pass
                if actuallysucceeded and not success:
                    raise AssertionError("unexpected success")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "autorepeat tt test"):
            self.do_write_long("""
                r dco3 and= ~ 1000_0000  ; enable autorepeat
                a 100
                 nop
                 nop
                 inc ax
                 int3
                 .
                t=100
                """)
            os.write(self.m, b"\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f]{4}:0102 +40 +inc +ax")
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"; empty\r\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotRegex(self.buffer,
                r"\r\n[0-9A-Fa-f]{4}:[0-9A-Fa-f]{4,8} ")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "autorepeat tt with blanks test"):
            self.do_write_long("""
                r dco3 and= ~ 1000_0000  ; enable autorepeat
                a 100
                 nop
                 nop
                 inc ax
                 int3
                 .
                t=100
                """)
            os.write(self.m, b"  \r")
            self.do_read(sleepduration = 0.2)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f]{4}:0102 +40 +inc +ax")
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"  ; empty\r  \r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotRegex(self.buffer,
                r"\r\n[0-9A-Fa-f]{4}:[0-9A-Fa-f]{4,8} ")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "autorepeat disable tt test"):
            self.do_write_long("""
                r dco3 or= 1000_0000  ; disable autorepeat
                a 100
                 nop
                 nop
                 inc ax
                 int3
                 .
                t=100
                """)
            os.write(self.m, b"\r")
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f]{4}:0102 +40 +inc +ax")
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"; empty\r\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotRegex(self.buffer,
                r"\r\n[0-9A-Fa-f]{4}:[0-9A-Fa-f]{4,8} ")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "hh no overflow test"):
            levels = 12
            # this number of parens appears to work on lDDebug and lDebug
            os.write(self.m, ("h " + levels * "(" + "1" + levels * ")" + "\r").encode())
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotIn("Stack overflow occurred", self.buffer)
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "hh overflow test"):
            levels = 48
            # this number of parens appears to fail on all builds
            os.write(self.m, ("h " + levels * "(" + "1" + levels * ")" + "\r").encode())
            self.do_read(sleepduration = 0.2)
            self.assertNotIn("^ Error", self.buffer)
            self.assertIn("Stack overflow occurred", self.buffer)
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "register change highlighting test"):
            os.write(self.m, b"r dco3 or= 4_0000\rr\rr ax ^:= word -1\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.buffer = ""
            os.write(self.m, b"r\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)AX *= *\x1B\[7m[0-9A-Fa-f]{4,8}\x1B\[m")
        os.write(self.m, b"r dco3 := dcs3\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "history test"):
            os.write(self.m, b"; test command\r\x1B[A\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(~?[\-#]; test command\r\n){2}")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "line edit test"):
            os.write(self.m,
                b"r ax xyz\x08\x08\x08."
                + b"\x1B[D" * 4
                + b"e\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"\r\nEAX +[0-9A-Fa-f]{8}\r\n")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "sleep test"):
            self.do_write_long("""
                r v0 := dword [$40:6C]
                sleep 1 ticks
                h v0 == dword [$40:6C]
                """)
            self.do_read(sleepduration = 0.2)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"\r\n0+  decimal")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "paging test"):
            self.do_write_long("""
                r v0 := dsr
                r dsr := 4
                d 100 l 10 * 6
                """)
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"d 100 l 10 \* 6(\r\n[0-9A-Fa-f:]{9,13}  ([0-9A-Fa-f]{2}[\- ]){16}.{16}){2}\r\n\[more\]$")
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b" ")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"^([0-9A-Fa-f:]{9,13}  ([0-9A-Fa-f]{2}[\- ]){16}.{16}\r\n){3}\[more\]$")
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b" ")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"^([0-9A-Fa-f:]{9,13}  ([0-9A-Fa-f]{2}[\- ]){16}.{16}\r\n){1}~?[\-#]$")
            self.assertNotRegex(self.buffer,
                r"\[more\]")
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"r dsr := v0\rh dsr\r")
            self.do_read()
            self.assertNotIn("^ Error", self.buffer)
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "compare test"):
            self.do_write_long("""
                f 100 l 8 90
                e 200 90 90 90 90 0 1 2 3
                c 100 l 8 200
                """)
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertEqual(4, len(re.findall(
                r"[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2} +[0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n", self.buffer)))
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "t/tp/p test"):
            self.do_write_long("""
                a 100
                 mov cx, 8
                 loop 103
                 mov cx, 8
                 mov si, 100
                 rep lodsb
                 nop
                 int3
                 jmp 100
                 .
                t=100
                """)
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +loop ")
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"t\r")
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +loop ")
            self.assertEqual(1, len(re.findall(
                r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} ", self.buffer)))
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"tp\r")
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +loop ")
            self.assertEqual(1, len(re.findall(
                r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} ", self.buffer)))
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"p\rp\rp\r")
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +loop ")
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +mov +cx")
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +mov +si")
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +rep +lodsb")
            self.assertEqual(3, len(re.findall(
                r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} ", self.buffer)))
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"t\r")
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +rep +lodsb")
            self.assertEqual(1, len(re.findall(
                r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} ", self.buffer)))
            if self.debug:
                print(self.buffer)
            self.buffer = ""
            os.write(self.m, b"tp\r")
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"(?i)\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} +nop")
            self.assertEqual(1, len(re.findall(
                r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} ", self.buffer)))
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "ff longer test"):
            self.do_write_long("""
                f 200 l 10 0
                f 200 l 8  1 2 3 4 5 6 7 8 9
                e 300  1 2 3 4 5 6 7 8, 0 0 0 0, 0 0 0 0
                c 200 l 10  300
                """)
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertEqual(0, len(re.findall(
                r"[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2} +[0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n", self.buffer)))
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "ff range test"):
            self.do_write_long("""
                f 400 l 10 0
                f 208 l 8 90
                f 400 l 8  range 200 l 10
                c 300 l 10  400
                """)
            self.do_read(sleepduration = 0.1)
            self.assertNotIn("^ Error", self.buffer)
            self.assertEqual(0, len(re.findall(
                r"[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2} +[0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n", self.buffer)))
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "ss range test"):
            self.do_write_long("""
                s 400 l 10  range 300 l 10
                """)
            self.do_read(sleepduration = 0.2)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer, r"\r\n0*1 match(es)?\r\n")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "ss range fail test"):
            self.do_write_long("""
                s 400 l 10  range 300 l 11
                """)
            self.do_read(sleepduration = 0.2)
            self.assertIn("^ Error", self.buffer)
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        with self.subtestcontext(msg = "ss list fail test"):
            self.do_write_long("""
                s 400 l 10  0 1 2 3, 4 5 6 7, 8 9 A B, C D E F, 10
                """)
            self.do_read(sleepduration = 0.2)
            self.assertIn("^ Error", self.buffer)
        if self.debug:
            print(self.buffer)

    @unittest.expectedFailure
    def test_timeout(self):
        time.sleep(0.05)
        os.write(self.m, b"sleep #10 seconds\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")

    @unittest.skipIf(booting, "test is non-booting only")
    def test_int2D_unhook(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"r tryamisnum := 26\rr dco4 or= 8\rh dco4 and 8\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n0*8  decimal:")
        self.buffer = ""
        os.write(self.m, b"h amisnum\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n0*26  decimal:")
        self.buffer = ""
        os.write(self.m, b"r amisnum 0\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\nThis lDebug variable cannot be written to.")
        self.buffer = ""
        os.write(self.m, b"di 2D\rr dpr .\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        p = re.compile(r"\r\nDPR ([0-9A-Fa-f]{4})\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        debuggerentrysegment = m.group(1)
        self.assertTrue(debuggerentrysegment is not None)
        self.assertRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggerentrysegment)
        self.buffer = ""
        self.do_write_long("""
            a 100
             mov ax, 352D
             int 21
             mov ah, 25
             mov word [200], EA90
             mov word [202], bx
             mov word [204], es
             mov word [206], cx
             mov dx, 200
             int 21
             int3
             nop
             .
            r v0 := aao
            a
             push ds
             lds dx, [202]
             mov ax, 252D
             int 21
             pop ds
             int3
             nop
             .
            r cx 0
            g=100
            r dco4 and= ~ 8
            di 2D
            """, sleepduration=0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        p = re.compile(r"CS=([0-9A-Fa-f]{4})")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        debuggeecodesegment = m.group(1)
        self.assertTrue(debuggeecodesegment is not None)
        self.assertRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggeecodesegment)
        self.buffer = ""
        self.do_write_long("""
            g=v0
            r dco4 or= 8
            di 2D
            r dco4 and= ~8
            """)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggerentrysegment)
        self.buffer = ""
        os.write(self.m, b"di 2D\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggerentrysegment)
        self.assertNotRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggeecodesegment)
        self.buffer = ""
        self.do_write_long("""
            r dco4 or= 8
            r cx := #"KB"
            g=100
            r dco4 and= ~8
            di 2D
            h base=10 width=4 word [204]
            """, sleepduration=0.1)
        self.do_read(sleepduration = 0.5)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggerentrysegment)
        self.assertRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggeecodesegment)
        self.assertNotRegex(self.buffer,
            r"\r\n%s\r\n" % debuggerentrysegment)
        self.buffer = ""
        os.write(self.m, b"g=v0\rdi 2D\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggerentrysegment)
        self.assertNotRegex(self.buffer,
            r"\r\nint 2D %s:[0-9A-Fa-f]{4}( \(.*\))?\r\n" % debuggeecodesegment)

    def test_bb_gg(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        self.do_write_long("""
            a 100
             inc ax
             inc bx
             inc cx
             inc dx
             int3
             jmp 100
             .
            bp at 101 counter=4008 when= 1 == 1 id = inc bx
            bp at 102 when= 1 == 1 id = inc cx
            """)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"bl\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n"
            + r"BP 00 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0101"
            + r" \(CC\) Counter=4008, ID: inc bx\r\n"
            + r" WHEN 1 == 1\r\n"
            + r"BP 01 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0102"
            + r" \(CC\) Counter=8000, ID: inc cx\r\n"
            + r" WHEN 1 == 1\r\n"
            )
        self.buffer = ""
        os.write(self.m, b"g=100\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertIn("\r\nPassed permanent breakpoint 00,"
            + " counter=4007, ID: inc bx\r\n",
            self.buffer);
        self.assertIn("\r\nHit permanent breakpoint 01, ID: inc cx\r\n",
            self.buffer);
        self.assertRegex(self.buffer, r"(?s)\r\n[0-9A-Fa-f]{4}:0101 "
            + r".*\r\n[0-9A-Fa-f]{4}:0102 ")
        self.assertEqual(2, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.assertEqual(2, len(re.findall(
            r"permanent breakpoint", self.buffer)))
        self.assertEqual(0, len(re.findall(
            r"Unexpected breakpoint interrupt", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"g\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(1, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.assertEqual(0, len(re.findall(
            r"permanent breakpoint", self.buffer)))
        self.assertEqual(1, len(re.findall(
            r"Unexpected breakpoint interrupt", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"g 100\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(1, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.assertEqual(0, len(re.findall(
            r"permanent breakpoint", self.buffer)))
        self.assertEqual(0, len(re.findall(
            r"Unexpected breakpoint interrupt", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"g 100\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertIn("\r\nPassed permanent breakpoint 00,"
            + " counter=4006, ID: inc bx\r\n",
            self.buffer);
        self.assertIn("\r\nHit permanent breakpoint 01, ID: inc cx\r\n",
            self.buffer);
        self.assertRegex(self.buffer, r"(?s)\r\n[0-9A-Fa-f]{4}:0101 "
            + r".*\r\n[0-9A-Fa-f]{4}:0102 ")
        self.assertEqual(2, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.assertEqual(2, len(re.findall(
            r"permanent breakpoint", self.buffer)))
        self.assertEqual(0, len(re.findall(
            r"Unexpected breakpoint interrupt", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"ttest =100 ffff silent 2\r")
        time.sleep(0.20)
        os.write(self.m, b"\x03")
        self.do_read(sleepduration = 0.20)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"[^\-#]\^C\r\n")
        self.assertEqual(2, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"bc all\rbp at 101 when=0\rt=100\rbl\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(1, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.assertRegex(self.buffer,
            r"\r\n"
            + r"BP 00 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0101"
            + r" \(CC\) Counter=8000\r\n"
            + r" WHEN 0\r\n"
            )
        self.buffer = ""
        self.do_write_long(r"""
            re.append @r v0 -:= 1
            re.append @if (v0) then goto :eof
            re.append @r dco2 or= 8000   \; cancel tpg run
            re.list
            r v0 := 2
            t=100 4 silent 1
            """, sleepmodulo=2)
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(1, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"bl 0F\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n"
            + r"BP 0F Unused\r\n"
            )
        self.assertNotRegex(self.buffer, r" WHEN ")
        self.buffer = ""
        self.do_write_long("""
            bd all
            re.replace @R
            r ip 100
            t 10 while value byte [cs:ip] in from 40 to 4F
            """, sleepmodulo=2)
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(4, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.assertRegex(self.buffer,
            r'\r\nWhile condition ".*" no longer true.\r\n')
        self.buffer = ""
        self.do_write_long("""
            bw 0
            bp at 102
            bp at 103
            bl all
            """, sleepmodulo=2)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(3, len(re.findall(
            r"\r\nBP [0-9A-Fa-f]{2} [\-+] ", self.buffer)))
        self.assertRegex(self.buffer,
            r'\r\nBP 00 - .*\r\nBP 01 \+ .*\r\nBP 02 \+ ')
        self.buffer = ""
        os.write(self.m, b"bt all\rbl all\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(3, len(re.findall(
            r"\r\nBP [0-9A-Fa-f]{2} [\-+] ", self.buffer)))
        self.assertRegex(self.buffer,
            r'\r\nBP 00 \+ .*\r\nBP 01 - .*\r\nBP 02 - ')
        self.buffer = ""
        os.write(self.m, b"be 02\rbl all\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(3, len(re.findall(
            r"\r\nBP [0-9A-Fa-f]{2} [\-+] ", self.buffer)))
        self.assertRegex(self.buffer,
            r'\r\nBP 00 \+ .*\r\nBP 01 - .*\r\nBP 02 \+ ')
        self.buffer = ""
        os.write(self.m, b"bc all\rbl all\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(0, len(re.findall(
            r"\r\nBP [0-9A-Fa-f]{2} [\-+] ", self.buffer)))
        self.assertRegex(self.buffer,
            r'\r\nNo breakpoints set currently.\r\n')
        self.buffer = ""
        os.write(self.m, b"g=100 100 102\rg list\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(2, len(re.findall(
            r"\r\n *[0-9]+(th|st|nd|rd) +G breakpoint", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"g=100 again 103\rg list\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(3, len(re.findall(
            r"\r\n *[0-9]+(th|st|nd|rd) +G breakpoint", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"g again 104 remember\rg list\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(4, len(re.findall(
            r"\r\n *[0-9]+(th|st|nd|rd) +G breakpoint", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"g remember\rg list\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r'\r\nThe G breakpoint list is empty.\r\n')
        self.buffer = ""
        os.write(self.m, b"re.replace @R; @sleep 1 ticks\rt=100 4 silent 2\r")
        self.do_read(sleepduration = 0.01)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(0, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))
        self.buffer = ""
        self.do_read(sleepduration = 1.0 / 18.2 * 4.0)
        if self.debug:
            print(self.buffer)
        self.assertEqual(2, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9} [0-9A-Fa-f]{2,} ", self.buffer)))

    def test_bb_fill(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        self.do_write_long("""
            bp at 100 when ax id accumulator
            bp at 200 when = cx id = counter
            bp at 300 when =   dx   id = data
            bp at 400 counter=4008 when bx id base
            """, sleepduration = 0.1, sleepmodulo = 3)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"bl\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n"
            + r"BP 00 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0100"
            + r" \(CC\) Counter=8000, ID: accumulator\r\n"
            + r" WHEN ax\r\n"
            + r"BP 01 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0200"
            + r" \(CC\) Counter=8000, ID: counter\r\n"
            + r" WHEN cx\r\n"
            + r"BP 02 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0300"
            + r" \(CC\) Counter=8000, ID: data\r\n"
            + r" WHEN dx\r\n"
            + r"BP 03 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0400"
            + r" \(CC\) Counter=4008, ID: base\r\n"
            + r" WHEN bx\r\n"
            + r"~?-$"
            )
        self.buffer = ""
        os.write(self.m, b"bc 2\rbl\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n"
            + r"BP 00 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0100"
            + r" \(CC\) Counter=8000, ID: accumulator\r\n"
            + r" WHEN ax\r\n"
            + r"BP 01 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0200"
            + r" \(CC\) Counter=8000, ID: counter\r\n"
            + r" WHEN cx\r\n"
            + r"BP 03 \+ Lin=[_0-9A-Fa-f]+  [0-9A-Fa-f]{4}:0400"
            + r" \(CC\) Counter=4008, ID: base\r\n"
            + r" WHEN bx\r\n"
            + r"~?-$"
            )
        self.buffer = ""
        os.write(self.m, b"bc all\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        found = 0
        for x in range(0, 256 + 4):
            self.buffer = ""
            os.write(self.m, ("bp at %04X\r" % x).encode())
            self.do_read()
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            if "\r\nNo unused breakpoint left!\r\n" in self.buffer:
                found = 1
                amount = x
                break
        self.assertTrue(found)
        self.buffer = ""
        os.write(self.m, b"bc all\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        found = 0
        for x in range(0, amount - 1):
            self.buffer = ""
            os.write(self.m, ("bp at %04X id = %s\r" % (x, 20 * "ID")).encode())
            self.do_read(sleepduration = 0.1)
            if self.debug:
                print(self.buffer)
            length = len("-bp at 0000 id = ")
            pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
            if re.search(pattern, self.buffer):
                found = 1
                break
            self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, pattern)
        self.assertTrue(found)
        self.buffer = ""
        os.write(self.m, b"bc all\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        found = 0
        for x in range(0, amount - 1):
            self.buffer = ""
            os.write(self.m, ("bp at %04X when = %s\r"
                % (x, (20 * "1 == ") + "1")).encode())
            self.do_read(sleepduration = 0.3)
            if self.debug:
                print(self.buffer)
            length = len("-bp at 0000 when = ")
            pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
            if re.search(pattern, self.buffer):
                found = 1
                break
            self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, pattern)
        self.assertTrue(found)
        self.buffer = ""
        os.write(self.m, b"bc all\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = 1234_5678\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        length = len("-bp at 0000 number = 1234_5678")
        pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
        self.assertRegex(self.buffer, pattern)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = -1\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = FFFF_FFFF\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        length = len("-bp at 0000 number = FFFF_FFFF")
        pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
        self.assertRegex(self.buffer, pattern)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = sdword 8000\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        length = len("-bp at 0000 number = sdword 8000")
        pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
        self.assertRegex(self.buffer, pattern)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = sdword FFFF\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        length = len("-bp at 0000 number = sdword FFFF")
        pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
        self.assertRegex(self.buffer, pattern)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = 1_0000\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        length = len("-bp at 0000 number = 1_0000")
        pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
        self.assertRegex(self.buffer, pattern)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = -8001\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        length = len("-bp at 0000 number = -8001")
        pattern = re.compile(r"\r\n {%u,%u}\^ Error" % (length - 2, length + 2))
        self.assertRegex(self.buffer, pattern)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = -8000\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = -7FFF\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = sdword 7FFF\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"bp at 0000 number = FFFF\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)

    def test_access_var(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"r readadr0 .\rr readlen0 .\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        if "^ Error" in self.buffer:
            raise unittest.SkipTest("build does not include access variables")
        sleepduration = 0.05
        sleepmodulo = 4
        self.buffer = ""
        self.do_write_long("""
            a 100
             xor ax, ax
             mov es, ax
             mov bx, 46C
             int3
             mov dx, word [es:bx]
             int3
             add word [es:bx], 0
             int3
             mov word [200], dx
             int3
             push word [200]
             int3
             pop word [200]
             int3
             jmp 100
             .
            r ip 100
            """, sleepduration = sleepduration, sleepmodulo = sleepmodulo)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        self.do_write_long("""
            g   ; up to mov dx
            r readadr0 .
            r readlen0 .
            r readadr1 .
            r readlen1 .
            r writadr0 .
            r writlen0 .
            """, sleepduration = sleepduration, sleepmodulo = sleepmodulo)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"(?i) mov +dx")
        self.assertRegex(self.buffer, r"\r\nREADADR0 0*46C\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nREADADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR0 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN0 0*0\r\n")
        self.buffer = ""
        self.do_write_long("""
            g   ; up to add
            r readadr0 .
            r readlen0 .
            r readadr1 .
            r readlen1 .
            r writadr0 .
            r writlen0 .
            r writadr1 .
            r writlen1 .
            """, sleepduration = sleepduration, sleepmodulo = sleepmodulo)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"(?i) add +word")
        self.assertRegex(self.buffer, r"\r\nREADADR0 0*46C\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nREADADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR0 0*46C\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN1 0*0\r\n")
        self.buffer = ""
        self.do_write_long("""
            g   ; up to mov
            r readadr0 .
            r readlen0 .
            r writadr0 .
            r writlen0 .
            r writadr1 .
            r writlen1 .
            h linear ds:200
            """, sleepduration = sleepduration, sleepmodulo = sleepmodulo)
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"(?i) mov.*\[")
        p = re.compile(r"\r\n0*([1-9A-Fa-f][0-9A-Fa-f]*)  decimal:.*\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        debuggeedatalinear = m.group(1)
        self.assertTrue(debuggeedatalinear is not None)
        self.assertRegex(self.buffer, r"\r\nREADADR0 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN0 0*0\r\n")
        self.assertRegex(self.buffer,
            r"\r\nWRITADR0 0*%s\r\n" % debuggeedatalinear)
        self.assertRegex(self.buffer, r"\r\nWRITLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN1 0*0\r\n")
        self.buffer = ""
        self.do_write_long("""
            g   ; up to push
            r readadr0 .
            r readlen0 .
            r readadr1 .
            r readlen1 .
            r writadr0 .
            r writlen0 .
            r writadr1 .
            r writlen1 .
            h linear ss:sp - 2
            """, sleepduration = sleepduration, sleepmodulo = sleepmodulo)
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"(?i) push.*\[")
        p = re.compile(r"\r\n0*([1-9A-Fa-f][0-9A-Fa-f]*)  decimal:.*\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        debuggeestacklinear = m.group(1)
        self.assertTrue(debuggeestacklinear is not None)
        self.assertRegex(self.buffer,
            r"\r\nREADADR0 0*%s\r\n" % debuggeedatalinear)
        self.assertRegex(self.buffer, r"\r\nREADLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nREADADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN1 0*0\r\n")
        self.assertRegex(self.buffer,
            r"\r\nWRITADR0 0*%s\r\n" % debuggeestacklinear)
        self.assertRegex(self.buffer, r"\r\nWRITLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN1 0*0\r\n")
        self.buffer = ""
        self.do_write_long("""
            g   ; up to pop
            r readadr0 .
            r readlen0 .
            r readadr1 .
            r readlen1 .
            r writadr0 .
            r writlen0 .
            r writadr1 .
            r writlen1 .
            """, sleepduration = sleepduration, sleepmodulo = sleepmodulo)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"(?i) pop.*\[")
        self.assertTrue(debuggeestacklinear is not None)
        self.assertRegex(self.buffer,
            r"\r\nREADADR0 0*%s\r\n" % debuggeestacklinear)
        self.assertRegex(self.buffer, r"\r\nREADLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nREADADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN1 0*0\r\n")
        self.assertRegex(self.buffer,
            r"\r\nWRITADR0 0*%s\r\n" % debuggeedatalinear)
        self.assertRegex(self.buffer, r"\r\nWRITLEN0 0*2\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN1 0*0\r\n")
        self.buffer = ""
        self.do_write_long("""
            g   ; up to jmp
            r readadr0 .
            r readlen0 .
            r readadr1 .
            r readlen1 .
            r writadr0 .
            r writlen0 .
            r writadr1 .
            r writlen1 .
            """, sleepduration = sleepduration, sleepmodulo = sleepmodulo)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"(?i) jmp ")
        self.assertRegex(self.buffer, r"\r\nREADADR0 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN0 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nREADLEN1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR0 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN0 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITADR1 0*0\r\n")
        self.assertRegex(self.buffer, r"\r\nWRITLEN1 0*0\r\n")

    @unittest.skipIf(booting, "test is non-booting only")
    @unittest.skipIf("x" not in build_name, "build does not include dpmi")
    @unittest.skipIf(not dpmiavailable, "dpmi not available")
    def test_dpmimini(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"r dps .\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        if "^ Error" in self.buffer:
            raise unittest.SkipTest("build does not include dpmi")
        sleepduration = 0.05
        sleepmodulo = 4
        self.buffer = ""
        if self.dpmiexe is not None:
            self.do_write_long("""
                n %s
                l
                g
                """ % self.dpmiexe)
            self.do_read()
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotIn("File not found", self.buffer)
            self.assertNotIn("Path not found", self.buffer)
            self.assertNotIn("Access denied", self.buffer)
            self.assertNotIn("Insufficient memory", self.buffer)
            self.assertNotRegex(self.buffer, r"Error .* opening file")
            self.buffer = ""
        self.do_write_long("""
            n %sdpmimini.com
            l
            s 100 l bxcx "Protected mode breakpoint at "
            """ % self.dpmidir, sleepmodulo = 2, sleepduration = 0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("File not found", self.buffer)
        self.assertNotIn("Path not found", self.buffer)
        self.assertNotIn("Access denied", self.buffer)
        self.assertNotIn("Insufficient memory", self.buffer)
        self.assertNotRegex(self.buffer, r"Error .* opening file")
        self.assertIn("DPMI entry hooked", self.buffer)
        p = re.compile(r"\r\n[0-9A-Fa-f:]{9}  "
            + r"([0-9A-Fa-f]{2}[- ]){16}([0-9A-Fa-f]{4})h"
            + r".*\r\n0001 matches\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        breakpoint = m.group(2)
        self.assertTrue(breakpoint is not None)
        self.buffer = ""
        self.do_write_long("""
            g
            g %s
            """ % breakpoint)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"[\r\n]~?#$")
        self.buffer = ""
        os.write(self.m, b"rvm\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        p = re.compile(r"\r\nEntry segment=[0-9A-Fa-f]{4} "
            + r"selector=([0-9A-Fa-f]{4})\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        debuggerentryselector = m.group(1)
        self.assertTrue(debuggerentryselector is not None)
        p = re.compile(r"\r\nData segment=[0-9A-Fa-f]{4} "
            + r"selector=([0-9A-Fa-f]{4})\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        debuggerdataselector = m.group(1)
        self.assertTrue(debuggerdataselector is not None)
        self.buffer = ""
        if "ddebug" not in build_name:
            # Note: Can fail in dosemu2 prior to our hg b2212e4d65fb.
            #  Does not appear to fail in qemu.
            #
            # Skipped for ddebugx because the GPF would go to the
            #  default system handler instead of our debugger's,
            self.buffer = ""
            os.write(self.m, ("dl %s\r" % debuggerdataselector).encode())
            self.do_read()
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer,
                r"limit=0+[Ff]{4}\b")
            self.buffer = ""
            os.write(self.m, b"r dword [cs:A0] := FFFF_FFFF\r")
            self.do_read()
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotIn("General protection fault", self.buffer)
        os.write(self.m, b"di 0\rdi 1\rdi 3\rdi 6\rdi 0C\rdi 0D\rdi 0E\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\nint [0-9A-Fa-f]{2}( +[0-9A-Fa-f:]{9,13}){2}\r\n")
        if "ddebug" not in build_name:
            self.assertEqual(7, len(re.findall(
                r"\r\nint [0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13} +%s:[0-9A-Fa-f]{4,8}\r\n" % debuggerentryselector, self.buffer)))
        else:
            self.assertEqual(7, len(re.findall(
                r"\r\nint [0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f:]{9,13}\r\n", self.buffer)))
        self.assertNotRegex(self.buffer,
            r"\r\nint [0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n")
        self.buffer = ""
        os.write(self.m, b"di 21\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nint [0-9A-Fa-f]{2}( +[0-9A-Fa-f:]{9,13}){2}\r\n")
        self.assertRegex(self.buffer,
            r"\r\nint [0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n")
        if "ddebug" not in build_name:
            self.assertEqual(1, len(re.findall(
                r"\r\nint [0-9A-Fa-f]{2} +%s:[0-9A-Fa-f]{4,8}\r\n" % debuggerentryselector, self.buffer)))
        else:
            self.assertEqual(1, len(re.findall(
                r"\r\nint [0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n", self.buffer)))
        self.buffer = ""
        miscpmtests = [
            ("psp var", "h segment ppi == dpr || dif & 4000", "\r\n0*1  decimal:"),
            ("psp var", "h ppr == dpr || dif & 4000", "\r\n0*1  decimal:"),
            ("psp var", "h psp != 0 || dif & 4000", "\r\n0*1  decimal:"),
            ("psp var", "h (linear psps:0) == (psp << 4) || dif & 4000", "\r\n0*1  decimal:", 1, 1),
            ]
        for tuple in miscpmtests:
            msg = tuple[0]
            cmd = tuple[1]
            match = tuple[2]
            if len(tuple) >= 4:
                success = tuple[3]
            else:
                success = 1
            if len(tuple) >= 5:
                delay = tuple[4]
            else:
                delay = 0
            with self.subtestcontext(msg = msg + ": " + cmd.replace("\r","##")):
                if self.debug:
                    print(self.buffer)
                    print("msg=%s cmd=%s match=%s success=%d"
                        % (msg, cmd.replace("\r","##"),
                            match.replace("\r","##").replace("\n","%%"),
                            success))
                self.buffer = ""
                actuallysucceeded = 0
                try:
                    os.write(self.m, ("%s\r" % cmd).encode())
                    if delay:
                        time.sleep(0.05)
                    self.do_read()
                    self.assertNotRegex(self.buffer,
                        r"\^ Error",
                        "error in command")
                    self.assertRegex(self.buffer,
                        match,
                        "match mismatch")
                    actuallysucceeded = 1
                except AssertionError as e:
                    if success:
                        raise e
                    else:
                        if self.debug:
                            print("passing on upon AssertionError(\""
                                + str(e) + "\")")
                        pass
                if actuallysucceeded and not success:
                    raise AssertionError("unexpected success")
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"g\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertIn("Program terminated normally", self.buffer)
        self.assertRegex(self.buffer, r"[\r\n]~?-$")

    @unittest.skipIf(booting, "test is non-booting only")
    @unittest.skipIf("x" not in build_name, "build does not include dpmi")
    @unittest.skipIf(not dpmiavailable, "dpmi not available")
    def test_dpmioffs(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"r dps .\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        if "^ Error" in self.buffer:
            raise unittest.SkipTest("build does not include dpmi")
        sleepduration = 0.05
        sleepmodulo = 4
        self.buffer = ""
        if self.dpmiexe is not None:
            self.do_write_long("""
                n %s
                l
                g
                """ % self.dpmiexe)
            self.do_read()
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotIn("File not found", self.buffer)
            self.assertNotIn("Path not found", self.buffer)
            self.assertNotIn("Access denied", self.buffer)
            self.assertNotIn("Insufficient memory", self.buffer)
            self.assertNotRegex(self.buffer, r"Error .* opening file")
            self.buffer = ""
        self.do_write_long("""
            n %sdpmioffs.com
            l
            s 100 l bxcx "32-bit es / 16-bit ds breakpoint at "
            """ % self.dpmidir, sleepmodulo = 2, sleepduration = 0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("File not found", self.buffer)
        self.assertNotIn("Path not found", self.buffer)
        self.assertNotIn("Access denied", self.buffer)
        self.assertNotIn("Insufficient memory", self.buffer)
        self.assertNotRegex(self.buffer, r"Error .* opening file")
        self.assertIn("DPMI entry hooked", self.buffer)
        p = re.compile(r"\r\n[0-9A-Fa-f:]{9}  "
            + r"([0-9A-Fa-f]{2}[- ]){16}([0-9A-Fa-f]{4})h"
            + r".*\r\n0001 matches\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        ds16breakpoint = m.group(2)
        self.assertTrue(ds16breakpoint is not None)
        self.buffer = ""
        self.do_write_long("""
            s 100 l bxcx "32-bit ds / 16-bit es breakpoint at "
            """, sleepmodulo = 2, sleepduration = 0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        p = re.compile(r"\r\n[0-9A-Fa-f:]{9}  "
            + r"([0-9A-Fa-f]{2}[- ]){16}([0-9A-Fa-f]{4})h"
            + r".*\r\n0001 matches\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        ds32breakpoint = m.group(2)
        self.assertTrue(ds32breakpoint is not None)
        self.buffer = ""
        self.do_write_long("""
            g
            g %s
            """ % ds16breakpoint)
        self.do_read(sleepduration = 0.5)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Program terminated normally", self.buffer)
        self.assertRegex(self.buffer, r"[\r\n]~?#$")
        self.buffer = ""
        os.write(self.m, b"c es:edi l 1 es:edi + 1\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f]{4}:000[0-9A-Fa-f]{4}0 +00 +"
            + r"01 +[0-9A-Fa-f]{4}:000[0-9A-Fa-f]{4}1\r\n")
        self.buffer = ""
        os.write(self.m, b"c ds:0 l 1 ds:0 + 1\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f]{4}:0000 +00 +"
            + r"01 +[0-9A-Fa-f]{4}:0001\r\n")
        self.buffer = ""
        self.do_write_long("""
            g %s
            """ % ds32breakpoint)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Program terminated normally", self.buffer)
        self.buffer = ""
        os.write(self.m, b"c ds:edi l 1 ds:edi + 1\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f]{4}:000[0-9A-Fa-f]{4}0 +00 +"
            + r"01 +[0-9A-Fa-f]{4}:000[0-9A-Fa-f]{4}1\r\n")
        self.buffer = ""
        os.write(self.m, b"c es:0 l 1 es:0 + 1\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f]{4}:0000 +00 +"
            + r"01 +[0-9A-Fa-f]{4}:0001\r\n")
        self.buffer = ""
        os.write(self.m, b"c es:(dword [ds:1_0000] & 0) l 1 es:0 + 1\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f]{4}:0000 +00 +"
            + r"01 +[0-9A-Fa-f]{4}:0001\r\n")
        self.buffer = ""
        os.write(self.m, b"c es:(linear ds:1_0000, & 0) l 1 es:0 + 1\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f]{4}:0000 +00 +"
            + r"01 +[0-9A-Fa-f]{4}:0001\r\n")
        self.buffer = ""
        os.write(self.m, b"g\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertIn("Program terminated normally", self.buffer)
        self.assertRegex(self.buffer, r"[\r\n]~?-$")

    @unittest.skipIf(booting, "test is non-booting only")
    @unittest.skipIf("x" not in build_name, "build does not include dpmi")
    @unittest.skipIf(not dpmiavailable, "dpmi not available")
    def test_dpmialoc(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"r dps .\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        if "^ Error" in self.buffer:
            raise unittest.SkipTest("build does not include dpmi")
        sleepduration = 0.05
        sleepmodulo = 4
        self.buffer = ""
        if self.dpmiexe is not None:
            self.do_write_long("""
                n %s
                l
                g
                """ % self.dpmiexe)
            self.do_read()
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotIn("File not found", self.buffer)
            self.assertNotIn("Path not found", self.buffer)
            self.assertNotIn("Access denied", self.buffer)
            self.assertNotIn("Insufficient memory", self.buffer)
            self.assertNotRegex(self.buffer, r"Error .* opening file")
            self.buffer = ""
        self.do_write_long("""
            n %sdpmialoc.com
            l
            """ % self.dpmidir, sleepmodulo = 2, sleepduration = 0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("File not found", self.buffer)
        self.assertNotIn("Path not found", self.buffer)
        self.assertNotIn("Access denied", self.buffer)
        self.assertNotIn("Insufficient memory", self.buffer)
        self.assertNotRegex(self.buffer, r"Error .* opening file")
        self.assertIn("DPMI entry hooked", self.buffer)
        self.buffer = ""
        os.write(self.m, b"r cs .\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        p = re.compile(r"\r\nCS\s+([0-9A-Fa-f]{4})\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        cs = m.group(1)
        self.assertTrue(cs is not None)
        self.buffer = ""
        self.do_write_long("""
            g
            g
            """)
        self.do_read(sleepduration = 0.5)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Program terminated normally", self.buffer)
        self.assertRegex(self.buffer, r"[\r\n]~?#$")
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f:]{13}\s+[0-9A-Fa-f]+\s+push\s+ebx\r\n")
        self.buffer = ""
        os.write(self.m, b"bp at bx:0\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"g\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\nHit permanent breakpoint 00\r\n")
        self.assertRegex(self.buffer, r"[\r\n]~?#$")
        self.buffer = ""
        os.write(self.m, b"g\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"[\r\n]~?-$")
        self.assertRegex(self.buffer,
            r"\r\n%s:[0-9A-Fa-f]{4}\s+" % cs)
        self.buffer = ""
        self.do_write_long("""
            a cs:A0
             int 12
             nop
             nop
             int3
             jmp (ip)
             .
            r ip A0
            r
            """)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"t\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Internal error in disassembler!", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n%s:00A2\s+90\s+nop\r\n" % cs)
        self.assertNotRegex(self.buffer,
            r"\r\n%s:[0-9A-Fa-f]{4}\s+CD12\s+int\s+12\r\n" % cs)

    @unittest.skipIf(booting, "test is non-booting only")
    def test_missing_executable(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        self.do_write_long("""
            n nonexist.$$$
            l
            """, sleepmodulo = 2, sleepduration = 0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertIn("File not found", self.buffer)
        self.assertNotIn("Path not found", self.buffer)
        self.assertNotIn("Access denied", self.buffer)
        self.assertNotIn("Insufficient memory", self.buffer)
        self.assertNotRegex(self.buffer, r"Error .* opening file")
        self.buffer = ""
        self.do_write_long("""
            n nonexist.$$$\\nonexist.$$$
            l
            """, sleepmodulo = 2, sleepduration = 0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("File not found", self.buffer)
        self.assertIn("Path not found", self.buffer)
        self.assertNotIn("Access denied", self.buffer)
        self.assertNotIn("Insufficient memory", self.buffer)
        self.assertNotRegex(self.buffer, r"Error .* opening file")

    @unittest.skipIf(booting, "test is non-booting only")
    def test_error_executable(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        self.do_write_long("""
            n scripts\\toolarge.exe
            l
            """, sleepmodulo = 2, sleepduration = 0.1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("File not found", self.buffer)
        self.assertNotIn("Path not found", self.buffer)
        self.assertNotIn("Access denied", self.buffer)
        self.assertIn("Insufficient memory", self.buffer)
        self.assertNotRegex(self.buffer, r"Error .* opening file")
        if machine == 'qemu':
            # dosemu's redirector apparently gives "File not found" for this
            self.buffer = ""
            self.do_write_long("""
                n scripts
                l
                """, sleepmodulo = 2, sleepduration = 0.1)
            self.do_read(sleepduration = 0.1)
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            self.assertNotIn("File not found", self.buffer)
            self.assertNotIn("Path not found", self.buffer)
            self.assertIn("Access denied", self.buffer)
            self.assertNotIn("Insufficient memory", self.buffer)
            self.assertNotRegex(self.buffer, r"Error .* opening file")

    @unittest.skipIf(not booting, "test is booting only")
    def test_load_boot(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m,
            ("boot protocol=ldos ldp/l%s.com\rr\r" % build_name).encode())
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer, r"\r\n0200:0400 ")
        self.buffer = ""
        os.write(self.m,
            ("boot protocol=ldos checkvalue=FFFF ldp/l%s.com\r" % build_name).encode())
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertIn("Boot failure: Check mismatch", self.buffer)
        self.buffer = ""
        os.write(self.m,
            ("boot protocol=rxdos.3\r").encode())
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertIn("Boot failure: File not found", self.buffer)
        self.buffer = ""
        os.write(self.m,
            ("boot protocol=freedos ldp/l%s.com\rr\r" % build_name).encode())
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer, r"\r\n0060:0000 ")
        self.buffer = ""
        os.write(self.m, b"boot dir ldp/\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer, r"\r\nLD?DEBUGX? +COM +[\-AHSR]{4} +[0-9]+")
        self.assertRegex(self.buffer, r"\r\nLD?DEBUG +SLD +[\-AHSR]{4} +[0-9]+")
        self.buffer = ""
        os.write(self.m, b"boot read ldp 1000\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.buffer = ""
        os.write(self.m, b"s 1000:0 l 200 'FAT12   '\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer, r"\r\n0001 match(es)?\r\n")
        self.buffer = ""
        os.write(self.m, b"d 1000:1F0 l 10\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n[0-9A-Fa-f:]{9,13}  ([0-9A-Fa-f]{2}[\- ]){14}55 AA .{16}\r\n")
        self.buffer = ""
        os.write(self.m, b"r byte [1000:400] = 0\r"
            + b"boot read ldp 1020\rc 1000:0 l 200 1000:200\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2} +[0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n")
        self.buffer = ""
        os.write(self.m, b"boot read ldp 1020 0\rc 1000:0 l 200 1000:200\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2} +[0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n")
        self.buffer = ""
        os.write(self.m, b"boot read ldp 1020 0 1\rc 1000:0 l 200 1000:200\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2} +[0-9A-Fa-f]{2} +[0-9A-Fa-f:]{9,13}\r\n")
        self.buffer = ""
        os.write(self.m, b"h byte [1000:400]\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\n0+ +decimal")

    def test_yy(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"y scripts/simple.sld\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; simple script content\r\n")
        self.buffer = ""
        os.write(self.m, b"y scripts/levels20.sld\r")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertNotRegex(self.buffer, r"\r\nV0+=0+ ")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 20 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 19 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 18 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 18 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 19 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 20 end\r\n")
        self.buffer = ""
        os.write(self.m, b"y scripts/levels02.sld\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 start\r\n")
        self.assertRegex(self.buffer, r"\r\nV0+=0+ ")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 end\r\n")
        self.buffer = ""
        os.write(self.m, b"r v0 := 0\rr v1 := 0\rr v2 := 0\ry scripts/sleep02.sld\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 end\r\n")
        self.buffer = ""
        os.write(self.m, b"r v0 := #18\rr iol := 0\ry scripts/sleep02.sld\r")
        time.sleep(0.20)
        os.write(self.m, b"\x03")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 end\r\n")
        self.buffer = ""
        os.write(self.m, b"r v0 := #18\rr iol := 1\ry scripts/sleep02.sld\r")
        time.sleep(0.20)
        os.write(self.m, b"\x03")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 start\r\n")
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]; level 00 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 end\r\n")
        self.buffer = ""
        os.write(self.m, b"r v0 := #18\rr v1 := v0\rr iol := 1\ry scripts/sleep02.sld\r")
        time.sleep(0.20)
        os.write(self.m, b"\x03")
        time.sleep(0.20)
        os.write(self.m, b"\x03")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 start\r\n")
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]; level 00 end\r\n")
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]; level 01 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 end\r\n")
        self.buffer = ""
        os.write(self.m, b"r iol := 2\ry scripts/sleep02.sld\r")
        time.sleep(0.20)
        os.write(self.m, b"\x03")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 01 start\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 00 start\r\n")
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]; level 00 end\r\n")
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]; level 01 end\r\n")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]; level 02 end\r\n")
        self.buffer = ""
        os.write(self.m, b"y scripts/self.sld :entry\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer,
            r"\r\n~?[\-#]; self entry"
            + r"\r\n~?[\-#]y :another"
            + r"\r\n~?[\-#]; another entry"
            + r"\r\n~?[\-#]y :yetanother"
            + r"\r\n~?[\-#]; yet another"
            + r"\r\n~?[\-#]goto :eof"
            + r"\r\n~?[\-#]; another exit"
            + r"\r\n~?[\-#]goto :eof"
            + r"\r\n~?[\-#]; self exit"
            )
        self.buffer = ""
        os.write(self.m, b"y scripts/hide.sld\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("Boot failure", self.buffer)
        self.assertNotRegex(self.buffer,
            r"\r\nY command has too many open files.\r\n"
            + r"|\r\nY command failed to open file.\r\n")
        self.assertRegex(self.buffer,
            r"\r\n~?[\-#]; visible 1"
            + r"\r\n~?[\-#]r ysf or= 4000"
            + r"\r\nAX [0-9A-Fa-f]{4}"
            + r"\r\n~?[\-#]; visible 2"
            )
        self.assertNotRegex(self.buffer, "invisible")

    def test_double_ctrlc(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        self.do_write_long("""
            f cs:100 l 100 90
            t=100  ; hook int2F in DebugX
            a 100
             mov cx, 000A
             mov bx, 40
             mov es, bx
             .
            r v0 := aao
            a
             mov dx, word [es:6C]
             .
            r v1 := aao
            a
             cmp dx, word [es:6C]
             jne 140
             mov ax, 1680
             .
            r v2 := aao
            a
             int 2F
             test al, al
             jz (v1)
             sti
             hlt
             jmp (v1)
             ;
             org 140
             loop (v0)
             int3
             .
            r dco4 or= 4  ; hook interrupt 8
            r cs .
            """, sleepmodulo = 4)
        self.do_write_long("""
            if (ri2Fo == ffff || ri2Fs == 0) then r word [cs:v2] := 9090
            h word [cs:v2]
            """, sleepmodulo = 1)
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        p = re.compile(r"(?i)\r\nCS +([0-9A-Fa-f]{4})\r\n")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        codesegment = m.group(1)
        self.assertTrue(codesegment is not None)
        self.buffer = ""
        os.write(self.m, b"g=100\r")
        self.do_read(sleepduration = 0.1)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, "g=100\r\n$")
        self.buffer = ""
        os.write(self.m, b"\x03\x03")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, "^Detected double Control-C via serial\r\n")
        p = re.compile(r"\r\n([0-9A-Fa-f]{4}):([0-9A-Fa-f]{4}) +[0-9A-Fa-f]{2,} ")
        self.assertRegex(self.buffer, p)
        m = p.search(self.buffer)
        self.assertTrue(m is not None)
        breaksegment = m.group(1)
        breakoffset = m.group(2)
        self.assertTrue(breaksegment is not None)
        self.assertTrue(breakoffset is not None)
        breakoffset = int("0x" + breakoffset, base=16)
        if breaksegment == codesegment:
            self.assertTrue(breakoffset >= 0x100 and breakoffset < 0x200)
        else:
            self.buffer = ""
            self.do_write_long("""
                r vf := 0
                re.replace @R
                re.append @r dco2 or= vf
                re.append @if (value byte [cs:eip] in CF, CA) then r vf := 8000
                tp ffff silent 1
                """, sleepmodulo = 2)
            self.do_read(sleepduration = 0.1)
            if self.debug:
                print(self.buffer)
            self.assertNotIn("^ Error", self.buffer)
            self.assertRegex(self.buffer, p)
            m = p.search(self.buffer)
            self.assertTrue(m is not None)
            breaksegment = m.group(1)
            breakoffset = m.group(2)
            self.assertTrue(breaksegment is not None)
            self.assertTrue(breakoffset is not None)
            breakoffset = int("0x" + breakoffset, base=16)
            self.assertTrue(breaksegment == codesegment)
            self.assertTrue(breakoffset >= 0x100 and breakoffset < 0x200)

    def test_eee_interactive(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"f 100 l 100 90\re 100\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r"\r\n[0-9A-Fa-f:]{9,13} +90\.$")
        self.buffer = ""
        os.write(self.m, b"26")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        os.write(self.m, b"\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        os.write(self.m, b"e 100\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r"\r\n[0-9A-Fa-f:]{9,13} +26\.$")
        self.buffer = ""
        os.write(self.m, b" ")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r" +90\.$")
        self.buffer = ""
        os.write(self.m, b".")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        os.write(self.m, b"e 100\r  ")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r"\r\n[0-9A-Fa-f:]{9,13} +26\. +90\. +90\.$")
        self.buffer = ""
        os.write(self.m, b"38")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        os.write(self.m, b"-")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r"\r\n[0-9A-Fa-f:]{9,13} +90\.$")
        self.buffer = ""
        os.write(self.m, b" ")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r" +38\.$")
        self.buffer = ""
        os.write(self.m, b"\n")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        os.write(self.m, b"e 107\r ")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r"(\r\n[0-9A-Fa-f:]{9,13} +90\.){2}$")
        self.buffer = ""
        os.write(self.m, b"\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        os.write(self.m, b"e 100\r7\b")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotRegex(self.buffer, r"\r\n~?[\-#]$")
        self.assertRegex(self.buffer, r"\r\n[0-9A-Fa-f:]{9,13} +26\.7\x08 \x08$")
        self.buffer = ""
        os.write(self.m, b".")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        os.write(self.m, b"h byte [100]\r")
        self.do_read(sleepduration = 0.1)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertRegex(self.buffer, r"\r\n0*26 ")

    def test_rc(self):
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.buffer = ""
        os.write(self.m, b"rc.replace r\rrc\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(1, len(re.findall(
            r"\r\n~?[&%]", self.buffer)))
        self.assertEqual(1, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} ", self.buffer)))
        self.buffer = ""
        os.write(self.m, b"rc.append r;r\rrc\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertEqual(3, len(re.findall(
            r"\r\n~?[&%]", self.buffer)))
        self.assertEqual(3, len(re.findall(
            r"\r\n[0-9A-Fa-f:]{9,13} +[0-9A-Fa-f]{2,} ", self.buffer)))
        self.buffer = ""
        self.do_write_long("""
            rc.replace @:loop; @r ax +:= 1; @r ax .; @if (ax <= 4) then goto :loop
            r ax := 0
            """)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"rc\r")
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn("if", self.buffer)
        self.assertNotIn("goto", self.buffer)
        self.assertNotIn("loop", self.buffer)
        self.assertEqual(0, len(re.findall(
            r"\r\n~?[&%]", self.buffer)))
        self.assertEqual(5, len(re.findall(
            r"\r\nAX +[0-9A-Fa-f]{4}", self.buffer)))
        self.assertNotRegex(self.buffer, r"\r\n~?[&%]$")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        self.do_write_long("""
            rc.replace re
            re.replace @:loop; @r ax +:= 1; @if (ax <= 40) then goto :loop
            r ax := 0
            r rclimit := 10
            """)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"rc\r")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn(" processing reached ", self.buffer)
        self.assertNotIn("if", self.buffer)
        self.assertNotIn("goto", self.buffer)
        self.assertNotIn("loop", self.buffer)
        self.assertEqual(1, len(re.findall(
            r"\r\n~?[&%]", self.buffer)))
        self.assertNotRegex(self.buffer, r"\r\n~?[&%]$")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")
        self.buffer = ""
        self.do_write_long("""
            rc.replace y scripts/loop.sld
            r ax := 0
            r rclimit := 10
            """)
        self.do_read()
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.buffer = ""
        os.write(self.m, b"rc\r")
        self.do_read(sleepduration = 0.2)
        if self.debug:
            print(self.buffer)
        self.assertNotIn("^ Error", self.buffer)
        self.assertNotIn(" processing reached ", self.buffer)
        self.assertNotIn("if", self.buffer)
        self.assertNotIn("goto", self.buffer)
        self.assertNotIn(":loop", self.buffer)
        self.assertEqual(1, len(re.findall(
            r"\r\n~?[&%]", self.buffer)))
        self.assertNotRegex(self.buffer, r"\r\n~?[&%]$")
        self.assertRegex(self.buffer, r"\r\n~?[\-#]$")

if __name__ == '__main__':
    unittest.main()
