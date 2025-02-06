#!/usr/bin/python3
import subprocess
import unittest
import typing


from textwrap import dedent
from unittest import TestCase


OCAWK = "./_build/install/default/bin/ocawk"
ONELINER = "./test/test_data/oneliner.txt"
TSV_FILE = "./test/test_data/example.tsv"
TEN = "./test/test_data/ten.txt"

def run(code, files, timeout=1):
    if isinstance(files, str):
        return subprocess.run([OCAWK, code, files], encoding="UTF-8", timeout=timeout, capture_output=True)
    if isinstance(files, typing.Sequence):
        return subprocess.run([OCAWK, code, *files], encoding="UTF-8", timeout=timeout, capture_output=True)
    raise


class TestVariableAssignment(TestCase):
    def test_custom_vars(self):
        res = run(r"BEGIN {foo = 17; print foo}", TSV_FILE)
        self.assertEqual(res.stdout, "17\n")
        
        res2 = run(r"{x = 10 + 17 * 3; print x}", ONELINER)
        self.assertEqual(res2.stdout, "61\n")
        
    def test_field_assignment(self):
        res = run(r"""{$7 = "injection"; print}""", ONELINER)
        self.assertEqual(res.stdout, "ala ma kota! Ala kocha kota! injection jest ambiwalentny względem ali :(\n")
        
        res2 = run(r"""{$(3+4) = "injection"; print}""", ONELINER)
        self.assertEqual(res2.stdout, "ala ma kota! Ala kocha kota! injection jest ambiwalentny względem ali :(\n")
        
    def test_record_assignment(self):
        res = run(r"""{$0 = "im overwriting things"; print $0; print$2}""", ONELINER)
        self.assertEqual(res.stdout, "im overwriting things\noverwriting\n")
        
    def test_negative_field_access(self):
        res1 = run(r"""{$(-1) = "im breaking things"}""", ONELINER)
        self.assertEqual(res1.stdout, "")
        self.assertEqual(res1.stderr, "AccessError: Attempt to access field -1\n")
        
        res2 = run(r"""{print $(-1)}""", ONELINER)
        self.assertEqual(res2.stdout, "")
        self.assertEqual(res2.stderr, "AccessError: Attempt to access field -1\n")
        
        
        
    def test_field_inc_dec(self):
        res = run(r"{print ++$1; print $1++; print $1; print $1--; print --$1; print $1}", TEN)
        self.assertEqual(res.stdout, "11\n11\n12\n12\n10\n10\n")

        

class TestFieldOperator(TestCase):
    def test_reference_expr(self):
        res = run("""BEGIN {x = 1} {print $(++x); print x}""", ONELINER)
        
        self.assertEqual(res.stdout, "ma\n2\n")
      
    def test_field_modification(self):
        res = run(r"""BEGIN { FS="\t"; OFS="," } {$1=$1; print $0}""", TSV_FILE)
        
        EXPECTED = dedent("""\
            Przedmiot,Ocena z ćwiczeń,Ocena z egzaminu/projektu
            Analiza Numeryczna,4.5,4
            Prog. Fun.,5,5 (mam nadzieję)
            MDM,5 (nie wiem jak?!),3 (jak dobrze pójdzie)
                          """)
        self.assertEqual(res.stdout, EXPECTED)
        
    def test_record_modification(self):
        res = run(r"""BEGIN { FS="\t"; OFS="," } {$0="Ala\tma\tkota!"; print $2}""", TSV_FILE)
        
        EXPECTED = dedent("""ma\nma\nma\nma\n""")
        self.assertEqual(res.stdout, EXPECTED)
        
class TestBinops(TestCase):
    def test_add(self):
        res = run(r"{print 1+1}", ONELINER)
        self.assertEqual(res.stdout, "2\n")
    
    def test_sub(self):
        res = run(r"{print 1234 - 23.2}", ONELINER)
        self.assertEqual(res.stdout, "1210.8\n")
    
    def test_mul(self):
        res = run(r"{print -3 * 17.3}", ONELINER)
        self.assertEqual(res.stdout, "-51.9\n")

    def test_div(self):
        res = run(r"{print 15.5 / 5}", ONELINER)
        self.assertEqual(res.stdout, "3.1\n")

    def test_num_num_lt(self):
        res = run(r"{print 1.3 < 2.6}", ONELINER)
        self.assertEqual(res.stdout, "1\n")
        res = run(r"{print 14.3 < -12.6}", ONELINER)
        self.assertEqual(res.stdout, "0\n")
    
    # comparing nums with str should cast the num to str
    def test_num_str_lt(self):
        res = run(r"""{print 10 < "2.6"}""", ONELINER)
        self.assertEqual(res.stdout, "1\n")
        res = run(r"""{print "10" < 2.6}""", ONELINER)
        self.assertEqual(res.stdout, "1\n")
        
    def test_str_str_lt(self):
        res = run(r"""{print "ala" < "bartek"}""", ONELINER)
        self.assertEqual(res.stdout, "1\n")
        res = run(r"""{print "ala" < "alan"}""", ONELINER)
        self.assertEqual(res.stdout, "1\n")
        
    def test_num_num_gt(self):
        res = run(r"{print (1.3 > 2.6)}", ONELINER)
        self.assertEqual(res.stdout, "0\n")
        res = run(r"{print (14.3 > -12.6)}", ONELINER)
        self.assertEqual(res.stdout, "1\n")
    
    # comparing nums with str should cast the num to str
    def test_num_str_gt(self):
        res = run(r"""{print (10 > "2.6")}""", ONELINER)
        self.assertEqual(res.stdout, "0\n")
        res = run(r"""{print ("10" > 2.6)}""", ONELINER)
        self.assertEqual(res.stdout, "0\n")
        
    def test_str_str_gt(self):
        res = run(r"""{print ("ala" > "bartek")}""", ONELINER)
        self.assertEqual(res.stdout, "0\n")
        res = run(r"""{print ("ala" > "alan")}""", ONELINER)
        self.assertEqual(res.stdout, "0\n")
        
        
    def test_concat(self):
        # ONELINER starts with "ala"
        res = run(r"""{print $1 " " "ma" " " 1 " " "Kota!"}""", ONELINER)
        self.assertEqual(res.stdout, "ala ma 1 Kota!\n")
        

class TestOperatorPrecedence(TestCase):
    
    def test_concat_assign(self):
        res = run(r"""{print x = 1 2; print x}""", ONELINER)
        self.assertEqual(res.stdout, "12\n12\n")

class TestLoops(TestCase):
    
    def test_for(self):
        res1 = run(r"""{for(x = 0; x<10; x++) print x}""", ONELINER)
        self.assertEqual(res1.stdout, "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n")
        
        res2 = run(r"""{x = -3; for(; x<7; x++) print x}""", ONELINER)
        self.assertEqual(res2.stdout, "-3\n-2\n-1\n0\n1\n2\n3\n4\n5\n6\n")
        
        res3 = run(r"""{x = -3; for(; x<4;) {print x; x++}}""", ONELINER)
        self.assertEqual(res3.stdout, "-3\n-2\n-1\n0\n1\n2\n3\n")
        
    def test_multiple_fors(self):
        prog = dedent(
                    """\
                    BEGIN{
                    OFS = ""
                    ORS = "";
                    }
                    {
                        for(i=0; i<=5; i++){
                            for (j = 0;j<i; j++){
                                print " "
                            };
                            print i, "\\n"
                        };
                    }""")
        res = run(prog, ONELINER)
        self.assertEqual(res.stdout, "0\n 1\n  2\n   3\n    4\n     5\n")
    
    def test_while(self):
        res = run(r"""{x = 4; while(x>=-2){print x; x--}}""", ONELINER)
        self.assertEqual(res.stdout, "4\n3\n2\n1\n0\n-1\n-2\n")
        
    def test_infinte_while(self):
        """ This will timeout """
        with self.assertRaises(subprocess.TimeoutExpired):
            res = run(r"""{while(){}}""", ONELINER, timeout=1)
        


class TestMiscellaneous(TestCase):
    def test_run_on_multiple_files(self):
        res = run(r"{print FILENAME}", [TEN, ONELINER])
        self.assertEqual(res.stdout, "./test/test_data/ten.txt\n./test/test_data/oneliner.txt\n")
        
    def test_NF(self):
        res = run(r"{print NF}", [TEN, ONELINER])
        self.assertEqual(res.stdout, "1\n12\n")
        
    def test_FNR(self):
        res = run(r"{print FNR}", [TSV_FILE, ONELINER])
        self.assertEqual(res.stdout, "1\n2\n3\n4\n1\n")
        
    def test_NR(self):
        res = run(r"{print NR}", [TSV_FILE, ONELINER])
        self.assertEqual(res.stdout, "1\n2\n3\n4\n5\n")
        
    def test_OFMT(self):
        res = res = run(r"""{OFMT="%.8g"; print -0.839071529076; OFMT="%.1g"; print -0.8}""", ONELINER)
        self.assertEqual(res.stdout, "-0.83907153\n-0.8\n")

class TestFunctionCalls(TestCase):
    def test_cos(self):
        res = run(r"{print cos($1)}", TEN)
        self.assertEqual(res.stdout, "-0.839072\n")
        

class TestTriggers(TestCase):
    def test_or(self):
        res = run(r"NR==1 || NR==3 {print}", TSV_FILE)
        self.assertEqual(res.stdout, "Przedmiot	Ocena z ćwiczeń	Ocena z egzaminu/projektu\nProg. Fun.	5	5 (mam nadzieję)\n")
        
    def test_and(self):
        res = run(r"NR>3 && $0~/5/ {print}", TSV_FILE)
        self.assertEqual(res.stdout, "MDM\t5 (nie wiem jak?!)\t3 (jak dobrze pójdzie)\n")
        
    def test_regex(self):
        res = run(r"/3/ {print}", TSV_FILE)
        self.assertEqual(res.stdout, "MDM\t5 (nie wiem jak?!)\t3 (jak dobrze pójdzie)\n")
    
    def test_regex_or(self):
        res = run(r"/3/ || /4/ {print}", TSV_FILE)
        self.assertEqual(res.stdout, "Analiza Numeryczna\t4.5\t4\nMDM\t5 (nie wiem jak?!)\t3 (jak dobrze pójdzie)\n")
    
    # NOTE: It's currently impossible to and/or regex patterns with expression patterns like this:
    # `NR>3 && /5/`.
    # use `NR>3 && $0~/5/` Instead


if __name__ == "__main__":
     unittest.main()