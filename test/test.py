#!/usr/bin/python3
import subprocess
import unittest

from textwrap import dedent
from unittest import TestCase


OCAWK = "./_build/install/default/bin/ocawk"
ONELINER = "./test/test_data/oneliner.txt"
TSV_FILE = "./test/test_data/example.tsv"
TEN = "./test/test_data/ten.txt"

def run(code, file):
    return subprocess.run([OCAWK, code, file], encoding="UTF-8", timeout=1, capture_output=True)


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
        res = run(r"""{$(-1) = "im breaking things"}""", ONELINER)
        self.assertEqual(res.stdout, "")
        self.assertEqual(res.stderr, "AccessError: Attempt to access field -1\n")
        
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
        
    # TODO: FIXME: add tests for regex matching

class TestOperatorPrecedence(TestCase):
    
    def test_concat_assign(self):
        res = run(r"""{print x = 1 2; print x}""", ONELINER)
        self.assertEqual(res.stdout, "12\n12\n")

# TODO: add tests for loops and if and print
# TODO: tests for print redirection to file

# TODO: add test for running on multiple files
# TODO: add tests for build in variables like NR FNR FILENAME etc.
# TODO: tests for icrement decrement

#TODO: tests for function calls

# TODO: tests for pattern regex and/or/not

if __name__ == "__main__":
     unittest.main()