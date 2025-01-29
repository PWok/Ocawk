#!/usr/bin/python3
import subprocess
import unittest

from textwrap import dedent
from unittest import TestCase


OCAWK = "./_build/install/default/bin/ocawk"


def run(code, file):
    return subprocess.run([OCAWK, code, file], encoding="UTF-8", timeout=5, capture_output=True)


class TestVariableAssignment(TestCase):
    def test_custom_vars(self):
        res = run(r"BEGIN {foo = 17; print foo}", "./test/test_data/example.tsv")
        self.assertEqual(res.stdout, "17\n")
        
    def test_field_modification(self):
        res = run(r"""BEGIN { FS="\t"; OFS="," } {$1=$1; print $0}""", "./test/test_data/example.tsv")
        
        EXPECTED = dedent("""\
            Przedmiot,Ocena z ćwiczeń,Ocena z egzaminu/projektu
            Analiza Numeryczna,4.5,4
            Prog. Fun.,5,5 (mam nadzieję)
            MDM,5 (nie wiem jak?!),3 (jak dobrze pójdzie)
                          """)
        self.assertEqual(res.stdout, EXPECTED)
        
    def test_record_modification(self):
        res = run(r"""BEGIN { FS="\t"; OFS="," } {$0="Ala\tma\tkota!"; print $2}""", "./test/test_data/example.tsv")
        
        EXPECTED = dedent("""ma\nma\nma\nma\n""")
        self.assertEqual(res.stdout, EXPECTED)
        
class TestBinops(TestCase):
    def test_add(self):
        res = run(r"{print 1+1}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "2\n")
    
    def test_sub(self):
        res = run(r"{print 1234 - 23.2}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "1210.8\n")
    
    def test_mul(self):
        res = run(r"{print -3 * 17.3}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "-51.9\n")

    def test_div(self):
        res = run(r"{print 15.5 / 5}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "3.1\n")

    def test_num_num_lt(self):
        res = run(r"{print 1.3 < 2.6}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "1\n")
        res = run(r"{print 14.3 < -12.6}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "0\n")
    
    # comparing nums with str should cast the num to str
    def test_num_str_lt(self):
        res = run(r"""{print 10 < "2.6"}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "1\n")
        res = run(r"""{print "10" < 2.6}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "1\n")
        
    def test_str_str_lt(self):
        res = run(r"""{print "ala" < "bartek"}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "1\n")
        res = run(r"""{print "ala" < "alan"}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "1\n")
        
    def test_num_num_gt(self):
        res = run(r"{print (1.3 > 2.6)}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "0\n")
        res = run(r"{print (14.3 > -12.6)}", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "1\n")
    
    # comparing nums with str should cast the num to str
    def test_num_str_gt(self):
        res = run(r"""{print (10 > "2.6")}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "0\n")
        res = run(r"""{print ("10" > 2.6)}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "0\n")
        
    def test_str_str_gt(self):
        res = run(r"""{print ("ala" > "bartek")}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "0\n")
        res = run(r"""{print ("ala" > "alan")}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "0\n")
        
        
    def test_concat(self):
        # "./test/test_data/oneliner.txt" starts with "ala"
        res = run(r"""{print $1 " " "ma" " " 1 " " "Kota!"}""", "./test/test_data/oneliner.txt")
        self.assertEqual(res.stdout, "ala ma 1 Kota!\n")
        
    # TODO: FIXME: add tests for regex matching


# TODO: add tests for loops and if and print
# TODO: tests for print redirection to file

# TODO: tests for icrement decrement
if __name__ == "__main__":
     unittest.main()