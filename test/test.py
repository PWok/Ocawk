#!/usr/bin/python3
import subprocess
import unittest

from textwrap import dedent
from unittest import TestCase


OCAWK = "./_build/install/default/bin/ocawk"

class TestVariableAssignment(TestCase):
    def test_custom_vars(self):
        res = subprocess.run([OCAWK, r"BEGIN {foo = 17; print foo}", "./test/test_data/example.tsv"], encoding="UTF-8", timeout=5, capture_output=True)
        self.assertEqual(res.stdout, "17.\n")
        
    def test_field_modification(self):
        res = subprocess.run([OCAWK, r"""BEGIN { FS="\t"; OFS="," } {$1=$1; print $0}""", "./test/test_data/example.tsv"], encoding="UTF-8", timeout=5, capture_output=True)
        
        EXPECTED = dedent("""\
            Przedmiot,Ocena z ćwiczeń,Ocena z egzaminu/projektu
            Analiza Numeryczna,4.5,4
            Prog. Fun.,5,5 (mam nadzieję)
            MDM,5 (nie wiem jak?!),3 (jak dobrze pójdzie)
                          """)
        self.assertEqual(res.stdout, EXPECTED)
        
    def test_record_modification(self):
        res = subprocess.run([OCAWK, r"""BEGIN { FS="\t"; OFS="," } {$0="Ala\tma\tkota!"; print $2}""", "./test/test_data/example.tsv"], encoding="UTF-8", timeout=5, capture_output=True)
        
        EXPECTED = dedent("""ma\nma\nma\nma\n""")
        self.assertEqual(res.stdout, EXPECTED)
        
        

if __name__ == "__main__":
     unittest.main()