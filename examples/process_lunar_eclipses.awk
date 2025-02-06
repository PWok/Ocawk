BEGIN {FS = ","; OFS = "\t"; print "Processing file"}
{if ( ($2 + 0) > 2024 || NR == 1) print $2, $3, $4, $17, $18 > "procesowane_zacmienia.tsv"}
END {close("procesowane_zacmienia.tsv"); print "DONE. Saved to procesowane_zacmienia.tsv"}