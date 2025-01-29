BEGIN {FS = ","; OFS = "\t"; print "Lunar eclipses of the XXI century that have not happened yet."}
{if ( ($2 + 0)  > 2024 || NR == 1) print $2,$3,$4, $17, $18 > "procesowane_zacmienia.tsv"}