set style data histogram
set style histogram rowstacked
set term png
set output "bench.png"

plot 'bench.txt' using 3:xtic(1)
