# Set the terminal type and output file format
set terminal pngcairo size 800,600 enhanced font "Verdana,10"
set output "final.png"

# Handle missing or undefined values
set datafile missing "NaN"

# Set up the plot style
set pm3d at s
set palette defined (-1 "red", 1 "blue")
set cbrange [-1:1]

# Enable contours on both the surface and the base
set contour both
set cntrparam levels auto 3

# Set surface lines to black
set style line 100 linecolor rgb "black" linewidth 1

# Set labels and title
set xlabel "i"
set ylabel "j"
set yrange [0:40]
set zlabel "{/Symbol F}"
set title "Relaxation Output: Movie"

# Set the view angle for a 3D perspective
set view 60, 120

# Loop over each iteration in the data file

splot "final.dat" matrix with pm3d, \
    ""  matrix with lines linestyle 100 notitle