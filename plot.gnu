# Set the output format (e.g., PNG)
set terminal png size 800,600 enhanced font 'Arial,12'
set output 'potential_surface.png'

# Set the title and labels
set title "Potential Surface Plot"
set xlabel "X"
set ylabel "Y"
set zlabel "Potential"

# Generate the surface plot
splot 'relaxation.dat' matrix with lines title "Potential"

# Create a contour plot
set output 'potential_contour.png'
set title "Potential Contour Plot"
unset zlabel
set contour
set view map
unset surface
splot 'relaxation.dat' matrix with lines title "Potential"