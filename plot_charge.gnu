# Set the terminal type and output file format
set terminal pngcairo size 800,600 enhanced font 'Verdana,10'
set output 'charge_density.png'

# Set up the plot style
set pm3d at b        # Enable 3D surface plot with color mapping at the bottom
set palette defined (-1 "blue", 0 "white", 1 "red")  # Define a color palette
set cbrange [-0.1:0]  # Set the range for the color bar
set style data lines

# Set labels and title
set xlabel "i"
set ylabel "j"
set zlabel "{/Symbol r}"
set title "Charge Density (Gaussian Distribution)"

# Plot the data
splot 'charge_density.dat' matrix with pm3d notitle