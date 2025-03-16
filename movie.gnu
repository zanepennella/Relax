# Set the output format (e.g., PNG)
set terminal png size 800,600 enhanced font 'Helvetica,12'

# Loop through all iteration files
do for [iter=1:100] {
    # Set the output file name
    set output sprintf('frame_%03d.png', iter)

    # Set the title and labels
    set title sprintf("Potential at Iteration %d", iter)
    set xlabel "X"
    set ylabel "Y"
    set zlabel "Potential"

    # Generate the surface plot
    splot sprintf('potential_iter_%03d.dat', iter) matrix with lines title "Potential"
}

# Combine frames into a movie using ffmpeg
! ffmpeg -framerate 10 -i frame_%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p potential_movie.mp4