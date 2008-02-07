In order to run the two tests in this directory, you should have Netgen 4.5
(based on Netgen CVS 15. Jan 2008 or later) installed on you system. To be
more precise, "ng" should be a valid command visible to "which ng", and
NETGENDIR should have an appropriate value.

If ng is ok, you should place somwhere in your system following script:

#!/bin/sh
#
# Mesh3D for Elmer
#
if which ng 1> /dev/null 2> /dev/null
then
  ng -batchmode -meshfiletype="Elmer Format" -geofile=$1 -meshfile=$2 -meshsizefile=$3
else
  echo "ERROR: ng not found, unable to generate 3D mesh"
fi


Call the script "Mesh3D" and make sure it is executable:

$ chmod +x Mesh3D

Also make sure that the directory containing Mesh3D in in your PATH.

You can then run the examples as

$ cd sculpture
$ ElmerSolver

and 

$ cd fichera
$ ElmerSolver
