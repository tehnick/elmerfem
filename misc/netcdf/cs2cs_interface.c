/***** Vili Forsell, 23.6.2011
* Function for interfacing with cs2cs (i.e. PROJ.4)
* See "/user/include/proj_api.h" for cs2cs application interface
*/
#include <stdlib.h>
#include <proj_api.h>
#include <stdio.h>

char const MSG_HEADER[] = "GridDataMapper: CS2CS coordinate system transformation:";

// Add boolean values
enum bool {
	true = 1,
	false = 0
	};
typedef enum bool bool;

/* Function: main()
@param argc The number of given command line arguments
@param argv The given command line arguments (including the name of the program call)
@return EXIT_SUCCESS, if all went well
*/
void cs2cs_transform( double coord[3], int hasZ, int isRad, char elmer_proj[], char netcdf_proj[], double res[3] ) {
	projPJ pj_elmer = NULL, pj_netcdf = NULL; // Coordinate system definitions
	bool hasErrors = false;
	int transf_val = 0; // For return value from the transformation predefined by cs2cs as 0, if no errors and as error code otherwise

	printf("(%.2f, %.2f, %.2f) ; hasZ = %d ; isRad = %d \nelmer_proj = %s\nnetcdf_proj = %s\n",coord[0],coord[1],coord[2],hasZ,isRad,elmer_proj,netcdf_proj);

        fprintf(stdout,"\n\"%s\"\n",elmer_proj);
        fprintf(stdout,"\n\"%s\"\n",netcdf_proj);

	// Input information
//	double coord[3]; // Contains the coordinates
//	coord[0] = -16;
//	coord[1] = 20.25;
//	coord[2] = 0;
	bool isZset = true; // True, if z coordinate is used
	if ( hasZ == 0 ) isZset = false;
	bool isInputRad = false; // True, if the given input is in radians
	if ( isRad != 0 ) isRad = true;

//	char elmer_proj[] = "+proj=latlong +ellps=clrk66"; // "+proj=stere +lat_ts=71N +lat_0=90 +lon_0=96W +x_0=0 +y_0=0 +ellps=WGS86" // Coordinate system parameters
//	char netcdf_proj[] = "+proj=merc +ellps=clrk66 +lat_ts=33";

	// Set constant
	long point_count = 1; // Number of processed points (x,y,z can be arrays)
	int point_offset = 1; // Step size, if multiple points (f.ex. every second) processed

	// Initializes the coordinate systems for the read Elmer data point and the NetCDF data
	pj_elmer = pj_init_plus(elmer_proj);
	if ( pj_elmer == NULL ){
		printf("%s Failed to initialize Elmer coordinate system with parameters: %s!\n", MSG_HEADER, elmer_proj);
		hasErrors = true;
	}
	pj_netcdf = pj_init_plus(netcdf_proj);
	if ( pj_netcdf == NULL ){
		printf("%s Failed to initialize NetCDF coordinate system with parameters: %s!\n", MSG_HEADER, netcdf_proj);
		hasErrors = true;
	}

	// Transformation from degrees to radians, if necessary
	if ( !isInputRad ) {
		printf("%s Converting values from degrees to radians.\n", MSG_HEADER);
		coord[0] *= DEG_TO_RAD;
		coord[1] *= DEG_TO_RAD;
		coord[2] *= DEG_TO_RAD;
	}

	// Performs the transformation
	if ( hasErrors != true ) {
		if (isZset) transf_val = pj_transform(elmer_proj,netcdf_proj,point_count,point_offset,&coord[0],&coord[1],&coord[2]);
		else transf_val = pj_transform(elmer_proj,netcdf_proj,point_count,point_offset,&coord[0],&coord[1],NULL);
		if ( transf_val != 0 ) {
			printf("%s Failed to transform coordinates; Proj.4 error code %d.\n",MSG_HEADER,transf_val);
			hasErrors = true;
		}
	}

	// Saves output
	if ( hasErrors == true ) {
          res[0] = 0;
          res[1] = 0;
          res[2] = 0;
        } else {
          res[0] = coord[0];
          res[1] = coord[1];
          res[2] = coord[2];
        }
  
	printf("Done with (%.2f, %.2f)\n", coord[0], coord[1], transf_val);

//	return EXIT_SUCCESS;
}
