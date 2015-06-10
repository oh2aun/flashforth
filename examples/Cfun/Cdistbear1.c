// Distance and Bearing demo 1
// Example for FlashForth using an External Cfunction call
//
// This Cfunction is called via Cdistbear1.s
// We get 2 results with a single return from the C function !!
//
// Calculates distance in meters and initial bearing in degree based on
// latidute and longitude (decimal degree) of two points on Earth.
//
// distbear (lat1 lon1 lat2 lon2 -- bearing distance)   \ all floats
//
// Uses the Haversine formula
// (c) IgorM 10.6.2015
// GNU GPL v3
// No warranties of any kind
// Provided as-is

#include <math.h>
#include <stdio.h>

union retData
{
   float i[2];
   double j;
};

#define R 6371000.0
#define PI 3.1415926535897932384626433832795

double Cdistbear (float lat1, float lon1, float lat2, float lon2) {

	union retData result;

	double phi1 = (double)lat1 * PI / 180.0;
	double phi2 = (double)lat2 * PI / 180.0;
	double dphi = ((double)lat2 - (double)lat1) * PI / 180.0;
	double dlambda = ((double)lon2 - (double)lon1)* PI / 180.0;

	double a1 = sin(dphi/2.0)*sin(dphi/2.0);
	double a2 = cos(phi1)*cos(phi2);
	double a3 = sin(dlambda/2.0)*sin(dlambda/2.0);
	double a = a1 + (a2 * a3);
	double c = 2.0 * atan2(sqrt(a), sqrt(1.0-a));
	double dis = R * c;

	double y = sin(dlambda)*cos(phi2);
	double x = cos(phi1)*sin(phi2)-sin(phi1)*cos(phi2)*cos(dlambda);
	double brn = (atan2(y,x)) * 180.0 / PI;

	result.i[0] = dis;	// create 2 floats result
	result.i[1] = brn;

	return result.j;	// return 2 floats via 1 double
}

// lat1  lon1   lat2     lon2 (degree)
// 50.0e 15.0e 50.0001e 15.0001e    distbear fs. fe. (13.2185m, 32.73222deg)
// 50.0e 15.0e 60.0e    80.0e       distbear fs. fe. (4107.799km, 48.93694deg)
// 50.0e 15.0e -60.0e   -170.0e     distbear fs. fe. (18859.128km, 166.025deg)

