// Distance and Bearing
// Example for FlashForth usinf External Cfunctions
// This is called via Cdistbear.s
// Calculates distance in meters and initial bearing in degrees based on
// latidute and longitude (decimal) of two points on Earth.
// Uses the Haversine formula
// c IgorM 6/2015
// GNU GPL v3
// No warranties of any kind
// Provided as-is

#include <math.h>
#include <stdio.h>

#define R 6371000.0
#define PI 3.1415926535897932384626433832795

float Cdist (float lat1, float lon1, float lat2, float lon2) {

double phi1 = (double)lat1 * PI / 180.0;
double phi2 = (double)lat2 * PI / 180.0;
double dphi = ((double)lat2 - (double)lat1) * PI / 180.0;
double dlambda = ((double)lon2 - (double)lon1)* PI / 180.0;

double a1 = sin(dphi/2.0)*sin(dphi/2.0);
double a2 = cos(phi1)*cos(phi2);
double a3 = sin(dlambda/2.0)*sin(dlambda/2.0);
double a = a1 + (a2 * a3);
double c = 2.0 * atan2(sqrt(a), sqrt(1.0-a));
double d = R * c;

return (float)d;
}

float Cbear (float lat1, float lon1, float lat2, float lon2) {

double phi1 = (double)lat1 * PI / 180.0;
double phi2 = (double)lat2 * PI / 180.0;
double dlambda = ((double)lon2 - (double)lon1) * PI / 180.0;

double y = sin(dlambda)*cos(phi2);
double x = cos(phi1)*sin(phi2)-sin(phi1)*cos(phi2)*cos(dlambda);
double brn = (atan2(y,x)) * 180.0 / PI;

return (float)brn;
}

// lat1  lon1   lat2     lon2 (degree)
// 50.0e 15.0e 50.0001e 15.0001e    dist fs.
// 50.0e 15.0e 50.0001e 15.0001e    bear fe. (13.2185m, 32 43 56 = 32.73222)
// 50.0e 15.0e 60.0e    80.0e       dist fs.
// 50.0e 15.0e 60.0e    80.0e       bear fe. (4107.799km, 48 56 13 = 48.93694)
// 50.0e 15.0e -60.0e   -170.0e     dist fs.
// 50.0e 15.0e -60.0e   -170.0e     bear fe. (18859.128km, 166 01 30 = 166.025)

