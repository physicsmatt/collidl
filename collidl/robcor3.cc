#define NUM_BINS 5793 // should get this to be dynamically set, but that could be difficult
#define S_NUM_BINS 33554433 //should ideally be NUM_BINS squared, which is 33554433
#define MAX_BONDS 950000
#include <math.h>
#include <stdio.h>


#include <ctime> 
double timeLoopalone;
clock_t start,finish;


float bondsx[MAX_BONDS], bondsy[MAX_BONDS], bondsangle[MAX_BONDS];
double bonds_cos[MAX_BONDS], bonds_sin[MAX_BONDS];
double s_robcor[S_NUM_BINS];  // make this a double?
long s_pairs[S_NUM_BINS];
 
/*these WERE global variables, to avoid a stack overflow when is large.  Decided to move them inside the program and define them using bond count*/

void main ()
{


  FILE* data;
  FILE* output;
  long bondcount;
  int dummy, dummy1, dummy2;
  float robcor[NUM_BINS];
  long pairs[NUM_BINS], multiplier, count, jj;
  register int d;   //These register declarations are probably useless, but are also harmless.
  register float xdiff,ydiff;
  register int i,j;
  int ds;
  float bondsx_i, bondsy_i;
  double bonds_cos_i, bonds_sin_i;

start = clock();

  data=fopen("bonds.dat","r");
  output=fopen("robcor.dat","w"); // remember you did this, jake.  idiot.  Sincerely, Jake.
  multiplier=4;  
  fscanf_s(data, "%i", &multiplier); // reads the first line and makes it the multiplier.  Rather important I suspect as elsewise it fuckerates things

  fscanf_s(data,"%i",&bondcount); // is intended to grab the bonds number.  Works.
  printf("There are %i bonds\n",bondcount);

  /*float bondsx[bondcount]={0};
  float bondsy[bondcount]={0};
  float bondsangle[bondcount]={0};
  double bonds_cos[bondcount]={0};
  double bonds_sin[bondcount]={0};  // 
  */
  bondcount=(bondcount/multiplier);
  
  for (i=0; i<(bondcount); i++) {
	for (jj=0;jj<(multiplier-1);jj++)
	  fscanf_s(data,"%f %f %f", &dummy, &dummy1, &dummy2);
    fscanf_s(data,"%f %f %f", &bondsx[i], &bondsy[i], &bondsangle[i]);
	}
  //printf("done with reading bonds\n");
  for (i=0; i<bondcount; i++) {  
 	bonds_cos[i] = cos(6*bondsangle[i]);
	bonds_sin[i] = sin(6*bondsangle[i]);
	}
  for (i=0;i<NUM_BINS;i++) pairs[i]=robcor[i]=0;  

  count=bondcount;

	

  for (i=0;i<count; i++)
//  for (i=0;i<10000; i++)
  {
    //if(i/1000.0==i/1000)
    //	printf("Working on bond %i\n", i);
//    for (jj=ii;jj<count;jj++)
	bondsx_i=bondsx[i];
	bondsy_i=bondsy[i];
	bonds_cos_i=bonds_cos[i];
	bonds_sin_i=bonds_sin[i];
    for (j=(i+1);j<bondcount;j++)
    {
		xdiff = bondsx_i-bondsx[j];
		ydiff = bondsy_i-bondsy[j];
//        d=sqrt(xdiff*xdiff+ydiff*ydiff);
         d=xdiff*xdiff+ydiff*ydiff;
/*     if (d< S_NUM_BINS)        //For speed, this IF could be removed, once the memory is handled better.|deprecated due to superior memory management
    { */
//        robcor[d]+=cos(6*(bondsangle[i]-bondsangle[j]));
		  s_robcor[d]+=bonds_cos[j]*bonds_cos_i + bonds_sin[j]*bonds_sin_i;

		  /*Note the change made in the previous two lines, which are identical by the 
trigonometric identity cos(a-b) = cos(b)*cos(a)+sin(b)*sin(a).  
There is a significant time penalty for calculating the transcendental functions
sin() and cos(), and the old way calculates a cosine on order bondcount^2 times.  
I have paid this penalty up front, calculating sin(6*theta) and cos(6*theta)
for all bond angles only once, a total of bondcount times.  The calculations I have to 
do bondcount^2 times are two multiplications and one addition, which is apparently faster.*/
        s_pairs[d]++;

//	  }  deprecated if statement
    } //end of j (inner) loop
  }  //end of i (outer) loop


  //Now step through and fill out robcor and pairs
printf("Now filling up robcor\n");
  for(ds=0;ds < S_NUM_BINS; ds++) {
	  d = sqrt((float)ds);
	  robcor[d] += s_robcor[ds];
	  pairs[d] += s_pairs[ds];
  }


  for (i=0;i<NUM_BINS;i++)
    if (pairs[i]>0)
      fprintf(output,"%d     %f      %d\n",i, robcor[i]/pairs[i], pairs[i]);

  fclose(data);
  fclose(output);
	finish = clock();
	timeLoopalone = (double(finish)-double(start))/CLOCKS_PER_SEC;
	printf("Calculations took %lf seconds.\n",timeLoopalone);
	getchar();
}
