#define NUM_BINS 5793 // should get this to be dynamically set, but that could be difficult
#include <math.h>
#include <stdio.h>
 
float bondsx[1000000], bondsy[1000000], bondsangle[1000000];
double bonds_cos[1000000], bonds_sin[1000000];
/*these WERE global variables, to avoid a stack overflow when is large.  Decided to move them inside the program and define them using bond count*/
// what the hell does floor do?
void main ()
{
  FILE* data;
  FILE* output;
  long bondcount;
  float dist;
  float robcor[NUM_BINS];
  long pairs[NUM_BINS], multiplier, count, ii, jj;
  register int d;   //These register declarations are probably useless, but are also harmless.
  register float xdiff,ydiff;
  register int i,j;

  data=fopen("bonds.dat","r");
  output=fopen("robcor.dat","w");
  
  multiplier=4;  
  fscanf(data, "%i", &multiplier); // reads the first line and makes it the multiplier.  Rather important I suspect as elsewise it fuckerates things

  fscanf(data,"%i",&bondcount); // is intended to grab the bonds number.  May or may not work.  Slightly upset.
  //printf("There are %i bonds\n",bondcount);

  /*float bondsx[bondcount]={0};
  float bondsy[bondcount]={0};
  float bondsangle[bondcount]={0};
  double bonds_cos[bondcount]={0};
  double bonds_sin[bondcount]={0};  // 
  */
  for (i=0; i<bondcount; i++)
    fscanf(data,"%f %f %f", &bondsx[i], &bondsy[i], &bondsangle[i]);
  //printf("done with reading bonds\n");
 
  for (i=0; i<bondcount; i++) {  
 	bonds_cos[i] = cos(6*bondsangle[i]);
	bonds_sin[i] = sin(6*bondsangle[i]);
	}
  
  for (i=0;i<NUM_BINS;i++) pairs[i]=robcor[i]=0;  // this could be our problem! what the hell is this supposed to do?!!!


  count=(long) floor(bondcount/multiplier); /// curious about this.  what is floor?

  for (ii=0;ii<count; ii++)
  {
    i=ii*multiplier;
    //if(i/1000.0==i/1000)
    //	printf("Working on bond %i\n", i);
//    for (jj=ii;jj<count;jj++)
    for (j=ii+multiplier;j<bondcount;j+=multiplier)
    {
//      j=jj*multiplier;
//      dist=sqrt((bondsx[i]-bondsx[j])*(bondsx[i]-bondsx[j])+(bondsy[i]-bondsy[j])*(bondsy[i]-bondsy[j]));

		xdiff = bondsx[i]-bondsx[j];
		ydiff = bondsy[i]-bondsy[j];
        d=floor(sqrt(xdiff*xdiff+ydiff*ydiff));
      if (d<NUM_BINS) 
     {
//        robcor[d]+=cos(6*(bondsangle[i]-bondsangle[j]));
		  robcor[d]+=bonds_cos[j]*bonds_cos[i] + bonds_sin[j]*bonds_sin[i];
/*Note the change made in the previous two lines, which are identical by the 
trigonometric identity cos(a-b) = cos(b)*sin(a)+sin(b)*sin(a).  
There is a significant time penalty for calculating the transcendental functions
sin() and cos(), and the old way calculates a cosine on order bondcount^2 times.  
I have paid this penalty up front, calculating sin(6*theta) and cos(6*theta)
for all bond angles only once, a total of bondcount times.  The calculations I have to 
do bondcount^2 times are two multiplications and one addition, which is apparently faster.*/
        pairs[d]++;
	  }
    } //end of j (inner) loop
  }  //end of i (outer) loop

  for (i=0;i<NUM_BINS;i++)
    if (pairs[i]>0)
      fprintf(output,"%d     %f      %d\n",i, robcor[i]/pairs[i], pairs[i]);

  fclose(data);
  fclose(output);

}
