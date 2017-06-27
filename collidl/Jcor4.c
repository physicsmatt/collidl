#define NUM_BINS 5793 // should get this to be dynamically set, but that could be difficult
#define S_NUM_BINS 33554433 //should ideally be NUM_BINS squared, which is 33554433
#define MAX_BONDS 950000
#include <math.h>
#include <stdio.h>
//#include <ctime> //temporary again.
//temporary!!! timer implementation! flagging all related code with !~! in commenting
//ex:  //!~!
//clock_t start2,finish2;
//double timeLoopalone; //this is going to be printed at the end.  be careful please and remember that.





//Declaration of the new struct, used to cradle our beloved data.  It also makes dynamic memory allocation a lot tighter to code, and allows
// padding and easier chunking of data.  Should simplify our caching endeavor immensely.
struct InitialStorage {
float Bondx, Bondy, BondAngle;
//need to talk to Kelly about how much padding we will need. If any.
};
typedef struct InitialStorage InitialStorage;

// Struct used to store the data once processing and loading is done.  Not SURE we need to do this.
struct ProcessingStorage {
 float Bondx, Bondy, Cos, Sin; 
//need to talk to Kelly about how much padding we will need.  If any.
};
typedef struct ProcessingStorage ProcessingStorage;
/* float bondsx[MAX_BONDS], bondsy[MAX_BONDS], bondsangle[MAX_BONDS];
float bonds_cos[MAX_BONDS], bonds_sin[MAX_BONDS];
these WERE global variables, to avoid a stack overflow when is large.
_Now_ we handle it with the New operand inside the prog, and a pointer to the previously declared structs*/

void main ()
{
  FILE* data;
  FILE* output;
  long bondcount;
  //InitialStorage * IntoBox;
  ProcessingStorage * ProcArray; // ProcessingArray the array calculations should actually be done on
  int dummy, dummy1, dummy2;
  float robcor[NUM_BINS];
  long pairs[NUM_BINS], multiplier, count, jj;
  register int d;   //These register declarations are probably useless, but are also harmless.
  register float xdiff,ydiff;  //this has GOT to stop being floats
  register int i,j;
  register ProcessingStorage CurrentSet;

  data=fopen("bonds.dat","r");
  output=fopen("robcor.dat","w"); // remember you did this, jake.  idiot.  Sincerely, Jake.
  multiplier=4;  //defaults the multiplier.  clinically useless but I'm keeping it, just like we keep politicians
  fscanf_s(data, "%i", &multiplier); // reads the first line and makes it the multiplier.  Rather important I suspect as elsewise it fuckerates things

  fscanf_s(data,"%i",&bondcount); // is intended to grab the bonds number.  Works.
  printf("There are %i bonds\n",bondcount);
// new code block.  Structing this shizat!
  bondcount=(bondcount/multiplier);
  InitialStorage * IntoBox = new InitialStorage [bondcount];
  ProcArray = new ProcessingStorage [bondcount];
  
  for (i=0; i<(bondcount); i++) {
	for (jj=0;jj<(multiplier-1);jj++)
	  fscanf_s(data,"%f %f %f", &dummy, &dummy1, &dummy2);
	  fscanf_s(data,"%f %f %f", &IntoBox[i].Bondx, &IntoBox[i].Bondy, &IntoBox[i].BondAngle);
	}

  //printf("done with reading bonds\n");
  //shifts the data over and performs the cos and sine calculations.  Then we destroy the original storage array.  This is
  // done to keep the array absolutely as small as we can.
  for (i=0; i<bondcount; i++) 
	{

	  ProcArray[i].Cos = cos(6*IntoBox[i].BondAngle);
	  ProcArray[i].Sin = sin(6*IntoBox[i].BondAngle);
	  ProcArray[i].Bondx = IntoBox[i].Bondx;
	  ProcArray[i].Bondy = IntoBox[i].Bondy;
	
	}
delete [] IntoBox;
  // end workzone

//start2 = clock();
// start outer loop, inside block
  for (i=0;i<NUM_BINS;i++) pairs[i]=robcor[i]=0;  

  count=bondcount; // leaving this in, I think I might be able to use it later.  I'm such a packrat coder...

	  for (i=0;i<count; i++)
//  for (i=0;i<10000; i++)
  {
    //if(i/1000.0==i/1000)
    //	printf("Working on bond %i\n", i);
//    for (jj=ii;jj<count;jj++)
	//here's where I'm stopping to implement blocking. ------------------FLAG-------------------
///took out the temporary I structures.  holding off on the next part
	CurrentSet=ProcArray[i];
	  for (j=(i+1);j<bondcount;j++)
	    {
			xdiff = CurrentSet.Bondx-ProcArray[j].Bondx;
			ydiff = CurrentSet.Bondy-ProcArray[j].Bondy;
		      d=sqrt(xdiff*xdiff+ydiff*ydiff);
//|##########|d=xdiff*xdiff+ydiff*ydiff; (phasing sqrt back in for unrepentant convenience)
//|##########|The infrastructure has been removed because if we do avoid sqrt it'll be using the 2d array
/*     if (d< S_NUM_BINS)        //deprecated due to superior memory management
    { */
//        robcor[d]+=cos(6*(bondsangle[i]-bondsangle[j]));
			robcor[d]+=ProcArray[j].Cos*CurrentSet.Cos + ProcArray[j].Sin*CurrentSet.Sin;

		  /*Note the change made in the previous two lines, which are identical by the 
trigonometric identity cos(a-b) = cos(b)*cos(a)+sin(b)*sin(a).  
There is a significant time penalty for calculating the transcendental functions
sin() and cos(), and the old way calculates a cosine on order bondcount^2 times.  
I have paid this penalty up front, calculating sin(6*theta) and cos(6*theta)
for all bond angles only once, a total of bondcount times.  The calculations I have to 
do bondcount^2 times are two multiplications and one addition, which is apparently faster.*/
			pairs[d]++;

//	  }  deprecated if statement
    } //end of j (inner) loop
  }  //end of i (outer) loop

//	finish2 = clock(); //!~! finishing the clocking
//	timeLoopalone = (double(finish2)-double(start2)); //!~! clock line!
//	printf("This took %lf clockticks and %lf seconds.\n",timeLoopalone, timeLoopalone/CLOCKS_PER_SEC);
//	getchar();

  for (i=0;i<NUM_BINS;i++)
    if (pairs[i]>0)
      fprintf(output,"%d     %f      %d\n",i, robcor[i]/pairs[i], pairs[i]);
	
	fclose(data);
	fclose(output);

}
