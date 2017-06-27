#define NUM_BINS 5794 // should get this to be dynamically set, but that could be difficult
#define S_NUM_BINS 33554433 //should ideally be NUM_BINS squared, which is 33554433
#define MAX_BONDS 950000
#include <math.h>
#include <stdio.h>
#include <ctime> //temporary again.
//temporary!!! timer implementation! flagging all related code with !~! in commenting
//ex:  //!~!
clock_t start2,finish2, start, finish; // !~!
double timeLoopalone, timer; //this is going to be printed at the end.  be careful please and remember that.


// adding in blocking.

// core operating constants are declared here. change with caution
const int BLOCKSIZE=2000; // controls the chunklet sized processed in a single go.  Intended to improve cache handling and locality.  Used with prefetching commands.


//Declaration of the new struct, used to cradle our beloved data.  It also makes dynamic memory allocation a lot tighter to code, and allows
// padding and easier chunking of data.  Should simplify our caching endeavor immensely.
struct InitialStorage {
	float Bondx, Bondy, BondAngle;
	//need to talk to Kelly about how much padding we will need. If any.
};
typedef struct InitialStorage InitialStorage; //carried over from c code, harmless.

// Struct used to store the data once processing and loading is done.  Not SURE we need to do this.
struct ProcessingStorage {
	float Bondx, Bondy, Cos, Sin; 
	//need to talk to Kelly about how much padding we will need.  If any.
};
typedef struct ProcessingStorage ProcessingStorage; //carried over from c code, harmless.
/* float bondsx[MAX_BONDS], bondsy[MAX_BONDS], bondsangle[MAX_BONDS];
float bonds_cos[MAX_BONDS], bonds_sin[MAX_BONDS];
these WERE global variables, to avoid a stack overflow when is large.
_Now_ we handle it with the New operator inside the prog, and a pointer to the previously declared structs*/

void main ()
{
	start= clock(); //!~! clock line
	FILE* data;
	FILE* output;
	int bondcount, blockscount;
	InitialStorage * IntoBox;
	ProcessingStorage * ProcArray; // ProcessingArray the array calculations should actually be done on
	int dummy, dummy1, dummy2;
	float robcor[NUM_BINS];
	long pairs[NUM_BINS];
	int countA=0, multiplier=0, countB=0;
	register int d;   //These register declarations are probably useless, but are also harmless.
	register float xdiff,ydiff;  //this has GOT to stop being floats. fsckfsckfsck sqrt is float only. 
	register int i,j, jj;
	int CntBLC_A=0, CntBLC_B=0;


	data=fopen("bonds.dat","r");
	output=fopen("robcor.dat","w"); // remember you did this, jake.  idiot.  Sincerely, Jake.
	multiplier=4;  //defaults the multiplier.  clinically useless but I'm keeping it, just like we keep politicians
	fscanf(data, "%i", &multiplier); // reads the first line and makes it the multiplier.  Rather important I suspect as elsewise it fuckerates things

	fscanf(data,"%i",&bondcount); // is intended to grab the bonds number.  Works.
	bondcount=(bondcount/multiplier);
	IntoBox = new InitialStorage [bondcount];
	ProcArray = new ProcessingStorage [bondcount];

	for (i=0; i<(bondcount); i++) {
		for (jj=0;jj<(multiplier-1);jj++)
			fscanf(data,"%f %f %f", &dummy, &dummy1, &dummy2);
		fscanf(data,"%f %f %f", &IntoBox[i].Bondx, &IntoBox[i].Bondy, &IntoBox[i].BondAngle);
	}
	printf("There are %i bonds\n",bondcount);
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
	printf("There are %i bonds\n",bondcount);
	delete [] IntoBox;

	blockscount=bondcount-(bondcount%BLOCKSIZE);
	printf("This produces %i after insuring full blocks.  Pun Intended.\n", blockscount);
	for (i=0;i<NUM_BINS;i++) pairs[i]=robcor[i]=0; 

	finish = clock(); //!~! clock line

	timer = (double(finish)-double(start)); //!~! clock line

	start2 = clock(); 
	// ***************************************** work zone right now, this doesn't test the last chunk, if the last chunk is sized such that it doesn't fit inna block.  will fix this.  right now just getting it to run.
	for (CntBLC_A=0;CntBLC_A<blockscount; CntBLC_A+=BLOCKSIZE)
	{
				 countA=CntBLC_A+BLOCKSIZE;
		/*printf("%i %i\n",CntBLC_A,countA);
		getchar(); */
		// b block loop.  
		for (CntBLC_B=(CntBLC_A+BLOCKSIZE); CntBLC_B < blockscount; CntBLC_B+=BLOCKSIZE) // this line is really critical.  it offsets it to keep if from retesting a datapoint against itself.
		{
			countB=CntBLC_B+BLOCKSIZE;
			//	start algorithm loop, inside block loops
			for(i=CntBLC_A;i<countA; i++)
				//  for (i=0;i<10000; i++)
			{
				/*			printf("%i\n", i);
				printf("%i %i\n",CntBLC_A,countA);
				printf("%i %i\n",CntBLC_B,countB);
				getchar();*/
				//if(i/1000.0==i/1000)
				//	printf("Working on bond %i\n", i);
				//    for (jj=ii;jj<count;jj++)
				//here's where I'm stopping to implement blocking. ------------------FLAG-------------------
				///took out the temporary I structures.  holding off on the next part
				for (j=CntBLC_B; j<countB;j++) // working right here.  issue is that I need to keep if from testing the pair against itself.
				{
					xdiff = ProcArray[i].Bondx-ProcArray[j].Bondx;
					ydiff = ProcArray[i].Bondy-ProcArray[j].Bondy;
					d=(int) sqrt((xdiff*xdiff)+(ydiff*ydiff));
					//|##########|d=xdiff*xdiff+ydiff*ydiff; (phasing sqrt back in for unrepentant convenience)
					//|##########|The infrastructure has been removed because if we do avoid sqrt it'll be using the 2d array
					/*     if (d< S_NUM_BINS)        //deprecated due to superior memory management
					{ */
					//        robcor[d]+=cos(6*(bondsangle[i]-bondsangle[j]));
					robcor[d]+=ProcArray[j].Cos*ProcArray[i].Cos + ProcArray[j].Sin*ProcArray[i].Sin;

					/*Note the change made in the previous two lines, which are identical by the 
					trigonometric identity cos(a-b) = cos(b)*cos(a)+sin(b)*sin(a).  
					There is a significant time penalty for calculating the transcendental functions
					sin() and cos(), and the old way calculates a cosine on order bondcount^2 times.  
					I have paid this penalty up front, calculating sin(6*theta) and cos(6*theta)
					for all bond angles only once, a total of bondcount times.  The calculations I have to 
					do bondcount^2 times are two multiplications and one addition, which is apparently faster.*/
					pairs[d]++;
					//			  }  deprecated if statement


				} //end of j (inner) loop

			}  //end of i (outer) loop
		} // End of b block loop.  Cycles down the list.



	}// end of block A loop, end of algorithm core.
	finish2 = clock(); //!~! finishing the clocking
	timeLoopalone = (double(finish2)-double(start2)); //!~! clock line!	  
	printf("Calculations took %lf clockticks and %lf seconds.\n",timeLoopalone, timeLoopalone/CLOCKS_PER_SEC); //!~! clock line
	printf("Overhead cost %lf clockticks and %lf seconds.\n",timer, timer/CLOCKS_PER_SEC); //!~! clock line
	getchar(); //!~! clock line

	for (i=0;i<NUM_BINS;i++)
		if (pairs[i]>0)
			fprintf(output,"%d     %f      %d\n",i, robcor[i]/pairs[i], pairs[i]);

	fclose(data);
	fclose(output);

}
