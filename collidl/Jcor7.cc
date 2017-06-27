#define NUM_BINS 5794 // should get this to be dynamically set, but that could be difficult
#include <math.h>
#include <stdio.h>
#include <ctime> 
#include <stdlib.h> 
#include <algorithm>// STL template library, copyright all appropriate.
#include <xmmintrin.h>
using namespace std;//makes the code a little more readable by avoiding the :: scope operators.

/*  Kind of an ISSUE.  Right now, the pointer array used to build proc array has one extra spot at the end.
I will fix this momentarily, but it may require a rebuild of the logic involved.  Doh.
*/



//timer implementation! flagging all related code with !~! in commenting
//ex:  //!~!
clock_t start3,finish3,start2,finish2, start1, finish1; // !~!
double timeLoopalone, timeSpecialCases, timeOverhead; //this is going to be printed at the end.  be careful please and remember that.

// core operating constants are declared here. change with caution
const int BLOCKSIZE=64; // controls the chunklet sized processed in a single go.  Intended to improve cache handling and locality.  Used with prefetching commands.


//Declaration of the new struct, used to cradle our beloved data.  It also makes dynamic memory allocation a lot tighter to code, and allows
// padding and easier chunking of data.  Should simplify our caching endeavor immensely.
struct InitialStorage {
	float Bondx, Bondy, BondAngle;
	//need to talk to Kelly about how much padding we will need. If any.
};
struct ProcessingStorage {
	int Bondx, Bondy, Cos, Sin; 
	//need to talk to Kelly about how much padding we will need.  If any.
};

// these define boolean operations for the purpose of the sort command. if statement sorts the y, and also the x.
bool operator<(const ProcessingStorage& a, const ProcessingStorage& b) {
//	if(a.Bondy == b.Bondy){
//	return a.Bondx < b.Bondx;
//	}
//	else
//	{
	return a.Bondy < b.Bondy;
//	}
}

bool operator>(const ProcessingStorage& a, const ProcessingStorage& b) {
    return a.Bondy > b.Bondy;
}

bool operator<=(const ProcessingStorage& a, const ProcessingStorage& b) {
    return a.Bondy <= b.Bondy;
}

bool operator >=(const ProcessingStorage& a, const ProcessingStorage& b) {
    return a.Bondy >= b.Bondy;
}
struct RefResults {
	long Pairs, Value; 
	//need to talk to Kelly about how much padding we will need.  If any.
};
typedef struct RefResults RefResults; //carried over from c code, harmless.

/* float bondsx[MAX_BONDS], bondsy[MAX_BONDS], bondsangle[MAX_BONDS];
float bonds_cos[MAX_BONDS], bonds_sin[MAX_BONDS];
these WERE global variables, to avoid a stack overflow when is large.
_Now_ we handle it with the New operator inside the prog, and a pointer to the previously declared structs*/


//RefResults **Results; // constructed with the new operator inside the program.
//RefResults Results[4096][4096];
RefResults Results[16777216];
//RefResults *finaloutput;
RefResults finaloutput[NUM_BINS];
void main ()
{
	start1= clock(); //!~! clock line
	FILE* data;
	FILE* output;
	int bondcount, blockscount;
	InitialStorage * IntoBox;
	ProcessingStorage * ProcArray; // ProcessingArray the array calculations should actually be done on
	int dummy, dummy1, dummy2;
	int Rcolumns=0,Rrows=0;
	int OutputLength=0;
	int countA=0, multiplier=0, countB=0, CntBLC_A=0, CntBLC_B=0;//countA countB, and CntBLC_A as well as CntBLC_B have changed in function considerably.
	register int d;   //These register declarations are probably useless, but are also harmless.
//	register int xdiff,ydiff;  //this has GOT to stop being floats. fsckfsckfsck sqrt is float only. 
	register int i,j, jj, jplus, jplus2, jplus3, jplus4, jplus5, jplus6, jplus7;
	int x_i, y_i, cos_i, sin_i;
	int results_index, results_index1, results_index2, results_index3, results_index4, results_index5, results_index6, results_index7;
	int spacingcounter=0; // this is used in the newly reconstructed main loop to shift the structure around to follow the second mode.
	// basically it spaces it out to keep spatial locality on the otherwise cache-blowing results array
	
	
	
	
	data=fopen("bonds.dat","r");
	multiplier=4;  //defaults the multiplier.  clinically useless but I'm keeping it, just like we keep politicians
	fscanf(data, "%i", &multiplier); // reads the first line and makes it the multiplier.  Rather important I suspect as elsewise it fuckerates things
	fscanf(data,"%i",&bondcount); // is intended to grab the bonds number.  Works.
	printf("There are %i bonds being used.\n",bondcount);
	bondcount=(bondcount/multiplier);
	IntoBox = new InitialStorage [bondcount];
	ProcArray = new ProcessingStorage [bondcount];
	for (i=0; i<(bondcount); i++) {
		for (jj=0;jj<(multiplier-1);jj++)
			fscanf(data,"%f %f %f", &dummy, &dummy1, &dummy2);
		fscanf(data,"%f %f %f", &IntoBox[i].Bondx, &IntoBox[i].Bondy, &IntoBox[i].BondAngle);
	}
	//printf("done with reading bonds\n");
	//shifts the data over and performs the cos and sine calculations.  Then we destroy the original storage array.  This is
	// done to keep the array absolutely as small as we can.  could be better optimized, but written this way for ease of maintence.
printf("There are %i bonds being used after multiplier.\n",bondcount);

	for (i=0; i<bondcount; i++) 
	{

		ProcArray[i].Cos = cos(6*IntoBox[i].BondAngle)*100; //fixed point math
		ProcArray[i].Sin = sin(6*IntoBox[i].BondAngle)*100; // fixed point math
		ProcArray[i].Bondx = IntoBox[i].Bondx;
		ProcArray[i].Bondy = IntoBox[i].Bondy*4096;
		if(Rcolumns<ProcArray[i].Bondx) {Rcolumns=ProcArray[i].Bondx;}
	}
	delete [] IntoBox;
   // sort implemented here!
	sort(ProcArray, (ProcArray+(bondcount+1))); // this SHOULD give the C++ std sort the address it needs for the end of sort, which is the address AFTER last address in the array.
	// a brief reminder: an array's name when used as a variable is a pointer to the first unit in the array.  array indexing is done with size of array component already
	// taken into account.  this taken together makes the arithmetic above work.  ~jake
	//I've been thinking and THIS may be what was causing that very strange final value,
	//as I think it may actually overshoot the final array spot, sort a strange value in, and then "meander off."

	//sort(ProcArray, (ProcArray+(bondcount+1))); // this will then (probably) sort along x.

	Rcolumns+=10;
	
	Rrows=ProcArray[bondcount-1].Bondy+10; // if there is a crash, it may derive from here or the related lines that create the
	//results array.  There was a crash issue with this, it's a little worrisome how it happened though.  It didn't fire an exception properly here
	// instead it crashed later during the construction of the dynamic array.  Which suggests that the very last cell in proc array is either still a pointer
	// or is never getting assigned.  This isn't terrible, as I don't think the logic ever calls the last array as I wrote it thinking (apparently) that
	// the array was one shorter than it actually was.


	// creates the results array
	/*Results = new RefResults *[Rrows]; // might be able to use int here instead of refresults.  I'm not sure about the syntax precisely
		for ( i=0; i < Rrows; i++)	Results[i] = new RefResults [Rcolumns]; 
		  // creates the final output array
		  if(Rcolumns<Rrows)
		  {
			finaloutput = new RefResults [(int)((sqrt(2.0)*(Rcolumns))+2)];
			OutputLength=(int)((sqrt(2.0)*(Rcolumns))+2);
		  }
		  else
		  {
			finaloutput = new RefResults [(int)((sqrt(2.0)*(Rrows))+2)];
			OutputLength=(int)((sqrt(2.0)*(Rrows))+2);
		  }
	*/
	blockscount=bondcount-(bondcount%BLOCKSIZE);
	printf("This produces %i after ensuring full blocks.\n", blockscount);

	finish1 = clock(); //!~! clock line
	start2 = finish1; 
	
	for (spacingcounter=0; spacingcounter<(blockscount)-BLOCKSIZE; spacingcounter+=BLOCKSIZE)// issue is here.  problems with this logic format, all in all.  might need to rework it.
	{
		// b block loop.
		countB=0;
		countA=0;
		CntBLC_B=0;

		for (CntBLC_A=0; countB < (blockscount+1)-BLOCKSIZE; CntBLC_A+=BLOCKSIZE) // next issue to look at.  must pwn. lawl.
		{
			countA=CntBLC_A+BLOCKSIZE;
			CntBLC_B=countA+spacingcounter;
			countB=CntBLC_B+BLOCKSIZE;
			//put prefetch of entire B block (j loop) here

		//	start algorithm loop, inside block loops
//prefetch for the entire inner loop for the NEXT i
			for(jj=0; jj<BLOCKSIZE; jj+=2)
				_mm_prefetch((char*)&(ProcArray[countB+jj].Bondx),_MM_HINT_NTA);

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
//prefetch of following i goes here:
				_mm_prefetch((char*)&(ProcArray[i+1].Bondx),_MM_HINT_NTA); 
				x_i = ProcArray[i].Bondx;
				y_i = ProcArray[i].Bondy;
				cos_i = ProcArray[i].Cos;
				sin_i = ProcArray[i].Sin;

				for (j=CntBLC_B; j<countB;j+=8) // working right here.  issue is that I need to keep if from testing the pair against itself.
				{
					//the following line hashing it across a two dimensional array.  thanks to the way these things are stored in C++ it actually can be a gain.
//printf("pair is %d, %d\n",i,j);
					//By manually "unrolling" the inner loop by a factor of 8, we saw a speed gain from 
					//11 clock cycles per pair to about 11 clock cycles per pair.

					jplus = j+1;
					jplus2 = j+2;
					jplus3 = j+3;
					jplus4 = j+4;
					jplus5 = j+5;
					jplus6 = j+6;
					jplus7 = j+7;

					results_index = abs(ProcArray[j].Bondx - x_i) + ProcArray[j].Bondy - y_i;
					Results[results_index].Value += ProcArray[j].Cos * cos_i + ProcArray[j].Sin * sin_i;
					Results[results_index].Pairs++;

					results_index1 = abs(ProcArray[jplus].Bondx - x_i) + ProcArray[jplus].Bondy - y_i;
					Results[results_index].Value += ProcArray[jplus].Cos * cos_i + ProcArray[jplus].Sin * sin_i;
					Results[results_index].Pairs++;

					results_index2 = abs(ProcArray[jplus2].Bondx - x_i) + ProcArray[jplus2].Bondy - y_i;
					Results[results_index].Value += ProcArray[jplus2].Cos * cos_i + ProcArray[jplus2].Sin * sin_i;
					Results[results_index].Pairs++;

					results_index3 = abs(ProcArray[jplus3].Bondx - x_i) + ProcArray[jplus3].Bondy - y_i;
					Results[results_index].Value += ProcArray[jplus3].Cos * cos_i + ProcArray[jplus3].Sin * sin_i;
					Results[results_index].Pairs++;

					results_index4 = abs(ProcArray[jplus4].Bondx - x_i) + ProcArray[jplus4].Bondy - y_i;
					Results[results_index].Value += ProcArray[jplus4].Cos * cos_i + ProcArray[jplus4].Sin * sin_i;
					Results[results_index].Pairs++;

					results_index5 = abs(ProcArray[jplus5].Bondx - x_i) + ProcArray[jplus5].Bondy - y_i;
					Results[results_index].Value += ProcArray[jplus5].Cos * cos_i + ProcArray[jplus5].Sin * sin_i;
					Results[results_index].Pairs++;

					results_index6 = abs(ProcArray[jplus6].Bondx - x_i) + ProcArray[jplus6].Bondy - y_i;
					Results[results_index].Value += ProcArray[jplus6].Cos * cos_i + ProcArray[jplus6].Sin * sin_i;
					Results[results_index].Pairs++;

					results_index7 = abs(ProcArray[jplus7].Bondx - x_i) + ProcArray[jplus7].Bondy - y_i;
					Results[results_index].Value += ProcArray[jplus7].Cos * cos_i + ProcArray[jplus7].Sin * sin_i;
					Results[results_index].Pairs++;
				} //end of j (inner) loop

			}  //end of i (outer) loop
	//		printf("There are bonds being used 4. %i %i %i %i %i\n", spacingcounter,CntBLC_A, countA, CntBLC_B, countB);

		} // End of b block loop.  Cycles down the list.


	}// end of block A loop, end of algorithm core.



finish2 = clock();
start3 = finish2;
//special case one, was covered by the array offset but that seems to have been double counting.  this tests block a against block a.
//printf("special case 1\n");

	for (CntBLC_A=0;CntBLC_A<blockscount; CntBLC_A+=BLOCKSIZE)
	{
				 countA=CntBLC_A+BLOCKSIZE;
			for(i=CntBLC_A;i<countA; i++)
			{
				x_i = ProcArray[i].Bondx;
				y_i = ProcArray[i].Bondy;
				cos_i = ProcArray[i].Cos;
				sin_i = ProcArray[i].Sin;
				
				for (j=i+1; j<countA;j++) // working right here.  issue is that I need to keep if from testing the pair against itself.
				{
//printf("pair is %d, %d\n",i,j);
					results_index = abs(ProcArray[j].Bondx - x_i) + ProcArray[j].Bondy - y_i;
					Results[results_index].Value += ProcArray[j].Cos * cos_i + ProcArray[j].Sin * sin_i;
					Results[results_index].Pairs++;
				} //end of j (inner) loop
			}  //end of i (outer) loop
	}// end of block A loop, end of algorithm core.
//printf("special case 2\n");

// tests the ragged end against itself.  will, deliciously enough, fail if there is no ragged end.
			for(i=blockscount;i<bondcount; i++)
			{
				x_i = ProcArray[i].Bondx;
				y_i = ProcArray[i].Bondy;
				cos_i = ProcArray[i].Cos;
				sin_i = ProcArray[i].Sin;
				
				for (j=i+1; j<bondcount;j++) // working right here.  issue is that I need to keep if from testing the pair against itself.
				{
//printf("pair is %d, %d\n",i,j);
					results_index = abs(ProcArray[j].Bondx - x_i) + ProcArray[j].Bondy - y_i;
					Results[results_index].Value += ProcArray[j].Cos * cos_i + ProcArray[j].Sin * sin_i;
					Results[results_index].Pairs++;
				} //end of j (inner) loop
			}  //end of i (outer) loop

//printf("special case 3\n");

// this MAY crash, might I mention.  Only in extremely special cases of ragged ends of zero or 1.  I don't think it will though.			
// tests the ragged end against the blocks from zitherelse
	for (CntBLC_A=0;CntBLC_A<blockscount; CntBLC_A+=BLOCKSIZE)
	{
		countA=CntBLC_A+BLOCKSIZE;
			for(i=blockscount;i<bondcount; i++)
			{
				x_i = ProcArray[i].Bondx;
				y_i = ProcArray[i].Bondy;
				cos_i = ProcArray[i].Cos;
				sin_i = ProcArray[i].Sin;
				
				for (j=CntBLC_A; j<countA;j++) // working right here.  issue is that I need to keep if from testing the pair against itself.
				{
//printf("pair is %d, %d\n",i,j);
					results_index = abs(ProcArray[j].Bondx - x_i) + abs(ProcArray[j].Bondy - y_i); // there's a second abs
					//here because this is sorted across the array where Y J is lower than Y I.  You could just reverse the statement, but I felt it was simply safest to write it this way.
					Results[results_index].Value += ProcArray[j].Cos * cos_i + ProcArray[j].Sin * sin_i; // crash occurs here because this goes back across lower y values...
					Results[results_index].Pairs++;
				} //end of j (inner) loop
			}  //end of i (outer) loop
	}

finish3 = clock(); //!~! finishing the clocking

	
	timeOverhead = (double(finish1)-double(start1))/CLOCKS_PER_SEC; //!~! clock line!
	timeLoopalone = (double(finish2)-double(start2))/CLOCKS_PER_SEC; //!~! clock line!
	timeSpecialCases = (double(finish3)-double(start3))/CLOCKS_PER_SEC; //!~! clock line!
	printf("Overhead cost %lf seconds.\n",timeOverhead); //!~! clock line
	double numpairs = double(blockscount)*double(blockscount)/2.0;
	printf("Main loop Calculations took %lf seconds and %lf clockcycles per pair.\n",timeLoopalone, (timeLoopalone*2200000000.0)/numpairs); //!~! clock line
	printf("Special cases took %lf seconds.\n",timeSpecialCases); //!~! clock line
	
	getchar(); //!~! clock line

	// transforms the 2d to a 1 d.
	for (i=0;i<4096;i++)
	{
		for (j=0;j<4096;j++)
		{
			if(Results[i*4096+j].Pairs>0)
			{ 
			d=sqrt((float)(i*i+j*j));
			finaloutput[d].Value+=Results[i*4096+j].Value;
			finaloutput[d].Pairs+=Results[i*4096+j].Pairs;			
			}
		}
	}

	// outputs a the 1d.
	fclose(data);
	output=fopen("robcor.dat","w"); // remember you did this, jake.  idiot.  Sincerely, Jake.
	
	for (i=0;i<NUM_BINS;i++)
	{
		// this reverses the fixed point math used to save time, so we get our precision and our cake too!
		if (finaloutput[i].Pairs>0)	fprintf(output,"%i     %f      %i\n",i, (finaloutput[i].Value/finaloutput[i].Pairs)/10000.0, finaloutput[i].Pairs); //conversion to float will eventually happen here.  It's in but it may be goofy.
	
	}
	fclose(output);


}
