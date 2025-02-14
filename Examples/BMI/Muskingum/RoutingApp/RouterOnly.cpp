// RouterOnly.cpp : Runs Muskingum Router directly.
//

#include "stdafx.h"
#include "MuskingumRouter.h"

int main()
{
	MuskingumRouter myRouter = MuskingumRouter();
	double uEntry;
	double timeStep = 1.;
	double flow = 0.;

	printf("Enter Paramters\n Muskingum K: ");
	scanf_s("%lf", &uEntry);
	myRouter.muskK(uEntry);
	printf(" Muskingum X: ");
	scanf_s("%lf", &uEntry);
	myRouter.muskX(uEntry);
	printf("Enter initial flow [Q(0)]: ");
	scanf_s("%lf", &uEntry);
	double time = 0.;
	flow = myRouter.flow(time, uEntry);
	printf("\nEnter additional inflows [I(t)] below.\n");
	printf("Enter a negative value to end.\n\n");

	while (uEntry >= 0) {
		try {
			printf("Q(%f) = %f\n", time, flow);
			time += timeStep;
			printf("I(%f) = ", time);
			scanf_s("%lf", &uEntry);
			flow = myRouter.flow(time, uEntry);
		}
		catch (std::runtime_error e) {
			printf_s(e.what());
			return -1;
		}
	}


	return 0;
}


