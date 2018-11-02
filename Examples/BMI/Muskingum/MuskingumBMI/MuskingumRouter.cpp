#include "stdafx.h"
#include "MuskingumRouter.h"
#include <assert.h>


MuskingumRouter::MuskingumRouter()
{
	_lastTime = 0.;
	_lastOutflow = 0.;
	_inputVarName = "volume_inflow_rate";
	_outputVarName = "volume_outflow_rate";
}

MuskingumRouter::MuskingumRouter(double init_time, double init_flow)
{
	_lastTime = init_time;
	_lastOutflow = init_flow;
}


MuskingumRouter::~MuskingumRouter()
{
}

double MuskingumRouter::flow(double time, double inflow)
{
	if (time < 0. || time < _lastTime)
	{
		std::string msg = "Reverse time not supported.";
		throw std::runtime_error(msg);
	}

	// special case for time 0 if not initialized: initial flow is assumed to be 
	// steady-state, so flow now is the same as inflow, and 
	// has been indefinitely
	if (time == 0. && _lastTime == 0.) {
		_lastOutflow = inflow;
		_lastInflow = inflow;
		_lastTime = 0.;
		return inflow;
	}

	double dt = time - _lastTime;
	double c1 = (dt - 2 * _muskK*_muskX) / (2 * _muskK*(1. - _muskX) + dt);
	double c2 = (dt + 2 * _muskK*_muskX) / (2 * _muskK*(1. - _muskX) + dt);
	double c3 = (2 * _muskK*(1. - _muskX) - dt) / (2 * _muskK*(1. - _muskX) + dt);

	// Validation 
	assert(c1 + c2 + c3 == 1.0); // sums to 1.0

	// feasible region criteria:
	// musk_k*musk_x >= dt / 2. >= musk_k*(1. - musk_x)
	// consider making the model approximate K = dt by dividing into sub-
	// reaches automatically
	if (dt < 2.*_muskK*_muskX) {
		std::string msg = "Muskingum feasibility violation: KX too large for time step.";
		throw std::runtime_error(msg);
	}
	if (dt > 2*_muskK * (1. - _muskX)) {
		std::string msg = "Muskingum feasibility violation: KX too small for time step.";
		throw std::runtime_error(msg);
	}

	double flow = c1 * inflow + c2 * _lastInflow + c3 * _lastOutflow;

	_lastTime = time;
	_lastOutflow = flow;
	_lastInflow = inflow;
	return flow;
}

void MuskingumRouter::reset(double flow)
{
	_lastTime = 0.;
	_lastInflow = flow;
	_lastOutflow = flow;
}

