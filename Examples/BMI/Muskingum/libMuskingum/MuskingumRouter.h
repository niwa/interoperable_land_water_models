#pragma once
#
class MuskingumRouter
{
public:
	MuskingumRouter();
	MuskingumRouter(double, double);
	~MuskingumRouter();
	double flow(double, double);
	void reset(double);

	// sets
	void reachName(std::string aName) { _reachName = aName; }
	void inputVarName(std::string aName) { _inputVarName = aName; }
	void outputVarName(std::string aName) { _outputVarName = aName; }
	void muskX(double x) { _muskX = x; }
	void muskK(double x) { _muskK = x;}
	void startTime(double x) { _lastTime = x; }
	void startFlow(double x) { _lastOutflow = x; _lastInflow = x;}

	// gets
	std::string reachName() { return _reachName; }
	std::string inputVarName() { return _inputVarName; }
	std::string outputVarName() { return _outputVarName; }
	double muskK() { return _muskK; }
	double muskX() { return _muskX; }
	double time() { return _lastTime; }
	double flow() { return _lastOutflow; }

private:
	std::string _reachName;
	std::string _inputVarName;
	std::string _outputVarName;
	double _muskK;
	double _muskX;
	double _lastTime;
	double _lastOutflow;
	double _lastInflow;
};
