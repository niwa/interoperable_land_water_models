using System;

namespace BasicModelInterface
{
    /// <summary>
    /// C# version of the Basic Model Interface. API implemented in <see cref="BasicModelInterfaceLibrary"/>.
    /// For C/C++ version of the API see https://github.com/openearth/bmi/blob/master/models/include/bmi.h
    /// </summary>
    public interface IBasicModelInterface
    {
        DateTime StartTime { get; }

        DateTime StopTime { get; }

        DateTime CurrentTime { get; }

        TimeSpan TimeStep { get; }

        /// <summary>
        /// Gets names of all available variables.
        /// </summary>
        string[] VariableNames { get; }

        /*/// <summary>
        /// Injects logger into the model so that it can log messages. A callback method.
        /// </summary>
        Logger Logger { get; set; }*/

        /// <summary>
        /// Initializes model using a given configuration file..
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        int Initialize(string path);

        /// <summary>
        /// Performs simulation.
        /// </summary>
        /// <param name="dt">When default value -1 is used - model computes time step automatically.</param>
        /// <returns></returns>
        int Update(double dt = -1);

        /// <summary>
        /// Finishes model. Deallocates all arrays, closes all handles, frees all other used resources.
        /// </summary>
        /// <returns></returns>
        int Finish();

        /// <summary>
        /// Gets shape of the variable (sizes of all dimensions).
        /// </summary>
        /// <param name="variable"></param>
        /// <returns></returns>
        int[] GetShape(string variable);

        /// <summary>
        /// Gets variable values.
        /// </summary>
        /// <param name="variable"></param>
        /// <returns></returns>
        Array GetValues(string variable);

        /// <summary>
        /// Gets variable values by index (flattened nD index).
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="index"></param>
        /// <returns></returns>
        Array GetValues(string variable, int[] index);

        /// <summary>
        /// Gets variable values by slice (flattened nD array of Rank x NumberOfValues).
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="start"></param>
        /// <param name="count"></param>
        /// <returns></returns>
        Array GetValues(string variable, int[] start, int[] count);

        /// <summary>
        /// Sets variable values.
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="values"></param>
        void SetValues(string variable, Array values);

        /// <summary>
        /// Sets variable values by slice (start + count for every dimension).
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="start"></param>
        /// <param name="count"></param>
        /// <param name="values"></param>
        void SetValues(string variable, int[] start, int[] count, Array values);

        /// <summary>
        /// Sets variable values by indices (flattened nD array of Rank x NumberOfValues).
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="index"></param>
        /// <param name="values"></param>
        void SetValues(string variable, int[] index, Array values);
    }
}