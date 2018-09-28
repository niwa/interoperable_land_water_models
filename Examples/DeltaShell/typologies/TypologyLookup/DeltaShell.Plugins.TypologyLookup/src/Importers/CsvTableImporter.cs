using System;
using System.Collections.Generic;
using System.Drawing;
using DelftTools.Shell.Core;
using log4net;

using DeltaShell.Plugins.TypologyLookup.DataObjects;
using Microsoft.VisualBasic.FileIO;
using System.IO;
using DelftTools.Utils.Collections.Generic;

namespace DeltaShell.Plugins.TypologyLookup.Importers
{
    class CsvTableImporter : IFileImporter
    {
        // Handle for writing log messages
        private static readonly ILog Log = LogManager.GetLogger(typeof(CsvTableImporter));

        /// <summary>
        /// The name of the importer
        /// </summary>
        /// <remarks>Used in importer selection dialogs</remarks>
        public string Name => "Typology Lookup CSV Table Importer";

        /// <summary>
        /// The category of the importer
        /// </summary>
        /// <remarks>Used in importer selection dialogs</remarks>
        public string Category => "Typology Lookup Importers";
        
        /// <summary>
        /// The image of the importer
        /// </summary>
        /// <remarks>Used in importer selection dialogs</remarks>    
        public Bitmap Image => Properties.Resources.import_csv16;

        /// <summary>
        /// The data types supported by the importer
        /// </summary>
        public IEnumerable<Type> SupportedItemTypes
        {
            get
            {
                yield return typeof(TypologyTable);
            }
            
        }

        /// <summary>
        /// Indicates that the importer can import at root level (folder/project). In other
        /// words, indicates that the <see cref="ImportItem"/> method can be called without
        /// specifying a target...
        /// </summary>
        public bool CanImportOnRootLevel => false;

        /// <summary>
        /// The file filter of the importer
        /// </summary>
        /// <remarks>Used in file selection dialogs</remarks>
        public string FileFilter => "CSV files|*.csv";

        /// <summary>
        /// Path where external data files can be copied into
        /// </summary>
        public string TargetDataDirectory { get; set; }

        /// <summary>
        /// Whether or not an import task should be cancelled
        /// </summary>
        public bool ShouldCancel { get; set; }

        /// <summary>
        /// Fired when progress has been changed
        /// </summary>
        public ImportProgressChangedDelegate ProgressChanged { get; set; }

        /// <summary>
        /// Whether or not to try and open a view for the imported item
        /// </summary>
        public bool OpenViewAfterImport => false;

        /// <summary>
        /// Check if an import can be done on the targetObject
        /// </summary>
        /// <param name="targetObject">Object to check</param>
        public bool CanImportOn(object targetObject)
        {
            return targetObject is TypologyTable;
        }

        /// <summary>
        /// Imports csv data from the file with path <paramref name="path"/> to the
        /// typology table <paramref name="target"/>
        /// </summary>
        /// <remarks>
        /// The target parameter is optional. If a target table is specified, the
        /// importer should import the csv data to this existing table. When
        //  no target is set, the importer should create a new typology table.
        /// </remarks>
        public object ImportItem(string path, object target = null)
        {
            // Check the file path
            if (!File.Exists(path))
            {
                Log.Error("File does not exist");
                return null;
            }
            // Obtain a new typology table or check the provided target for being a typology table
            var table = target == null ? new TypologyTable() : target as TypologyTable;
            if (table == null)
            {
                Log.Error("Target is of the wrong type (should be time series)");
                return null;
            }
            else
            {
                table.Clear();
            }

            using (TextFieldParser parser = new TextFieldParser(path))
            {
                parser.TextFieldType = FieldType.Delimited;
                parser.SetDelimiters(";");
                // Check header has right number of fields
                if (parser.EndOfData)
                {
                    Log.Error("File is empty");
                    return null;
                }
                string[] fields = parser.ReadFields();
                if (fields.Length != 3)
                {
                    Log.Error("Typology CSV file should have exactly 3 columns");
                    return null;
                }
                // Process data rows
                while (!parser.EndOfData)
                {
                    fields = parser.ReadFields();
                    table.AddRow(fields[0], Convert.ToDouble(fields[1]), Convert.ToDouble(fields[2]));
                }
            }

            // Return the typology table
            return table;
        }
    }
}
