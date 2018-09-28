namespace DeltaShell.Plugins.TypologyLookup.Views
{
    partial class TypologyTableView
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.tableView1 = new DelftTools.Controls.Swf.Table.TableView();
            ((System.ComponentModel.ISupportInitialize)(this.tableView1)).BeginInit();
            this.SuspendLayout();
            // 
            // tableView1
            // 
            this.tableView1.AllowAddNewRow = true;
            this.tableView1.AllowColumnFiltering = false;
            this.tableView1.AllowColumnPinning = true;
            this.tableView1.AllowColumnSorting = true;
            this.tableView1.AllowDeleteRow = true;
            this.tableView1.AutoGenerateColumns = true;
            this.tableView1.AutoScroll = true;
            this.tableView1.AutoSizeRows = false;
            this.tableView1.ColumnAutoWidth = false;
            this.tableView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableView1.EditButtons = true;
            this.tableView1.HeaderHeigth = -1;
            this.tableView1.IncludeHeadersOnCopy = false;
            this.tableView1.InvalidCellBackgroundColor = System.Drawing.Color.Tomato;
            this.tableView1.IsEndEditOnEnterKey = false;
            this.tableView1.Location = new System.Drawing.Point(0, 0);
            this.tableView1.MultipleCellEdit = true;
            this.tableView1.MultiSelect = true;
            this.tableView1.Name = "tableView1";
            this.tableView1.ReadOnly = false;
            this.tableView1.ReadOnlyCellBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(244)))), ((int)(((byte)(244)))), ((int)(((byte)(244)))));
            this.tableView1.ReadOnlyCellForeColor = System.Drawing.Color.Black;
            this.tableView1.RowHeight = -1;
            this.tableView1.RowSelect = false;
            this.tableView1.RowValidator = null;
            this.tableView1.ShowImportExportToolbar = false;
            this.tableView1.ShowRowNumbers = false;
            this.tableView1.Size = new System.Drawing.Size(401, 315);
            this.tableView1.TabIndex = 0;
            this.tableView1.UseCenteredHeaderText = false;
            this.tableView1.ViewInfo = null;
            // 
            // TypologyTableView
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tableView1);
            this.Name = "TypologyTableView";
            this.Size = new System.Drawing.Size(401, 315);
            ((System.ComponentModel.ISupportInitialize)(this.tableView1)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private DelftTools.Controls.Swf.Table.TableView tableView1;
    }
}
