# Getting started

## Installing Visual Studio

1. Download Visual Studio Community from https://visualstudio.microsoft.com/downloads/
2. Run the installer. Select the components to be installed (at least *.Net desktop development*)
3. Launch Visual Studio 2017 and sign in with a Microsoft account (or proceed with the 30 day trial)

## Creating a new DeltaShell plugin

Follow the instructions from this [tutorial](https://publicwiki.deltares.nl/display/TOOLS/Create+a+new+Delta+Shell+plugin).

### Remarks

- Replace any occurences of *VolumeModel* with the name of your plugin.
- While creating a new project, select **.NET Framework 4.6.1** instead of 4.0
- You can **skip the NuGet update** part
- Accessing NuGet packages from [https://build.deltares.nl](https://build.deltares.nl/project.html?projectId=DeltaShell&tab=projectOverview) requires a privileged Deltares account, so use the **local files** option instead. The required packages can be found in this repository under `Examples\DeltaShell\DeltaresNuGetPackages\`

### Adding a plugin icon

After completing the tutorial, both ApplicationPlugin and GuiPlugin should be visible in the File > Plugins window. At this point the plugins have no associated image yet. To add a blank image, edit the `*ApplicationPlugin.cs` and `*GuiPlugin.cs` files by adding

```c#
using System.Drawing;
```

and

```c#
public override Image Image
{
    get
    {
        // Blank image
        return new Bitmap(32, 32);
    }
}
```

This will add an empty icon and make the plugin names display nicely in the plugin window.

To add an actual Image, go to the project settings resource page and load your image file. You can refer to it using:

```c#
public override Image Image
{
    get
    {
        return new Bitmap(Properties.Resources.plugin32);
    }
}
```

