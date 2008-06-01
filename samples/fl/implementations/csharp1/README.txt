A C#-based implementation of FL that covers:
- unmarshaling XML data into objects
- pretty-printing
- evaluation

xsd.exe is used to generated an object model from a schema for FL.
xsd.exe must be in the path.
A typical location of xsd.exe is this:
C:\Program Files\Microsoft SDKs\Windows\v6.0a\bin\xsd.exe.

The implementation has been tested with Visual C# 2008.
The .exe may be built with Visual C# or the command-line compiler.
For the latter, run "make" to test the implementation.
The compiler (csc.exe) must be in the path.
A typical location of the C# compiler is this:
C:\WINDOWS\Microsoft.NET\Framework\v3.5;
