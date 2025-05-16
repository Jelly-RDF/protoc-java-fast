# protoc-java-fast

protoc plugin generating highly optimized Java code

**THIS PROJECT IS ARCHIVED -- IT WAS MERGED INTO THE [JELLY-JVM PROJECT](https://github.com/Jelly-RDF/jelly-jvm) WHERE IT'S CONTINUED**

The code here is based on the QuickBuffers project by HEBI Robotics: **https://github.com/HebiRobotics/QuickBuffers**

It was translated from Java to Scala, and then a lot of the code was restructured or removed.

There are also small parts of the code copied or inspired by Google's Protobuf project: **https://github.com/protocolbuffers/protobuf**

Finally, the scalapb project also served as an inspiration for some of the code: **https://github.com/scalapb/ScalaPB**

## Development

- Run the `CrunchyProtocPluginSpec` test in the `generator` module to generate the Java code.
- The code will put in the `test-project` directory.
- You can then try to compile the generated code.
