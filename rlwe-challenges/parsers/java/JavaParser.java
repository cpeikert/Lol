
// Download the jar file http://search.maven.org/remotecontent?filepath=com/google/protobuf/protobuf-java/2.6.1/protobuf-java-2.6.1.jar
// > javac -cp "protobuf-java-2.6.1.jar" crypto/proto/lol/*.java
// > javac -cp .:protobuf-java-2.6.1.jar -d . JavaParser.java
// > java -cp .:protobuf-java-2.6.1.jar JavaParser

import crypto.proto.lol.*;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintStream;

class JavaParser {
  public static void main(String[] args) throws Exception {
    Challenges.Challenge chall =
      Challenges.Challenge.parseFrom(new FileInputStream("../chall-id0000-rlwec-m256-q769-l3-short-toy.challenge"));
    System.out.println(chall);

    Challenges.InstanceCont1 inst =
      Challenges.InstanceCont1.parseFrom(new FileInputStream("../chall-id0000-rlwec-m256-q769-l3-short-toy-00.instance"));
    System.out.println(inst);

    Challenges.Secret1 secret =
      Challenges.Secret1.parseFrom(new FileInputStream("../chall-id0000-rlwec-m256-q769-l3-short-toy-00.secret"));
    System.out.println(secret);
  }
}
