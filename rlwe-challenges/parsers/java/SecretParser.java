
// This file prints the contents of a secret file.
// The path to a .secret file must be provided.
// See comments in the code for examples of how to access message contents.

// The included jar file was obtained from http://search.maven.org/remotecontent?filepath=com/google/protobuf/protobuf-java/2.6.1/protobuf-java-2.6.1.jar
// > javac -cp protobuf-java-2.6.1.jar crypto/proto/lol/*.java
// > javac -cp .:protobuf-java-2.6.1.jar crypto/proto/RLWE/*.java
// > javac -cp .:protobuf-java-2.6.1.jar crypto/proto/RLWE/challenges/*.java
// > javac -cp .:protobuf-java-2.6.1.jar SecretParser.java
// > java  -cp .:protobuf-java-2.6.1.jar SecretParser path/to/.secret

import crypto.proto.RLWE.challenges.*;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintStream;

class SecretParser {
  public static void main(String[] args) throws Exception {

    // read a secret file
    Challenges.Secret secret =
      Challenges.Secret.parseFrom(new FileInputStream(args[0]));
    System.out.println(secret);
    //System.out.println(secret.getS().getXs(0));
  }
}
