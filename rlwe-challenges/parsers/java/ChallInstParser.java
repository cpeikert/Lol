
// This file prints the contents of challenge and instance files.
// The path to a .challenge file must be provided; the path to a corresponding
// instance file may also be provided. The challenge contents are printed, and
// the instance contents are printed if a path was provided.

// The included jar file was obtained from http://search.maven.org/remotecontent?filepath=com/google/protobuf/protobuf-java/2.6.1/protobuf-java-2.6.1.jar
// > javac -cp protobuf-java-2.6.1.jar crypto/proto/lol/*.java
// > javac -cp .:protobuf-java-2.6.1.jar crypto/proto/RLWE/*.java
// > javac -cp .:protobuf-java-2.6.1.jar crypto/proto/RLWE/challenges/*.java
// > javac -cp .:protobuf-java-2.6.1.jar ChallInstParser.java
// > java  -cp .:protobuf-java-2.6.1.jar ChallInstParser path/to/.challenge [path/to/.instance]

import crypto.proto.RLWE.challenges.*;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintStream;

class ChallInstParser {
  public static void main(String[] args) throws Exception {

    // read a challenge file
    Challenges.Challenge chall =
      Challenges.Challenge.parseFrom(new FileInputStream(args[0]));
    System.out.println(chall);

    if (args.length == 2) {
      // also read an instance file
      if (chall.hasCparams()) {
        Challenges.InstanceCont inst =
          Challenges.InstanceCont.parseFrom(new FileInputStream(args[1]));
        System.out.println(inst);
      }
      else if (chall.hasDparams()) {
        Challenges.InstanceDisc inst =
          Challenges.InstanceDisc.parseFrom(new FileInputStream(args[1]));
        System.out.println(inst);
      }
      else if (chall.hasRparams()) {
        Challenges.InstanceRLWR inst =
          Challenges.InstanceRLWR.parseFrom(new FileInputStream(args[1]));
        System.out.println(inst);
      }
      else {
        System.out.println("Challenge parse error.");
        System.exit(0);
      }
    }




/*
    Challenges.Secret secret =
      Challenges.Secret.parseFrom(new FileInputStream("../chall-id0000-rlwec-m256-q769-l3-short-toy-00.secret"));
    System.out.println(secret);
*/
  }
}
