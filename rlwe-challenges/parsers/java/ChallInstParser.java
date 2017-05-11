
// This file prints the contents of challenge and instance files.
// The path to a .challenge file must be provided; the path to a corresponding
// instance file may also be provided. The challenge contents are printed, and
// the instance contents are printed if a path was provided.
// See comments in the code for examples of how to access message contents.

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
		System.out.println(chall); // print all contents
		// some examples showing how to access message contents
		/*
		System.out.println(chall.getNumInstances()); // access a field
		System.out.println(chall.getParamsCase()); // an enum showing which type of parameter the message contains
		switch (chall.getParamsCase()) {
			case CPARAMS: {
        System.out.println(chall.getCparams().getM());
        break;
      }
      case DPARAMS: {
        System.out.println(chall.getDparams().getNumSamples());
        break;
      }
      case RPARAMS: {
        System.out.println(chall.getRparams().getQ());
        break;
      }
		}
		*/
		if (args.length == 2) {
			// also read an instance file
			if (chall.hasCparams()) {
				Challenges.InstanceCont inst =
					Challenges.InstanceCont.parseFrom(new FileInputStream(args[1]));
				System.out.println(inst); // print all contents
				// it is easy to access any member of the parsed message
        //System.out.println(inst.getSamples(0).getA().getXs(0));
			}
			else if (chall.hasDparams()) {
				Challenges.InstanceDisc inst =
					Challenges.InstanceDisc.parseFrom(new FileInputStream(args[1]));
				System.out.println(inst); // print all contents
			}
			else if (chall.hasRparams()) {
				Challenges.InstanceRLWR inst =
					Challenges.InstanceRLWR.parseFrom(new FileInputStream(args[1]));
				System.out.println(inst); // print all contents
			}
			else {
				System.out.println("Challenge parse error.");
				System.exit(0);
			}
		}
	}
}
