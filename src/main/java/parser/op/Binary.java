package parser.op;

import parser.exp.Exp;

public interface Binary {
	
	boolean execute(Exp left, Exp right);
}
