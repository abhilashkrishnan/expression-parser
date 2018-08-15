package parser.op;

import parser.exp.Exp;

public interface Unary {

	boolean execute(Exp left, Exp right);
}
