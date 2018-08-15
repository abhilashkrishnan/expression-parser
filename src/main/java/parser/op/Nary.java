package parser.op;

import parser.exp.Exp;

public interface Nary {

	boolean execute(Exp...exps);
}
