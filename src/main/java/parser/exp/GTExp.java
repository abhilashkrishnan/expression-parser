package parser.exp;

import parser.op.Binary;

public class GTExp<T> extends Exp<T> implements Binary {

	public GTExp() {
		super();
	}

	@Override
	public boolean execute(Exp left, Exp right) {
		return ((int) right.val) > ((int) left.val) ? true : false;
	}

}
