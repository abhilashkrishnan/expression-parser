package parser.exp;

import java.util.Stack;

import parser.op.Binary;
import parser.op.Nary;

public class ORExp<T> extends Exp<T> implements Binary, Nary {

	public ORExp() {
		super();
	}

	@Override
	public boolean execute(Exp left, Exp right) {
		return ((boolean) left.val) || (boolean) (right.val);
	}

	@Override
	public boolean execute(Exp... exps) {

		boolean res = false;
		Stack<Boolean> stack = new Stack<>();

		for (int i = 0; i < exps.length; i++) {
			stack.push(((boolean) exps[i].val));
		}

		int i = 0;
		while (i <= stack.size()) {
			if (stack.size() == 1)
				res = stack.pop();
			else {
				boolean res1 = stack.pop();
				boolean res2 = stack.pop();
				boolean res12 = res1 || res2;
				res = stack.push(res12);
				i++;
			}
		}

		return res;
	}
}
