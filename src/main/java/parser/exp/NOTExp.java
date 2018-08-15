package parser.exp;

import java.util.Stack;

import parser.op.Nary;
import parser.op.Unary;

public class NOTExp<T> extends Exp<T> implements Unary, Nary {

	public NOTExp() {
		super();
	}

	public NOTExp(T t) {
		super(t);
	}

	@Override
	public boolean execute(Exp left, Exp right) {

		if (right == null) {
			return !(boolean) ((NOTExp) left).val;
		} else
			throw new RuntimeException("RValue should be empty.");
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
				boolean res1 = !stack.pop();
				boolean res2 = !stack.pop();
				boolean res12 = res1 && res2;
				res = stack.push(res12);
				i++;
			}
		}

		return res;
	}
}
