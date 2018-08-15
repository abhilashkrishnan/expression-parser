package parser.exp;

import java.util.ArrayList;
import java.util.List;

public class StringExp<T> extends Exp<T> {

	private List<String> entries = new ArrayList<>();
	
	public StringExp() {
		super();
	}
	
	public StringExp(T t) {
		super(t);
	}
	
	public StringExp(List<String> entries) {
		this.entries = entries;
	}
	
	public List<String> getEntries() {
		return this.entries;
	}
	
	public void addEntry(String entry) {
		this.entries.add(entry);
	}
}
