class A {
    int x;

    A() { }

    A(int x) { this.x = x; }
    
}

class B extends A {
    int x;

    B() { }

    B(int x) {
	this.x = x; 
	((A)this).x = x;
    }

}

class Main {
    public static void main(String args[]) {
	A a = new A(1);
    }
}
