class A {
	A () {
		System.out.print("Création de a\n");
	}
	int a ;
}

class B {
	int renvoit_zero() {
		System.out.print("Renvoit 0\n");
		return 0 ;
	}
}

class Main {
	public static void main(String args[]) {
		B b = new B() ;
		(new A() ).a = b.renvoit_zero() ;
	}
}
