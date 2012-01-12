class A {
	void test (int a, int b) {}
	int arg1 () {
		System.out.print ("a") ;
		return 1 ;
	}
	int arg2 () {
		System.out.print ("b") ;
		return 1 ;
	}
}
class Main {
	public static void main( String args[] ) {
		A a = new A() ;
		a.test( a.arg1() , a.arg2() );
	}
}
