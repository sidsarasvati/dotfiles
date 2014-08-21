#include "cogito/standard/unittest/cogitotest.h++"
#include <iostream>

namespace cogito {
    namespace standard {

        using namespace std;

        Test::Test() {}
        Test::~Test() {}

        void Test::Setup() {
            cout << "Setup: Test\n";
        }

        void Test::Teardown() {
            cout << "Teardown: Test\n";
        }

        void Test::Run() {
            Setup();
            Body();
            Teardown();
        }
    }
}



