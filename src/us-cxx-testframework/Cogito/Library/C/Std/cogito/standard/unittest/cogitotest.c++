#include "cogito/standard/unittest/cogitotest.h++"
#include "cogito/standard/log/logger.h++"

#include <iostream>

namespace cogito {
    namespace standard {

        using namespace std;

        Test::Test() {}
        Test::~Test() {}

        void Test::Setup() {
            LOG_INFO("Setup: Test\n");
        }

        void Test::Teardown() {
            LOG_INFO("Teardown: Test\n");
        }

        void Test::Run() {
            Setup();
            Body();
            Teardown();
        }
    }
}



