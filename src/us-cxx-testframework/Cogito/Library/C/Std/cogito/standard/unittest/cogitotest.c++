#include "cogito/standard/unittest/cogitotest.h++"
#include <iostream>

namespace cogito {
    namespace standard {

        using namespace std;

        bool MakeAndRegisterTest(std::string test_class_name,
                                 std::string test_name,
                                 TestFactoryBase* factory) {
            UnitTest::GetInstance().
                AddTestCase(test_class_name, test_name, factory);
            return true;
        }


        void UnitTest::AddTestCase(string test_case_name,
                                   string test_name,
                                   TestFactoryBase* factory) {
            LOG_INFO("Entering cogito::standard::UnitTest::AddTestCase()");
            //TODO: create new test case if not alteady exist
            cout << "Registering new tests.\n"
                 << "TestCase Name: " << test_case_name << endl
                 << "Test Name: " << test_name << endl;
            TestCase* tc = new TestCase();
            tc->AddTest(factory);
            m_test_cases.push_back(tc);
            LOG_INFO("Exiting cogito::standard::UnitTest::AddTestCase()");
        }

        void UnitTest::RunAllTests() {
            cout << "Running all(%d) test cases" << m_test_cases.size();

            for(vector<TestCase*>::iterator tc = m_test_cases.begin();
                tc != m_test_cases.end(); tc++) {
                (*tc)->RunAllTests();
            }
        }

        UnitTest& UnitTest::GetInstance() {
            static UnitTest _instance;
            return _instance;
        }

        UnitTest::~UnitTest() {
            LOG_INFO("UnitTest::~UnitTest()")
            //delete all test cases
        }

        TestCase::~TestCase() {
            LOG_INFO("TestCase::~TestCase()")
            //delete all factories
        }

        void TestCase::RunAllTests() {
            cout << "Running all(%d) tests" << m_factories.size();
            for(vector<TestFactoryBase*>::iterator tf = m_factories.begin();
                tf != m_factories.end(); tf++) {
                (*tf)->CreateTest()->Run();
                //TODO: delete test after run?
            }
        }

        void TestCase::AddTest(TestFactoryBase* factory) {
            LOG_INFO("Entering cogito::standard::TestCase::AddTest()")
            LOG_INFO("Adding new test factory");
            m_factories.push_back(factory);
            LOG_INFO("Exiting cogito::standard::TestCase::AddTest()")
        }

        Test::~Test() {}
        Test::Test() {}

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
