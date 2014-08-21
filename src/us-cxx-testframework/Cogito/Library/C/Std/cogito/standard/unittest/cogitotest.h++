//===========================================================================
//
// (C) Copyright 2013 by Cogito Health, Inc.
// All Rights Reserved.  CONFIDENTIAL.
//---------------------------------------------------------------------------
// A set of macros for unit tests. Those macros are defined samiliar to gtest,
// so that they can be easily replaced by gtest as needed.
//
//===========================================================================

// cogito/standard/unittest/cogitotest.h++

#ifndef cogito_standard_unittest_cogitotest_H_
#define cogito_standard_unittest_cogitotest_H_


namespace cogito {
    namespace standard {
        // TODO(sid@cogitocorp.com): refine the doc
        // Test is an abstract class that all the tests inherit from.
        // Test class respresents a single test which can be run independently
        // and in isolation. It uses the Template Method Pattern and
        // encapsulates the following algorithm for executing each test:
        // Test::run() {
        //      Setup();
        //      Body();
        //      Teardown();
        //  }
        //

        class Test {
        public:
            virtual ~Test();
            void Run();
        protected:
            Test();
            virtual void Setup();
            virtual void Teardown();
            virtual void Body() = 0;
        private:
            Test(const Test& src);
            Test& operator=(const Test& src);
        };

    } //namespace standard
} //namespace cogito

#endif //cogito_standard_unittest_cogitotest_H_
