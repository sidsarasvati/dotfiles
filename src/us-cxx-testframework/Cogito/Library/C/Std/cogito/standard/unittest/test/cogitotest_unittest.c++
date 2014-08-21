#include "cogito/standard/unittest/cogitotest.h++"
#include <iostream>

typedef cogito::standard::Test Test;
using namespace std;

class MyTest : public Test {

    virtual void Body() {
        cout << "Body: MyTest\n";
    }
};

int main(int argc, char** argv) {
    cout << "## COGITOTEST UNITTEST ##\n";
    cout << "Creating Test instance..";
    MyTest t1;
    cout << "SUCCESS\n";
    cout << "Executing Test...\n\n";
    t1.Run();
    cout <<" \n ## ALL DONE ## \n\n";
}
