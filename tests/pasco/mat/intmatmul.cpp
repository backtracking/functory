#include <iostream>
#include <fstream>
#include <cstdlib>
#include "gmpxx.h"

using namespace std;

/**
 * Read an integer matrix from a file, and sets M and N its row and col dimension
 */
void readMatrix(char* filename, mpz_class*& A, size_t& M, size_t& N)
{
    string s;
    ifstream fs(filename, ios::in);
    if (fs){
        fs >> M;
        fs >> N;
        
        size_t i=0, j=0;
        A = new mpz_class[M*N];
        string s;
        fs>>s;
        while (!fs.eof()){
            *(A+i*N+j) = mpz_class(s);
            j++;
            if (j == N){
                j = 0;
                i++;
            }
            fs>>s;
        }
        
        fs.close();
    }else{
        cerr<<"Problem opening file: "<<filename<<endl;
        exit(1);
        
    }
    
}

/**
 * Write an integer matrix to a file, 
 */
void writeMatrix (char* filename, mpz_class* A, size_t M, size_t N, size_t lda)
{
    ofstream fs(filename);
    fs<<M;
    fs<<" ";
    fs<<N;
    fs<<endl;
    for (size_t i=0; i<M; ++i)
        for (size_t j=0; j<N; ++j)
            fs<<*(A+i*lda+j)<<endl;
    
    fs.close();
        
}

/**
 * Computes the product C = A x B
 * A is MxK
 * B is KxN
 * C is MxN and has already been initialized
 * the lda, ldb, ldc parameters are the leading dimension: it is the increment to move 
 * by one position along a column, i.e. A[i,j] is accessed by *(A+i*lda+j)
 */
mpz_class* IntMatMul (mpz_class* C, const size_t ldc,
                        const mpz_class* A, const size_t lda,
                        const mpz_class* B, const size_t ldb,
                        const size_t M,
                        const size_t K,
                        const size_t N){

    for (size_t i = 0; i < M; ++i)
        for (size_t j = 0; j < N; ++j)
            *(C + i * ldc + j) = 0;
    for (size_t i = 0; i < M; ++i)
        for (size_t l = 0; l < K ; ++l)
            for (size_t j = 0; j < N; ++j)
                    //z [i * n + j] += x [i * n +l] * y [l * n + j];
                mpz_addmul ((C + i * ldc + j)->get_mpz_t(), (A + i * lda +l)->get_mpz_t(), (B + l * ldb + j)->get_mpz_t());
	return C;
}

int main(int argc, char** argv)
{
    if (argc!=4){
        cerr<<"Usage: intmatmul A B C"<<endl;
        exit(1);
    }
    size_t M,N,K;
    mpz_class* A, *B, *C;
    
    readMatrix (argv[1], A, M,K);
    if (M != K){
        cerr<<"Matrix A is not square"<<endl;
        exit(-1);
    }
    readMatrix (argv[2], B, K,N);
    if (K != N){
        cerr<<"Matrix B is not square"<<endl;
        exit(-1);
    }
    if (M != N){
        cerr<<"Matrix dimensions do not match"<<endl;
        exit(-1);
    }
    
    C = new mpz_class[N*N];

    IntMatMul (C, N, A, N, B, N, N, N ,N);
    
    writeMatrix(argv[3], C, N, N, N);
    
    delete[] A;
    delete[] B;
    delete[] C;
    
    
}
