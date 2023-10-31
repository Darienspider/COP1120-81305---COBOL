// Chapter7Assignment.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

/*
Design, code, and test a program that accepts positive integers until the special value 999 is entered. After input terminates, the 
program should report the total number of even integers (excluding the special value) , the average value of the even integers, 
the total number of odd integers, and the average value of the odd integers. The program should also report "Yowza, there were more evens!", 
"Sheesh, there were more odds!", or "It was a crazy tie!" based on which total was greater. Test your program thoroughly and submit output with at 
least 9 integers as input. Make sure your loop is a structured loop.
*/

#include <stdio.h>
#include <stdbool.h> // Enables the usage of bool
#define MAX_SIZE 100 // Maximum size of the list


int main()
{
    int evenCounter = 0;
    int evenNumbers[MAX_SIZE]; // a list of integers

    int oddCounter = 0;
    int oddNumbers[MAX_SIZE]; // a list of integers

    int requestedNumber;
    int stoppingNumber = 999;
    bool invalidNumber = true;
    bool numberIsGuessed = false;
    
    while (numberIsGuessed == false) {
        printf_s("Please enter a positive number: ");
        scanf_s("%d", &requestedNumber);

        if (requestedNumber < 0 || requestedNumber > stoppingNumber) {
            printf("The number entered is not valid. Please try again. \n");
            continue; //jumps to the next iteration
        }
        
        if (requestedNumber == stoppingNumber) {
            numberIsGuessed = true;
        }

        if (requestedNumber % 2 == 1) {
            //if the number is odd
            oddNumbers[oddCounter] = requestedNumber;
            oddCounter += 1;

        }
        else {
            evenNumbers[evenCounter] = requestedNumber;
            evenCounter += 1;
            
        }

    }

    //number of even/odd integers(excluding the special value)
    if (stoppingNumber % 2 == 0) {
        evenCounter - 1;
    }
    else {
        oddCounter - 1;
    }


    //average calculations
    float evenAverage = 0;
    int evenTempTotal = 0;

    for (int i = 0; i < evenCounter; i++) {
        evenTempTotal += evenNumbers[i];
    }
    evenAverage = evenTempTotal / evenCounter;


    //average calculations
    float oddAverage = 0;
    int oddTempTotal = 0;

    for (int i = 0; i < oddCounter; i++) {
        oddTempTotal += oddNumbers[i];
    }
    oddAverage = oddTempTotal / oddCounter;






    //message printer
    if (evenCounter > oddCounter) {
        printf("Yowza, there were more evens!");
    }
    else if (evenCounter < oddCounter) {
        printf("Sheesh, there were more odds!");
    }
    else {
        printf("It was a crazy tie!");
    }





    printf("\nEven numbers Guessed: %d", evenCounter);
    printf("\Average of the Even numbers Guessed: %f", evenAverage);

    printf("\nOdd numbers Guessed: %d", oddCounter);
    printf("\nAverage of the Odd numbers Guessed: %f", oddAverage);


    return 0;

}
