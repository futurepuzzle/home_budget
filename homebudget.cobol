       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDGET.
       AUTHOR. SPEAKER
      *for figuring out a budget. currently limited to max. 2 earners

       ENVIRONMENT DIVISION.

       DATA DIVISION.
      *BLANK FOR NOW.
       WORKING-STORAGE SECTION.
       01 USER1 PIC A(20).
       01 USER2 PIC A(20).
       01 NUMEARNERS PIC 9.
       01 EARNER-1 PIC 9999.
       01 EARNER-2 PIC 9999.
       01 HSEHLD   PIC 9999.
       01 RENT     PIC 9999.
       01 PHONE    PIC 9999.
       01 PWR      PIC 9999.
       01 INTNET   PIC 9999.
       01 FDWK1    PIC 9999V9.
       01 FDWK2    PIC 9999V9.
       01 FDWK3    PIC 9999V9.
       01 FDWK4    PIC 9999V9.
       01 FDTOTAL  PIC 999V99.
       01 SVINGS   PIC 999.
       01 INSRNCE  PIC 999.
       01 GAS      PIC 999.
       01 CLOTH    PIC 999.
       01 HAIR     PIC 999.
       01 ENTRTIN  PIC 999.
       01 PETS     PIC 9.
       01 PLTES    PIC 999.
       01 CAR      PIC X.
       01 CNSME    PIC 999V9.
       01 OHEAL    PIC 999V9.
       01 MEDS     PIC 999V9.
       01 DNTL     PIC 999V9.
       01 PETFOOD  PIC 99V99.
       01 GROOMING PIC 99V99.
       01 VET      PIC 99V99.
       01 TRAINING PIC 999V9.
       01 PETCOSTS PIC 999V99.
       01 MAINCOSTS PIC 9999V99.
       01 MISCCSTS PIC 999V9.
       01 COSTS    PIC 9999V99.
       01 LEFTOVER PIC S999V999.

       PROCEDURE DIVISION.
           0100-START-HERE.
               DISPLAY "Alright, let's make a monthly budget.".
               DISPLAY "This will be based on your entire "
                   DISPLAY "household income".
               DISPLAY "Who are you?".
               ACCEPT USER1.
               DISPLAY "OK ", USER1.
               DISPLAY "How many wage earners in your household?".
               ACCEPT NUMEARNERS.
               IF NUMEARNERS < 2
                   DISPLAY "How much do you earn ", USER1 "?"
      *            DISPLAY " ?"
                   ACCEPT EARNER-1
               ELSE  
                   DISPLAY "who is the other earner?"
                   ACCEPT USER2
                   DISPLAY "So you earn?"
                   ACCEPT EARNER-1
                   DISPLAY "And ", USER2 "earns?"
      *            DISPLAY " earns?"
                   ACCEPT EARNER-2
               END-IF.
               COMPUTE HSEHLD = EARNER-1 + EARNER-2.
               DISPLAY "OK, so your household income is: ", HSEHLD "$".

      *Monthly costs. 
           0200-MONTHLY-COSTS.
               DISPLAY "now, let's figure out the monthly costs.".
               DISPLAY "First, how much is the rent/mortgage payment?".
               ACCEPT RENT.
               DISPLAY "Okay, what is your phone bill?".
               ACCEPT PHONE.
               DISPLAY "Alright, so how much is power?".
               ACCEPT PWR.
               DISPLAY "And internet?".
               ACCEPT INTNET.
               DISPLAY "Now give me how much food is each week.".
               ACCEPT FDWK1.
               ACCEPT FDWK2.
               ACCEPT FDWK3.
               ACCEPT FDWK4.
               DISPLAY "How much do you plan to save this month?".
               ACCEPT SVINGS.
               DISPLAY "How much is your monthly insurance cost?".
               ACCEPT INSRNCE.
               DISPLAY "Do you own a vehicle? (y/n)"
               ACCEPT CAR.
               IF CAR = "y"
                    DISPLAY "ok, how much are the plates per month?"
                    ACCEPT PLTES
               ELSE 
                    DISPLAY "ok then, we'll skip that"
               END-IF.
               DISPLAY "How much was gas/transportation?".
               ACCEPT GAS.
               DISPLAY "And what about clothes?".
               ACCEPT CLOTH.
               DISPLAY "Any haircare costs?".
               ACCEPT HAIR.
               DISPLAY "How much is spent on consumables".
               ACCEPT CNSME.
               DISPLAY "Dental costs?".
               ACCEPT DNTL.
               DISPLAY "What about medication?".
               ACCEPT MEDS.
               DISPLAY "How much was spent on other health care?".
               ACCEPT OHEAL.
               DISPLAY "What are your fun (music/video stream, etc.)".
               DISPLAY "costs per month".
               ACCEPT ENTRTIN.
               DISPLAY "how many pets do you have?".
               ACCEPT PETS.
               IF PETS > 0
                  DISPLAY "What are your pet food costs?"
                  ACCEPT PETFOOD
                  DISPLAY "Okay, and grooming?"
                  ACCEPT GROOMING 
                  DISPLAY "Vet costs?"
                  ACCEPT VET 
                  DISPLAY "and what about training fees?"
                  ACCEPT TRAINING
               ELSE 
                   DISPLAY "Sounds good, we'll skip that then."
               END-IF. 
      * breakdowns
           0300-BREAKDOWN.
               COMPUTE PETCOSTS = PETFOOD + GROOMING + VET + TRAINING.
               DISPLAY "Alright, your total monthly costs are: ".
                   COMPUTE COSTS = (RENT + PHONE + PWR + INTNET)
                   + (FDWK1 + FDWK2 + FDWK3 + FDWK4) + (SVINGS) +
                   (INSRNCE + GAS + CLOTH) + (HAIR + ENTRTIN) +
                   PETCOSTS + PLTES + MEDS + CNSME + DNTL + OHEAL.
               DISPLAY COSTS.
               DISPLAY "Here are your cost breakdowns".
                   COMPUTE FDTOTAL = (FDWK1 + FDWK2 + FDWK3 + FDWK4).
                   COMPUTE MAINCOSTS = RENT + INSRNCE + PWR + GAS +
                   MEDS + PLTES + OHEAL + DNTL.
                   COMPUTE MISCCSTS = CLOTH + ENTRTIN + HAIR + PHONE +
                   CNSME.
               DISPLAY "total food:".
               DISPLAY FDTOTAL.
               DISPLAY "pet costs:".
               DISPLAY PETCOSTS.
               DISPLAY "Main costs (rent, insurance, power, gas,etc.):".
               DISPLAY MAINCOSTS.
               DISPLAY "Miscellaneous costs (clothes, fun, etc.):".
               DISPLAY MISCCSTS.
               DISPLAY "What you put in savings:".
               DISPLAY SVINGS.
      *What's left
           0400-LEFT.
               DISPLAY "Now, let's see what's left over."
                   COMPUTE LEFTOVER = HSEHLD - COSTS.
               DISPLAY LEFTOVER.
               DISPLAY "Left after the month".
               DISPLAY "Okay you need at least roughly ", COSTS.
               DISPLAY "per month to stay afloat.".
               DISPLAY "And you will have ", LEFTOVER " leftover".
      *        DISPLAY "left over.".
           END PROGRAM BUDGET.