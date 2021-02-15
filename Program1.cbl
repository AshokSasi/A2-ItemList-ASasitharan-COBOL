       identification division.
       program-id. A2-ItemList.
       Date-written. 2020-01-21.
       Author. Ashok Sasitharan.
      * Description: Assignment 2 for MAFD 4202 which does calculations
      *  and output using the A2.dat file
       environment division.
       configuration section.
      *
       input-output section.
       file-control.
      *
       select input-file
           assign to "../../../data/A2.dat"
           organization is line sequential.
      *
       select output-file
           assign to "..\..\..\data\A2-ItemList.out"
           organization is line sequential.
      *
       data division.
       file section.
      *Input declaration
       fd input-file
           data record is input-line
           record contains 27 characters.
       01 input-line.
         05 il-item-number             pic 9(4).
         05 il-product-class           pic x(1).
         05 il-product-description     pic x(13).
         05 il-quantity                pic 9(3).
         05 il-price-per-unit          pic 9(4)V99.
      *
      * Output declaration
       fd output-file
           data record is output-line
           record contains 160 characters.
      *
       01 output-line                  pic x(160).
      *
       working-storage section.
      *
       01 ws-report-heading.
         05 filler pic x(109) value spaces.
         05 filler pic x(20) value "Ashok Sasitharan, A2".
      *
       01 ws-column-heading.
         05 filler                     pic x(4)  value "Item".
         05 filler                     pic x(4)  value spaces.
         05 filler                     pic x(11) value "Description".
         05 filler                     pic x(4)  value spaces.
         05 filler                     pic x(3)  value "Qty".
         05 filler                     pic x(3)  value spaces.
         05 filler                     pic x(5)  value "Price".
         05 filler                     pic x(11) value spaces.
         05 filler                     pic x(8)  value "Extended".
         05 filler                     pic x(6)  value spaces.
         05 filler                     pic x(8)  value "Discount".
         05 filler                     pic x(9)  value spaces.
         05 filler                     pic x(9)  value "Net Price".
         05 filler                     pic x(7)  value spaces.
         05 filler                     pic x(5)  value "Class".
         05 filler                     pic x(7)  value spaces.
         05 filler                     pic x(5)  value "Trans".
         05 filler                     pic x(6)  value spaces.
         05 filler                     pic x(14) value "Transportation".
      *
       01 ws-column-heading-bot.
         05 filler                     pic x(6)  value "Number".
         05 filler                     pic x(23) value spaces.
         05 filler                     pic x(8)  value "Per Unit".
         05 filler                     pic x(8)  value spaces.
         05 filler                     pic x(5)  value "Price".
         05 filler                     pic x(9)  value spaces.
         05 filler                     pic x(6)  value "Amount".
         05 filler                     pic x(41) value spaces.
         05 filler                     pic x(1)  value "%".
         05 filler                     pic x(8)  value spaces.
         05 filler                     pic x(6)  value "Charge".
      *
       01 ws-detail-line.
         05 ws-item-number             pic 9(4).
         05 filler                     pic x(4)  value spaces.
         05 ws-product-description     pic x(13).
         05 filler                     pic x(2)  value spaces.
         05 ws-quantity                pic 9(3).
         05 filler                     pic x(1)  value spaces.
         05 ws-price-per-unit          pic ZZZ,ZZ9.99.
         05 filler                     pic x(4)  value spaces.
         05 ws-extended-price          pic Z,ZZZ,ZZ9.99.
         05 filler                     pic x(4)  value spaces.
         05 ws-discount-amount         pic ZZZ,ZZ9.99.
         05 filler                     pic x(5)  value spaces.
         05 ws-net-price               pic Z,ZZZ,ZZ9.99.
         05 filler                     pic x(10) value spaces.
         05 ws-product-class           pic x(1).
         05 filler                     pic x(8)  value spaces.
         05 ws-product-class-transportation pic ZZ9.9.
         05 ws-percent-sign            pic x.
         05 filler                     pic x(8)  value spaces.
         05 ws-transport-charge        pic Z,ZZZ,ZZ9.99.
      *
       01 ws-summary-line.
         05 filler                     pic x(39) value spaces.
         05 sl-total-extended-price    pic $$$,$$$,$99.99.
         05 filler                     pic x(17) value spaces.
         05 sl-total-net-price         pic $$$,$$$,$99.99.
         05 filler                     pic x(31) value spaces.
         05 sl-total-trans-charge      pic $$$,$$$,$99.99.
      *
       01 ws-discount-analysis.
         05 filler                     pic x(25)
                                   value "ITEMS WITHOUT DISCOUNT = ".
         05 da-percent-discount        pic 99.9.
         05 da-percent-sign            pic x.
      *
       01 ws-counters.
         05 ws-extended-price-count    pic 9(8)V99.
         05 ws-net-price-count         pic 9(8)V99.
         05 ws-transport-charge-count  pic 9(8)V99.
         05 ws-discount-count          pic 9(5)  value 0.
         05 ws-items-count             pic 9(3)  value 0.
      *
       01 ws-flags.
         05 ws-eof-flag                pic x     value "n".
      *
       01 ws-calcs.
         05 ws-extended-price-calc     pic 9(7)V99 value 0.
         05 ws-discount-amount-calc    pic 9(7)V99 value 0.
         05 ws-net-price-calc          pic 9(7)V99 value 0.
         05 ws-transportation-calc     pic 9(7)V99 value 0.
         05 ws-transportation-percent-calc pic 99V9 value 0.
         05 ws-percent-discount        pic 999V9   value 0.
      *
       01 ws-cnsts.
         05 discount-cnst              pic 9V99  value 0.05.
         05 trans-charge-A             pic 9V999 value 0.125.
         05 trans-charge-D             pic 9V999 value 0.085.
         05 trans-charge-F             pic 9V999 value 0.045.
         05 trans-charge-Default       pic 9V999 value 0.065.
         05 ws-percent-sign-cnst       pic x     value "%".
         05 ws-percent                 pic 999V9 value 100.0.
       procedure division.

       000-main.
      *
      *Open the input and output file.
           open input input-file.
           open output output-file.
      *
      * Write out the report heading
           write output-line from ws-report-heading
             after advancing 1 line.
      * write out the column headings
           write output-line from ws-column-heading
             after advancing 2 line.
           write output-line from ws-column-heading-bot
             after advancing 1 line.
           write output-line from spaces
             after advancing 1 line.

      * Initial read of the input file
           read input-file
               at end
                   move "y" to ws-eof-flag.

      *Process each input record and read in the next records
           perform 100-process-file
             until ws-eof-flag equals "y".
      *
      * Find The percent of items without a discount
           multiply ws-discount-count by ws-percent
           giving ws-discount-count.
             
           divide ws-discount-count
             by ws-items-count
             giving ws-percent-discount rounded.
      *
      * Output summary line
           move ws-extended-price-count to sl-total-extended-price.
           move ws-net-price-count to sl-total-net-price.
           move ws-transport-charge-count to sl-total-trans-charge.
           write output-line from ws-summary-line
             after advancing 3 lines.
      * Output the discount analysis line
           move ws-percent-discount to da-percent-discount.
           move ws-percent-sign-cnst to da-percent-sign.
           write output-line from ws-discount-analysis
             after advancing 2 lines.
      *    
      *    close the input and output file.
           close input-file
             output-file.
      *  
           goback.
       100-process-file.
      *
      * Initialize detail calculation variables
           move 0 to ws-discount-amount-calc
             ws-extended-price-calc
             ws-net-price-calc
             ws-transportation-calc.

      * increment item count by one
           add 1 to ws-items-count.
      * calculate extended price
           multiply il-quantity by il-price-per-unit
             giving ws-extended-price-calc rounded.

      * Discount calculation- increment if there is no discount
           if il-product-class = "A" and ws-extended-price-calc > 100
             then
               multiply ws-extended-price-calc by discount-cnst
                 giving ws-discount-amount-calc rounded
           else
               if il-product-class = "A" and ws-extended-price-calc <=
                 100 then
                   add 1 to ws-discount-count
               end-if
           end-if.
      *
           if il-product-class = "F" and ws-extended-price-calc > 50
             then
               multiply ws-extended-price-calc by discount-cnst
                 giving ws-discount-amount-calc rounded
           else
               if il-product-class = "F" and ws-extended-price-calc <= 
               50 then 
                   add 1 to ws-discount-count
               end-if
           end-if
      *
           if il-product-class = "B" and il-quantity > 5
             then
               multiply ws-extended-price-calc by discount-cnst
                 giving ws-discount-amount-calc rounded
           else
               if il-product-class = "B" and il-quantity <=5 
                   then
                   add 1 to ws-discount-count
               end-if
           end-if
      *
      * This if statement checks if the class is no A,F or B and 
      * increments the counter
      *
           if (il-product-class = "C") or (il-product-class =
             "D") or (il-product-class = "Z") or (il-product-class = "G"
             ) then
               add 1 to ws-discount-count
           end-if

      *Calculate Net Price
           subtract ws-discount-amount-calc from ws-extended-price-calc
             giving ws-net-price-calc.

      *Transport percent and calculate transport charge
           if il-product-class = "A" then

               move 12.5 to ws-transportation-percent-calc
               multiply trans-charge-A by
                 ws-extended-price-calc giving ws-transportation-calc
                 rounded
           end-if.
      *
           if il-product-class = "D" then

               move 8.5 to ws-transportation-percent-calc
               multiply trans-charge-D by 
                 ws-extended-price-calc giving ws-transportation-calc 
                 rounded
           end-if.
      *
           if il-product-class = "F" then
               move 4.5 to ws-transportation-percent-calc
               multiply trans-charge-F by
                 ws-extended-price-calc giving ws-transportation-calc
               rounded
           end-if.
      *Checks if the class is not equal to the above and sets the
      * percentage to 6.5
           if (il-product-class not = "F") and
             (il-product-class) not = "D" and
             (il-product-class) not = "A"
             and (il-quantity <= 100) then
               move 6.5 to ws-transportation-percent-calc
               multiply trans-charge-Default by
                 ws-extended-price-calc giving ws-transportation-calc 
                 rounded
      * Sets the rate to flat $45.00 if qty is more than 100 
           else
               if (il-product-class not = "F") and
                 (il-product-class not = "D") and
                 (il-product-class not = "A")
                 and (il-quantity > 100) then
                   move 0.0 to ws-transportation-percent-calc
                   move 45 to ws-transportation-calc
               end-if
           end-if.
      *
      * Add up the total for extended price, net price, and trans charge
           add ws-extended-price-calc to ws-extended-price-count.
           add ws-net-price-calc to ws-net-price-count.
           add ws-transportation-calc to ws-transport-charge-count.

      *Move values to the output
           move spaces to ws-detail-line.
           move il-item-number to ws-item-number.
           move il-product-description to ws-product-description.
           move il-quantity to ws-quantity.
           move il-price-per-unit to ws-price-per-unit.
           move ws-extended-price-calc to ws-extended-price.
           move ws-discount-amount-calc to ws-discount-amount.
           move ws-net-price-calc to ws-net-price.
           move il-product-class to ws-product-class.
           move ws-transportation-percent-calc to
             ws-product-class-transportation.
           move ws-percent-sign-cnst to ws-percent-sign.
           move ws-transportation-calc to ws-transport-charge.
      *Output the detail line
           write output-line from ws-detail-line
             after advancing 2 lines.
      *
           read input-file
               at end
                   move "y" to ws-eof-flag.
      *
       end program A2-ItemList.