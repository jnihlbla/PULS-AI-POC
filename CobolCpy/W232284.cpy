000100 01  W232284.                                                             
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 KDOI                 PIC S9(3)           COMP-3.                  
000700*                                 ORDERINGÅNGSTYP                         
000800     03 KVPER                PIC S9(3)           COMP-3.                  
000900*                                 ANTAL PERIODER MED O-INGÅNG             
001000     03 OIFLT                OCCURS 18 TIMES.                             
001100        05 TIAAP             PIC S9(3)           COMP-3.                  
001200*                                 ÅR - PLANERINGSPERIOD (ÅÅP)             
001300        05 KVOI              PIC S9(7)           COMP-3.                  
001400*                                 ORDERINGÅNG I STYCK                     
001500        05 KVOT              PIC S9(7)           COMP-3.                  
001600*                                 ANTAL ORDERTRÄFF                        
001700*** END COPY W232284CC0  LENGTH=192                                       
