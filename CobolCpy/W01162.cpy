000100 01  NOT-W01162.                                                          
000200*                                 NOTERINGAR FRÅN ARTIKELREGISTER         
000300*                                 1 = ANSKAFFNINGSINFORMATION 1           
000400*                                 2 = ANSKAFFNINGSINFORMATION 2           
000500*                                 3 = BEREDNINGSINFORMATION 1             
000600*                                 4 = SPÄRRKODSNOTERING                   
000700*                                 5 = REFILLNOTERING                      
000800*                                 6 = VARIANTNOTERING                     
000900*                                 7 = BEREDNINGSINFORMATION 2             
001000*                                 8 = STÄLLKOSTNADSNOTERING               
001100     03 NOT-IDARTNR          PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 NOT-KDNOTTYP         PIC S9              COMP-3.                  
001500*                                 NOTERINGSTYP                            
001600*                                 NOTE TYPE                               
001700     03 NOT-TEARTNOT         PIC X(40).                                   
001800*                                 ARTIKEL NOTERING                        
001900*                                 PART REMARKS NOTE                       
002000*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
