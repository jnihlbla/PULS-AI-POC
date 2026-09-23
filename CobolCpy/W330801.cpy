000100 01  W330801.                                                             
000200*                                 PRODUKTSLAGSVOLYMTRANS                  
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000600*                                 PRODUKTSLAG                             
000700     03 FILLER               PIC X(6).                                    
000800     03 KVVOL                PIC S9(9)           COMP-3.                  
000900*                                 FÖRSÄLJNINGSVOLYM                       
001000     03 SUFAKT-N             PIC S9(9)V9(2)      COMP-3.                  
001100*                                 SUMMA FAKTURERADE RADER                 
001200     03 SUSJK-N              PIC S9(9)V9(2)      COMP-3.                  
001300*                                 SUMMA SJÄLVKOSTNAD                      
001400*** END COPY W330801CC0  LENGTH=28                                        
