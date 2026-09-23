000100 01  W330800.                                                             
000200*                                 ARTIKELVOLYMTRANS                       
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000800*                                 PRODUKTSLAG                             
000900     03 FLPB                 PIC S9              COMP-3.                  
001000*                                 FÖRS VOLYM = PERIODBEHOV ?              
001100     03 KVVOL                PIC S9(9)           COMP-3.                  
001200*                                 FÖRSÄLJNINGSVOLYM                       
001300     03 SUFAKT-N             PIC S9(9)V9(2)      COMP-3.                  
001400*                                 SUMMA FAKTURERADE RADER                 
001500     03 SUSJK-N              PIC S9(9)V9(2)      COMP-3.                  
001600*                                 SUMMA SJÄLVKOSTNAD                      
001700*** END COPY W330800CC0  LENGTH=28                                        
