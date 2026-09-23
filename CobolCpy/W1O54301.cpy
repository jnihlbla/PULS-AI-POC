000100 01  MOD-W1O54301.                                                        
000200*                                 MOD-COPYTEXT F÷R BILD                   
000300*                                 MODELLINFO MASTER                       
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDCATNR-IN       PIC X(5).                                    
001300*                                 KATALOG-ID                              
001400     03 MOD-IDCATNR-UT       PIC X(5).                                    
001500*                                 KATALOG-ID                              
001600     03 MOD-BEART            PIC X(25).                                   
001700*                                 ARTIKELBENƒMNING                        
001800     03 MOD-BEMASTER         PIC X(12).                                   
001900*                                 MASTERNAMN F÷R FORDON                   
002000     03 MOD-KOLUMN           OCCURS 2 TIMES.                              
002100*                                 RAD                                     
002200        05 MOD-KOLUMNRAD     OCCURS 11 TIMES.                             
002300*                                 KOLUMN                                  
002400           07 MOD-IDMODELL   PIC X(3).                                    
002500*                                 BILENS NUMERISKA MODELLBET.             
002600           07 MOD-BEMODELL   PIC X(15).                                   
002700*                                 BILENS MODELLBESKRIVNING.               
002800           07 MOD-TIMODAAR-STA                                            
002900                             PIC 9(4).                                    
003000*                                 MODELL≈R (≈≈≈≈) START≈R                 
003100           07 MOD-TIMODAAR-STO                                            
003200                             PIC 9(4).                                    
003300*                                 MODELL≈R (≈≈≈≈) STOPP≈R                 
003400           07 MOD-IDCATNR    PIC Z(4)9.                                   
003500*                                 KATALOG-ID                              
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 846 BYTES                                 
