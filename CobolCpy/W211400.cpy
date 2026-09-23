000100 01  W211400.                                                             
000200*                                 POSTTYP 221                             
000300*                                                                         
000400     03 IDTTYP               PIC X(3).                                    
000500*                                 TRANSAKTIONSTYP                         
000600     03 IDARTNR-S            PIC 9(8).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 KDCLAGER-S           PIC 9.                                       
000900*                                 CENTRALLAGERKOD                         
001000     03 SORTFLT1             PIC 9(2).                                    
001100*                                 SORTERINGSFÄLT                          
001200     03 NOLLOR-200           PIC X(20).                                   
001300     03 POSTLGD              PIC 9(3).                                    
001400     03 IDPTYP               PIC 9(3).                                    
001500*                                 INLEVERANS-POSTTYP                      
001600     03 NOLLOR-20            PIC 9(2).                                    
001700     03 IDARTNR              PIC 9(8).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 KDCLAGER             PIC 9.                                       
002000*                                 CENTRALLAGERKOD                         
002100     03 KVRETUR              PIC S9(6)           COMP-3.                  
002200*                                 RETURNERAT ANTAL    KVRETUR-002         
002300     03 IDLEVNR-INL          PIC X(5).                                    
002400*                                 LEVERANTÖR FÖR AKTUELL INLEV.           
002500     03 FLUPPBR              PIC 9.                                       
002600*                                 BESTÄLLNINGSREST UPPDATERAS?            
002700*                                 (1 = JA)                                
002800     03 NOLLOR-50            PIC 9(5).                                    
002900     03 IDKONTO              PIC 9(4).                                    
003000*                                 HUVUDKONTO         IDHKONTO-002         
003100     03 NOLLOR-200           PIC X(20).                                   
003200*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
