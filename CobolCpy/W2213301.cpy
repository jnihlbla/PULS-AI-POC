000100 01  W2213301-CTX.                                                        
000200*                                 CALL OFF INFO TO TMS - HEAD             
000300*                                                                         
000400     03 IDPTYP-015           PIC X(15).                                   
000500*                                 POSTTYP              IDPTYP-015         
000600     03 IDVTYP               PIC X(2).                                    
000700*                                 POSTTYPSVERSION                         
000800     03 IDSYSTEM-SEND        PIC X(10).                                   
000900*                                 VOLVO SƒNDANDE SYSTEM                   
001000     03 IDINK                PIC X(5).                                    
001100*                                 INK÷PARNUMMER                           
001200     03 IDLEVNR              PIC X(5).                                    
001300*                                 LEVERANT÷RNUMMER                        
001400     03 IDDELTYP             PIC X(5).                                    
001500     03 IDARTNR-010          PIC X(10).                                   
001600*                                 ARTIKELNR TILL TMS 10 POS.              
001700     03 IDLEVNR-SHIP         PIC X(5).                                    
001800*                                 SKEPPANDE LEVERANT÷R                    
001900     03 ADINPORT             PIC X(10).                                   
002000*                                 AVLASTNINGSPORT                         
002100     03 DAGILTIG-FOM         PIC 9(8).                                    
002200*                                 GILTIGHETSDATUM FOM  (≈≈≈≈MMDD)         
002300     03 DAGILTIG-TOM         PIC X(8).                                    
002400*** END OF VILMAII-COPY LENGTH= 83 BYTES                                  
