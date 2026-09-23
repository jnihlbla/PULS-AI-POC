000100 01  W56175A.                                                             
000200*                                 RECEIVING GOODS BY LPC                  
000300     03 IDFAKT               PIC S9(7)           COMP-3.                  
000400*                                 FAKTURANUMMER                           
000500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
000600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000700     03 IDDC-SEND            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDDC-RECV            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 KDPSLLOC             PIC 9(2).                                    
001200*                                 PRODUKTSLAG LOKALT                      
001300     03 DESCRIPTION          PIC X(25).                                   
001400     03 TOTAL-PRICE          PIC S9(7)V9(2)      COMP-3.                  
001500*                                 ARTIKELPRIS NETTO                       
001600     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 RELANDCO             PIC S9(3)V9(2)      COMP-3.                  
001900*                                 LANDING COST PROCENT                    
002000     03 FREIGHT-MODE         PIC X(25).                                   
002100     03 SULANDCO             PIC S9(13)V9(2)     COMP-3.                  
002200*                                 VÄRDE TILL LANDED COST                  
002300*** END OF VILMAII-COPY LENGTH= 82 BYTES                                  
