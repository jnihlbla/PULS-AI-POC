000100 01  3132-WDGX3132.                                                       
000200*                                 ARTIKELSTATISTIK                        
000300*                                 JUSTERING AV FAKTURERING                
000400*                                 ELLER SJÄLVKOSTNAD                      
000500*                                 FYSISK NYCKEL: KY3132                   
000600*                                 (IDARTNR + IDDISTR +                    
000700*                                  DAFSGVV)                               
000800     03 3132-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 3132-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 3132-DAFSGVV         PIC 9(6).                                    
001500*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
001600*                                 PART SALES WEEK                         
001700     03 3132-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
001800*                                 ARTIKELPRIS NETTO                       
001900*                                 NET PRICE EACH   (FOB NET)              
002000     03 3132-KVLEVART        PIC S9(7)           COMP-3.                  
002100*                                 LEVERERAT ANTAL STYCK                   
002200*                                 DELIVERED QUANTITY                      
002300     03 3132-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
002400*                                 ARTIKELNS SJÄLVKOSTNAD                  
002500*                                 COST OF SALES                           
002600     03 3132-IDUSER          PIC X(8).                                    
002700*                                 ANVÄNDARENS SÄKERHETS ID                
002800*                                 USER SECURITY-IDENTITY                  
002900*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
