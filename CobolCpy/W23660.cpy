000100 01  W23660.                                                              
000200*                                                                         
000300*                                 ARTIKLAR SOM EJ BLIVIT                  
000400*                                 INLEVERERADE I TID                      
000500*                                                                         
000600*                                 PTYP SEN = FÖRSENAD INLEVERANS          
000700*                                 PTYP NST = NÄSTA INLEVERANS             
000800*                                                                         
000900     03 SORTDEL.                                                          
001000        05 IDPTYP            PIC X(3).                                    
001100*                                 POSTTYP                                 
001200        05 IDANSK            PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400        05 IDLEVNR           PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600        05 IDARTNR           PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800        05 KDAVRPRIO         PIC S9              COMP-3.                  
001900*                                 PRIORITET VID FÖRSENAT AVROP            
002000        05 ANTAL-RO-RADER    PIC S9(7)           COMP-3.                  
002100        05 TIAVROP-AVS-FORSENAT                                           
002200                             PIC S9(5)           COMP-3.                  
002300*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002400*                                 (ÅÅVV)                                  
002500     03 KVAVROP-FORSENAT     PIC S9(7)           COMP-3.                  
002600*                                 AVROPSKVANTITET                         
002700     03 IDFTG                PIC 9(2).                                    
002800*                                 FÖRETAGSID EKONOM REDOVISNING           
002900     03 BEART                PIC X(25).                                   
003000*                                 ARTIKELBENÄMNING                        
003100     03 KVAVROP-NAESTA       PIC S9(7)           COMP-3.                  
003200*                                 AVROPSKVANTITET                         
003300     03 TIAVROP-AVS-NAESTA   PIC S9(5)           COMP-3.                  
003400*                                 AVSÄNDNINGSVECKA (PLANERAD)             
003500*                                 (ÅÅVV)                                  
003600     03 TIAVSDAT             PIC S9(7)           COMP-3.                  
003700*                                 AVISERINGSDATUM (YYMMDD)                
003800     03 IDAVINR              PIC S9(7)           COMP-3.                  
003900*                                 AVI-NUMMER                              
004000     03 BELEVART             PIC X(30).                                   
004100*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004200*** END OF VILMAII-COPY LENGTH= 99 BYTES                                  
