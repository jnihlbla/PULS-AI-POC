000100 01  4436-WDGX4436.                                                       
000200*                                 BESKRIVNING AV ARBETSTIDER              
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDPRC + KVKALTIM + LOW-VALUE)          
000500     03 4436-IDPRC           PIC X(4).                                    
000600*                                 PRODUKTIONSKANAL                        
000700*                                 PRODUCTION CHANNEL                      
000800     03 4436-KVKALTIM        PIC 9(2).                                    
000900*                                 TILLGÄNGLIG KALENDERTID                 
001000*                                 AVAILABLE CALENDARTIME                  
001100     03 4436-LOW-VALUE       PIC X(4).                                    
001200     03 4436-TISTAMIN-PAC    PIC S9(5)           COMP-3.                  
001300*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
001400*                                 WORKINGDAYS BEGINNING (PICKING)         
001500     03 4436-TISTOMIN-PAC    PIC S9(5)           COMP-3.                  
001600*                                 ARBETSDAGENS SLUT (PACKNING)            
001700*                                 WORKINGDAYS END (PICKING)               
001800     03 4436-TISTAMIN-ADM    PIC S9(5)           COMP-3.                  
001900*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
002000*                                 WORKINGDAYS BEGINNING(ORDEROFF)         
002100     03 4436-TISTOMIN-ADM    PIC S9(5)           COMP-3.                  
002200*                                 ARBETSDAGENS SLUT (ORDERKONT)           
002300*                                 WORKINGDAYS END (ORDEROFFICE)           
002400     03 4436-TISTAMIN-LAST   PIC S9(5)           COMP-3.                  
002500*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
002600*                                 WORKINGDAYS BEGINNING (LOADING)         
002700     03 4436-TISTOMIN-LAST   PIC S9(5)           COMP-3.                  
002800*                                 ARBETSDAGENS SLUT (LASTNING)            
002900*                                 WORKINGDAYS END (LOADING)               
003000     03 4436-FILLER          PIC X(2).                                    
003100*** END COPY WDGX4436C0  LENGTH=30                                        
