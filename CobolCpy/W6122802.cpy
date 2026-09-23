000100 01  W6122802.                                                            
000200*                                 DATA TO CREATE PROPOSED ETA             
000300     03 IDDC-REC             PIC X(2).                                    
000400*                                 MOTTAGANDE LAGER                        
000500*                                 RECEIVING WAREHOUSE                     
000600     03 IDLBBET              PIC X(12).                                   
000700*                                 LASTBÄRARBETECKNING                     
000800*                                 TRAILER NUMBER                          
000900     03 IDDC-SEND            PIC X(2).                                    
001000*                                 SÄNDANDE LAGER                          
001100*                                 SENDING WAREHOUSE                       
001200     03 TIFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001400*                                 INVOICING DATE   (YYMMDD)               
001500     03 KDTRPSTA             PIC X.                                       
001600*                                 TRANSPORTSTATUS                         
001700*                                 TRANSPORT STATUS                        
001800     03 KDTRPSTA-SORT        PIC X.                                       
001900*                                 TRANSPORTSTATUS                         
002000*                                 TRANSPORT STATUS                        
002100     03 KVRADER-PRIO         PIC S9(5)           COMP-3.                  
002200*                                 ANTAL PRIORITERADE RADER                
002300*                                 NUMBER OF PRIORITY LINES                
002400     03 KVRADER-FAKT         PIC S9(5)           COMP-3.                  
002500*                                 ANTAL RADER PER FAKTURA                 
002600*                                 NUMBER OF LINES PER INVOICE             
002700     03 KVRADER-MOT          PIC S9(5)           COMP-3.                  
002800*                                 ANTAL MOTTAGNA  RADER                   
002900*                                 NUMBER OF LINES RECEIVED                
003000     03 DABERANK-DISCH       PIC S9(7)           COMP-3.                  
003100*                                 DISCHARGED ETA DATUM                    
003200     03 DABERANK             PIC 9(8).                                    
003300*                                 BERÄKNAD ANKOMSTDATUM                   
003400*                                 ESTIMATED RECEIVING DATE                
003500     03 DABERANK-PROP        PIC S9(7)           COMP-3.                  
003600*                                 PROPOSED ETA FROM P44 & PULS            
003700     03 FLMANETA             PIC X.                                       
003800*                                 MANUALLY UPDATE ETA FLAG (Y/N)          
003900     03 ETA-POD-DEP          PIC S9(7)           COMP-3.                  
004000*                                 ACTUAL ETA DATE FROM PROJECT4           
004100     03 KVDAGAR-ETA          PIC 9(3).                                    
004200*                                 ANTAL DAGAR                             
004300*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
