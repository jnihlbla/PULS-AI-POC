000100 01  RYES-WDGZRYES-CTX.                                                   
000200*                                 RYE, DEL 2 LÄGGES I SORTAREAN           
000300*                                 SKAPAS VID UTSKRIFT AV EN               
000400*                                 ORDERRAD.                               
000500*                                 ANVÄNDS VID SKAPANDE AV                 
000600*                                 TRANSAKTIONER TILL ÖVRIGA SYSTE         
000700*                                 M.                                      
000800     03 RYES-IDDISTR         PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 RYES-IDKUNDNR        PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 RYES-FLVORKO         PIC X.                                       
001300*                                 VOR-KÖ FLAGGA                           
001400     03 RYES-FLFORBI         PIC X.                                       
001500*                                 FÖRBIORDERFLAGGA                        
001600     03 RYES-FLOVRLEV        PIC X.                                       
001700*                                 ÖVERLEVERANS                            
001800     03 RYES-FILLERX7        PIC X(7).                                    
001900     03 RYES-KDERS           PIC S9(3)           COMP-3.                  
002000*                                 ERSÄTTNINGSKOD                          
002100     03 RYES-KDFRAKT         PIC S9(3)           COMP-3.                  
002200*                                 FRAKTSÄTT C1-C2 TILL KUND               
002300     03 RYES-KDORDKL         PIC S9              COMP-3.                  
002400*                                 ORDERKLASS                              
002500     03 RYES-KDTPOTYP        PIC S9              COMP-3.                  
002600*                                 TYP AV TIDPLANERAD ORDER                
002700     03 RYES-KVBEART         PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT ANTAL STYCKEN                  
002900     03 RYES-KVSLATT         PIC S9(7)           COMP-3.                  
003000*                                 BERÄKNAD SLATTGRÄNS                     
003100     03 RYES-KVQPACK-1       PIC S9(5)           COMP-3.                  
003200*                                 ANTAL I Q1 FÖRPACKNING                  
003300     03 RYES-FILLERX2        PIC X(2).                                    
003400*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
