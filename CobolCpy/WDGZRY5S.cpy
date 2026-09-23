000100 01  RY5S-WDGZRY5S-CTX.                                                   
000200*                                 RY5, DEL 2 LÄGGES I SORTAREAN           
000300*                                 SKAPAS VID ANNULLATION AV               
000400*                                 UTSKRIVNA EJ PACKRAPPORTERADE           
000500*                                 RADER.                                  
000600*                                 ANVÄNDS VID SKAPANDE AV                 
000700*                                 TRANSAKTIONER TILL ÖVRIGA SYSTE         
000800*                                 M.                                      
000900     03 RY5S-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 RY5S-IDKUNDNR        PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 RY5S-FLVORKO         PIC X.                                       
001400*                                 VOR-KÖ FLAGGA                           
001500     03 RY5S-FLFORBI         PIC X.                                       
001600*                                 FÖRBIORDERFLAGGA                        
001700     03 RY5S-FLOVRLEV        PIC X.                                       
001800*                                 ÖVERLEVERANS                            
001900     03 RY5S-KDPRODSL        PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTSLAG                             
002100     03 RY5S-FILLERX5        PIC X(5).                                    
002200     03 RY5S-IDSYSTEM        PIC X(4).                                    
002300*                                 SKAPANDE SYSTEMNUMMER                   
002400     03 RY5S-KDTPOTYP        PIC S9              COMP-3.                  
002500*                                 TYP AV TIDPLANERAD ORDER                
002600     03 RY5S-KVSLATT         PIC S9(7)           COMP-3.                  
002700*                                 BERÄKNAD SLATTGRÄNS                     
002800     03 RY5S-FILLERX10       PIC X(10).                                   
002900*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
