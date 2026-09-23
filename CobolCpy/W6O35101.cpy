000100 01  MOD-W6O35101.                                                        
000200*                                 COPYTEXT FÖR MOD W6035101               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
001100*                                 MFS ATTRIBUTFÄLT                        
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 MOD-IDDC-UT          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 MOD-LVL1-IDDC        PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 MOD-RAD-LVL2         OCCURS 26 TIMES.                             
002200        05 MOD-LVL2-IDDC     PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500        05 MOD-LVL2-IDUSER   OCCURS 4 TIMES                               
002600                             PIC X(7).                                    
002700*                                 ANVÄNDARENS SÄKERHETS ID                
002800*                                 USER SECURITY-IDENTITY                  
002900        05 MOD-KDDC          PIC X.                                       
003000        05 MOD-FLLVL3        PIC X.                                       
003100     03 MOD-RAD-LVL3         OCCURS 24 TIMES.                             
003200        05 MOD-LVL3-IDDC     PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400*                                 WAREHOUSE IDENTIFIER                    
003500     03 MOD-CMD-ATTR         PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-CMD-IN           PIC X.                                       
003800     03 MOD-NY-IDDC-ATTR     PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-NY-IDDC-IN       PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200*                                 WAREHOUSE IDENTIFIER                    
004300     03 MOD-LEVEL-ATTR       PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-LEVEL-IN         PIC 9.                                       
004600     03 MOD-UNDER-IDDC-ATTR  PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-UNDER-IDDC-IN    PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000*                                 WAREHOUSE IDENTIFIER                    
005100     03 MOD-NY-IDUSER        OCCURS 4 TIMES.                              
005200        05 MOD-IDUSER-ATTR   PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-IDUSER-IN     PIC X(7).                                    
005500*                                 ANVÄNDARENS SÄKERHETS ID                
005600*                                 USER SECURITY-IDENTITY                  
005700     03 MOD-KDDC-ATTR        PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-KDDC-IN          PIC X.                                       
006000     03 MOD-TEMFSINF         PIC X(55).                                   
006100*                                 INFORMATIONSMEDDELANDE                  
006200*                                 INFORMATION MESSAGE                     
006300*** END OF VILMAII-COPY LENGTH= 1040 BYTES                                
