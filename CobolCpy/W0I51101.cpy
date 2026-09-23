000100 01  MID-W0I51101.                                                        
000200*                                 MID TILL USER-INIT                      
000300*                                 I USER-INIT-REG                         
000400     03 MID-IDUSER-IN        PIC X(8).                                    
000500*                                 ANVÄNDARENS SÄKERHETS ID                
000600*                                 USER SECURITY-IDENTITY                  
000700     03 MID-IDUSER-UT        PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900*                                 USER SECURITY-IDENTITY                  
001000     03 MID-KDCMDVAL         PIC X(3).                                    
001100*                                 GENERELL KOMMANDOKOD                    
001200*                                 GENERAL COMMAND-CODE                    
001300     03 MID-BEANST           PIC X(25).                                   
001400*                                 ANSTÄLLDS NAMN                          
001500*                                 NAME OF EMPLOYED                        
001600     03 MID-IDAVD            PIC X(5).                                    
001700*                                 DEN ANSTÄLLDES AVDELNING/               
001800*                                 KOSTNADSSTÄLLE                          
001900*                                 DEPARTMENT OF EMPLOYED/                 
002000*                                 COST CENTER                             
002100     03 MID-IDFTG            PIC 9(2).                                    
002200*                                 FÖRETAGSID EKONOM REDOVISNING           
002300*                                 COMPANY IDENTITY ACCOUNTING             
002400     03 MID-IDDC             PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700     03 MID-IDLAND-SPR       PIC X(2).                                    
002800*                                 2-STÄLLIG SPRÅKBETECKNING               
002900*                                 2-LETTER CODE FOR LANGUAGE              
003000     03 MID-IDLTERM          PIC X(8).                                    
003100*                                 LOGISKT TERMINALNAMN                    
003200*                                 IDENTITY OF LOGICAL TERMINAL            
003300     03 MID-IDCSS            PIC X(8).                                    
003400*                                 CSS-STILMALL                            
003500*                                 CASCADING STYLESHEET                    
003600     03 MID-IDSPRAK          PIC X(2).                                    
003700*                                 2-STÄLLIG ISO SPRÅKKOD                  
003800*                                 2-LETTER ISO LANGUAGE CODE              
003900     03 MID-IDNODE           PIC X(8).                                    
004000*                                 VTAM NODE-NAMN                          
004100*                                 VTAM NODE NAME                          
004200     03 MID-IDRT-KEY         PIC X(3).                                    
004300*                                 RETURTERMINAL                           
004400*                                 RETURN TERMINAL                         
004500     03 MID-IDTFN            PIC X(20).                                   
004600*                                 TELEFONNUMMER EXTERNT                   
004700*                                 TELEPHONE NUMBER  EXTERNAL              
004800     03 MID-IDTFX            PIC X(20).                                   
004900*                                 TELEFAXNUMMER                           
005000*                                 FAXNUMBER                               
005100     03 MID-IDTIDZON         PIC X(2).                                    
005200*                                 TIDZONER PÅ JORDEN.                     
005300*                                 TIME ZONE ON EARTH                      
005400     03 MID-KDMATT           PIC X.                                       
005500*                                 MÅTTKOD                                 
005600*                                 MEASUREMENT CODE                        
005700*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
