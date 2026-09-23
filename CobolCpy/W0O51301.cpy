000100 01  MOD-W0O51301.                                                        
000200*                                 MOD TILL EGEN-ÄNDRING                   
000300*                                 I USER-INIT-REG                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDUSER-IN        PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200*                                 USER SECURITY-IDENTITY                  
001300     03 MOD-IDUSER-UT        PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500*                                 USER SECURITY-IDENTITY                  
001600     03 MOD-BEANST           PIC X(25).                                   
001700*                                 ANSTÄLLDS NAMN                          
001800*                                 NAME OF EMPLOYED                        
001900     03 MOD-IDAVD            PIC X(5).                                    
002000*                                 DEN ANSTÄLLDES AVDELNING/               
002100*                                 KOSTNADSSTÄLLE                          
002200*                                 DEPARTMENT OF EMPLOYED/                 
002300*                                 COST CENTER                             
002400     03 MOD-IDFTG            PIC 9(2).                                    
002500*                                 FÖRETAGSID EKONOM REDOVISNING           
002600*                                 COMPANY IDENTITY ACCOUNTING             
002700     03 MOD-IDDC             PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900*                                 WAREHOUSE IDENTIFIER                    
003000     03 MOD-IDLAND-SPR       PIC X(2).                                    
003100*                                 2-STÄLLIG SPRÅKBETECKNING               
003200*                                 2-LETTER CODE FOR LANGUAGE              
003300     03 MOD-IDLTERM          PIC X(8).                                    
003400*                                 LOGISKT TERMINALNAMN                    
003500*                                 IDENTITY OF LOGICAL TERMINAL            
003600     03 MOD-IDCSS            PIC X(8).                                    
003700*                                 CSS-STILMALL                            
003800*                                 CASCADING STYLESHEET                    
003900     03 MOD-IDSPRAK          PIC X(2).                                    
004000*                                 2-STÄLLIG ISO SPRÅKKOD                  
004100*                                 2-LETTER ISO LANGUAGE CODE              
004200     03 MOD-IDNODE           PIC X(8).                                    
004300*                                 VTAM NODE-NAMN                          
004400*                                 VTAM NODE NAME                          
004500     03 MOD-IDRT-KEY         PIC X(3).                                    
004600*                                 RETURTERMINAL                           
004700*                                 RETURN TERMINAL                         
004800     03 MOD-IDTFN            PIC X(20).                                   
004900*                                 TELEFONNUMMER EXTERNT                   
005000*                                 TELEPHONE NUMBER  EXTERNAL              
005100     03 MOD-IDTFX            PIC X(20).                                   
005200*                                 TELEFAXNUMMER                           
005300*                                 FAXNUMBER                               
005400     03 MOD-IDTIDZON         PIC X(2).                                    
005500*                                 TIDZONER PÅ JORDEN.                     
005600*                                 TIME ZONE ON EARTH                      
005700     03 MOD-KDMATT           PIC X.                                       
005800*                                 MÅTTKOD                                 
005900*                                 MEASUREMENT CODE                        
006000     03 MOD-TIREGDAT         PIC X(6).                                    
006100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006200*                                 REGISTRATION DATE (YYMMDD)              
006300     03 MOD-TIUPPDAT         PIC X(6).                                    
006400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
006500*                                 UPDATING DATE     (YYMMDD)              
006600     03 MOD-TIUPPTID         PIC X(8).                                    
006700*                                 UPPDATERINGSTID  (TTMMSSTH)             
006800*                                 UPDATING TIME    (HHMMSSTH)             
006900     03 MOD-TILOKDAT         PIC X(8).                                    
007000*                                 DATUM FÖR LOKAL TID     AAMMDD          
007100*                                 DATE FOR LOCAL TIME     YYMMDD          
007200     03 MOD-TILOKTID         PIC X(5).                                    
007300*                                 TID (KLOCKAN) FÖR LOKAL TID             
007400*                                 LOCAL TIME AS  HHMM                     
007500     03 MOD-TEMFSINF         PIC X(55).                                   
007600*                                 INFORMATIONSMEDDELANDE                  
007700*                                 INFORMATION MESSAGE                     
007800*** END OF VILMAII-COPY LENGTH= 256 BYTES                                 
