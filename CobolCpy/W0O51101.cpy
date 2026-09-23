000100 01  MOD-W0O51101.                                                        
000200*                                 MOD TILL USER-INIT                      
000300*                                 I USER-INIT-REG                         
000400     03 MOD-IDTRANS-UT       PIC X(4).                                    
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
001600     03 MOD-KDCMDVAL-ATTR    PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-KDCMDVAL         PIC X(3).                                    
001900*                                 GENERELL KOMMANDOKOD                    
002000*                                 GENERAL COMMAND-CODE                    
002100     03 MOD-BEANST           PIC X(25).                                   
002200*                                 ANSTÄLLDS NAMN                          
002300*                                 NAME OF EMPLOYED                        
002400     03 MOD-IDAVD            PIC X(5).                                    
002500*                                 DEN ANSTÄLLDES AVDELNING/               
002600*                                 KOSTNADSSTÄLLE                          
002700*                                 DEPARTMENT OF EMPLOYED/                 
002800*                                 COST CENTER                             
002900     03 MOD-IDFTG            PIC 9(2).                                    
003000*                                 FÖRETAGSID EKONOM REDOVISNING           
003100*                                 COMPANY IDENTITY ACCOUNTING             
003200     03 MOD-IDDC-ATTR        PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-IDDC             PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600*                                 WAREHOUSE IDENTIFIER                    
003700     03 MOD-IDLAND-SPR       PIC X(2).                                    
003800*                                 2-STÄLLIG SPRÅKBETECKNING               
003900*                                 2-LETTER CODE FOR LANGUAGE              
004000     03 MOD-IDLTERM          PIC X(8).                                    
004100*                                 LOGISKT TERMINALNAMN                    
004200*                                 IDENTITY OF LOGICAL TERMINAL            
004300     03 MOD-IDCSS            PIC X(8).                                    
004400*                                 CSS-STILMALL                            
004500*                                 CASCADING STYLESHEET                    
004600     03 MOD-IDSPRAK          PIC X(2).                                    
004700*                                 2-STÄLLIG ISO SPRÅKKOD                  
004800*                                 2-LETTER ISO LANGUAGE CODE              
004900     03 MOD-IDNODE           PIC X(8).                                    
005000*                                 VTAM NODE-NAMN                          
005100*                                 VTAM NODE NAME                          
005200     03 MOD-IDRT-KEY         PIC X(3).                                    
005300*                                 RETURTERMINAL                           
005400*                                 RETURN TERMINAL                         
005500     03 MOD-IDTFN            PIC X(20).                                   
005600*                                 TELEFONNUMMER EXTERNT                   
005700*                                 TELEPHONE NUMBER  EXTERNAL              
005800     03 MOD-IDTFX            PIC X(20).                                   
005900*                                 TELEFAXNUMMER                           
006000*                                 FAXNUMBER                               
006100     03 MOD-IDTIDZON         PIC X(2).                                    
006200*                                 TIDZONER PÅ JORDEN.                     
006300*                                 TIME ZONE ON EARTH                      
006400     03 MOD-KDMATT           PIC X.                                       
006500*                                 MÅTTKOD                                 
006600*                                 MEASUREMENT CODE                        
006700     03 MOD-IDTRANS          PIC X(4).                                    
006800*                                 BILDNUMMER                              
006900*                                 SCREEN NUMBER                           
007000     03 MOD-KDMFSFOR         PIC X.                                       
007100*                                 TYP AV MFS-FORMAT                       
007200*                                 1 = W-FORMAT  2 = N-FORMAT              
007300*                                 TYPE OF MFS FORMAT                      
007400     03 MOD-KDSVAR           PIC X.                                       
007500*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
007600*                                 RETURN CODE FROM PROGRAM                
007700     03 MOD-TIREGDAT         PIC X(6).                                    
007800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007900*                                 REGISTRATION DATE (YYMMDD)              
008000     03 MOD-TIUPPDAT         PIC X(6).                                    
008100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
008200*                                 UPDATING DATE     (YYMMDD)              
008300     03 MOD-TIUPPTID         PIC X(8).                                    
008400*                                 UPPDATERINGSTID  (TTMMSSTH)             
008500*                                 UPDATING TIME    (HHMMSSTH)             
008600     03 MOD-TILOKDAT         PIC X(8).                                    
008700*                                 DATUM FÖR LOKAL TID     AAMMDD          
008800*                                 DATE FOR LOCAL TIME     YYMMDD          
008900     03 MOD-TILOKTID         PIC X(5).                                    
009000*                                 TID (KLOCKAN) FÖR LOKAL TID             
009100*                                 LOCAL TIME AS  HHMM                     
009200     03 MOD-TEMFSINF         PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
009400*                                 INFORMATION MESSAGE                     
009500*** END OF VILMAII-COPY LENGTH= 269 BYTES                                 
