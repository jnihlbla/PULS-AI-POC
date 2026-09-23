000100 01  RESP-W60116O1.                                                       
000200*                                 MODCOPYTEXT TILL W6011610.              
000300     03 RESP-IDLBBET         PIC X(12).                                   
000400*                                 LASTBÄRARBETECKNING                     
000500*                                 TRAILER NUMBER                          
000600     03 RESP-IDFTG-IN-ATTR   PIC X(2).                                    
000700*                                 MFS ATTRIBUTFÄLT                        
000800     03 RESP-IDFTG-IN        PIC X(2).                                    
000900*                                 FÖRETAGSID EKONOM REDOVISNING           
001000*                                 COMPANY IDENTITY ACCOUNTING             
001100     03 RESP-IDKONTO-IN-ATTR PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 RESP-IDKONTO-IN      PIC X(10).                                   
001400*                                 KONTO                                   
001500*                                 ACCOUNT                                 
001600     03 RESP-IDANALYS-IN-ATTR                                             
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 RESP-IDANALYS-IN     PIC X(12).                                   
002000*                                 ANALYSNUMMER                            
002100*                                 ANALYSIS NUMBER                         
002200     03 RESP-IDKST-IN-ATTR   PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 RESP-IDKST-IN        PIC X(10).                                   
002500*                                 KOSTNADSSTÄLLE                          
002600*                                 COST CENTRE                             
002700     03 RESP-FLGODK-IN-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 RESP-FLGODK-IN       PIC X.                                       
003000*                                 GODKÄNT?  JA/NEJ                        
003100*                                 APPROVED? J/N                           
003200     03 RESP-KVRADER         PIC 9(5).                                    
003300*                                 ANTAL RADER                             
003400*                                 NUMBER OF LINES                         
003500     03 RESP-UPDATE          OCCURS 500 TIMES.                            
003600*                                 UPDATE                                  
003700        05 RESP-IDARTNR-LINE PIC X(8).                                    
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000        05 RESP-IDARTNR-LINE-ATTR                                         
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 RESP-KVAVIS-LINE  PIC X(6).                                    
004400*                                 AVISERAT ANTAL                          
004500*                                 QUANTITY NOTIFIED                       
004600        05 RESP-KVAVIS-LINE-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900*** END OF VILMAII-COPY LENGTH= 9062 BYTES                                
