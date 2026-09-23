000100 01  RESP-WL0108O1.                                                       
000200*                                 RESPONS FROM PGM WL0108                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-KDBEH-KEY       PIC X.                                       
000700*                                 BEHANDLINGSKOD                          
000800*                                 TREATMENT STATUS CODE                   
000900     03 RESP-IDFTG           PIC X(2).                                    
001000*                                 FÖRETAGSID EKONOM REDOVISNING           
001100*                                 COMPANY IDENTITY ACCOUNTING             
001200     03 RESP-KDRT            PIC X(2).                                    
001300*                                 REDOVISNINGSTYP                         
001400*                                 TYPE OF ACCOUNTING                      
001500     03 RESP-IDKONTO         PIC Z(10).                                   
001600*                                 KONTO                                   
001700*                                 ACCOUNT                                 
001800     03 RESP-IDANALYS        PIC X(12).                                   
001900*                                 ANALYSNUMMER                            
002000*                                 ANALYSIS NUMBER                         
002100     03 RESP-IDKST           PIC X(10).                                   
002200*                                 KOSTNADSSTÄLLE                          
002300*                                 COST CENTRE                             
002400     03 RESP-KVRADER         PIC Z(4)9.                                   
002500*                                 ANTAL RADER                             
002600*                                 NUMBER OF LINES                         
002700     03 RESP-R34POST         OCCURS 100 TIMES.                            
002800        05 RESP-IDARTNR      PIC Z(7)9.                                   
002900*                                 ARTIKELNUMMER                           
003000*                                 PART NUMBER                             
003100        05 RESP-KVAVIS       PIC Z(5)9.                                   
003200*                                 AVISERAT ANTAL                          
003300*                                 QUANTITY NOTIFIED                       
003400        05 RESP-IDAVINR      PIC Z(6)9.                                   
003500*                                 AVI-NUMMER                              
003600*                                 ADVICE NOTE NUMBER                      
003700        05 RESP-IDARTNR-FROM PIC Z(7)9.                                   
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000        05 RESP-IDMSG-ERROR-LINE                                          
004100                             PIC X(3).                                    
004200*                                 FELMEDDELANDE ID                        
004300*                                 ERROR MESSAGE ID                        
004400*** END OF VILMAII-COPY LENGTH= 3244 BYTES                                
