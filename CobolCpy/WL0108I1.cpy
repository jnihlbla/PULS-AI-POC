000100 01  REQU-WL0108I1.                                                       
000200*                                 REQUEST TO PGM WL0108                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-KDBEH-KEY       PIC 9.                                       
000700*                                 BEHANDLINGSKOD                          
000800*                                 TREATMENT STATUS CODE                   
000900     03 REQU-IDFTG           PIC X(2).                                    
001000*                                 FÖRETAGSID EKONOM REDOVISNING           
001100*                                 COMPANY IDENTITY ACCOUNTING             
001200     03 REQU-KDRT            PIC X(2).                                    
001300*                                 REDOVISNINGSTYP                         
001400*                                 TYPE OF ACCOUNTING                      
001500     03 REQU-IDKONTO         PIC X(10).                                   
001600*                                 KONTO                                   
001700*                                 ACCOUNT                                 
001800     03 REQU-IDANALYS        PIC X(12).                                   
001900*                                 ANALYSNUMMER                            
002000*                                 ANALYSIS NUMBER                         
002100     03 REQU-IDKST           PIC X(10).                                   
002200*                                 KOSTNADSSTÄLLE                          
002300*                                 COST CENTRE                             
002400     03 REQU-KVRADER         PIC 9(5).                                    
002500*                                 ANTAL RADER                             
002600*                                 NUMBER OF LINES                         
002700     03 REQU-R34POST         OCCURS 100 TIMES.                            
002800        05 REQU-IDARTNR      PIC 9(8).                                    
002900*                                 ARTIKELNUMMER                           
003000*                                 PART NUMBER                             
003100        05 REQU-KVAVIS       PIC 9(6).                                    
003200*                                 AVISERAT ANTAL                          
003300*                                 QUANTITY NOTIFIED                       
003400        05 REQU-IDAVINR      PIC 9(7).                                    
003500*                                 AVI-NUMMER                              
003600*                                 ADVICE NOTE NUMBER                      
003700        05 REQU-IDARTNR-FROM PIC 9(8).                                    
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000*** END OF VILMAII-COPY LENGTH= 2944 BYTES                                
