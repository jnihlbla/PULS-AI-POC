000100 01  REQU-W60116I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6011610              
000300*                                                                         
000400     03 REQU-IDLEVNR-KEY     PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 REQU-IDFS-KEY        PIC X(8).                                    
000700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
000800     03 REQU-TIAVIDAT-KEY    PIC 9(6).                                    
000900*                                 AVISERINGSDATUM (YYMMDD)                
001000     03 REQU-ADINLOMR-PRT-KEY                                             
001100                             PIC X(4).                                    
001200*                                 PRINTERPLACERING                        
001300     03 REQU-IDDC-KEY        PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 REQU-KDRT-KEY        PIC 9(2).                                    
001600*                                 REDOVISNINGSTYP                         
001700     03 REQU-IDLBBET         PIC X(12).                                   
001800*                                 LASTBÄRARBETECKNING                     
001900     03 REQU-IDFTG           PIC X(2).                                    
002000*                                 FÖRETAGSID EKONOM REDOVISNING           
002100     03 REQU-IDKONTO         PIC X(10).                                   
002200*                                 KONTO                                   
002300     03 REQU-IDANALYS        PIC X(12).                                   
002400*                                 ANALYSNUMMER                            
002500     03 REQU-IDKST           PIC X(10).                                   
002600*                                 KOSTNADSSTÄLLE                          
002700     03 REQU-FLGODK          PIC X.                                       
002800*                                 GODKÄNT?  JA/NEJ                        
002900     03 REQU-KVRADER         PIC 9(5).                                    
003000*                                 ANTAL RADER                             
003100     03 REQU-INPUT           OCCURS 500 TIMES.                            
003200*                                 INDATA FÖR UPPDATERING                  
003300        05 REQU-IDARTNR-LINE PIC X(8).                                    
003400*                                 ARTIKELNUMMER                           
003500        05 REQU-KVAVIS-LINE  PIC X(6).                                    
003600*                                 AVISERAT ANTAL                          
003700*** END OF VILMAII-COPY LENGTH= 7079 BYTES                                
