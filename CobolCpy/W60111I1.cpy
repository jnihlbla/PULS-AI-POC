000100 01  REQU-W60111I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W60111                
000300*                                                                         
000400     03 REQU-IDLEVNR-KEY     PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 REQU-KDRT-KEY        PIC X(2).                                    
000700*                                 REDOVISNINGSTYP                         
000800     03 REQU-IDFS-KEY        PIC X(8).                                    
000900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001000     03 REQU-TIAVIDAT-KEY    PIC X(6).                                    
001100*                                 AVISERINGSDATUM (YYMMDD)                
001200     03 REQU-ADINLOMR-PRT-KEY                                             
001300                             PIC X(4).                                    
001400*                                 PRINTERPLACERING                        
001500     03 REQU-IDDC-KEY        PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 REQU-IDLBBET         PIC X(12).                                   
001800*                                 LASTBÄRARBETECKNING                     
001900     03 REQU-FLGODK          PIC X.                                       
002000*                                 GODKÄNT?  JA/NEJ                        
002100     03 REQU-INPUT           OCCURS 500 TIMES.                            
002200*                                 INDATA FÖR UPPDATERING                  
002300        05 REQU-IDARTNR-LINE PIC X(8).                                    
002400*                                 ARTIKELNUMMER                           
002500        05 REQU-KVAVIS-LINE  PIC X(6).                                    
002600*                                 AVISERAT ANTAL                          
002700*** END OF VILMAII-COPY LENGTH= 7040 BYTES                                
