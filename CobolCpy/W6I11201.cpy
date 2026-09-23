000100 01  MID-W6I11201.                                                        
000200*                                 MID-COPYTEXT FÖR W60112                 
000300     03 MID-NYCKLAR-GRP.                                                  
000400*                                 BILDGRUPP 6111-6119                     
000500        05 MID-IDLEVNR-IN    PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700        05 MID-IDLEVNR-UT    PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900        05 MID-IDFS-IN       PIC X(8).                                    
001000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001100        05 MID-IDFS-UT       PIC X(8).                                    
001200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001300        05 MID-TIAVIDAT-IN   PIC X(6).                                    
001400*                                 AVISERINGSDATUM (YYMMDD)                
001500        05 MID-TIAVIDAT-UT   PIC X(6).                                    
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700        05 MID-ADINLOMR-PRT-IN                                            
001800                             PIC X(4).                                    
001900*                                 PRINTERPLACERING                        
002000        05 MID-ADINLOMR-PRT-UT                                            
002100                             PIC X(4).                                    
002200*                                 PRINTERPLACERING                        
002300        05 MID-IDDC-IN       PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500        05 MID-IDDC-UT       PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700        05 MID-KDRT-IN       PIC X(2).                                    
002800*                                 REDOVISNINGSTYP                         
002900        05 MID-KDRT-UT       PIC X(2).                                    
003000*                                 REDOVISNINGSTYP                         
003100        05 MID-IDLBBET-IN    PIC X(12).                                   
003200*                                 LASTBÄRARBETECKNING                     
003300        05 MID-IDLBBET-UT    PIC X(12).                                   
003400*                                 LASTBÄRARBETECKNING                     
003500        05 MID-FLKLIVIS-IN   PIC X.                                       
003600*                                 JA/NEJ-FLAGGA                           
003700        05 MID-FLKLIVIS-UT   PIC X.                                       
003800*                                 JA/NEJ-FLAGGA                           
003900        05 MID-IDLOPNRM-IN   PIC X(8).                                    
004000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004100*                                 (0VVDLLLLK)                             
004200        05 MID-IDLOPNRM-UT   PIC X(8).                                    
004300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004400*                                 (0VVDLLLLK)                             
004500     03 MID-FLGODK-IN        PIC X.                                       
004600     03 MID-FLGODK-UT        PIC X.                                       
004700     03 MID-INPUT.                                                        
004800*                                 INDATA FÖR UPPDATERING                  
004900        05 MID-IDARTNR       OCCURS 24 TIMES                              
005000                             PIC X(8).                                    
005100*                                 ARTIKELNUMMER                           
005200        05 MID-KVAVIS        OCCURS 24 TIMES                              
005300                             PIC X(6).                                    
005400*                                 AVISERAT ANTAL                          
005500        05 MID-IDFS          OCCURS 24 TIMES                              
005600                             PIC X(8).                                    
005700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
