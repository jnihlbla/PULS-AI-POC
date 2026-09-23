000100 01  MID-W6I11601.                                                        
000200*                                 MID-COPYTEXT FÖR W60116                 
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
004500     03 MID-IDFTG-IN         PIC X(2).                                    
004600*                                 FÖRETAGSID EKONOM REDOVISNING           
004700     03 MID-IDKONTO-IN       PIC 9(10).                                   
004800*                                 KONTO                                   
004900     03 MID-IDANALYS-IN      PIC X(12).                                   
005000*                                 ANALYSNUMMER                            
005100     03 MID-IDKST-IN         PIC X(10).                                   
005200*                                 KOSTNADSSTÄLLE                          
005300     03 MID-FLGODK-IN        PIC X.                                       
005400     03 MID-IDFTG-UT         PIC X(2).                                    
005500*                                 FÖRETAGSID EKONOM REDOVISNING           
005600     03 MID-IDKONTO-UT       PIC 9(10).                                   
005700*                                 KONTO                                   
005800     03 MID-IDANALYS-UT      PIC X(12).                                   
005900*                                 ANALYSNUMMER                            
006000     03 MID-IDKST-UT         PIC X(10).                                   
006100*                                 KOSTNADSSTÄLLE                          
006200     03 MID-FLGODK-UT        PIC X.                                       
006300     03 MID-INPUT.                                                        
006400*                                 INDATA FÖR UPPDATERING                  
006500        05 MID-IDARTNR       OCCURS 36 TIMES                              
006600                             PIC X(8).                                    
006700*                                 ARTIKELNUMMER                           
006800        05 MID-KVAVIS        OCCURS 36 TIMES                              
006900                             PIC X(6).                                    
007000*                                 AVISERAT ANTAL                          
007100*** END OF VILMAII-COPY LENGTH= 670 BYTES                                 
