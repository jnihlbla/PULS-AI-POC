000100 01  MID-W6I12201.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I12201                                
000400     03 MID-GROUP.                                                        
000500*                                 LINES                                   
000600        05 MID-IDARTNR-IN    PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800        05 MID-IDARTNR-UT    PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000        05 MID-IDLOPNRM-IN   PIC X(8).                                    
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300        05 MID-IDLOPNRM-UT   PIC X(8).                                    
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600        05 MID-IDLEVNR-IN    PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800        05 MID-IDLEVNR-UT    PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000        05 MID-IDFS-IN       PIC X(8).                                    
002100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002200        05 MID-IDFS-UT       PIC X(8).                                    
002300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002400        05 MID-IDLBBET-IN    PIC X(12).                                   
002500*                                 LASTBÄRARBETECKNING                     
002600        05 MID-IDLBBET-UT    PIC X(12).                                   
002700*                                 LASTBÄRARBETECKNING                     
002800        05 MID-IDDC-IN       PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000        05 MID-IDDC-UT       PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200        05 MID-ADINLOMR-PRT  PIC X(4).                                    
003300*                                 PRINTERPLACERING                        
003400        05 MID-KDPRTVAL      PIC X.                                       
003500*                                 PRINTER-VAL KOD                         
003600     03 MID-SPAR-IDLOPNRM    PIC 9(9).                                    
003700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003800*                                 (0VVDLLLLK)                             
003900     03 MID-RAD              OCCURS 12 TIMES.                             
004000*                                 LINES                                   
004100        05 MID-KVFLETI       PIC X(2).                                    
004200        05 MID-KVINLART      PIC X(6).                                    
004300*                                 ANTAL I PARTIRAD                        
004400*** END OF VILMAII-COPY LENGTH= 198 BYTES                                 
