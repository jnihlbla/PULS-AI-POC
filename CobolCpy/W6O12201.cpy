000100 01  MOD-W6O12201.                                                        
000200*                                 COPYTEXT FOR MOD W6O12201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-GROUP.                                                        
000800*                                 LINES                                   
000900        05 MOD-IDARTNR-IN    PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100        05 MOD-IDARTNR-UT    PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300        05 MOD-IDLOPNRM-IN   PIC X(8).                                    
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600        05 MOD-IDLOPNRM-UT   PIC X(8).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900        05 MOD-IDLEVNR-IN    PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 MOD-IDLEVNR-UT    PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300        05 MOD-IDFS-IN       PIC X(8).                                    
002400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002500        05 MOD-IDFS-UT       PIC X(8).                                    
002600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002700        05 MOD-IDLBBET-IN    PIC X(12).                                   
002800*                                 LASTBÄRARBETECKNING                     
002900        05 MOD-IDLBBET-UT    PIC X(12).                                   
003000*                                 LASTBÄRARBETECKNING                     
003100        05 MOD-IDDC-IN       PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300        05 MOD-IDDC-UT       PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500        05 MOD-ADINLOMR-PRT-ATTR                                          
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-ADINLOMR-PRT  PIC X(4).                                    
003900*                                 PRINTERPLACERING                        
004000        05 MOD-KDPRTVAL-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-KDPRTVAL      PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400     03 MOD-SPAR-IDLOPNRM    PIC 9(9).                                    
004500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004600*                                 (0VVDLLLLK)                             
004700     03 MOD-KVAVIS           PIC X(6).                                    
004800*                                 AVISERAT ANTAL                          
004900     03 MOD-BEART            PIC X(25).                                   
005000*                                 ARTIKELBENÄMNING                        
005100     03 MOD-KDSORT           PIC X(2).                                    
005200*                                 SORT-KOD                                
005300     03 MOD-KDFARLIG-TXT     PIC X(10).                                   
005400     03 MOD-RAD              OCCURS 12 TIMES.                             
005500*                                 LINES                                   
005600        05 MOD-KVFLETI-ATTR  PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-KVFLETI       PIC X(2).                                    
005900        05 MOD-KVINLART-ATTR PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KVINLART      PIC X(6).                                    
006200*                                 ANTAL I PARTIRAD                        
006300     03 MOD-TEMFSINF         PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 393 BYTES                                 
