000100 01  MOD-W6O12101-CTX.                                                    
000200*                                 COPYTEXT FOR MOD W6O12101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-W6O12001-001-GRP.                                             
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
004000     03 MOD-FLKLAR-TOT-IN    PIC X.                                       
004100*                                 AVSLUTNINGSMARKERING                    
004200     03 MOD-FLKLAR-TOT-UT    PIC X.                                       
004300*                                 AVSLUTNINGSMARKERING                    
004400     03 MOD-ENTER-IDARTNR    PIC 9(9).                                    
004500*                                 ARTIKELNUMMER                           
004600     03 MOD-ENTER-IDLEVNR    PIC X(5).                                    
004700*                                 LEVERANTÖRNUMMER                        
004800     03 MOD-ENTER-IDFS       PIC X(8).                                    
004900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005000     03 MOD-NEXT-IDARTNR     PIC 9(9).                                    
005100*                                 ARTIKELNUMMER                           
005200     03 MOD-NEXT-IDLEVNR     PIC X(5).                                    
005300*                                 LEVERANTÖRNUMMER                        
005400     03 MOD-NEXT-IDFS        PIC X(8).                                    
005500*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005600     03 MOD-NEXT             PIC X.                                       
005700     03 MOD-W6O12101-001-GRP OCCURS 13 TIMES.                             
005800*                                 LINES                                   
005900        05 MOD-KDCMDVAL-INPUT-ATTR                                        
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-KDCMDVAL-INPUT                                             
006300                             PIC X(3).                                    
006400*                                 GENERELL KOMMANDOKOD                    
006500        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-IDLEVNR       PIC X(5).                                    
006800*                                 LEVERANTÖRNUMMER                        
006900        05 MOD-IDFS-ATTR     PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100        05 MOD-IDFS          PIC X(8).                                    
007200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
007300        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDARTNR       PIC X(8).                                    
007600*                                 ARTIKELNUMMER                           
007700        05 MOD-KVAVIS-TOT-ATTR                                            
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-KVAVIS-TOT    PIC X(6).                                    
008100*                                 AVISERAT ANTAL                          
008200        05 MOD-KVKOLLI-ATTR  PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-KVKOLLI       PIC X(4).                                    
008500*                                 ANTAL KOLLI                             
008600        05 MOD-KDLAGEMB-ATTR PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-KDLAGEMB      PIC X(4).                                    
008900*                                 EMBALLAGEBETECKNING                     
009000        05 MOD-BEFT-ATTR     PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-BEFT          PIC X(2).                                    
009300*                                 FÖRPACKNINGSTYP                         
009400        05 MOD-ADINLOMR-NXT-ATTR                                          
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-ADINLOMR-NXT  PIC X(4).                                    
009800*                                 INLEVERANSOMRÅDE NÄSTA                  
009900        05 MOD-KVAVIS-ATTR   PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-KVAVIS        PIC X(6).                                    
010200*                                 AVISERAT ANTAL                          
010300        05 MOD-KVAVIS-PRIO-ATTR                                           
010400                             PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600        05 MOD-KVAVIS-PRIO   PIC X(6).                                    
010700*                                 AVISERAT ANTAL                          
010800        05 MOD-KVAVIS-KIT-ATTR                                            
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-KVAVIS-KIT    PIC X(6).                                    
011200*                                 AVISERAT ANTAL                          
011300        05 MOD-ADTRDEST-KIT-ATTR                                          
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 MOD-ADTRDEST-KIT  PIC X(3).                                    
011700*                                 TRANSPORTDESTINATION SATSER             
011800     03 MOD-FLKLAR-BIL-ATTR  PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-FLKLAR-BIL       PIC X.                                       
012100*                                 AVSLUTNINGSMARKERING                    
012200     03 MOD-ADINLOMR-ATTR    PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-ADINLOMR         PIC X(4).                                    
012500*                                 INLEVERANSOMRÅDE                        
012600     03 MOD-TELOSSN1         PIC X(39).                                   
012700     03 MOD-TELOSSN2         PIC X(66).                                   
012800     03 MOD-TEMFSINF         PIC X(55).                                   
012900*                                 INFORMATIONSMEDDELANDE                  
013000*** END OF VILMAII-COPY LENGTH= 1537 BYTES                                
