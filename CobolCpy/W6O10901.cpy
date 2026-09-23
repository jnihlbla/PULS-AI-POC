000100 01  MOD-W6O10901.                                                        
000200*                                 MODCOPYTEXT TILL W601109                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
001000*                                 ODETTE KOLLINUMMER                      
001100     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
001900*                                 ODETTE KOLLINUMMER                      
002000     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
002100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002200*                                 (0VVDLLLLK)                             
002300     03 MOD-IDDC-UT          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDARTNR          PIC Z(7)9.                                   
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-KVAVIS-HUV       PIC Z(5)9.                                   
002800*                                 AVISERAT ANTAL                          
002900     03 MOD-BEART            PIC X(25).                                   
003000*                                 ARTIKELBENÄMNING                        
003100     03 MOD-KDKVAINL         PIC X(2).                                    
003200*                                 INLEVERANS TILL KVALITETSKOLL           
003300     03 MOD-IDFS             PIC X(8).                                    
003400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003500     03 MOD-TIAVIDAT         PIC 9(6).                                    
003600*                                 AVISERINGSDATUM (YYMMDD)                
003700     03 MOD-IDLBBET          PIC X(12).                                   
003800*                                 LASTBÄRARBETECKNING                     
003900     03 MOD-KDRT             PIC Z9.                                      
004000*                                 REDOVISNINGSTYP                         
004100     03 MOD-FLKVAANT         PIC X.                                       
004200*                                 ANTALSKONTROLL UTFÖRD                   
004300     03 MOD-IDLEVNR          PIC X(5).                                    
004400*                                 LEVERANTÖRNUMMER                        
004500     03 MOD-TIINLMOT         PIC 9(6).                                    
004600*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
004700     03 MOD-ADINLOMR         OCCURS 6 TIMES                               
004800                             PIC X(4).                                    
004900*                                 INLEVERANSOMRÅDE                        
005000     03 MOD-KVAVIS           PIC Z(5)9.                                   
005100*                                 AVISERAT ANTAL                          
005200     03 MOD-KDSORT-VIKT      PIC X(2).                                    
005300*                                 SORT-KOD                                
005400     03 MOD-VKART            PIC Z(6)9.                                   
005500*                                 ARTIKELVIKT (G)                         
005600     03 MOD-ADLAGOMR         PIC Z9.                                      
005700*                                 LAGEROMRÅDE                             
005800     03 MOD-ADGANG           PIC Z9.                                      
005900*                                 GÅNG                                    
006000     03 MOD-ADPLATS          PIC Z(4)9.                                   
006100*                                 LAGERPLATSNUMMER                        
006200     03 MOD-KVAVIS-PRIO      PIC Z(5)9.                                   
006300*                                 BERÄKN PRIORITERAD KVANT TOT            
006400     03 MOD-BESORT-VOLYM     PIC X(6).                                    
006500*                                 BENÄMNING PÅ SORT/ENHET                 
006600     03 MOD-VLARTNTO         PIC Z(7)9.9.                                 
006700*                                 ARTIKELVOLYM NETTO (CM3)                
006800     03 MOD-ADBUFFOMR        PIC Z9.                                      
006900*                                 BUFFERTOMRÅDE                           
007000     03 MOD-ADBUFFGANG       PIC Z9.                                      
007100*                                 BUFFERT GÅNG                            
007200     03 MOD-ADBUFFPL         PIC Z(4)9.                                   
007300*                                 BUFFERPLATSNUMMER                       
007400     03 MOD-KVKVAPRIM        PIC Z(5)9.                                   
007500*                                 ANTAL TILL PRIMÄRKONTROLL               
007600     03 MOD-KDLAGEMB         PIC X(4).                                    
007700*                                 EMBALLAGEBETECKNING                     
007800     03 MOD-KDARTURS         PIC X(2).                                    
007900*                                 ARTIKELURSPRUNGSKOD                     
008000     03 MOD-KVKVASEK         PIC Z(5)9.                                   
008100*                                  ANTAL TILL SEKUNDÄRKONTROLL            
008200     03 MOD-BEFT             PIC Z9.                                      
008300*                                 FÖRPACKNINGSTYP                         
008400     03 MOD-IDANSK           PIC Z(2)9.                                   
008500*                                 ANSKAFFARNUMMER                         
008600     03 MOD-ADTRDEST         PIC X(3).                                    
008700*                                 TRANSPORTDESTINATION                    
008800     03 MOD-KVAVIS-KIT       PIC Z(5)9.                                   
008900*                                 AVISERAT ANTAL FÖR SATS                 
009000     03 MOD-KDSORT           PIC X(2).                                    
009100*                                 SORT-KOD                                
009200     03 MOD-KDFARLIG-TEXT    PIC X(15).                                   
009300     03 MOD-KVQPACK-3        PIC -(5)9.                                   
009400*                                 ANTAL I Q3 FÖRPACKNING                  
009500     03 MOD-KDKONTR          PIC Z(4)9.                                   
009600*                                 KVAL.KONTR.KOD DATAELEMENT UTG.         
009700     03 MOD-FLAR             PIC X.                                       
009800     03 MOD-BELEV1           PIC X(33).                                   
009900     03 MOD-BELEV2           PIC X(33).                                   
010000     03 MOD-ADINLOMR-PRT-ATTR                                             
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
010400*                                 PRINTERPLACERING                        
010500     03 MOD-TEMFSINF         PIC X(55).                                   
010600*                                 INFORMATIONSMEDDELANDE                  
010700*** END OF VILMAII-COPY LENGTH= 442 BYTES                                 
