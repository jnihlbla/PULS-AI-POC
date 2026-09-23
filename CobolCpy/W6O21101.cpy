000100 01  MOD-W6O21101.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-IDARTNR-IN       PIC X(2).                                    
000700*                                 MFS BEHANDLING AV INPUTFÄLT             
000800     03 MOD-IDARTNR-UT       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDLEVNR-IN       PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 MOD-IDKVAINF-IN      PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600     03 MOD-IDKVAINF-UT      PIC X(2).                                    
001700*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
001800     03 MOD-TIREGDAT-IN      PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000     03 MOD-TIREGDAT-UT      PIC X(6).                                    
002100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002200     03 MOD-KDKVAINF-IN      PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400     03 MOD-KDKVAINF-UT      PIC X.                                       
002500*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
002600     03 MOD-IDFORDON-ENTER   PIC 9(3).                                    
002700*                                 FORDONSSLAG                             
002800     03 MOD-IDFORDON-NEXT    PIC 9(3).                                    
002900*                                 FORDONSSLAG                             
003000     03 MOD-TIOMBRYT-1-ENTER PIC 9(6).                                    
003100*                                 OMBRYTNINGSDATUM                        
003200     03 MOD-TIOMBRYT-1-NEXT  PIC 9(6).                                    
003300*                                 OMBRYTNINGSDATUM                        
003400     03 MOD-BEART-SVE        PIC X(25).                                   
003500*                                 SVENSK ARTIKELBENÄMNING                 
003600     03 MOD-KVLS             PIC -(6)9.                                   
003700*                                 LAGERSALDO                              
003800     03 MOD-BEART-ENG        PIC X(25).                                   
003900*                                 ENGELSK ARTIKELBENÄMNING                
004000     03 MOD-KVAKS-CDC        PIC Z(6)9.                                   
004100*                                 ANKOMSTSALDO                            
004200     03 MOD-KVAKS-PAV        PIC Z(6)9.                                   
004300*                                 ANKOMSTSALDO                            
004400     03 MOD-KVAKS-T          PIC Z(6)9.                                   
004500*                                 ANKOMSTSALDO                            
004600     03 MOD-IDLEVNR          PIC X(5).                                    
004700*                                 LEVERANTÖRNUMMER                        
004800     03 MOD-KVROS            PIC -(6)9.                                   
004900*                                 RESTORDERSALDO                          
005000     03 MOD-IDANSK           PIC Z(2)9.                                   
005100*                                 ANSKAFFARNUMMER                         
005200     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
005300*                                 FUNKTIONSGRUPP                          
005400     03 MOD-KVPB-SEP         OCCURS 3 TIMES                               
005500                             PIC Z(7)9.9.                                 
005600*                                 SEPARAT PERIODBEHOV                     
005700     03 MOD-IDBERED          PIC Z(2)9.                                   
005800*                                 BEREDARENUMMER                          
005900     03 MOD-KDSORT           PIC X(2).                                    
006000*                                 SORT-KOD                                
006100     03 MOD-KVPB-SATS        PIC Z(5)9.9.                                 
006200*                                 SATS-PERIODBEHOV                        
006300     03 MOD-FLGEMART         PIC X.                                       
006400*                                 FLAGGA GEMENSAM ARTIKEL                 
006500     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
006600*                                 ARTIKELSTANDARDPRIS                     
006700     03 MOD-BEFT-ATTR        PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-BEFT             PIC 9(3).                                    
007000*                                 FÖRPACKNINGSTYP                         
007100     03 MOD-ADARTADR.                                                     
007200        05 MOD-ADLAGOMR      PIC Z9B.                                     
007300*                                 LAGEROMRÅDE                             
007400        05 MOD-ADGANG        PIC Z9B.                                     
007500*                                 GÅNG                                    
007600        05 MOD-ADPLATS       PIC Z(4)9.                                   
007700*                                 LAGERPLATSNUMMER                        
007800     03 MOD-IDRITN           PIC X(10).                                   
007900*                                 RITNINGSNUMMER                          
008000     03 MOD-ERS-KOD.                                                      
008100        05 MOD-KDERS-ATTR    PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-KDERS         PIC 9(2).                                    
008400*                                 ERSÄTTNINGSKOD                          
008500     03 MOD-IDKAT            OCCURS 11 TIMES                              
008600                             PIC X(5).                                    
008700*                                 KATALOGBETECKNING                       
008800     03 MOD-TEARTNOT1        PIC X(40).                                   
008900*                                 ARTIKEL NOTERING                        
009000     03 MOD-TEARTNOT7        PIC X(40).                                   
009100*                                 ARTIKEL NOTERING                        
009200     03 MOD-KDYTBEH-IN-ATTR  PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-KDYTBEH-IN       PIC X(2).                                    
009500*                                 YTBEHANDLINGSKOD                        
009600     03 MOD-KDYTBEH-UT-ATTR  PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-KDYTBEH-UT       PIC Z9.                                      
009900*                                 YTBEHANDLINGSKOD                        
010000     03 MOD-KDFARLIG-IN-ATTR PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-KDFARLIG-IN      PIC 9.                                       
010300*                                 KOD FÖR FARLIGT GODS                    
010400     03 MOD-KDFARLIG-UT-ATTR PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 MOD-KDFARLIG-UT      PIC X.                                       
010700*                                 KOD FÖR FARLIGT GODS                    
010800     03 MOD-LIKARE           OCCURS 4 TIMES.                              
010900        05 MOD-IDLIKARE-ATTR PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-IDLIKARE      PIC X(9).                                    
011200*                                 LIKARIDENTITET                          
011300     03 MOD-KDLEVSP-UT       PIC Z9.                                      
011400*                                 SPÄRRKOD LEVERANS                       
011500     03 MOD-TEARTNOT2        PIC X(40).                                   
011600*                                 ARTIKEL NOTERING                        
011700     03 MOD-KDKVATYP-ATTR    PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-KDKVATYP         PIC X.                                       
012000*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
012100     03 MOD-KDKVATYP-TEXT-ATTR                                            
012200                             PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-KDKVATYP-TEXT    PIC X(30).                                   
012500     03 MOD-IDPROVPL-PRI-ATTR                                             
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800     03 MOD-IDPROVPL-PRI     PIC X.                                       
012900*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
013000     03 MOD-FLAGGA-KH        PIC X.                                       
013100*                                 ALLMÄN FLAGGA                           
013200     03 MOD-KDKVAINF         PIC X.                                       
013300*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
013400     03 MOD-FLUPG            PIC X.                                       
013500*                                 FLAGGA UTFALLSPROV GODKÄNT              
013600     03 MOD-TIUPG            PIC 9(6).                                    
013700*                                 TID NÄR UTFALLSPROV GJORTS              
013800     03 MOD-IDPROVPL-SEK-ATTR                                             
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-IDPROVPL-SEK     PIC X.                                       
014200*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
014300     03 MOD-FLAGGA-KR        PIC X.                                       
014400*                                 ALLMÄN FLAGGA                           
014500     03 MOD-KDTPD            PIC X.                                       
014600*                                 KOD TILLFÄLLIGT UTFALLSPROV             
014700     03 MOD-TITPD            PIC 9(6).                                    
014800*                                 TID TILLFÄLLIGT UTFALLSPROV             
014900     03 MOD-KDKVAULG-ATTR    PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100     03 MOD-KDKVAULG         PIC X.                                       
015200*                                 UNDERLAG FÖR KVALITETSKONTROLL          
015300     03 MOD-ADKVAULG-ATTR    PIC X(2).                                    
015400*                                 MFS ATTRIBUTFÄLT                        
015500     03 MOD-ADKVAULG         PIC X(2).                                    
015600*                                 PLATS UNDERLAG KVAL.KONTROLL            
015700     03 MOD-BEKVAULG-ATTR    PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900     03 MOD-BEKVAULG         PIC X(30).                                   
016000     03 MOD-TEMFSINF         PIC X(55).                                   
016100*                                 INFORMATIONSMEDDELANDE                  
016200*** END OF VILMAII-COPY LENGTH= 663 BYTES                                 
