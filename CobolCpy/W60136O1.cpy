000100 01  RESP-W60136O1.                                                       
000200*                                 COPYTEXT FÖR RESP  W6013600             
000300     03 RESP-IDLOPNRM        PIC X(9).                                    
000400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000500*                                 (0VVDLLLLK)                             
000600     03 RESP-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 RESP-IDARTNR-ATTR    PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 RESP-IDARTNR         PIC Z(7)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 RESP-KVAVIS-ATTR     PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 RESP-KVAVIS          PIC Z(5)9.                                   
001500*                                 AVISERAT ANTAL                          
001600     03 RESP-BEART-ATTR      PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 RESP-BEART           PIC X(25).                                   
001900*                                 ARTIKELBENÄMNING                        
002000     03 RESP-KDSORT-ATTR     PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 RESP-KDSORT          PIC X(2).                                    
002300*                                 SORT-KOD                                
002400     03 RESP-BEFT-ATTR       PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 RESP-BEFT            PIC Z9.                                      
002700*                                 FÖRPACKNINGSTYP                         
002800     03 RESP-BEFARLIG-ATTR   PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 RESP-BEFARLIG        PIC X(10).                                   
003100     03 RESP-KVAVIS-KVAR-ATTR                                             
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 RESP-KVAVIS-KVAR     PIC Z(5)9.                                   
003500*                                 AVISERAT ANTAL                          
003600     03 RESP-KVAVIS-FPK-KVAR-ATTR                                         
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 RESP-KVAVIS-FPK-KVAR PIC Z(5)9.                                   
004000*                                 AVISERAT ANTAL                          
004100     03 RESP-KVAVIS-PRIO-KVAR-ATTR                                        
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 RESP-KVAVIS-PRIO-KVAR                                             
004500                             PIC Z(5)9.                                   
004600*                                 AVISERAT ANTAL                          
004700     03 RESP-KDLAGEMB-ATTR   PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 RESP-KDLAGEMB        PIC X(4).                                    
005000*                                 EMBALLAGEBETECKNING                     
005100     03 RESP-ADLAGOMR-ATTR   PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 RESP-ADLAGOMR        PIC Z9.                                      
005400*                                 LAGEROMRÅDE                             
005500     03 RESP-ADGANG-ATTR     PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 RESP-ADGANG          PIC Z9.                                      
005800*                                 GÅNG                                    
005900     03 RESP-ADPLATS-ATTR    PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 RESP-ADPLATS         PIC Z(4)9.                                   
006200*                                 LAGERPLATSNUMMER                        
006300     03 RESP-FLKVAANT-TOT-ATTR                                            
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 RESP-FLKVAANT-TOT    PIC X.                                       
006700*                                 ANTALSKONTROLL UTFÖRD                   
006800     03 RESP-KVAVIS-KIT-KVAR-ATTR                                         
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 RESP-KVAVIS-KIT-KVAR PIC Z(5)9.                                   
007200*                                 AVISERAT ANTAL                          
007300     03 RESP-ADTRDEST-KIT    PIC X(3).                                    
007400*                                 TRANSPORTDESTINATION SATSER             
007500     03 RESP-KVROS           PIC Z(5)9.                                   
007600*                                 RESTORDERSALDO                          
007700     03 RESP-ADINLOMR-PRT-ATTR                                            
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 RESP-ADINLOMR-PRT    PIC X(4).                                    
008100*                                 PRINTERPLACERING                        
008200     03 RESP-FLSVS-ATTR      PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 RESP-FLSVS           PIC X(2).                                    
008500*                                 MFS BEHANDLING AV INPUTFÄLT             
008600     03 RESP-KVRADER         PIC 9(5).                                    
008700*                                 ANTAL RADER                             
008800     03 RESP-BEPRTLST        PIC X(25).                                   
008900*                                 LOGISK LISTA+PRINTER BENÄMNING          
009000     03 RESP-IDLEVNR-KOLLI   PIC X(5).                                    
009100*                                 LEVERANTÖRNUMMER KOLLI                  
009200     03 RESP-IDOKOLLI        PIC Z(8)9.                                   
009300*                                 ODETTE KOLLINUMMER                      
009400     03 RESP-IDLEVNR         PIC X(5).                                    
009500*                                 LEVERANTÖRNUMMER                        
009600     03 RESP-IDFS            PIC X(8).                                    
009700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
009800     03 RESP-TIAVIDAT        PIC 9(6).                                    
009900*                                 AVISERINGSDATUM (YYMMDD)                
010000     03 RESP-IDRADNR-INL     PIC 9(5).                                    
010100*                                 RADNUMMER INLEVERANS                    
010200     03 RESP-VKKOLLIN        PIC 9(5)V9(1).                               
010300*                                 KOLLI-VIKT-NETTO                        
010400     03 RESP-TIINLMOT        PIC 9(6).                                    
010500*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
010600     03 RESP-INDATA.                                                      
010700*                                 UPDATE                                  
010800        05 RESP-FLPREPRA-ATTR                                             
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 RESP-FLPREPRA     PIC X.                                       
011200*                                 FÖRBEHANDLINGSRAPPORTSFLAGGA            
011300        05 RESP-KVAVIS-MOT-ATTR                                           
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 RESP-KVAVIS-MOT   PIC X(6).                                    
011700*                                 AVISERAT ANTAL                          
011800        05 RESP-IDANSTNR-ATTR                                             
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100        05 RESP-IDANSTNR     PIC X(5).                                    
012200*                                 ANSTÄLLNINGSNUMMER                      
012300        05 RESP-LINES        OCCURS 500 TIMES.                            
012400*                                 UPDATE                                  
012500           07 RESP-KDFLETI-LINE-ATTR                                      
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800           07 RESP-KDFLETI-LINE                                           
012900                             PIC X(2).                                    
013000*                                 FLAGGA/ETIKETTVAL                       
013100           07 RESP-KVFLETI-LINE-ATTR                                      
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400           07 RESP-KVFLETI-LINE                                           
013500                             PIC X(2).                                    
013600*                                 ANTAL FLAGGOR EL ETIKETTER              
013700           07 RESP-KVINLART-LINE1-ATTR                                    
013800                             PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000           07 RESP-KVINLART-LINE1                                         
014100                             PIC X(6).                                    
014200*                                 ANTAL I PARTIRAD                        
014300           07 RESP-KDKLIPRI-LINE-ATTR                                     
014400                             PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600           07 RESP-KDKLIPRI-LINE                                          
014700                             PIC X.                                       
014800*                                 PRIORITETSKOD KOLLI                     
014900           07 RESP-FLSATS-LINE-ATTR                                       
015000                             PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200           07 RESP-FLSATS-LINE                                            
015300                             PIC X.                                       
015400*                                 SATSARTIKEL                             
015500           07 RESP-FLPREPKL-LINE-ATTR                                     
015600                             PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800           07 RESP-FLPREPKL-LINE                                          
015900                             PIC X.                                       
016000*                                 FÖRPACKAT?                              
016100*** END OF VILMAII-COPY LENGTH= 12749 BYTES                               
