000100 01  MOD-W6O13601.                                                        
000200*                                 COPYTEXT FÖR MID   W6013600             
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
000800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000900*                                 (0VVDLLLLK)                             
001000     03 MOD-IDDC-IN          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
001300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001400*                                 (0VVDLLLLK)                             
001500     03 MOD-IDDC-UT          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-IDARTNR          PIC Z(7)9.                                   
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-KVAVIS-ATTR      PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-KVAVIS           PIC Z(5)9.                                   
002400*                                 AVISERAT ANTAL                          
002500     03 MOD-BEART-ATTR       PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-BEART            PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900     03 MOD-KDSORT-ATTR      PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-KDSORT           PIC X(2).                                    
003200*                                 SORT-KOD                                
003300     03 MOD-BEFT-ATTR        PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-BEFT             PIC Z9.                                      
003600*                                 FÖRPACKNINGSTYP                         
003700     03 MOD-BEFARLIG-ATTR    PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-BEFARLIG         PIC X(10).                                   
004000     03 MOD-KVAVIS-KVAR-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-KVAVIS-KVAR      PIC Z(5)9.                                   
004300*                                 AVISERAT ANTAL                          
004400     03 MOD-KVAVIS-FPK-KVAR-ATTR                                          
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KVAVIS-FPK-KVAR  PIC Z(5)9.                                   
004800*                                 AVISERAT ANTAL                          
004900     03 MOD-KVAVIS-PRIO-KVAR-ATTR                                         
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KVAVIS-PRIO-KVAR PIC Z(5)9.                                   
005300*                                 AVISERAT ANTAL                          
005400     03 MOD-KDLAGEMB-ATTR    PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KDLAGEMB         PIC X(4).                                    
005700*                                 EMBALLAGEBETECKNING                     
005800     03 MOD-ADLAGOMR-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-ADLAGOMR         PIC Z9.                                      
006100*                                 LAGEROMRÅDE                             
006200     03 MOD-ADGANG-ATTR      PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-ADGANG           PIC Z9.                                      
006500*                                 GÅNG                                    
006600     03 MOD-ADPLATS-ATTR     PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-ADPLATS          PIC Z(4)9.                                   
006900*                                 LAGERPLATSNUMMER                        
007000     03 MOD-FLKVAANT-TOT-ATTR                                             
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-FLKVAANT-TOT     PIC X.                                       
007400*                                 ANTALSKONTROLL UTFÖRD                   
007500     03 MOD-KVAVIS-KIT-KVAR-ATTR                                          
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-KVAVIS-KIT-KVAR  PIC Z(5)9.                                   
007900*                                 AVISERAT ANTAL                          
008000     03 MOD-ADTRDEST-KIT     PIC X(3).                                    
008100*                                 TRANSPORTDESTINATION SATSER             
008200     03 MOD-KVROS            PIC Z(5)9.                                   
008300*                                 RESTORDERSALDO                          
008400     03 MOD-INDATA.                                                       
008500*                                 UPDATE                                  
008600        05 MOD-FLPREPRA-IN-ATTR                                           
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-FLPREPRA-IN   PIC X.                                       
009000*                                 FÖRBEHANDLINGSRAPPORTSFLAGGA            
009100        05 MOD-RAD           OCCURS 6 TIMES.                              
009200*                                 UPDATE                                  
009300           07 MOD-KDFLETI-IN-ATTR                                         
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600           07 MOD-KDFLETI-IN PIC X(2).                                    
009700*                                 FLAGGA/ETIKETTVAL                       
009800           07 MOD-KVFLETI-IN-ATTR                                         
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100           07 MOD-KVFLETI-IN PIC X(2).                                    
010200*                                 ANTAL FLAGGOR EL ETIKETTER              
010300           07 MOD-KVINLART-IN-ATTR                                        
010400                             PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600           07 MOD-KVINLART-IN                                             
010700                             PIC X(6).                                    
010800*                                 ANTAL I PARTIRAD                        
010900           07 MOD-KDKLIPRI-IN-ATTR                                        
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200           07 MOD-KDKLIPRI-IN                                             
011300                             PIC X.                                       
011400*                                 PRIORITETSKOD KOLLI                     
011500           07 MOD-FLSATS-IN-ATTR                                          
011600                             PIC X(2).                                    
011700*                                 MFS ATTRIBUTFÄLT                        
011800           07 MOD-FLSATS-IN  PIC X.                                       
011900*                                 SATSARTIKEL                             
012000           07 MOD-FLPREPKL-IN-ATTR                                        
012100                             PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300           07 MOD-FLPREPKL-IN                                             
012400                             PIC X.                                       
012500*                                 FÖRPACKAT?                              
012600        05 MOD-KVAVIS-MOT-IN-ATTR                                         
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900        05 MOD-KVAVIS-MOT-IN PIC X(6).                                    
013000*                                 AVISERAT ANTAL                          
013100        05 MOD-IDANSTNR-IN-ATTR                                           
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400        05 MOD-IDANSTNR-IN   PIC X(5).                                    
013500*                                 ANSTÄLLNINGSNUMMER                      
013600     03 MOD-ADINLOMR-PRT-ATTR                                             
013700                             PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
014000*                                 PRINTERPLACERING                        
014100     03 MOD-FLSVS-ATTR       PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300     03 MOD-FLSVS            PIC X(2).                                    
014400*                                 MFS BEHANDLING AV INPUTFÄLT             
014500     03 MOD-IDINLVGN-IN      PIC X(3).                                    
014600*                                 VAGNSIDENTITET                          
014700     03 MOD-IDINLVGN-UT      PIC X(3).                                    
014800*                                 VAGNSIDENTITET                          
014900     03 MOD-ADINLOMR-IN      PIC X(4).                                    
015000*                                 INLEVERANSOMRÅDE                        
015100     03 MOD-ADINLOMR-UT      PIC X(4).                                    
015200*                                 INLEVERANSOMRÅDE                        
015300     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
015400*                                 INLEVERANSOMRÅDE NÄSTA                  
015500     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
015600*                                 INLEVERANSOMRÅDE NÄSTA                  
015700     03 MOD-KDINLQ-IN        PIC X.                                       
015800*                                 INLEVERANSKÖTYP KOLLI/PARTI             
015900     03 MOD-KDINLQ-UT        PIC X.                                       
016000*                                 INLEVERANSKÖTYP KOLLI/PARTI             
016100     03 MOD-BEFT-FOM-IN      PIC X(2).                                    
016200*                                 FÖRPACKNINGSTYP                         
016300     03 MOD-BEFT-FOM-UT      PIC X(2).                                    
016400*                                 FÖRPACKNINGSTYP                         
016500     03 MOD-BEFT-TOM-IN      PIC X(2).                                    
016600*                                 FÖRPACKNINGSTYP                         
016700     03 MOD-BEFT-TOM-UT      PIC X(2).                                    
016800*                                 FÖRPACKNINGSTYP                         
016900     03 MOD-FLINLFB-IN       PIC X.                                       
017000*                                 VALD TILL FÖRBEHANDLING                 
017100     03 MOD-FLINLFB-UT       PIC X.                                       
017200*                                 VALD TILL FÖRBEHANDLING                 
017300     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
017400*                                 LEVERANTÖRNUMMER KOLLI                  
017500     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
017600*                                 LEVERANTÖRNUMMER KOLLI                  
017700     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
017800*                                 ODETTE KOLLINUMMER                      
017900     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
018000*                                 ODETTE KOLLINUMMER                      
018100     03 MOD-TEMFSINF         PIC X(55).                                   
018200*                                 INFORMATIONSMEDDELANDE                  
018300*** END OF VILMAII-COPY LENGTH= 491 BYTES                                 
