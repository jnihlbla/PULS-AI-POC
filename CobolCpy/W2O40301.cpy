000100 01  W2O40301.                                                            
000200*                                 COPYTEXT FÖR MOD W2O40301               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 IDARTNR-IN           PIC X(2).                                    
000600*                                 MFS BEHANDLING AV INPUTFÄLT             
000700     03 IDARTNR.                                                          
000800        05 IDARTNR-UT        PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000        05 STRECK-1          PIC X.                                       
001100        05 REKSIFFR          PIC X.                                       
001200*                                 KONTROLLSIFFRA                          
001300     03 IDDC-IN              PIC X(2).                                    
001400*                                 MFS BEHANDLING AV INPUTFÄLT             
001500     03 IDDC-UT              PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 KDAVROP-IN           PIC X.                                       
001800*                                 AVROPSKOD                               
001900     03 KDAVROP-UT           PIC X.                                       
002000*                                 AVROPSKOD                               
002100     03 IDLEVNR-IN           PIC X(2).                                    
002200*                                 MFS BEHANDLING AV INPUTFÄLT             
002300     03 IDLEVNR-UT           PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500     03 IDLEVNR-SHIP         PIC X(5).                                    
002600*                                 SKEPPANDE LEVERANTÖR                    
002700     03 TEXT-PLANTYP         PIC X(8).                                    
002800     03 TIOMSPEC             PIC Z(6).                                    
002900*                                                    TIOMSPEC-002         
003000*                                 OMSPEC-DATUM (ÅÅVV) ELLER               
003100*                                 LEVERANSPLANEDATUM (ÅÅMMDD)             
003200     03 TEMFSFEL             PIC X(40).                                   
003300*                                 MFS FELMEDDELANDE                       
003400     03 BEART-ENG            PIC X(25).                                   
003500*                                 ENGELSK ARTIKELBENÄMNING                
003600     03 TEXT-ORSAK1-ATTR     PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 TEXT-ORSAK1          PIC X(8).                                    
003900     03 TEXT-ORSAK2-ATTR     PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 TEXT-ORSAK2          PIC X(8).                                    
004200     03 KDERS-ATTR           PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400     03 KDERS                PIC Z9.                                      
004500*                                 ERSÄTTNINGSKOD                          
004600     03 FLERSATT             PIC X.                                       
004700*                                 ERSATT I VIPS                           
004800     03 IDARTNR-ERS          PIC X(9).                                    
004900*                                 ERSATT ARTIKELNUMMER                    
005000     03 IDARTNR-TILLK        PIC X(9).                                    
005100*                                 TILLKOMMANDE ARTIKELNUMMER              
005200     03 TELPORSX             PIC X(10).                                   
005300*                                 LEVERANSPLANEORSAK VARNING              
005400     03 IDANSK               PIC Z(2)9.                                   
005500*                                 ANSKAFFARNUMMER                         
005600     03 KDAVT                PIC 9.                                       
005700*                                 AVTALSMÄRKNING                          
005800     03 KVVECKOR-LT          PIC Z9.                                      
005900*                                 ANTAL VECKOR LEDTID                     
006000     03 TIAAVVD-DAPUBL       PIC 9(5).                                    
006100*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
006200     03 TIURPROD             PIC 9(4).                                    
006300*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
006400     03 KDLPSP-ATTR          PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 KDLPSP               PIC 9.                                       
006700*                                 LEVERANSPLANESPÄRR                      
006800     03 TILPSP-ATTR          PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 TILPSP               PIC X(4).                                    
007100*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
007200     03 FLAGGA-SEASON        PIC X.                                       
007300*                                 ALLMÄN FLAGGA                           
007400     03 FLAGGA-TREND         PIC X.                                       
007500*                                 ALLMÄN FLAGGA                           
007600     03 KVPB-REF-SUM         PIC Z(5)9.9.                                 
007700*                                 PERIODBEHOV REFILLING                   
007800     03 ARRS                 OCCURS 5 TIMES.                              
007900        05 AVROP-ATTR        PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 KVAVROP-GAM       PIC Z(7).                                    
008200*                                 AVROPSKVANTITET                         
008300        05 SKILJETECKEN-GAM  PIC X.                                       
008400        05 TIAVROP-AVS-GAM   PIC X(4).                                    
008500*                                 AVSÄNDNINGSVECKA (PLANERAD)             
008600*                                 (ÅÅVV)                                  
008700     03 RADER                OCCURS 10 TIMES.                             
008800*                                 AVROPSTABELL   NYA AVROP                
008900        05 PERIOD.                                                        
009000           07 PERIOD-AAPP    PIC 9(4).                                    
009100*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
009200*                                 12 PER ÅR (OCKSÅ LOGISTIKPER)           
009300           07 PERIOD-PARENTES                                             
009400                             PIC X.                                       
009500*                                 TECKEN                                  
009600        05 AVROP-TABELL      OCCURS 5 TIMES.                              
009700           07 AVROP-TAB-ATTR PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900           07 AVROP-TAB.                                                  
010000              09 KVAVROP-TAB PIC Z(6).                                    
010100*                                 AVROPSKVANTITET                         
010200              09 IDTECKEN-TAB                                             
010300                             PIC X.                                       
010400              09 TIAVROP-AVS-TAB                                          
010500                             PIC Z(2).                                    
010600*                                                 TIAVROP-AVS-002         
010700*                                 AVSÄNDNINGSDATUM (VV)                   
010800              09 IDTECKEN-PARENTES-TAB                                    
010900                             PIC X.                                       
011000     03 KDKOM-IN-ATTR        PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 KDKOM-IN             PIC X.                                       
011300*                                 KOMMENTARKOD                            
011400     03 KDOMSPEC-IN-ATTR     PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 KDOMSPEC-IN          PIC X.                                       
011700*                                 OMSPEC 1=VANLIG,2=OPTIMAL               
011800     03 AVROP-INPUT          OCCURS 4 TIMES.                              
011900        05 TIAVROP-AVS-IN-ATTR                                            
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200        05 TIAVROP-AVS-IN    PIC X(4).                                    
012300*                                 AVROPSVECKA   (ÅÅVV)                    
012400        05 KVAVROP-IN-ATTR   PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 KVAVROP-IN        PIC Z(7).                                    
012700*                                 AVROPSKVANTITET                         
012800     03 KDLEVPLF-IN-ATTR     PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 KDLEVPLF-IN          PIC X.                                       
013100*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
013200     03 FLJIT-IN-ATTR        PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 FLJIT-IN             PIC X.                                       
013500*                                 JUST-IN-TIME FLAGGA                     
013600     03 TILPSP-IN-ATTR       PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800     03 TILPSP-IN            PIC X(4).                                    
013900     03 TEREFMED1-ATTR       PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 TEREFMED1            PIC X(36).                                   
014200     03 TEREFMED2-ATTR       PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 TEREFMED2            PIC X(36).                                   
014500     03 TEMFSINF             PIC X(55).                                   
014600*                                 INFORMATIONSMEDDELANDE                  
014700*** END OF VILMAII-COPY LENGTH= 1130 BYTES                                
