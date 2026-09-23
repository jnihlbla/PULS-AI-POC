000100 01  MOD-W2O13401.                                                        
000200*                                 MOD-COPYTEXT FÖR W2013400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BEART            PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 MOD-KVPB-TPO-C1-ATTR PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-KVPB-TPO-C1      PIC Z(5)9.9.                                 
001600*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
001700     03 MOD-KVPB-TPO-C1-UPP-ATTR                                          
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-KVPB-TPO-C1-UPP  PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-FLRADREF-ATTR    PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-FLRADREF         PIC X.                                       
002500*                                 KOMPLETTERANDE INFO. KRÄVS              
002600     03 MOD-FLRADREF-UPP-ATTR                                             
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLRADREF-UPP     PIC X(2).                                    
003000*                                 MFS BEHANDLING AV INPUTFÄLT             
003100     03 MOD-KVPB-TPO-C2      PIC Z(5)9.9.                                 
003200*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
003300     03 MOD-RECLPROC-C1-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-RECLPROC-C1      PIC Z(2)9.                                   
003600*                                 ANDEL SOM EJ FÅR ORDER-CLEARAS          
003700     03 MOD-FLTPO1-ATTR      PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-FLTPO1           PIC X.                                       
004000*                                 ARTIKELN GODKÄND FÖR TPO1               
004100     03 MOD-FLTPO1-UPP-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-FLTPO1-UPP       PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500     03 MOD-RECLPROC-C2-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-RECLPROC-C2      PIC Z(2)9.                                   
004800*                                 ANDEL SOM EJ FÅR ORDER-CLEARAS          
004900     03 MOD-KVFRYSTI-ATTR    PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-KVFRYSTI         PIC Z9.                                      
005200*                                 FRYSTID FÖR TPO-ORDER                   
005300     03 MOD-KVFRYSTI-UPP-ATTR                                             
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KVFRYSTI-UPP     PIC X(2).                                    
005700*                                 MFS BEHANDLING AV INPUTFÄLT             
005800     03 MOD-KDOPPLAN-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-KDOPPLAN         PIC X.                                       
006100*                                 OPTIMAL PLAN INOM FRYSTID               
006200     03 MOD-KDOPPLAN-UPP-ATTR                                             
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-KDOPPLAN-UPP     PIC X(2).                                    
006600*                                 MFS BEHANDLING AV INPUTFÄLT             
006700     03 MOD-KDUART-ATTR      PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-KDUART           PIC X.                                       
007000*                                 UNDANTAGSARTIKEL                        
007100     03 MOD-KDUART-UPP-ATTR  PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-KDUART-UPP       PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500     03 MOD-FLMANOSK-ATTR    PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-FLMANOSK         PIC X.                                       
007800*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
007900     03 MOD-FLMANOSK-UPP-ATTR                                             
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-FLMANOSK-UPP     PIC X(2).                                    
008300*                                 MFS BEHANDLING AV INPUTFÄLT             
008400     03 MOD-FLAUTREL-ATTR    PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-FLAUTREL         PIC X.                                       
008700*                                 AUT. SPÄRR FÖR VOR RELEASE              
008800     03 MOD-FLAUTREL-UPP-ATTR                                             
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-FLAUTREL-UPP     PIC X(2).                                    
009200*                                 MFS BEHANDLING AV INPUTFÄLT             
009300     03 MOD-PRORDSK-ATTR     PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-PRORDSK          PIC Z(4)9.9(2).                              
009600*                                 ORDERSÄRKOSTNAD                         
009700     03 MOD-PRORDSK-UPP-ATTR PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-PRORDSK-UPP      PIC X(2).                                    
010000*                                 MFS BEHANDLING AV INPUTFÄLT             
010100     03 MOD-TEMFSINF         PIC X(55).                                   
010200*                                 INFORMATIONSMEDDELANDE                  
010300*** END OF VILMAII-COPY LENGTH= 238 BYTES                                 
