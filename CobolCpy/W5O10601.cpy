000100 01  W5O10601.                                                            
000200     03 IDTRANS              PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 TEMFSFEL             PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 IDARTNR-IN           PIC X(2).                                    
000700*                                 MFS BEHANDLING AV INPUTFÄLT             
000800     03 IDARTNR-UT           PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC-IN              PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDDC-UT              PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 KOLLA-PF11-DOLD      PIC X.                                       
001500     03 BEART-ENG            PIC X(25).                                   
001600*                                 ENGELSK ARTIKELBENÄMNING                
001700     03 PRARTSTD             PIC Z(6)9.9(2).                              
001800*                                 ARTIKELSTANDARDPRIS                     
001900     03 KVAVIS               PIC Z(6)9.                                   
002000*                                 AVISERAT ANTAL                          
002100     03 TIAAVVD              PIC 9(5).                                    
002200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002300     03 KDERS                PIC 9(2).                                    
002400*                                 ERSÄTTNINGSKOD                          
002500     03 KDINVKAT             PIC 9(2).                                    
002600*                                 INVENTERINGSKATEGORI                    
002700     03 TIM-INV              PIC 9(6).                                    
002800*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
002900     03 KVAKS                PIC Z(6)9-.                                  
003000*                                 ANKOMSTSALDO                            
003100     03 KVLS-ATTR            PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 KVLS-X.                                                           
003400        05 KVLS              PIC Z(6)9-.                                  
003500*                                 LAGERSALDO                              
003600     03 KVEFRS               PIC Z(6)9-.                                  
003700*                                 EJ FAKTURERAT ANTAL STYCK               
003800     03 KVROS                PIC Z(6)9-.                                  
003900*                                 RESTORDERSALDO                          
004000     03 SALDO-KVBUFF-OF      PIC Z(6)9.                                   
004100*                                 BUFFERSALDO OFÖRÄDLAT GODS              
004200     03 SALDO-KVBUFF-F       PIC Z(6)9.                                   
004300*                                 FÖRÄDLAT BUFFERSALDO                    
004400     03 KVUTRS-ATTR          PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 KVUTRS-X.                                                         
004700        05 KVUTRS            PIC Z(6)9-.                                  
004800*                                 UTREDNINGSSALDO                         
004900     03 KVJUSTKV-IN-ATTR     PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 KVJUSTKV-IN          PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300     03 KDAVVTYP-ATTR        PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 KDAVVTYP             PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700     03 FLANTAL-ATTR         PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 FLANTAL              PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100     03 FLSLACK-ATTR         PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 FLSLACK              PIC X(2).                                    
006400*                                 FLAGGA SLÄCKNING AV INVENTERING         
006500*                                 1 = JA. ANNAT = NEJ.                    
006600     03 FLFLYTTN-ATTR        PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 FLFLYTTN             PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000     03 FILLER               OCCURS 6 TIMES.                              
007100        05 TIJUSTDA-ATTR     PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 TIJUSTDA-X.                                                    
007400           07 TIJUSTDA       PIC 9(5).                                    
007500*                                 JUSTERINGSDATUM                         
007600        05 KVJUSTKV-UT-ATTR  PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 KVJUSTKV-UT-X.                                                 
007900           07 KVJUSTKV-UT    PIC Z(6)9-.                                  
008000*                                 JUSTERAD KVANTITET                      
008100        05 KDJUSTYP-ATTR     PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 KDJUSTYP-X.                                                    
008400           07 KDJUSTYP       PIC 9.                                       
008500*                                 JUSTERINGSTYP                           
008600     03 KVLS-LAGR            PIC X(8).                                    
008700*                                 LAGERSALDO                              
008800     03 KVUTRS-LAGR          PIC X(8).                                    
008900*                                 UTREDNINGSSALDO                         
009000     03 HEADING              PIC X(12).                                   
009100     03 PRAVCOST             PIC Z(7).Z(2).                               
009200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
009300     03 TEMFSINF             PIC X(55).                                   
009400*                                 INFORMATIONSMEDDELANDE                  
009500*** END OF VILMAII-COPY LENGTH= 408 BYTES                                 
