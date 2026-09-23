000100 01  MOD-W4O10801.                                                        
000200*                                 MOD-COPYTEXT FÖR FRÅGE-BILD             
000300*                                 BUFFERTDATA-ARTIKEL  INFO               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-STRECK           PIC X.                                       
001300     03 MOD-REKSIFFR         PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-BEART            PIC X(25).                                   
002000*                                 ARTIKELBENÄMNING                        
002100     03 MOD-RUBR1            PIC X(3).                                    
002200     03 MOD-ADLAGOMR         PIC Z9.                                      
002300*                                 LAGEROMRÅDE                             
002400     03 MOD-ADGANG           PIC Z9.                                      
002500*                                 GÅNG                                    
002600     03 MOD-ADPLATS          PIC Z(4)9.                                   
002700*                                 LAGERPLATSNUMMER                        
002800     03 MOD-KVAKS            PIC -(7)9.                                   
002900*                                 ANKOMSTSALDO                            
003000     03 MOD-KVQPACK-1        PIC -(5)9.                                   
003100*                                 ANTAL I Q1 FÖRPACKNING                  
003200     03 MOD-KVLS             PIC -(7)9.                                   
003300*                                 LAGERSALDO                              
003400     03 MOD-KVUTRS           PIC -(7)9.                                   
003500*                                 UTREDNINGSSALDO                         
003600     03 MOD-KVQPACK-3        PIC -(5)9.                                   
003700*                                 ANTAL I Q3 FÖRPACKNING                  
003800     03 MOD-KVEFRS           PIC -(7)9.                                   
003900*                                 EJ FAKTURERAT ANTAL STYCK               
004000     03 MOD-KVPB-TOT         PIC Z(6)9.9.                                 
004100*                                 TOTALT PERIODBEHOV                      
004200     03 MOD-KVQPACK-4        PIC -(5)9.                                   
004300*                                 ANTAL I Q4 FÖRPACKNING                  
004400     03 MOD-KVROS            PIC -(7)9.                                   
004500*                                 RESTORDERSALDO                          
004600     03 MOD-KVPB-SATS        PIC Z(6)9.9.                                 
004700*                                 SATS-PERIODBEHOV                        
004800     03 MOD-KDERS            PIC Z9.                                      
004900*                                 ERSÄTTNINGSKOD                          
005000     03 MOD-BEFT             PIC Z9.                                      
005100*                                 FÖRPACKNINGSTYP                         
005200     03 MOD-KDPAF            PIC 9.                                       
005300*                                 PÅFYLLNADSKOD                           
005400     03 MOD-BUFF-ADR         OCCURS 7 TIMES.                              
005500        05 MOD-PLATSTYP      PIC X.                                       
005600        05 MOD-ADBUFFOMR     PIC Z9.                                      
005700*                                 BUFFERTOMRÅDE                           
005800        05 MOD-ADBUFFGANG    PIC Z9.                                      
005900*                                 BUFFERT GÅNG                            
006000        05 MOD-ADBUFFPL      PIC Z(4)9.                                   
006100*                                 BUFFERPLATSNUMMER                       
006200        05 MOD-KVBUFF-F      PIC -(7)9.                                   
006300*                                 FÖRÄDLAT BUFFERSALDO                    
006400        05 MOD-KVBUFF-OF     PIC -(7)9.                                   
006500*                                 BUFFERSALDO OFÖRÄDLAT GODS              
006600        05 MOD-KVKOLLI-F     PIC Z(3)9.                                   
006700*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
006800        05 MOD-KVKOLLI-OF    PIC Z(3)9.                                   
006900*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
007000        05 MOD-DABUFPAF      PIC Z(8).                                    
007100*                                 BUFFERT PÅFYLLNINGS DATUM               
007200     03 MOD-SUBUFF-F         PIC -(7)9.                                   
007300*                                 FÖRÄDLAT BUFFERSALDO                    
007400     03 MOD-SUBUFF-OF        PIC -(7)9.                                   
007500*                                 BUFFERSALDO OFÖRÄDLAT GODS              
007600     03 MOD-SUKOLLI-F        PIC Z(3)9.                                   
007700*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
007800     03 MOD-SUKOLLI-OF       PIC Z(3)9.                                   
007900*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
008000     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
008100*                                 MEDDELANDEFÄLT PÅ RAD 23                
008200*** END OF VILMAII-COPY LENGTH= 565 BYTES                                 
