000100 01  RESP-WL0171O1.                                                       
000200*                                 RESPONS FROM PGM WL0171                 
000300*                                                                         
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800     03 RESP-BEART-ENG       PIC X(25).                                   
000900*                                 ENGELSK ARTIKELBENÄMNING                
001000     03 RESP-TIAAVVD         PIC 9(5).                                    
001100*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001200     03 RESP-KVAVIS          PIC Z(5)9.                                   
001300*                                 AVISERAT ANTAL                          
001400     03 RESP-KDERS           PIC 9(2).                                    
001500*                                 ERSÄTTNINGSKOD                          
001600     03 RESP-KDINVKAT        PIC 9(2).                                    
001700*                                 INVENTERINGSKATEGORI                    
001800     03 RESP-TIM-INV         PIC 9(6).                                    
001900*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
002000     03 RESP-KVAKS           PIC Z(6)9-.                                  
002100*                                 ANKOMSTSALDO                            
002200     03 RESP-KVLS            PIC Z(6)9-.                                  
002300*                                 LAGERSALDO                              
002400     03 RESP-KVEFRS          PIC Z(6)9-.                                  
002500*                                 EJ FAKTURERAT ANTAL STYCK               
002600     03 RESP-KVROS           PIC Z(6)9-.                                  
002700*                                 RESTORDERSALDO                          
002800     03 RESP-KVUTRS          PIC Z(6)9-.                                  
002900*                                 UTREDNINGSSALDO                         
003000     03 RESP-KVJUSTKV-IN     PIC Z(7).                                    
003100*                                 JUSTERAD KVANTITET                      
003200     03 RESP-KDAVVTYP        PIC X.                                       
003300*                                 AVVIKELSETYP                            
003400*                                 1=POSITIV.  2=NEGATIV                   
003500     03 RESP-FLANTAL         PIC X.                                       
003600*                                 ANTALJUSTERINGSFLAGGA                   
003700*                                 1 = JA. ANNAT = NEJ.                    
003800*                                 5 = AVV. VID REFILL AV S-LAGER          
003900*                                 5 ANVÄNDS ENDAST I INVENTERING          
004000     03 RESP-FLSLACK         PIC X.                                       
004100*                                 ALLMÄN FLAGGA                           
004200     03 RESP-FLFLYTTN        PIC X.                                       
004300*                                 ALLMÄN FLAGGA                           
004400     03 RESP-KVRADER         PIC Z(4)9.                                   
004500*                                 ANTAL RADER                             
004600     03 RESP-FILLER          OCCURS 6 TIMES.                              
004700        05 RESP-TIJUSTDA     PIC 9(5).                                    
004800*                                 JUSTERINGSDATUM                         
004900        05 RESP-KVJUSTKV-UT  PIC -(6)9.                                   
005000*                                 JUSTERAD KVANTITET                      
005100        05 RESP-KDJUSTYP     PIC 9.                                       
005200*                                 JUSTERINGSTYP                           
005300*** END OF VILMAII-COPY LENGTH= 190 BYTES                                 
