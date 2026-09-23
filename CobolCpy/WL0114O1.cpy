000100 01  RESP-WL0114O1.                                                       
000200*                                 RESPONS FROM PGM WL0114                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000600*                                 ARTIKELNUMMER                           
000700     03 RESP-BEART           PIC X(25).                                   
000800*                                 ARTIKELBENÄMNING                        
000900     03 RESP-BEART-LOCAL     PIC X(100).                                  
001000*                                 UTÖKAD ARTIKELBENÄMNING                 
001100     03 RESP-BEART-FL        PIC X.                                       
001200*                                 ALLMÄN FLAGGA                           
001300     03 RESP-RUBR1           PIC X(3).                                    
001400     03 RESP-ADLAGOMR        PIC 9(2).                                    
001500*                                 LAGEROMRÅDE                             
001600     03 RESP-ADGANG          PIC 9(2).                                    
001700*                                 GÅNG                                    
001800     03 RESP-ADPLATS         PIC 9(5).                                    
001900*                                 LAGERPLATSNUMMER                        
002000     03 RESP-KVAKS           PIC -(7)9.                                   
002100*                                 ANKOMSTSALDO                            
002200     03 RESP-KVQPACK-1       PIC -(5)9.                                   
002300*                                 ANTAL I Q1 FÖRPACKNING                  
002400     03 RESP-KVLS            PIC -(7)9.                                   
002500*                                 LAGERSALDO                              
002600     03 RESP-KVUTRS          PIC -(7)9.                                   
002700*                                 UTREDNINGSSALDO                         
002800     03 RESP-KVQPACK-3       PIC -(5)9.                                   
002900*                                 ANTAL I Q3 FÖRPACKNING                  
003000     03 RESP-KVEFRS          PIC -(7)9.                                   
003100*                                 EJ FAKTURERAT ANTAL STYCK               
003200     03 RESP-KVPB-TOT        PIC Z(5)9.9.                                 
003300*                                 TOTALT PERIODBEHOV                      
003400     03 RESP-KVQPACK-4       PIC -(5)9.                                   
003500*                                 ANTAL I Q4 FÖRPACKNING                  
003600     03 RESP-KVROS           PIC -(7)9.                                   
003700*                                 RESTORDERSALDO                          
003800     03 RESP-KVPB-SATS       PIC Z(5)9.9.                                 
003900*                                 SATS-PERIODBEHOV                        
004000     03 RESP-KDERS           PIC Z9.                                      
004100*                                 ERSÄTTNINGSKOD                          
004200     03 RESP-BEFT            PIC Z9.                                      
004300*                                 FÖRPACKNINGSTYP                         
004400     03 RESP-KDPAF           PIC 9.                                       
004500*                                 PÅFYLLNADSKOD                           
004600     03 RESP-KVDISP-NDC      PIC -(7)9.                                   
004700*                                 DISPONIBELT LAGER                       
004800     03 RESP-KVOKS-NDC       PIC -(6)9.                                   
004900*                                 ORDERKÖSALDO                            
005000     03 RESP-KVAKS-PAV-NDC   PIC -(7)9.                                   
005100*                                 DEL AV AK PÅ VÄG                        
005200     03 RESP-KVBEART         PIC Z(5)9.                                   
005300*                                 BESTÄLLT ANTAL STYCKEN                  
005400     03 RESP-KDLEVSP-NDC     PIC Z9.                                      
005500*                                 SPÄRRKOD LEVERANS                       
005600     03 RESP-KVSPARR-KVAL-NDC                                             
005700                             PIC Z(6)9.                                   
005800*                                 SPÄRRAT ANTAL KVALITETSFEL              
005900     03 RESP-SUBUFF-F        PIC -(7)9.                                   
006000*                                 FÖRÄDLAT BUFFERSALDO                    
006100     03 RESP-SUBUFF-OF       PIC -(7)9.                                   
006200*                                 BUFFERSALDO OFÖRÄDLAT GODS              
006300     03 RESP-SUKOLLI-F       PIC Z(3)9.                                   
006400*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
006500     03 RESP-SUKOLLI-OF      PIC Z(3)9.                                   
006600*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
006700     03 RESP-KVPB-REF        PIC Z(5)9.9.                                 
006800*                                 PERIODBEHOV REFILLING                   
006900     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
007000*                                 MAX INDEX KOPPLAT TILL OCCURS N         
007100*                                 EDAN.                                   
007200     03 RESP-IDLEVNR         PIC X(5).                                    
007300*                                 LEVERANTÖRNUMMER                        
007400     03 RESP-VKART           PIC Z(7)9.                                   
007500*                                 ARTIKELVIKT (G)                         
007600     03 RESP-VLARTNTO        PIC Z(7)9.9.                                 
007700*                                 ARTIKELVOLYM (CM3)                      
007800     03 RESP-KDARTURS        PIC X(2).                                    
007900*                                 ARTIKELURSPRUNGSKOD                     
008000     03 RESP-KVRESS          PIC Z(6)9.                                   
008100*                                 RESERVERAT ANTAL ARTIKLAR               
008200     03 RESP-BELEV           PIC X(35).                                   
008300*                                 LEVERANTÖRSNAMN                         
008400     03 RESP-ADLEV-RAD1      PIC X(35).                                   
008500*                                 LEVERANTÖRENS GATUADRESS                
008600     03 RESP-ADLEV-ORT       PIC X(35).                                   
008700*                                 LEVERANTÖRSADRESS ORT                   
008800     03 RESP-ADLEVLND        PIC X(20).                                   
008900*                                 LEVERANTÖRSADRESS LAND                  
009000     03 RESP-BUFF-ADR        OCCURS 1 TO 200 TIMES                        
009100                             DEPENDING ON RESP-KVRADER-MAX1.              
009200        05 RESP-PLATSTYP     PIC X.                                       
009300        05 RESP-ADBUFFOMR    PIC 9(2).                                    
009400*                                 BUFFERTOMRÅDE                           
009500        05 RESP-ADBUFFGANG   PIC 9(2).                                    
009600*                                 BUFFERT GÅNG                            
009700        05 RESP-ADBUFFPL     PIC 9(5).                                    
009800*                                 BUFFERPLATSNUMMER                       
009900        05 RESP-KVBUFF-F     PIC -(7)9.                                   
010000*                                 FÖRÄDLAT BUFFERSALDO                    
010100        05 RESP-KVBUFF-OF    PIC -(7)9.                                   
010200*                                 BUFFERSALDO OFÖRÄDLAT GODS              
010300        05 RESP-KVKOLLI-F    PIC Z(3)9.                                   
010400*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
010500        05 RESP-KVKOLLI-OF   PIC Z(3)9.                                   
010600*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
010700*** END OF VILMAII-COPY LENGTH= 7259 BYTES                                
