000100 01  W5O10101.                                                            
000200     03 TRANS-NUMMER.                                                     
000300        05 TRANS-SIFF-1      PIC X.                                       
000400        05 TRANS-SIFF-2      PIC X.                                       
000500        05 TRANS-SIFF-3      PIC X.                                       
000600        05 TRANS-SIFF-4      PIC X.                                       
000700     03 MESSAGE              PIC X(40).                                   
000800*                                 MEDDELANDE                              
000900     03 IDARTNR-IN           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 BINDESTRECK          PIC X.                                       
001400     03 REKSIFFR             PIC 9.                                       
001500*                                 KONTROLLSIFFRA                          
001600     03 IDDC-IN              PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 IDDC-UT              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 AREA.                                                             
002100        05 BEART-SVE         PIC X(25).                                   
002200*                                 SVENSK ARTIKELBENÄMNING                 
002300        05 KVPB-TOT          OCCURS 2 TIMES                               
002400                             PIC Z(7)9.9.                                 
002500*                                 TOTALT PERIODBEHOV                      
002600        05 ADARTADR          OCCURS 2 TIMES.                              
002700           07 ADLAGOMR       PIC Z9B.                                     
002800*                                 LAGEROMRÅDE                             
002900           07 ADGANG         PIC Z9B.                                     
003000*                                 GÅNG                                    
003100           07 ADPLATS        PIC Z(4)9.                                   
003200*                                 LAGERPLATSNUMMER                        
003300        05 KVPB-SATS         OCCURS 2 TIMES                               
003400                             PIC Z(6)9.9.                                 
003500*                                 SATS-PERIODBEHOV                        
003600        05 TIINVDAT          OCCURS 2 TIMES                               
003700                             PIC 9(5).                                    
003800*                                 INVENTERINGSDATUM                       
003900        05 KVQPACK-1         PIC Z(4)9.                                   
004000*                                 ANTAL I Q1 FÖRPACKNING                  
004100        05 KVINVS            OCCURS 2 TIMES                               
004200                             PIC -(7)9.                                   
004300*                                 INVENTERINGSSALDO                       
004400        05 KDIART            PIC X.                                       
004500*                                 INGÅR I SATS                            
004600        05 KDINVKAT          OCCURS 2 TIMES                               
004700                             PIC X(12).                                   
004800        05 KDVTH             PIC 9.                                       
004900*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
005000        05 TIM-INV           OCCURS 2 TIMES                               
005100                             PIC 9(6).                                    
005200*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
005300        05 KDVVKL            PIC 9.                                       
005400*                                 VOLYMVÄRDESKLASS                        
005500        05 PRHEMTAG          PIC Z(6)9.9(2).                              
005600*                                 HEMTAGNINGSKOSTNAD                      
005700        05 KDPRODSL          PIC Z(2)9.                                   
005800*                                 PRODUKTSLAG                             
005900        05 KDKG              PIC 9.                                       
006000*                                 KURANSGRUPP                             
006100        05 PRINK             PIC Z(6)9.9(2).                              
006200*                                 INKÖPSPRIS                              
006300        05 KDPSLLOC          PIC 9(2).                                    
006400*                                 PRODUKTSLAG LOKALT                      
006500        05 KDGK              PIC 9.                                       
006600*                                 GODSMOTTAGAREKOD                        
006700        05 PRARTSTD          PIC Z(6)9.9(2).                              
006800*                                 ARTIKELSTANDARDPRIS                     
006900        05 KDLTK             PIC 9.                                       
007000*                                 LAGERTILLHÖRIGHETSKOD                   
007100        05 PRARTBES          PIC Z(6)9.9(2).                              
007200*                                 BESTÄLLNINGSPRIS I KRONOR               
007300        05 KDHF              PIC 9.                                       
007400*                                 HUVUDFÖRRÅDSMÄRKNING                    
007500        05 PRARTSJK          PIC Z(6)9.9(2).                              
007600*                                 ARTIKELNS SJÄLVKOSTNAD                  
007700        05 TIFINLV           PIC Z(4)9.                                   
007800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
007900        05 KVAKS-CDC         PIC -(7)9.                                   
008000*                                 DEL AV AK SOM LIGGER I CDC              
008100        05 KVAKS-T           PIC -(7)9.                                   
008200*                                 DEL AV AK I EN TERMINAL                 
008300        05 IDANSK            PIC Z(2)9.                                   
008400*                                 ANSKAFFARNUMMER                         
008500        05 KVAKS-SDC         PIC -(7)9.                                   
008600*                                 DEL AV AK SOM LIGGER I SDC              
008700        05 KDERS             PIC Z9(2).                                   
008800*                                 ERSÄTTNINGSKOD                          
008900        05 KVEFRS            OCCURS 2 TIMES                               
009000                             PIC -(7)9.                                   
009100*                                 EJ FAKTURERAT ANTAL STYCK               
009200        05 TIERSDAT          PIC Z(4)9.                                   
009300*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
009400        05 IDLEVNR           PIC X(5).                                    
009500*                                 LEVERANTÖRNUMMER                        
009600        05 KVLS              OCCURS 2 TIMES                               
009700                             PIC -(7)9.                                   
009800*                                 LAGERSALDO                              
009900        05 KDSORT            PIC X(2).                                    
010000*                                 SORT-KOD                                
010100        05 KVUTRS            OCCURS 2 TIMES                               
010200                             PIC -(7)9.                                   
010300*                                 UTREDNINGSSALDO                         
010400        05 IDLEVNR-SEN       OCCURS 2 TIMES                               
010500                             PIC X(5).                                    
010600*                                 LEVERANTÖRNUMMER                        
010700        05 KVROS             OCCURS 2 TIMES                               
010800                             PIC -(7)9.                                   
010900*                                 RESTORDERSALDO                          
011000        05 TIAVIDAT-SEN      OCCURS 2 TIMES                               
011100                             PIC Z9(6).                                   
011200*                                 AVISERINGSDATUM (YYMMDD)                
011300        05 KVRESS            OCCURS 2 TIMES                               
011400                             PIC -(7)9.                                   
011500*                                 RESERVERAT ANTAL ARTIKLAR               
011600        05 KVAVIS-SEN        OCCURS 2 TIMES                               
011700                             PIC -(7)9.                                   
011800*                                 AVISERAT ANTAL                          
011900        05 KVBR              PIC -(7)9.                                   
012000*                                 BESTÄLLNINGSREST                        
012100        05 KVBUFF-OF         PIC Z(6)9.                                   
012200*                                 BUFFERSALDO OFÖRÄDLAT GODS              
012300        05 KVBUFF-F          PIC Z(6)9.                                   
012400*                                 FÖRÄDLAT BUFFERSALDO                    
012500*** END OF VILMAII-COPY LENGTH= 471 BYTES                                 
