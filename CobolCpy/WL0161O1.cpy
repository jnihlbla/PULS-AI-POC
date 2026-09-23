000100 01  RESP-WL0161O1.                                                       
000200*                                 RESPONS FROM PGM WL0161                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDRAPPNR-KEY    PIC Z(6)9.                                   
001000*                                 RAPPORT NUMMER                          
001100     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001200*                                 ARTIKELNUMMER                           
001300     03 RESP-IDRADNR-KEY     PIC Z(3)9.                                   
001400*                                 RADNUMMER                               
001500     03 RESP-INPUT.                                                       
001600*                                                                         
001700        05 RESP-KDKREBEH     PIC X(3).                                    
001800*                                 BEHANDLINGSSTATUS                       
001900        05 RESP-FLSVAR       PIC X.                                       
002000*                                 ALLMÄN SVARSFLAGGA                      
002100     03 RESP-OUTPUT.                                                      
002200*                                                                         
002300        05 RESP-ANM-INFO.                                                 
002400*                                                                         
002500           07 RESP-BEART     PIC X(25).                                   
002600*                                 ARTIKELBENÄMNING                        
002700           07 RESP-KDANMORS  PIC X(2).                                    
002800*                                 ORSAK TILL LEVERANSANMÄRKNING           
002900           07 RESP-IDARTNR   PIC Z(7)9.                                   
003000*                                 ARTIKELNUMMER                           
003100           07 RESP-STRECK-1  PIC X.                                       
003200           07 RESP-REKSIFFR  PIC 9.                                       
003300*                                 KONTROLLSIFFRA                          
003400           07 RESP-KVLEVANM-BEKR                                          
003500                             PIC Z(5)9.                                   
003600*                                 BEKRÄFTAT RETURANTAL                    
003700           07 RESP-IDORDNR5  PIC Z(4)9.                                   
003800*                                 ORDERNUMMER                             
003900           07 RESP-IDKOLLI   PIC Z(4)9.                                   
004000*                                 KOLLINUMMER                             
004100           07 RESP-IDFAKT    PIC Z(6)9.                                   
004200*                                 FAKTURANUMMER                           
004300           07 RESP-TIFAKT    PIC 9(6).                                    
004400*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004500           07 RESP-FLTEXT    PIC X.                                       
004600*                                 FINNS TEXTINFORMATION ?                 
004700           07 RESP-IDDC      PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900           07 RESP-SULEVANM  PIC Z(5)9.                                   
005000*                                 LEVERANSANMÄRKNINGSANTAL                
005100           07 RESP-IDUSER-OREG                                            
005200                             PIC X(8).                                    
005300*                                 ANSVARIGT USERID ORDERREG.              
005400           07 RESP-TIREGDAT  PIC 9(6).                                    
005500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005600        05 RESP-FAKT-INFO.                                                
005700*                                                                         
005800           07 RESP-KDVALISO  PIC X(3).                                    
005900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006000           07 RESP-KVBEART-Q PIC Z(5)9.                                   
006100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006200           07 RESP-KVLEVART  PIC Z(6)9.                                   
006300*                                 LEVERERAT ANTAL STYCK                   
006400           07 RESP-VKORDBTO-KOLLI                                         
006500                             PIC Z(5)9.9.                                 
006600*                                 ORDERVIKT BRUTTO PER KOLLI              
006700           07 RESP-VKORDBTO-DIFF                                          
006800                             PIC -(6)9.9.                                 
006900*                                 DIFF VIKT FÖR KOLLI                     
007000           07 RESP-VKTARA    PIC Z(5)9.9.                                 
007100*                                 TARAVIKT (KG)                           
007200           07 RESP-KDKOLLI   PIC X(8).                                    
007300*                                 KOLLIKOD                                
007400           07 RESP-KDORDKL   PIC 9.                                       
007500*                                 ORDERKLASS                              
007600           07 RESP-FLDIRLEV  PIC X.                                       
007700*                                 DIREKTLEVERANS ?                        
007800           07 RESP-IDUSER-PACK                                            
007900                             PIC X(8).                                    
008000*                                 ANSVARIGT USERID PACKARE                
008100           07 RESP-IDPRODNR  PIC Z(6)9.                                   
008200*                                 PRODUKTIONSNUMMER                       
008300           07 RESP-KVORDRAD  PIC Z(4)9.                                   
008400*                                 ANTAL ORDERRADER                        
008500           07 RESP-VLORDBTO-KOLLI                                         
008600                             PIC Z(3)9.9(3).                              
008700*                                 ORDERVOLYM BRUTTO KOLLI                 
008800           07 RESP-KDFRAKT   PIC Z9.                                      
008900*                                 FRAKTSÄTT DC TILL KUND                  
009000        05 RESP-ART-INFO.                                                 
009100*                                                                         
009200           07 RESP-IDBORD    PIC X(3).                                    
009300*                                 PACK-BORD                               
009400           07 RESP-ADLAGOMR  PIC 9(2).                                    
009500*                                 LAGEROMRÅDE                             
009600           07 RESP-ADGANG    PIC 9(2).                                    
009700*                                 GÅNG                                    
009800           07 RESP-ADPLATS   PIC 9(5).                                    
009900*                                 LAGERPLATSNUMMER                        
010000           07 RESP-KVLS      PIC -(7)9.                                   
010100*                                 LAGERSALDO                              
010200           07 RESP-KVPB-TOT  PIC Z(5)9.9.                                 
010300*                                 TOTALT PERIODBEHOV                      
010400           07 RESP-KDERS     PIC Z9.                                      
010500*                                 ERSÄTTNINGSKOD                          
010600           07 RESP-VKART     PIC Z(6)9.                                   
010700*                                 ARTIKELVIKT (G)                         
010800           07 RESP-KDPRODSL  PIC 9(2).                                    
010900*                                 PRODUKTSLAG                             
011000           07 RESP-IDFKNGRP  PIC Z(3)9.                                   
011100*                                 FUNKTIONSGRUPP                          
011200           07 RESP-TIINVDAT  PIC 9(5).                                    
011300*                                 INVENTERINGSDATUM                       
011400           07 RESP-KVINVS    PIC Z(6)9.                                   
011500*                                 INVENTERINGSSALDO                       
011600           07 RESP-IDANSK    PIC Z(2)9.                                   
011700*                                 ANSKAFFARNUMMER                         
011800           07 RESP-PRFRAKT   PIC Z(6)9.9(2).                              
011900*                                 FRAKTKOSTNAD                            
012000*** END OF VILMAII-COPY LENGTH= 273 BYTES                                 
