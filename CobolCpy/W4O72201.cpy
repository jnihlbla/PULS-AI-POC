000100 01  W4O72201.                                                            
000200*                                 MODCOPYTEXT TILL W40722.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDDISTR-IN           PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR-IN          PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 IDRAPPNR-IN          PIC X(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 IDARTNR-IN           PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 IDRADNR-IN           PIC X(4).                                    
001600*                                 RADNUMMER                               
001700     03 IDDISTR-UT           PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 IDKUNDNR-UT          PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 IDRAPPNR-UT          PIC X(7).                                    
002200*                                 RAPPORT NUMMER                          
002300     03 IDARTNR-UT           PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 IDRADNR-UT           PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 INPUT.                                                            
002800*                                                                         
002900        05 KDKREBEH-ATTR     PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 KDKREBEH          PIC X(3).                                    
003200*                                 BEHANDLINGSSTATUS                       
003300        05 FLSVAR-ATTR       PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 FLSVAR            PIC X.                                       
003600*                                 ALLMÄN SVARSFLAGGA                      
003700        05 IDANSV-ATTR       PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 IDANSV            PIC X(6).                                    
004000        05 IDPRT-ATTR        PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 IDPRT             PIC X(3).                                    
004300*                                 LOGISK PRINTERIDENTITET                 
004400        05 IDBORD            PIC X(3).                                    
004500*                                 PACK-BORD                               
004600        05 FLRETUR-ATTR      PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 FLRETUR           PIC X.                                       
004900*                                 FLAGGA RETUR OK.                        
005000     03 OUTPUT.                                                           
005100*                                                                         
005200        05 KDVALISO          PIC X(3).                                    
005300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005400        05 ANM-INFO.                                                      
005500*                                                                         
005600           07 BEART          PIC X(25).                                   
005700*                                 ARTIKELBENÄMNING                        
005800           07 KDPSLLOC       PIC 9(2).                                    
005900*                                 PRODUKTSLAG LOKALT                      
006000           07 KDANMORS-ATTR  PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200           07 KDANMORS       PIC X(2).                                    
006300*                                 ORSAK TILL LEVERANSANMÄRKNING           
006400           07 KDANMORS-UPPD-ATTR                                          
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700           07 KDANMORS-UPPD  PIC X(2).                                    
006800*                                 ORSAK TILL LEVERANSANMÄRKNING           
006900           07 IDARTNR        PIC Z(7)9.                                   
007000*                                 ARTIKELNUMMER                           
007100           07 STRECK-1       PIC X.                                       
007200           07 REKSIFFR       PIC X.                                       
007300*                                 KONTROLLSIFFRA                          
007400           07 KVLEVANM-BEKR-ATTR                                          
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700           07 KVLEVANM-BEKR  PIC Z(5)9.                                   
007800*                                 BEKRÄFTAT RETURANTAL                    
007900           07 PRARTBTO       PIC Z(6)9.9(2).                              
008000*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
008100           07 PRARTBTO-LOC   PIC Z(6)9.9(2).                              
008200*                                 PRIS I LOKAL VALUTA                     
008300           07 IDORDNR5       PIC Z(4)9.                                   
008400*                                 ORDERNUMMER                             
008500           07 IDKOLLI        PIC Z(4)9.                                   
008600*                                 KOLLINUMMER                             
008700           07 IDFAKT-ATTR    PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900           07 KDFAKTYP       PIC X.                                       
009000*                                 FAKTURATYP                              
009100           07 STRECK-2       PIC X.                                       
009200           07 IDFAKT         PIC Z(6)9.                                   
009300*                                 FAKTURANUMMER                           
009400           07 KDFAKTYP-LOC   PIC X.                                       
009500*                                 FAKTURATYP                              
009600           07 STRECK-3       PIC X.                                       
009700           07 IDFAKT-LOC     PIC Z(6)9.                                   
009800*                                 FAKTURANR LOKALT                        
009900           07 TIFAKT         PIC 9(6).                                    
010000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
010100           07 TIFAKT-LOC     PIC 9(6).                                    
010200*                                 FAKTURADATUM LOKALT                     
010300           07 FLTEXT         PIC X.                                       
010400*                                 FINNS TEXTINFORMATION ?                 
010500           07 IDDC           PIC X(2).                                    
010600*                                 IDENTIFIERARE LAGER                     
010700           07 SULEVANM       PIC Z(5)9.                                   
010800*                                 LEVERANSANMÄRKNINGSANTAL                
010900           07 IDUSER-OREG    PIC X(8).                                    
011000*                                 ANSVARIGT USERID ORDERREG.              
011100           07 TIREGDAT       PIC 9(6).                                    
011200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011300        05 FAKT-INFO.                                                     
011400*                                                                         
011500           07 KVBEART-Q      PIC Z(5)9.                                   
011600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
011700           07 KVLEVART       PIC Z(5)9.                                   
011800*                                 LEVERERAT ANTAL STYCK                   
011900           07 VKORDBTO-KOLLI PIC Z(5)9.9.                                 
012000*                                 ORDERVIKT BRUTTO PER KOLLI              
012100           07 VKORDBTO-DIFF  PIC -(6)9.9.                                 
012200*                                 DIFF VIKT FÖR KOLLI                     
012300           07 VKTARA         PIC Z(5)9.9.                                 
012400*                                 TARAVIKT (KG)                           
012500           07 KDKOLLI        PIC X(8).                                    
012600*                                 KOLLIKOD                                
012700           07 KDORDKL        PIC 9.                                       
012800*                                 ORDERKLASS                              
012900           07 FLDIRLEV       PIC X.                                       
013000*                                 DIREKTLEVERANS ?                        
013100           07 IDUSER-PACK    PIC X(8).                                    
013200*                                 ANSVARIGT USERID PACKARE                
013300           07 IDPRODNR       PIC Z(6)9.                                   
013400*                                 PRODUKTIONSNUMMER                       
013500           07 KVORDRAD       PIC Z(4)9.                                   
013600*                                 ANTAL ORDERRADER                        
013700           07 VLORDBTO-KOLLI PIC Z(3)9.9(3).                              
013800*                                 ORDERVOLYM BRUTTO KOLLI                 
013900           07 KDFRAKT        PIC Z(2).                                    
014000*                                 FRAKTSÄTT DC TILL KUND                  
014100        05 ART-INFO.                                                      
014200*                                                                         
014300           07 ADLAGOMR       PIC Z9.                                      
014400*                                 LAGEROMRÅDE                             
014500           07 ADGANG         PIC Z9.                                      
014600*                                 GÅNG                                    
014700           07 ADPLATS        PIC Z(4)9.                                   
014800*                                 LAGERPLATSNUMMER                        
014900           07 KVLS           PIC -(7)9.                                   
015000*                                 LAGERSALDO                              
015100           07 KVPB-TOT       PIC Z(6)9.9.                                 
015200*                                 TOTALT PERIODBEHOV                      
015300           07 KDERS          PIC Z9.                                      
015400*                                 ERSÄTTNINGSKOD                          
015500           07 VKART          PIC Z(6)9.                                   
015600*                                 ARTIKELVIKT (G)                         
015700           07 PRARTBTO-EXP   PIC Z(6)9.9(2).                              
015800*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
015900           07 PRINK          PIC Z(6)9.9(2).                              
016000*                                 INKÖPSPRIS                              
016100           07 KDPRODSL       PIC Z9.                                      
016200*                                 PRODUKTSLAG                             
016300           07 IDFKNGRP       PIC Z(3)9.                                   
016400*                                 FUNKTIONSGRUPP                          
016500           07 TIINVDAT       PIC 9(5).                                    
016600*                                 INVENTERINGSDATUM                       
016700           07 KVINVS         PIC -(7)9.                                   
016800*                                 INVENTERINGSSALDO                       
016900           07 IDANSK         PIC Z(2)9.                                   
017000*                                 ANSKAFFARNUMMER                         
017100           07 PRFRAKT        PIC Z(7).Z(2).                               
017200*                                 FRAKTKOSTNAD                            
017300     03 TEMFSINF             PIC X(55).                                   
017400*                                 INFORMATIONSMEDDELANDE                  
017500*** END OF VILMAII-COPY LENGTH= 491 BYTES                                 
