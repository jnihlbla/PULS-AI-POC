000100 01  L16110-WL016110.                                                     
000200*                                 LÄNKAREA TILL PGM WL016110 -            
000300*                                 UTSKRIFT DETALJ RAD                     
000400     03 L16110-IDDC          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 L16110-IDDISTR       PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 L16110-IDKUNDNR      PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 L16110-IDRAPPNR      PIC 9(7).                                    
001100*                                 RAPPORT NUMMER                          
001200     03 L16110-IDARTNR       PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 L16110-IDRADNR       PIC 9(4).                                    
001500*                                 RADNUMMER                               
001600     03 L16110-IDUSER        PIC X(8).                                    
001700*                                 ANVÄNDARENS SÄKERHETS ID                
001800     03 L16110-ANM-INFO.                                                  
001900*                                                                         
002000        05 L16110-KDANMORS   PIC X(2).                                    
002100*                                 ORSAK TILL LEVERANSANMÄRKNING           
002200        05 L16110-KVLEVANM-BEKR                                           
002300                             PIC 9(6).                                    
002400*                                 BEKRÄFTAT RETURANTAL                    
002500        05 L16110-IDORDNR5   PIC 9(5).                                    
002600*                                 ORDERNUMMER                             
002700        05 L16110-IDKOLLI    PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900        05 L16110-KDFAKTYP   PIC X.                                       
003000*                                 FAKTURATYP                              
003100        05 L16110-IDFAKT     PIC 9(7).                                    
003200*                                 FAKTURANUMMER                           
003300        05 L16110-TIFAKT     PIC 9(6).                                    
003400*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003500     03 L16110-FAKT-INFO.                                                 
003600*                                                                         
003700        05 L16110-KVBEART-Q  PIC 9(6).                                    
003800*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003900        05 L16110-KVLEVART   PIC 9(7).                                    
004000*                                 LEVERERAT ANTAL STYCK                   
004100        05 L16110-VKORDBTO-KOLLI                                          
004200                             PIC 9(6)V9(1).                               
004300*                                 ORDERVIKT BRUTTO PER KOLLI              
004400        05 L16110-VKORDNTO-KOLLI                                          
004500                             PIC 9(6)V9(1).                               
004600*                                 ORDERVIKT NETTO PER KOLLI               
004700        05 L16110-VKTARA     PIC 9(6)V9(1).                               
004800*                                 TARAVIKT (KG)                           
004900        05 L16110-VKORDNTO-TOT                                            
005000                             PIC 9(6)V9(1).                               
005100*                                 ORDERVIKT NETTO (KG)                    
005200        05 L16110-KDORDKL    PIC 9.                                       
005300*                                 ORDERKLASS                              
005400        05 L16110-FLDIRLEV   PIC X.                                       
005500*                                 DIREKTLEVERANS ?                        
005600        05 L16110-IDUSER-PACK                                             
005700                             PIC X(8).                                    
005800*                                 ANSVARIGT USERID PACKARE                
005900        05 L16110-IDPRODNR   PIC 9(7).                                    
006000*                                 PRODUKTIONSNUMMER                       
006100        05 L16110-KVORDRAD   PIC 9(5).                                    
006200*                                 ANTAL ORDERRADER                        
006300        05 L16110-IDUSER-OREG                                             
006400                             PIC X(8).                                    
006500*                                 ANSVARIGT USERID ORDERREG.              
006600        05 L16110-TIREGDAT   PIC 9(6).                                    
006700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006800     03 L16110-ART-INFO.                                                  
006900*                                                                         
007000        05 L16110-ADLAGOMR   PIC 9(2).                                    
007100*                                 LAGEROMRÅDE                             
007200        05 L16110-ADGANG     PIC 9(2).                                    
007300*                                 GÅNG                                    
007400        05 L16110-ADPLATS    PIC 9(5).                                    
007500*                                 LAGERPLATSNUMMER                        
007600        05 L16110-KVLS       PIC 9(7).                                    
007700*                                 LAGERSALDO                              
007800        05 L16110-KVPB-TOT   PIC 9(6)V9(1).                               
007900*                                 TOTALT PERIODBEHOV                      
008000        05 L16110-KDERS      PIC 9(2).                                    
008100*                                 ERSÄTTNINGSKOD                          
008200        05 L16110-VKART      PIC 9(7).                                    
008300*                                 ARTIKELVIKT (G)                         
008400        05 L16110-KDPRODSL   PIC 9(2).                                    
008500*                                 PRODUKTSLAG                             
008600        05 L16110-IDFKNGRP   PIC 9(4).                                    
008700*                                 FUNKTIONSGRUPP                          
008800        05 L16110-TIINVDAT   PIC 9(5).                                    
008900*                                 INVENTERINGSDATUM                       
009000        05 L16110-KVINVS     PIC 9(7).                                    
009100*                                 INVENTERINGSSALDO                       
009200        05 L16110-IDANSK     PIC 9(3).                                    
009300*                                 ANSKAFFARNUMMER                         
009400        05 L16110-BEART      PIC X(25).                                   
009500*                                 ARTIKELBENÄMNING                        
009600        05 L16110-REKSIFFR   PIC X.                                       
009700*                                 KONTROLLSIFFRA                          
009800        05 L16110-PRARTBTO   PIC 9(7)V9(2).                               
009900*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
010000        05 L16110-PRARTBTO-EXP                                            
010100                             PIC 9(7)V9(2).                               
010200*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
010300        05 L16110-PRINK      PIC 9(7)V9(2).                               
010400*                                 INKÖPSPRIS                              
010500     03 L16110-TEXT-INFO.                                                 
010600*                                                                         
010700        05 L16110-TEANMNOT-REG                                            
010800                             OCCURS 3 TIMES                               
010900                             PIC X(70).                                   
011000*                                 FRI TEXT FRÅN REGISTRERINGEN            
011100        05 L16110-TEANMNOT-ADM                                            
011200                             OCCURS 3 TIMES                               
011300                             PIC X(70).                                   
011400*                                 FRI TEXT FRÅN ADMINISTRATION            
011500        05 L16110-TEANMNOT-REM                                            
011600                             OCCURS 3 TIMES                               
011700                             PIC X(70).                                   
011800*                                 FRI TEXT FRÅN REMISSINSTANS             
011900        05 L16110-TEANMNOT-RET                                            
012000                             OCCURS 3 TIMES                               
012100                             PIC X(70).                                   
012200*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
012300*** END OF VILMAII-COPY LENGTH= 1094 BYTES                                
