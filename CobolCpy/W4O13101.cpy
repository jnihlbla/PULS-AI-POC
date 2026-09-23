000100 01  W4O13101.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O13101                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDARTNR-IN           PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 IDARTNR-UT           PIC X(11).                                   
001100*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
001200     03 IDARTNR-ENTER        PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 IDFORDON-ENTER       PIC Z(2)9.                                   
001500*                                 FORDONSSLAG                             
001600     03 TIOMBRYT-ENTER       PIC 9(6).                                    
001700*                                 OMBRYTNINGSDATUM                        
001800     03 IDARTNR-NEXT         PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 IDFORDON-NEXT        PIC Z(2)9.                                   
002100*                                 FORDONSSLAG                             
002200     03 TIOMBRYT-NEXT        PIC 9(6).                                    
002300*                                 OMBRYTNINGSDATUM                        
002400     03 FLAGGA               PIC X.                                       
002500     03 BEART-1              PIC X(20).                                   
002600     03 BEART-2              PIC X(20).                                   
002700     03 BEART-3              PIC X(20).                                   
002800     03 KVLS-1               PIC -(7)9.                                   
002900*                                 LAGERSALDO                              
003000     03 KVLS-2               PIC -(7)9.                                   
003100*                                 LAGERSALDO                              
003200     03 ADLAGOMR             PIC Z9.                                      
003300*                                 LAGEROMRÅDE                             
003400     03 ADGANG               PIC Z9.                                      
003500*                                 GÅNG                                    
003600     03 ADPLATS              PIC Z(4)9.                                   
003700*                                 LAGERPLATSNUMMER                        
003800     03 FLERPL               PIC X.                                       
003900     03 PRARTBTO-EXP         PIC Z(6)9.9(2).                              
004000*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
004100     03 KVRESS-1             PIC -(7)9.                                   
004200*                                 RESERVERAT ANTAL ARTIKLAR               
004300     03 KVRESS-2             PIC -(7)9.                                   
004400*                                 RESERVERAT ANTAL ARTIKLAR               
004500     03 IDLEVNR              PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700     03 VKART                PIC Z(6)9.                                   
004800*                                 ARTIKELVIKT (G)                         
004900     03 KVOKS-1              PIC -(7)9.                                   
005000*                                 ANTAL                                   
005100     03 KVOKS-2              PIC -(7)9.                                   
005200*                                 ANTAL                                   
005300     03 KVOKS-VOR            PIC -(7)9.                                   
005400*                                 ANTAL                                   
005500     03 IDANSK               PIC Z(2)9.                                   
005600*                                 ANSKAFFARNUMMER                         
005700     03 VLARTNTO             PIC Z(7)9.9.                                 
005800*                                 ARTIKELVOLYM NETTO (CM3)                
005900     03 KVEFRS-1             PIC -(7)9.                                   
006000*                                 EJ FAKTURERAT ANTAL STYCK               
006100     03 KVEFRS-2             PIC -(7)9.                                   
006200*                                 EJ FAKTURERAT ANTAL STYCK               
006300     03 IDBERED              PIC Z9.                                      
006400*                                 BEREDARENUMMER                          
006500     03 KDFARLIG-ATTR        PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 KDFARLIG             PIC 9.                                       
006800*                                 KOD FÖR FARLIGT GODS                    
006900     03 KDFARLIG-TXT-ATTR    PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 KDFARLIG-TXT         PIC X(12).                                   
007200     03 KVAKS-1              PIC -(7)9.                                   
007300*                                 ANKOMSTSALDO                            
007400     03 KVAKS-2              PIC -(7)9.                                   
007500*                                 ANKOMSTSALDO                            
007600     03 IDFKNGRP             PIC Z(4)9.                                   
007700*                                 FUNKTIONSGRUPP                          
007800     03 KDERS                PIC Z(2)9.                                   
007900*                                 ERSÄTTNINGSKOD                          
008000     03 KVAKS-PAV-1          PIC -(7)9.                                   
008100*                                 ANKOMSTSALDO                            
008200     03 KVAKS-PAV-2          PIC -(7)9.                                   
008300*                                 ANKOMSTSALDO                            
008400     03 SPARRKOD-ATTR        PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 SPARRKOD             PIC X.                                       
008700*                                 UNDANTAGSARTIKEL                        
008800     03 KVUTRS-1             PIC -(7)9.                                   
008900*                                 UTREDNINGSSALDO                         
009000     03 KVUTRS-2             PIC -(7)9.                                   
009100*                                 UTREDNINGSSALDO                         
009200     03 KVSPANT-1            PIC -(7)9.                                   
009300*                                 SPÄRRAT ANTAL                           
009400     03 KVSPANT-2            PIC -(7)9.                                   
009500*                                 SPÄRRAT ANTAL                           
009600     03 KDARTURS             PIC X(2).                                    
009700*                                 ARTIKELURSPRUNGSKOD                     
009800     03 BEEMBLEM             OCCURS 10 TIMES                              
009900                             PIC X(5).                                    
010000*                                 EMBLEM                                  
010100     03 TIDISPIN-1           PIC 9(6).                                    
010200*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
010300     03 TIDISPIN-2           PIC 9(6).                                    
010400*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
010500     03 TEARTNOT-1           PIC X(40).                                   
010600*                                 ARTIKEL NOTERING                        
010700     03 TEARTNOT-2           PIC X(40).                                   
010800*                                 ARTIKEL NOTERING                        
010900     03 IDARTNR-ERS-1        PIC Z(7)9.                                   
011000*                                 ARTIKELNUMMER                           
011100     03 IDARTNR-ERS-2        PIC Z(7)9.                                   
011200*                                 ARTIKELNUMMER                           
011300     03 IDARTNR-ERS-3        PIC Z(7)9.                                   
011400*                                 ARTIKELNUMMER                           
011500     03 TILEVBSK             PIC 9(5).                                    
011600*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
011700     03 KVAVIS               PIC Z(5)9.                                   
011800*                                 AVISERAT ANTAL                          
011900     03 TEMFSINF             PIC X(55).                                   
012000*                                 INFORMATIONSMEDDELANDE                  
012100*** END OF VILMAII-COPY LENGTH= 606 BYTES                                 
