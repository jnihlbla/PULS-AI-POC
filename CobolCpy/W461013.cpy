000100 01  FRAD-W461013.                                                        
000200*                                 FAKTURA-RAD  TILL NOAC PT-013           
000300     03 FRAD-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 FRAD-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 FRAD-IDKUNDNR        PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 FRAD-KDFAKTYP        PIC X.                                       
001000*                                 FAKTURATYP                              
001100     03 FRAD-IDFAKT          PIC S9(7)           COMP-3.                  
001200*                                 FAKTURANUMMER                           
001300     03 FRAD-KDSORT2         PIC S9(3)           COMP-3.                  
001400*                                 SORTERINGSFÄLT                          
001500     03 FRAD-IDPRODNR        PIC S9(7)           COMP-3.                  
001600*                                 PRODUKTIONSNUMMER                       
001700     03 FRAD-IDKOLLI         PIC S9(5)           COMP-3.                  
001800*                                 KOLLINUMMER                             
001900     03 FRAD-IDARTNR         PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100     03 FRAD-IDORDNR         PIC S9(7)           COMP-3.                  
002200*                                 ORDERNR             IDORDNR-002         
002300     03 FRAD-REKSIFFR        PIC S9              COMP-3.                  
002400*                                 KONTROLLSIFFRA                          
002500     03 FRAD-BERADREF        PIC X(10).                                   
002600*                                 KUNDENS RADREFERENS                     
002700     03 FRAD-KVBEART         PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT ANTAL STYCKEN                  
002900     03 FRAD-KVLEVART        PIC S9(7)           COMP-3.                  
003000*                                 LEVERERAT ANTAL STYCK                   
003100     03 FRAD-RESERVG         PIC S9(3)V9(2)      COMP-3.                  
003200*                                 PROCENT SERVICEGRAD                     
003300     03 FRAD-PRARTBTO-EXP    PIC S9(7)V9(2)      COMP-3.                  
003400*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
003500     03 FRAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
003600*                                 ARTIKELPRIS NETTO                       
003700     03 FRAD-IDFKNGRP        PIC S9(5)           COMP-3.                  
003800*                                 FUNKTIONSGRUPP                          
003900     03 FRAD-KDPRODSL        PIC S9(3)           COMP-3.                  
004000*                                 PRODUKTSLAG                             
004100     03 FRAD-IDRONR          PIC S9(7)           COMP-3.                  
004200*                                 RESTORDERNUMMER      IDRONR-002         
004300     03 FRAD-TIRODAT         PIC S9(7)           COMP-3.                  
004400*                                 RESTORDERDATUM         (ÅÅMMDD)         
004500     03 FRAD-TIORDREG        PIC S9(7)           COMP-3.                  
004600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004700     03 FRAD-BEVOLREF        PIC X(10).                                   
004800*                                 VOLVO REFERENS                          
004900     03 FRAD-FLSPLIT         PIC X.                                       
005000*                                 RAD INGÅR I ETT EL. FLER KOLLI          
005100     03 FRAD-KDKVBRYT        PIC S9              COMP-3.                  
005200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005300     03 FRAD-KDDSP           PIC S9              COMP-3.                  
005400*                                 PÅVERKAN PÅ DSP                         
005500     03 FRAD-KDVVKL          PIC S9              COMP-3.                  
005600*                                 VOLYMVÄRDESKLASS                        
005700     03 FRAD-KDVRINFO        PIC S9              COMP-3.                  
005800*                                 PÅVERKAN I VR/DSP SYSTEM                
005900     03 FRAD-FLINVEST        PIC X.                                       
006000*                                 BYTES INVENTERINGSFLAGGA                
006100     03 FRAD-KDORDKL-URS     PIC S9              COMP-3.                  
006200*                                 URSPRUNGLIG ORDERKLASS                  
006300     03 FRAD-KDSRA           PIC S9(3)           COMP-3.                  
006400*                                 SRA-KOD                                 
006500     03 FRAD-KDSORT          PIC X(2).                                    
006600*                                 SORT-KOD                                
006700     03 FRAD-KVQPACK-1       PIC S9(5)           COMP-3.                  
006800*                                 ANTAL I Q1 FÖRPACKNING                  
006900     03 FRAD-BEART           PIC X(25).                                   
007000*                                 ARTIKELBENÄMNING                        
007100     03 FRAD-IDSTATNR        PIC S9(9)           COMP-3.                  
007200*                                 STATISTISKT NUMMER                      
007300*                                 1 = NORSKT                              
007400*                                 2 = ENGELSKT                            
007500*                                 3 = BELGISKT                            
007600*                                 4 = PERUANSKT                           
007700*                                 5 = SVENSKT                             
007800*                                 6 =                                     
007900     03 FRAD-KDARTURS        PIC X(2).                                    
008000*                                 ARTIKELURSPRUNGSKOD                     
008100     03 FRAD-VKART           PIC S9(7)           COMP-3.                  
008200*                                 ARTIKELVIKT (G)                         
008300     03 FRAD-FLPRTILL        PIC X.                                       
008400*                                 PRISTILLÄGGS FLAGGA                     
008500     03 FRAD-FLDIRLEV        PIC X.                                       
008600*                                 DIREKTLEVERANS ?                        
008700     03 FRAD-KDARTRAB        PIC 9(2).                                    
008800*                                 RABATTKOD (ARTIKELPRIS)                 
008900     03 FRAD-KDPSLLOC        PIC 9(2).                                    
009000*                                 PRODUKTSLAG LOKALT                      
009100     03 FRAD-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
009200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
009300     03 FRAD-PRAVCOST-CORE   PIC S9(7)V9(2)      COMP-3.                  
009400*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
009500*                                 UTL.VALUTA                              
009600     03 FRAD-IDBIL.                                                       
009700*                                 BILIDENTITET                            
009800        05 FRAD-IDBILTYP     PIC X(3).                                    
009900*                                 BILTYP                                  
010000        05 FRAD-TIAAAA       PIC X(4).                                    
010100*                                 ÅRTAL (ÅÅÅÅ)                            
010200        05 FRAD-IDCHASSI-PIE PIC X(6).                                    
010300*                                 CHASSINUMMER PIE                        
010400     03 FRAD-IDDC            PIC X(2).                                    
010500*                                 IDENTIFIERARE LAGER                     
010600*** END OF VILMAII-COPY LENGTH= 173 BYTES                                 
