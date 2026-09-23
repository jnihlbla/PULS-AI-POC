000100 01  FRAD-W461S013.                                                       
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 FAKTURA-RAD INFO TILL NOAC              
000400     03 FRAD-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 FRAD-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 FRAD-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 FRAD-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 FRAD-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 FRAD-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 FRAD-W461013.                                                     
001700*                                 FAKTURA-RAD  TILL NOAC PT-013           
001800        05 FRAD-IDPTYP       PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 FRAD-IDDISTR      PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200        05 FRAD-IDKUNDNR     PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400        05 FRAD-KDFAKTYP     PIC X.                                       
002500*                                 FAKTURATYP                              
002600        05 FRAD-IDFAKT       PIC S9(7)           COMP-3.                  
002700*                                 FAKTURANUMMER                           
002800        05 FRAD-KDSORT2      PIC S9(3)           COMP-3.                  
002900*                                 SORTERINGSFÄLT                          
003000        05 FRAD-IDPRODNR     PIC S9(7)           COMP-3.                  
003100*                                 PRODUKTIONSNUMMER                       
003200        05 FRAD-IDKOLLI      PIC S9(5)           COMP-3.                  
003300*                                 KOLLINUMMER                             
003400        05 FRAD-IDARTNR      PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 FRAD-IDORDNR      PIC S9(7)           COMP-3.                  
003700*                                 ORDERNR             IDORDNR-002         
003800        05 FRAD-REKSIFFR     PIC S9              COMP-3.                  
003900*                                 KONTROLLSIFFRA                          
004000        05 FRAD-BERADREF     PIC X(10).                                   
004100*                                 KUNDENS RADREFERENS                     
004200        05 FRAD-KVBEART      PIC S9(7)           COMP-3.                  
004300*                                 BESTÄLLT ANTAL STYCKEN                  
004400        05 FRAD-KVLEVART     PIC S9(7)           COMP-3.                  
004500*                                 LEVERERAT ANTAL STYCK                   
004600        05 FRAD-RESERVG      PIC S9(3)V9(2)      COMP-3.                  
004700*                                 PROCENT SERVICEGRAD                     
004800        05 FRAD-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
004900*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
005000        05 FRAD-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
005100*                                 ARTIKELPRIS NETTO                       
005200        05 FRAD-IDFKNGRP     PIC S9(5)           COMP-3.                  
005300*                                 FUNKTIONSGRUPP                          
005400        05 FRAD-KDPRODSL     PIC S9(3)           COMP-3.                  
005500*                                 PRODUKTSLAG                             
005600        05 FRAD-IDRONR       PIC S9(7)           COMP-3.                  
005700*                                 RESTORDERNUMMER      IDRONR-002         
005800        05 FRAD-TIRODAT      PIC S9(7)           COMP-3.                  
005900*                                 RESTORDERDATUM         (ÅÅMMDD)         
006000        05 FRAD-TIORDREG     PIC S9(7)           COMP-3.                  
006100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
006200        05 FRAD-BEVOLREF     PIC X(10).                                   
006300*                                 VOLVO REFERENS                          
006400        05 FRAD-FLSPLIT      PIC X.                                       
006500*                                 RAD INGÅR I ETT EL. FLER KOLLI          
006600        05 FRAD-KDKVBRYT     PIC S9              COMP-3.                  
006700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006800        05 FRAD-KDDSP        PIC S9              COMP-3.                  
006900*                                 PÅVERKAN PÅ DSP                         
007000        05 FRAD-KDVVKL       PIC S9              COMP-3.                  
007100*                                 VOLYMVÄRDESKLASS                        
007200        05 FRAD-KDVRINFO     PIC S9              COMP-3.                  
007300*                                 PÅVERKAN I VR/DSP SYSTEM                
007400        05 FRAD-FLINVEST     PIC X.                                       
007500*                                 BYTES INVENTERINGSFLAGGA                
007600        05 FRAD-KDORDKL-URS  PIC S9              COMP-3.                  
007700*                                 URSPRUNGLIG ORDERKLASS                  
007800        05 FRAD-KDSRA        PIC S9(3)           COMP-3.                  
007900*                                 SRA-KOD                                 
008000        05 FRAD-KDSORT       PIC X(2).                                    
008100*                                 SORT-KOD                                
008200        05 FRAD-KVQPACK-1    PIC S9(5)           COMP-3.                  
008300*                                 ANTAL I Q1 FÖRPACKNING                  
008400        05 FRAD-BEART        PIC X(25).                                   
008500*                                 ARTIKELBENÄMNING                        
008600        05 FRAD-IDSTATNR     PIC S9(9)           COMP-3.                  
008700*                                 STATISTISKT NUMMER                      
008800*                                 1 = NORSKT                              
008900*                                 2 = ENGELSKT                            
009000*                                 3 = BELGISKT                            
009100*                                 4 = PERUANSKT                           
009200*                                 5 = SVENSKT                             
009300*                                 6 =                                     
009400        05 FRAD-KDARTURS     PIC X(2).                                    
009500*                                 ARTIKELURSPRUNGSKOD                     
009600        05 FRAD-VKART        PIC S9(7)           COMP-3.                  
009700*                                 ARTIKELVIKT (G)                         
009800        05 FRAD-FLPRTILL     PIC X.                                       
009900*                                 PRISTILLÄGGS FLAGGA                     
010000        05 FRAD-FLDIRLEV     PIC X.                                       
010100*                                 DIREKTLEVERANS ?                        
010200        05 FRAD-KDARTRAB     PIC 9(2).                                    
010300*                                 RABATTKOD (ARTIKELPRIS)                 
010400        05 FRAD-KDPSLLOC     PIC 9(2).                                    
010500*                                 PRODUKTSLAG LOKALT                      
010600        05 FRAD-PRAVCOST     PIC S9(7)V9(2)      COMP-3.                  
010700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
010800        05 FRAD-PRAVCOST-CORE                                             
010900                             PIC S9(7)V9(2)      COMP-3.                  
011000*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
011100*                                 UTL.VALUTA                              
011200        05 FRAD-IDBIL.                                                    
011300*                                 BILIDENTITET                            
011400           07 FRAD-IDBILTYP  PIC X(3).                                    
011500*                                 BILTYP                                  
011600           07 FRAD-TIAAAA    PIC X(4).                                    
011700*                                 ÅRTAL (ÅÅÅÅ)                            
011800           07 FRAD-IDCHASSI-PIE                                           
011900                             PIC X(6).                                    
012000*                                 CHASSINUMMER PIE                        
012100        05 FRAD-IDDC         PIC X(2).                                    
012200*                                 IDENTIFIERARE LAGER                     
012300*** END OF VILMAII-COPY LENGTH= 194 BYTES                                 
