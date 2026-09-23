000100 01  UDET-W418UDET.                                                       
000200*                                 LÄNKAREA TILL W418UDET  -               
000300*                                 UTSKRIFT DETALJ RAD                     
000400     03 UDET-IDPRTLST        PIC X(8).                                    
000500*                                 LOGISK PRINTER+LISTA IDENTITET          
000600     03 UDET-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 UDET-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 UDET-IDRAPPNR        PIC 9(7).                                    
001100*                                 RAPPORT NUMMER                          
001200     03 UDET-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 UDET-IDRADNR         PIC S9(5)           COMP-3.                  
001500*                                 RADNUMMER                               
001600     03 UDET-ANM-INFO.                                                    
001700*                                                                         
001800        05 UDET-KDANMORS     PIC X(2).                                    
001900*                                 ORSAK TILL LEVERANSANMÄRKNING           
002000        05 UDET-KVLEVANM-BEKR                                             
002100                             PIC S9(7)           COMP-3.                  
002200*                                 BEKRÄFTAT RETURANTAL                    
002300        05 UDET-PRARTBTO     PIC S9(7)V9(2)      COMP-3.                  
002400*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
002500        05 UDET-IDORDNR5     PIC 9(5).                                    
002600*                                 ORDERNUMMER                             
002700        05 UDET-IDKOLLI      PIC S9(5)           COMP-3.                  
002800*                                 KOLLINUMMER                             
002900        05 UDET-KDFAKTYP     PIC X.                                       
003000         88 UDET-KDFAKTYP-HANDELS                                         
003100                             VALUE 'R'.                                   
003200         88 UDET-KDFAKTYP-KONSIGN                                         
003300                             VALUE 'K'.                                   
003400         88 UDET-KDFAKTYP-INTERN                                          
003500                             VALUE 'N'.                                   
003600         88 UDET-KDFAKTYP-GRATIS                                          
003700                             VALUE 'G'.                                   
003800         88 UDET-KDFAKTYP-TULL                                            
003900                             VALUE 'F'.                                   
004000         88 UDET-KDFAKTYP-PROFORMA                                        
004100                             VALUE 'P'.                                   
004200*                                 FAKTURATYP                              
004300        05 UDET-IDFAKT       PIC S9(7)           COMP-3.                  
004400*                                 FAKTURANUMMER                           
004500        05 UDET-TIFAKT       PIC S9(7)           COMP-3.                  
004600*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004700     03 UDET-FAKT-INFO.                                                   
004800*                                                                         
004900        05 UDET-KVBEART-Q    PIC S9(7)           COMP-3.                  
005000*                                 BESTÄLLT KVANTANPASSAT ANTAL            
005100        05 UDET-KVLEVART     PIC S9(7)           COMP-3.                  
005200*                                 LEVERERAT ANTAL STYCK                   
005300        05 UDET-VKORDBTO-KOLLI                                            
005400                             PIC S9(6)V9(1)      COMP-3.                  
005500*                                 ORDERVIKT BRUTTO PER KOLLI              
005600        05 UDET-VKORDNTO-KOLLI                                            
005700                             PIC S9(6)V9(1)      COMP-3.                  
005800*                                 ORDERVIKT NETTO PER KOLLI               
005900        05 UDET-VKTARA       PIC S9(6)V9(1)      COMP-3.                  
006000*                                 TARAVIKT (KG)                           
006100        05 UDET-VKORDNTO-TOT PIC S9(6)V9(1)      COMP-3.                  
006200*                                 ORDERVIKT NETTO (KG)                    
006300        05 UDET-KDORDKL      PIC S9              COMP-3.                  
006400         88 UDET-KDORDKL-VOR VALUE +0.                                    
006500         88 UDET-KDORDKL-DAG VALUE +1.                                    
006600         88 UDET-KDORDKL-2   VALUE +2.                                    
006700         88 UDET-KDORDKL-SNABB                                            
006800                             VALUE +2.                                    
006900         88 UDET-KDORDKL-SPECIAL                                          
007000                             VALUE +3.                                    
007100         88 UDET-KDORDKL-KVANT                                            
007200                             VALUE +4.                                    
007300         88 UDET-KDORDKL-SATS                                             
007400                             VALUE +5.                                    
007500*                                 ORDERKLASS                              
007600        05 UDET-FLDIRLEV     PIC X.                                       
007700*                                 DIREKTLEVERANS ?                        
007800        05 UDET-IDUSER-PACK  PIC X(8).                                    
007900*                                 ANSVARIGT USERID PACKARE                
008000        05 UDET-IDPRODNR     PIC S9(7)           COMP-3.                  
008100*                                 PRODUKTIONSNUMMER                       
008200        05 UDET-KVORDRAD     PIC S9(5)           COMP-3.                  
008300*                                 ANTAL ORDERRADER                        
008400        05 UDET-IDUSER-OREG  PIC X(8).                                    
008500*                                 ANSVARIGT USERID ORDERREG.              
008600        05 UDET-TIREGDAT     PIC S9(7)           COMP-3.                  
008700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008800     03 UDET-ART-INFO.                                                    
008900*                                                                         
009000        05 UDET-ADLAGOMR     PIC S9(3)           COMP-3.                  
009100*                                 LAGEROMRÅDE                             
009200        05 UDET-ADGANG       PIC S9(3)           COMP-3.                  
009300*                                 GÅNG                                    
009400        05 UDET-ADPLATS      PIC S9(5)           COMP-3.                  
009500*                                 LAGERPLATSNUMMER                        
009600        05 UDET-KVLS         PIC S9(7)           COMP-3.                  
009700*                                 LAGERSALDO                              
009800        05 UDET-KVPB-TOT     PIC S9(6)V9(1)      COMP-3.                  
009900*                                 TOTALT PERIODBEHOV                      
010000        05 UDET-KDERS        PIC S9(3)           COMP-3.                  
010100*                                 ERSÄTTNINGSKOD                          
010200        05 UDET-VKART        PIC S9(7)           COMP-3.                  
010300*                                 ARTIKELVIKT (G)                         
010400        05 UDET-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
010500*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
010600        05 UDET-PRINK        PIC S9(7)V9(2)      COMP-3.                  
010700*                                 INKÖPSPRIS                              
010800        05 UDET-KDPRODSL     PIC S9(3)           COMP-3.                  
010900*                                 PRODUKTSLAG                             
011000        05 UDET-IDFKNGRP     PIC S9(5)           COMP-3.                  
011100*                                 FUNKTIONSGRUPP                          
011200        05 UDET-TIINVDAT     PIC S9(5)           COMP-3.                  
011300*                                 INVENTERINGSDATUM                       
011400        05 UDET-KVINVS       PIC S9(7)           COMP-3.                  
011500*                                 INVENTERINGSSALDO                       
011600        05 UDET-IDANSK       PIC S9(3)           COMP-3.                  
011700*                                 ANSKAFFARNUMMER                         
011800        05 UDET-BEART        PIC X(25).                                   
011900*                                 ARTIKELBENÄMNING                        
012000        05 UDET-REKSIFFR     PIC 9.                                       
012100*                                 KONTROLLSIFFRA                          
012200     03 UDET-TEXT-INFO.                                                   
012300*                                                                         
012400        05 UDET-TEANMNOT-REG OCCURS 3 TIMES                               
012500                             PIC X(70).                                   
012600*                                 FRI TEXT FRÅN REGISTRERINGEN            
012700        05 UDET-TEANMNOT-ADM OCCURS 3 TIMES                               
012800                             PIC X(70).                                   
012900*                                 FRI TEXT FRÅN ADMINISTRATION            
013000        05 UDET-TEANMNOT-REM OCCURS 3 TIMES                               
013100                             PIC X(70).                                   
013200*                                 FRI TEXT FRÅN REMISSINSTANS             
013300        05 UDET-TEANMNOT-RET OCCURS 3 TIMES                               
013400                             PIC X(70).                                   
013500*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
013600*** END OF VILMAII-COPY LENGTH= 1022 BYTES                                
