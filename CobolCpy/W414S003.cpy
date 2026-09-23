000100 01  OBKR-W414S003.                                                       
000200*                                 SKAPAS FÖR ORDERBEKRÄFTELSE-            
000300*                                 RAD VID TÖMNING AV                      
000400*                                 TRANSAKTIONER.                          
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 OBKR-IDARTNR-S       PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 OBKR-IDARTNR-TILLK-S PIC S9(9)           COMP-3.                  
001000*                                 TILLKOMMANDE ARTIKELNUMMER              
001100     03 OBKR-W414003.                                                     
001200*                                 SKAPAS FÖR ORDERBEKRÄFTELSE-            
001300*                                 RAD VID TÖMNING AV                      
001400*                                 TRANSAKTIONER.                          
001500*                                 ANVÄNDS VID TRANSAKTION-                
001600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
001700        05 OBKR-IDPTYP       PIC X(3).                                    
001800*                                 POSTTYP                                 
001900        05 OBKR-IDARTNR      PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100        05 OBKR-BEERS        PIC X(20).                                   
002200*                                 ERSÄTTNINGSTEXT                         
002300        05 OBKR-BERADREF     PIC X(10).                                   
002400*                                 KUNDENS RADREFERENS                     
002500        05 OBKR-BEVARREF     PIC X(10).                                   
002600*                                 VÅR REFERENS                            
002700        05 OBKR-BEVOLREF     PIC X(10).                                   
002800*                                 VOLVO REFERENS                          
002900        05 OBKR-DIERS-KVOT   PIC S9(4)V9(3)      COMP-3.                  
003000*                                 KVOT MELLAN                             
003100*                                 DIERS-TILLK OCH DIERS-ERS               
003200        05 OBKR-FLINVEST     PIC X.                                       
003300*                                 BYTES INVENTERINGSFLAGGA                
003400        05 OBKR-FLOVRLEV     PIC X.                                       
003500*                                 ÖVERLEVERANS                            
003600        05 OBKR-FLPRTILL     PIC X.                                       
003700*                                 PRISTILLÄGGS FLAGGA                     
003800        05 OBKR-FLTILLK      PIC X.                                       
003900*                                 TILLKOMMANDE ARTIKEL ?                  
004000        05 OBKR-IDARTNR-TILLK                                             
004100                             PIC S9(9)           COMP-3.                  
004200*                                 TILLKOMMANDE ARTIKELNUMMER              
004300        05 OBKR-IDDISTR      PIC S9(5)           COMP-3.                  
004400*                                 DISTRIKTNUMMER                          
004500        05 OBKR-IDFKNGRP     PIC S9(5)           COMP-3.                  
004600*                                 FUNKTIONSGRUPP                          
004700        05 OBKR-IDKONTO      PIC S9(11)          COMP-3.                  
004800*                                 KONTO                                   
004900        05 OBKR-IDKST        PIC X(10).                                   
005000*                                 KOSTNADSSTÄLLE                          
005100        05 OBKR-IDKUNDNR     PIC S9(7)           COMP-3.                  
005200*                                 KUNDNUMMER                              
005300        05 OBKR-IDKUNDRF     PIC X(10).                                   
005400*                                 KUNDENS REFERENS (ORDERID)              
005500        05 OBKR-IDKUNDRF-RO  PIC X(10).                                   
005600*                                 KUND REF PÅ RO                          
005700        05 OBKR-IDLOPNR      PIC S9(3)           COMP-3.                  
005800*                                 LÖPNUMMER                               
005900        05 OBKR-IDORDER      PIC S9(7)           COMP-3.                  
006000*                                 VOLVO PARTS ORDERNUMMER                 
006100        05 OBKR-IDSEKVNR     PIC S9(3)           COMP-3.                  
006200*                                 GENERELLT SEKVENSNUMMER                 
006300        05 OBKR-IDSYSTEM     PIC X(4).                                    
006400*                                 VOLVO VCCS SYSTEMNUMMER                 
006500        05 OBKR-IDDC         PIC X(2).                                    
006600*                                 IDENTIFIERARE LAGER                     
006700        05 OBKR-KDDSP        PIC S9              COMP-3.                  
006800*                                 PÅVERKAN PÅ DSP                         
006900        05 OBKR-KDERS        PIC S9(3)           COMP-3.                  
007000*                                 ERSÄTTNINGSKOD                          
007100        05 OBKR-KDFAKTYP     PIC X.                                       
007200*                                 FAKTURATYP                              
007300        05 OBKR-KDFRAKT      PIC S9(3)           COMP-3.                  
007400*                                 FRAKTSÄTT DC TILL KUND                  
007500        05 OBKR-KDKVBRYT     PIC S9              COMP-3.                  
007600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
007700        05 OBKR-KDORDBEK     PIC 9(2).                                    
007800*                                 ORDERBEKRÄFTELSEKOD                     
007900        05 OBKR-KDORDKL      PIC S9              COMP-3.                  
008000*                                 ORDERKLASS                              
008100        05 OBKR-KDPRODSL     PIC S9(3)           COMP-3.                  
008200*                                 PRODUKTSLAG                             
008300        05 OBKR-KDPRTYP      PIC X.                                       
008400*                                 TYP AV PRISTILLÄMPNING                  
008500        05 OBKR-KDTIPPR      PIC S9              COMP-3.                  
008600*                                 TIPPAT PRIS KOD                         
008700        05 OBKR-KDTPOTYP     PIC S9              COMP-3.                  
008800*                                 TYP AV TIDPLANERAD ORDER                
008900        05 OBKR-KDUART       PIC X.                                       
009000*                                 UNDANTAGSARTIKEL                        
009100        05 OBKR-KDVRINFO     PIC S9              COMP-3.                  
009200*                                 PÅVERKAN I VR/DSP SYSTEM                
009300        05 OBKR-KVBEART      PIC S9(7)           COMP-3.                  
009400*                                 BESTÄLLT ANTAL STYCKEN                  
009500        05 OBKR-KVBEART-Q    PIC S9(7)           COMP-3.                  
009600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
009700        05 OBKR-KVBEART-TILLK                                             
009800                             PIC S9(7)           COMP-3.                  
009900*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
010000        05 OBKR-KVPREAVB     PIC S9(7)           COMP-3.                  
010100*                                 PREL-AVB KVANT                          
010200        05 OBKR-KVPRERO      PIC S9(7)           COMP-3.                  
010300*                                 PRELIMINÄR RO-KVANT                     
010400        05 OBKR-KVQPACK-1    PIC S9(5)           COMP-3.                  
010500*                                 ANTAL I Q1 FÖRPACKNING                  
010600        05 OBKR-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
010700*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
010800        05 OBKR-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
010900*                                 ARTIKELPRIS NETTO                       
011000        05 OBKR-PRARTSJK     PIC S9(7)V9(2)      COMP-3.                  
011100*                                 ARTIKELNS SJÄLVKOSTNAD                  
011200        05 OBKR-REKSIFFR     PIC S9              COMP-3.                  
011300*                                 KONTROLLSIFFRA                          
011400        05 OBKR-REKSIFFR-TILLK                                            
011500                             PIC S9              COMP-3.                  
011600*                                 TILLKOMMANDE KONTROLLSIFFRA             
011700        05 OBKR-TIDISPIN     PIC S9(7)           COMP-3.                  
011800*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
011900        05 OBKR-TIORDREG     PIC S9(7)           COMP-3.                  
012000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
012100        05 OBKR-TIREGDAT-OBKR                                             
012200                             PIC S9(7)           COMP-3.                  
012300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
012400        05 OBKR-TITPO        PIC S9(7)           COMP-3.                  
012500*                                 PLANERAD ORDERDATUM                     
012600        05 OBKR-TIREGDAT     PIC S9(7)           COMP-3.                  
012700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
012800        05 OBKR-TIKLOCK      PIC S9(9)           COMP-3.                  
012900*                                 KLOCKSLAG (TTMMSSTH)                    
013000        05 OBKR-TIREPDAT     PIC S9(7)           COMP-3.                  
013100*                                 REPAIR DATE                             
013200*** END OF VILMAII-COPY LENGTH= 227 BYTES                                 
