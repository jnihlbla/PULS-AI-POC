000100 01  OBKR-W414003.                                                        
000200*                                 SKAPAS FÖR ORDERBEKRÄFTELSE-            
000300*                                 RAD VID TÖMNING AV                      
000400*                                 TRANSAKTIONER.                          
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 OBKR-IDPTYP          PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 OBKR-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 OBKR-BEERS           PIC X(20).                                   
001200*                                 ERSÄTTNINGSTEXT                         
001300     03 OBKR-BERADREF        PIC X(10).                                   
001400*                                 KUNDENS RADREFERENS                     
001500     03 OBKR-BEVARREF        PIC X(10).                                   
001600*                                 VÅR REFERENS                            
001700     03 OBKR-BEVOLREF        PIC X(10).                                   
001800*                                 VOLVO REFERENS                          
001900     03 OBKR-DIERS-KVOT      PIC S9(4)V9(3)      COMP-3.                  
002000*                                 KVOT MELLAN                             
002100*                                 DIERS-TILLK OCH DIERS-ERS               
002200     03 OBKR-FLINVEST        PIC X.                                       
002300*                                 BYTES INVENTERINGSFLAGGA                
002400     03 OBKR-FLOVRLEV        PIC X.                                       
002500*                                 ÖVERLEVERANS                            
002600     03 OBKR-FLPRTILL        PIC X.                                       
002700*                                 PRISTILLÄGGS FLAGGA                     
002800     03 OBKR-FLTILLK         PIC X.                                       
002900*                                 TILLKOMMANDE ARTIKEL ?                  
003000     03 OBKR-IDARTNR-TILLK   PIC S9(9)           COMP-3.                  
003100*                                 TILLKOMMANDE ARTIKELNUMMER              
003200     03 OBKR-IDDISTR         PIC S9(5)           COMP-3.                  
003300*                                 DISTRIKTNUMMER                          
003400     03 OBKR-IDFKNGRP        PIC S9(5)           COMP-3.                  
003500*                                 FUNKTIONSGRUPP                          
003600     03 OBKR-IDKONTO         PIC S9(11)          COMP-3.                  
003700*                                 KONTO                                   
003800     03 OBKR-IDKST           PIC X(10).                                   
003900*                                 KOSTNADSSTÄLLE                          
004000     03 OBKR-IDKUNDNR        PIC S9(7)           COMP-3.                  
004100*                                 KUNDNUMMER                              
004200     03 OBKR-IDKUNDRF        PIC X(10).                                   
004300*                                 KUNDENS REFERENS (ORDERID)              
004400     03 OBKR-IDKUNDRF-RO     PIC X(10).                                   
004500*                                 KUND REF PÅ RO                          
004600     03 OBKR-IDLOPNR         PIC S9(3)           COMP-3.                  
004700*                                 LÖPNUMMER                               
004800     03 OBKR-IDORDER         PIC S9(7)           COMP-3.                  
004900*                                 VOLVO PARTS ORDERNUMMER                 
005000     03 OBKR-IDSEKVNR        PIC S9(3)           COMP-3.                  
005100*                                 GENERELLT SEKVENSNUMMER                 
005200     03 OBKR-IDSYSTEM        PIC X(4).                                    
005300*                                 VOLVO VCCS SYSTEMNUMMER                 
005400     03 OBKR-IDDC            PIC X(2).                                    
005500*                                 IDENTIFIERARE LAGER                     
005600     03 OBKR-KDDSP           PIC S9              COMP-3.                  
005700*                                 PÅVERKAN PÅ DSP                         
005800     03 OBKR-KDERS           PIC S9(3)           COMP-3.                  
005900*                                 ERSÄTTNINGSKOD                          
006000     03 OBKR-KDFAKTYP        PIC X.                                       
006100*                                 FAKTURATYP                              
006200     03 OBKR-KDFRAKT         PIC S9(3)           COMP-3.                  
006300*                                 FRAKTSÄTT DC TILL KUND                  
006400     03 OBKR-KDKVBRYT        PIC S9              COMP-3.                  
006500*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006600     03 OBKR-KDORDBEK        PIC 9(2).                                    
006700*                                 ORDERBEKRÄFTELSEKOD                     
006800     03 OBKR-KDORDKL         PIC S9              COMP-3.                  
006900*                                 ORDERKLASS                              
007000     03 OBKR-KDPRODSL        PIC S9(3)           COMP-3.                  
007100*                                 PRODUKTSLAG                             
007200     03 OBKR-KDPRTYP         PIC X.                                       
007300*                                 TYP AV PRISTILLÄMPNING                  
007400     03 OBKR-KDTIPPR         PIC S9              COMP-3.                  
007500*                                 TIPPAT PRIS KOD                         
007600     03 OBKR-KDTPOTYP        PIC S9              COMP-3.                  
007700*                                 TYP AV TIDPLANERAD ORDER                
007800     03 OBKR-KDUART          PIC X.                                       
007900*                                 UNDANTAGSARTIKEL                        
008000     03 OBKR-KDVRINFO        PIC S9              COMP-3.                  
008100*                                 PÅVERKAN I VR/DSP SYSTEM                
008200     03 OBKR-KVBEART         PIC S9(7)           COMP-3.                  
008300*                                 BESTÄLLT ANTAL STYCKEN                  
008400     03 OBKR-KVBEART-Q       PIC S9(7)           COMP-3.                  
008500*                                 BESTÄLLT KVANTANPASSAT ANTAL            
008600     03 OBKR-KVBEART-TILLK   PIC S9(7)           COMP-3.                  
008700*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
008800     03 OBKR-KVPREAVB        PIC S9(7)           COMP-3.                  
008900*                                 PREL-AVB KVANT                          
009000     03 OBKR-KVPRERO         PIC S9(7)           COMP-3.                  
009100*                                 PRELIMINÄR RO-KVANT                     
009200     03 OBKR-KVQPACK-1       PIC S9(5)           COMP-3.                  
009300*                                 ANTAL I Q1 FÖRPACKNING                  
009400     03 OBKR-PRARTBTO-EXP    PIC S9(7)V9(2)      COMP-3.                  
009500*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
009600     03 OBKR-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
009700*                                 ARTIKELPRIS NETTO                       
009800     03 OBKR-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
009900*                                 ARTIKELNS SJÄLVKOSTNAD                  
010000     03 OBKR-REKSIFFR        PIC S9              COMP-3.                  
010100*                                 KONTROLLSIFFRA                          
010200     03 OBKR-REKSIFFR-TILLK  PIC S9              COMP-3.                  
010300*                                 TILLKOMMANDE KONTROLLSIFFRA             
010400     03 OBKR-TIDISPIN        PIC S9(7)           COMP-3.                  
010500*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
010600     03 OBKR-TIORDREG        PIC S9(7)           COMP-3.                  
010700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
010800     03 OBKR-TIREGDAT-OBKR   PIC S9(7)           COMP-3.                  
010900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011000     03 OBKR-TITPO           PIC S9(7)           COMP-3.                  
011100*                                 PLANERAD ORDERDATUM                     
011200     03 OBKR-TIREGDAT        PIC S9(7)           COMP-3.                  
011300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011400     03 OBKR-TIKLOCK         PIC S9(9)           COMP-3.                  
011500*                                 KLOCKSLAG (TTMMSSTH)                    
011600     03 OBKR-TIREPDAT        PIC S9(7)           COMP-3.                  
011700*                                 REPAIR DATE                             
011800*** END OF VILMAII-COPY LENGTH= 217 BYTES                                 
