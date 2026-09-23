000100 01  TILLTPO-W414S005.                                                    
000200*                                 SKAPAS FÖR TILLÄGGS TPO:ER -            
000300*                                 VID TÖMNING AV                          
000400*                                 TRANSAKTIONER.                          
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 TILLTPO-IDARTNR-S    PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 TILLTPO-IDARTNR-TILLK-S                                           
001000                             PIC S9(9)           COMP-3.                  
001100*                                 TILLKOMMANDE ARTIKELNUMMER              
001200     03 TILLTPO-W414005.                                                  
001300*                                 SKAPAS FÖR TILLÄGG TPO:ER.              
001400*                                 ANVÄNDS VID TRANSAKTION-                
001500*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
001600        05 TILLTPO-IDPTYP    PIC X(3).                                    
001700*                                 POSTTYP                                 
001800        05 TILLTPO-IDARTNR   PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000        05 TILLTPO-BERADREF  PIC X(10).                                   
002100*                                 KUNDENS RADREFERENS                     
002200        05 TILLTPO-BEVARREF  PIC X(10).                                   
002300*                                 VÅR REFERENS                            
002400        05 TILLTPO-BEVOLREF  PIC X(10).                                   
002500*                                 VOLVO REFERENS                          
002600        05 TILLTPO-FLINVEST  PIC X.                                       
002700*                                 BYTES INVENTERINGSFLAGGA                
002800        05 TILLTPO-FLPRTILL  PIC X.                                       
002900*                                 PRISTILLÄGGS FLAGGA                     
003000        05 TILLTPO-FLTILLK   PIC X.                                       
003100*                                 TILLKOMMANDE ARTIKEL ?                  
003200        05 TILLTPO-IDDISTR   PIC S9(5)           COMP-3.                  
003300*                                 DISTRIKTNUMMER                          
003400        05 TILLTPO-IDFKNGRP  PIC S9(5)           COMP-3.                  
003500*                                 FUNKTIONSGRUPP                          
003600        05 TILLTPO-IDKONTO   PIC S9(11)          COMP-3.                  
003700*                                 KONTO                                   
003800        05 TILLTPO-IDKST     PIC X(10).                                   
003900*                                 KOSTNADSSTÄLLE                          
004000        05 TILLTPO-IDKUNDNR  PIC S9(7)           COMP-3.                  
004100*                                 KUNDNUMMER                              
004200        05 TILLTPO-IDKUNDRF  PIC X(10).                                   
004300*                                 KUNDENS REFERENS (ORDERID)              
004400        05 TILLTPO-IDSYSTEM  PIC X(4).                                    
004500*                                 VOLVO VCCS SYSTEMNUMMER                 
004600        05 TILLTPO-KDDSP     PIC S9              COMP-3.                  
004700*                                 PÅVERKAN PÅ DSP                         
004800        05 TILLTPO-KDFAKTYP  PIC X.                                       
004900*                                 FAKTURATYP                              
005000        05 TILLTPO-KDFRAKT   PIC S9(3)           COMP-3.                  
005100*                                 FRAKTSÄTT DC TILL KUND                  
005200        05 TILLTPO-KDKVBRYT  PIC S9              COMP-3.                  
005300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005400        05 TILLTPO-KDORDBEK  PIC 9(2).                                    
005500*                                 ORDERBEKRÄFTELSEKOD                     
005600        05 TILLTPO-KDORDING  PIC S9              COMP-3.                  
005700*                                 UPPDATERING ORDERINGÅNG                 
005800        05 TILLTPO-KDORDKL   PIC S9              COMP-3.                  
005900*                                 ORDERKLASS                              
006000        05 TILLTPO-KDPRTYP   PIC X.                                       
006100*                                 TYP AV PRISTILLÄMPNING                  
006200        05 TILLTPO-KDPRODSL  PIC S9(3)           COMP-3.                  
006300*                                 PRODUKTSLAG                             
006400        05 TILLTPO-KDTPOTYP  PIC S9              COMP-3.                  
006500*                                 TYP AV TIDPLANERAD ORDER                
006600        05 TILLTPO-KDVRINFO  PIC S9              COMP-3.                  
006700*                                 PÅVERKAN I VR/DSP SYSTEM                
006800        05 TILLTPO-KVBEART   PIC S9(7)           COMP-3.                  
006900*                                 BESTÄLLT ANTAL STYCKEN                  
007000        05 TILLTPO-KVBEART-Q PIC S9(7)           COMP-3.                  
007100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
007200        05 TILLTPO-KVQPACK-1 PIC S9(5)           COMP-3.                  
007300*                                 ANTAL I Q1 FÖRPACKNING                  
007400        05 TILLTPO-PRARTBTO-EXP                                           
007500                             PIC S9(7)V9(2)      COMP-3.                  
007600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
007700        05 TILLTPO-PRARTNTO  PIC S9(7)V9(2)      COMP-3.                  
007800*                                 ARTIKELPRIS NETTO                       
007900        05 TILLTPO-REKSIFFR  PIC S9              COMP-3.                  
008000*                                 KONTROLLSIFFRA                          
008100        05 TILLTPO-TIDISPIN  PIC S9(7)           COMP-3.                  
008200*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
008300        05 TILLTPO-TIREGDAT-TPO                                           
008400                             PIC S9(7)           COMP-3.                  
008500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008600        05 TILLTPO-TITPO     PIC S9(7)           COMP-3.                  
008700*                                 PLANERAD ORDERDATUM                     
008800        05 TILLTPO-TIREGDAT  PIC S9(7)           COMP-3.                  
008900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
009000        05 TILLTPO-TIKLOCK   PIC S9(9)           COMP-3.                  
009100*                                 KLOCKSLAG (TTMMSSTH)                    
009200*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
