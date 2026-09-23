000100 01  ORAD-W414S002.                                                       
000200*                                 SKAPAS FÖR ORDERRAD VID                 
000300*                                 TÖMNING AV TRANSAKTIONER.               
000400*                                 ANVÄNDS VID TRANSAKTION-                
000500*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000600     03 ORAD-IDARTNR-S       PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 ORAD-IDARTNR-TILLK-S PIC S9(9)           COMP-3.                  
000900*                                 TILLKOMMANDE ARTIKELNUMMER              
001000     03 ORAD-W414002.                                                     
001100*                                 SKAPAS FÖR ORDERRAD VID                 
001200*                                 TÖMNING AV TRANSAKTIONER.               
001300*                                 ANVÄNDS VID TRANSAKTION-                
001400*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
001500        05 ORAD-IDPTYP       PIC X(3).                                    
001600*                                 POSTTYP                                 
001700        05 ORAD-IDARTNR      PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900        05 ORAD-BERADREF     PIC X(10).                                   
002000*                                 KUNDENS RADREFERENS                     
002100        05 ORAD-BEVARREF     PIC X(10).                                   
002200*                                 VÅR REFERENS                            
002300        05 ORAD-BEVOLREF     PIC X(10).                                   
002400*                                 VOLVO REFERENS                          
002500        05 ORAD-FLINVEST     PIC X.                                       
002600*                                 BYTES INVENTERINGSFLAGGA                
002700        05 ORAD-FLPRTILL     PIC X.                                       
002800*                                 PRISTILLÄGGS FLAGGA                     
002900        05 ORAD-FLTILLK      PIC X.                                       
003000*                                 TILLKOMMANDE ARTIKEL ?                  
003100        05 ORAD-FLREFILL     PIC X.                                       
003200*                                 REFILLARTIKEL                           
003300        05 ORAD-FLOVRLEV     PIC X.                                       
003400*                                 ÖVERLEVERANS                            
003500        05 ORAD-IDDISTR      PIC S9(5)           COMP-3.                  
003600*                                 DISTRIKTNUMMER                          
003700        05 ORAD-IDFKNGRP     PIC S9(5)           COMP-3.                  
003800*                                 FUNKTIONSGRUPP                          
003900        05 ORAD-IDKAMPRF     PIC S9(7)           COMP-3.                  
004000*                                 KAMPANJREFERENS                         
004100        05 ORAD-IDKONTO      PIC S9(11)          COMP-3.                  
004200*                                 KONTO                                   
004300        05 ORAD-IDKST        PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500        05 ORAD-IDKUNDNR     PIC S9(7)           COMP-3.                  
004600*                                 KUNDNUMMER                              
004700        05 ORAD-IDKUNDRF     PIC X(10).                                   
004800*                                 KUNDENS REFERENS (ORDERID)              
004900        05 ORAD-IDKUNDRF-RO  PIC X(10).                                   
005000*                                 KUND REF PÅ RO                          
005100        05 ORAD-IDLOPNR      PIC S9(3)           COMP-3.                  
005200*                                 LÖPNUMMER                               
005300        05 ORAD-IDORDER      PIC S9(7)           COMP-3.                  
005400*                                 VOLVO PARTS ORDERNUMMER                 
005500        05 ORAD-IDSYSTEM     PIC X(4).                                    
005600*                                 VOLVO VCCS SYSTEMNUMMER                 
005700        05 ORAD-IDSYSTEM-OHUV                                             
005800                             PIC X(4).                                    
005900*                                 VOLVO VCCS SYSTEMNUMMER                 
006000        05 ORAD-IDUSER       PIC X(8).                                    
006100*                                 ANVÄNDARENS SÄKERHETS ID                
006200        05 ORAD-IDDC         PIC X(2).                                    
006300*                                 IDENTIFIERARE LAGER                     
006400        05 ORAD-IDDC-CLEAR   PIC X(2).                                    
006500*                                 LAGERPRIORITERING VID                   
006600*                                 ORDERCLEARING                           
006700        05 ORAD-KDDSP        PIC S9              COMP-3.                  
006800*                                 PÅVERKAN PÅ DSP                         
006900        05 ORAD-KDFAKTYP     PIC X.                                       
007000*                                 FAKTURATYP                              
007100        05 ORAD-KDFRAKT      PIC S9(3)           COMP-3.                  
007200*                                 FRAKTSÄTT DC TILL KUND                  
007300        05 ORAD-KDKVBRYT     PIC S9              COMP-3.                  
007400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
007500        05 ORAD-KDORDING     PIC S9              COMP-3.                  
007600*                                 UPPDATERING ORDERINGÅNG                 
007700        05 ORAD-KDORDKL      PIC S9              COMP-3.                  
007800*                                 ORDERKLASS                              
007900        05 ORAD-KDPRTYP      PIC X.                                       
008000*                                 TYP AV PRISTILLÄMPNING                  
008100        05 ORAD-KDPRODSL     PIC S9(3)           COMP-3.                  
008200*                                 PRODUKTSLAG                             
008300        05 ORAD-KDTIPPR      PIC S9              COMP-3.                  
008400*                                 TIPPAT PRIS KOD                         
008500        05 ORAD-KDTPOTYP     PIC S9              COMP-3.                  
008600*                                 TYP AV TIDPLANERAD ORDER                
008700        05 ORAD-KDUART       PIC X.                                       
008800*                                 UNDANTAGSARTIKEL                        
008900        05 ORAD-KDVRINFO     PIC S9              COMP-3.                  
009000*                                 PÅVERKAN I VR/DSP SYSTEM                
009100        05 ORAD-KVBEART      PIC S9(7)           COMP-3.                  
009200*                                 BESTÄLLT ANTAL STYCKEN                  
009300        05 ORAD-KVBEART-Q    PIC S9(7)           COMP-3.                  
009400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
009500        05 ORAD-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
009600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
009700        05 ORAD-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
009800*                                 ARTIKELPRIS NETTO                       
009900        05 ORAD-PRARTSJK     PIC S9(7)V9(2)      COMP-3.                  
010000*                                 ARTIKELNS SJÄLVKOSTNAD                  
010100        05 ORAD-PRBPRIS      PIC S9(7)V9(2)      COMP-3.                  
010200*                                 BASPRIS                                 
010300        05 ORAD-REDIRLEV     PIC S9V9(2)         COMP-3.                  
010400*                                 DIREKTLEVERANSANDEL                     
010500        05 ORAD-REKSIFFR     PIC S9              COMP-3.                  
010600*                                 KONTROLLSIFFRA                          
010700        05 ORAD-TIREGDAT-ORDER                                            
010800                             PIC S9(7)           COMP-3.                  
010900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011000        05 ORAD-TIRODAT      PIC S9(7)           COMP-3.                  
011100*                                 RESTORDERDATUM         (ÅÅMMDD)         
011200        05 ORAD-TIREGDAT     PIC S9(7)           COMP-3.                  
011300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011400        05 ORAD-TIKLOCK      PIC S9(9)           COMP-3.                  
011500*                                 KLOCKSLAG (TTMMSSTH)                    
011600*** END OF VILMAII-COPY LENGTH= 191 BYTES                                 
