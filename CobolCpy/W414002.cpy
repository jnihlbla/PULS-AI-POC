000100 01  ORAD-W414002.                                                        
000200*                                 SKAPAS FÖR ORDERRAD VID                 
000300*                                 TÖMNING AV TRANSAKTIONER.               
000400*                                 ANVÄNDS VID TRANSAKTION-                
000500*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000600     03 ORAD-IDPTYP          PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 ORAD-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 ORAD-BERADREF        PIC X(10).                                   
001100*                                 KUNDENS RADREFERENS                     
001200     03 ORAD-BEVARREF        PIC X(10).                                   
001300*                                 VÅR REFERENS                            
001400     03 ORAD-BEVOLREF        PIC X(10).                                   
001500*                                 VOLVO REFERENS                          
001600     03 ORAD-FLINVEST        PIC X.                                       
001700*                                 BYTES INVENTERINGSFLAGGA                
001800     03 ORAD-FLPRTILL        PIC X.                                       
001900*                                 PRISTILLÄGGS FLAGGA                     
002000     03 ORAD-FLTILLK         PIC X.                                       
002100*                                 TILLKOMMANDE ARTIKEL ?                  
002200     03 ORAD-FLREFILL        PIC X.                                       
002300*                                 REFILLARTIKEL                           
002400     03 ORAD-FLOVRLEV        PIC X.                                       
002500*                                 ÖVERLEVERANS                            
002600     03 ORAD-IDDISTR         PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800     03 ORAD-IDFKNGRP        PIC S9(5)           COMP-3.                  
002900*                                 FUNKTIONSGRUPP                          
003000     03 ORAD-IDKAMPRF        PIC S9(7)           COMP-3.                  
003100*                                 KAMPANJREFERENS                         
003200     03 ORAD-IDKONTO         PIC S9(11)          COMP-3.                  
003300*                                 KONTO                                   
003400     03 ORAD-IDKST           PIC X(10).                                   
003500*                                 KOSTNADSSTÄLLE                          
003600     03 ORAD-IDKUNDNR        PIC S9(7)           COMP-3.                  
003700*                                 KUNDNUMMER                              
003800     03 ORAD-IDKUNDRF        PIC X(10).                                   
003900*                                 KUNDENS REFERENS (ORDERID)              
004000     03 ORAD-IDKUNDRF-RO     PIC X(10).                                   
004100*                                 KUND REF PÅ RO                          
004200     03 ORAD-IDLOPNR         PIC S9(3)           COMP-3.                  
004300*                                 LÖPNUMMER                               
004400     03 ORAD-IDORDER         PIC S9(7)           COMP-3.                  
004500*                                 VOLVO PARTS ORDERNUMMER                 
004600     03 ORAD-IDSYSTEM        PIC X(4).                                    
004700*                                 VOLVO VCCS SYSTEMNUMMER                 
004800     03 ORAD-IDSYSTEM-OHUV   PIC X(4).                                    
004900*                                 VOLVO VCCS SYSTEMNUMMER                 
005000     03 ORAD-IDUSER          PIC X(8).                                    
005100*                                 ANVÄNDARENS SÄKERHETS ID                
005200     03 ORAD-IDDC            PIC X(2).                                    
005300*                                 IDENTIFIERARE LAGER                     
005400     03 ORAD-IDDC-CLEAR      PIC X(2).                                    
005500*                                 LAGERPRIORITERING VID                   
005600*                                 ORDERCLEARING                           
005700     03 ORAD-KDDSP           PIC S9              COMP-3.                  
005800*                                 PÅVERKAN PÅ DSP                         
005900     03 ORAD-KDFAKTYP        PIC X.                                       
006000*                                 FAKTURATYP                              
006100     03 ORAD-KDFRAKT         PIC S9(3)           COMP-3.                  
006200*                                 FRAKTSÄTT DC TILL KUND                  
006300     03 ORAD-KDKVBRYT        PIC S9              COMP-3.                  
006400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006500     03 ORAD-KDORDING        PIC S9              COMP-3.                  
006600*                                 UPPDATERING ORDERINGÅNG                 
006700     03 ORAD-KDORDKL         PIC S9              COMP-3.                  
006800*                                 ORDERKLASS                              
006900     03 ORAD-KDPRTYP         PIC X.                                       
007000*                                 TYP AV PRISTILLÄMPNING                  
007100     03 ORAD-KDPRODSL        PIC S9(3)           COMP-3.                  
007200*                                 PRODUKTSLAG                             
007300     03 ORAD-KDTIPPR         PIC S9              COMP-3.                  
007400*                                 TIPPAT PRIS KOD                         
007500     03 ORAD-KDTPOTYP        PIC S9              COMP-3.                  
007600*                                 TYP AV TIDPLANERAD ORDER                
007700     03 ORAD-KDUART          PIC X.                                       
007800*                                 UNDANTAGSARTIKEL                        
007900     03 ORAD-KDVRINFO        PIC S9              COMP-3.                  
008000*                                 PÅVERKAN I VR/DSP SYSTEM                
008100     03 ORAD-KVBEART         PIC S9(7)           COMP-3.                  
008200*                                 BESTÄLLT ANTAL STYCKEN                  
008300     03 ORAD-KVBEART-Q       PIC S9(7)           COMP-3.                  
008400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
008500     03 ORAD-PRARTBTO-EXP    PIC S9(7)V9(2)      COMP-3.                  
008600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
008700     03 ORAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
008800*                                 ARTIKELPRIS NETTO                       
008900     03 ORAD-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
009000*                                 ARTIKELNS SJÄLVKOSTNAD                  
009100     03 ORAD-PRBPRIS         PIC S9(7)V9(2)      COMP-3.                  
009200*                                 BASPRIS                                 
009300     03 ORAD-REDIRLEV        PIC S9V9(2)         COMP-3.                  
009400*                                 DIREKTLEVERANSANDEL                     
009500     03 ORAD-REKSIFFR        PIC S9              COMP-3.                  
009600*                                 KONTROLLSIFFRA                          
009700     03 ORAD-TIREGDAT-ORDER  PIC S9(7)           COMP-3.                  
009800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
009900     03 ORAD-TIRODAT         PIC S9(7)           COMP-3.                  
010000*                                 RESTORDERDATUM         (ÅÅMMDD)         
010100     03 ORAD-TIREGDAT        PIC S9(7)           COMP-3.                  
010200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010300     03 ORAD-TIKLOCK         PIC S9(9)           COMP-3.                  
010400*                                 KLOCKSLAG (TTMMSSTH)                    
010500*** END OF VILMAII-COPY LENGTH= 181 BYTES                                 
