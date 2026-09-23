000100 01  TILLTPO-W414005.                                                     
000200*                                 SKAPAS FÖR TILLÄGG TPO:ER.              
000300*                                 ANVÄNDS VID TRANSAKTION-                
000400*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000500     03 TILLTPO-IDPTYP       PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 TILLTPO-IDARTNR      PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 TILLTPO-BERADREF     PIC X(10).                                   
001000*                                 KUNDENS RADREFERENS                     
001100     03 TILLTPO-BEVARREF     PIC X(10).                                   
001200*                                 VÅR REFERENS                            
001300     03 TILLTPO-BEVOLREF     PIC X(10).                                   
001400*                                 VOLVO REFERENS                          
001500     03 TILLTPO-FLINVEST     PIC X.                                       
001600*                                 BYTES INVENTERINGSFLAGGA                
001700     03 TILLTPO-FLPRTILL     PIC X.                                       
001800*                                 PRISTILLÄGGS FLAGGA                     
001900     03 TILLTPO-FLTILLK      PIC X.                                       
002000*                                 TILLKOMMANDE ARTIKEL ?                  
002100     03 TILLTPO-IDDISTR      PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300     03 TILLTPO-IDFKNGRP     PIC S9(5)           COMP-3.                  
002400*                                 FUNKTIONSGRUPP                          
002500     03 TILLTPO-IDKONTO      PIC S9(11)          COMP-3.                  
002600*                                 KONTO                                   
002700     03 TILLTPO-IDKST        PIC X(10).                                   
002800*                                 KOSTNADSSTÄLLE                          
002900     03 TILLTPO-IDKUNDNR     PIC S9(7)           COMP-3.                  
003000*                                 KUNDNUMMER                              
003100     03 TILLTPO-IDKUNDRF     PIC X(10).                                   
003200*                                 KUNDENS REFERENS (ORDERID)              
003300     03 TILLTPO-IDSYSTEM     PIC X(4).                                    
003400*                                 VOLVO VCCS SYSTEMNUMMER                 
003500     03 TILLTPO-KDDSP        PIC S9              COMP-3.                  
003600*                                 PÅVERKAN PÅ DSP                         
003700     03 TILLTPO-KDFAKTYP     PIC X.                                       
003800*                                 FAKTURATYP                              
003900     03 TILLTPO-KDFRAKT      PIC S9(3)           COMP-3.                  
004000*                                 FRAKTSÄTT DC TILL KUND                  
004100     03 TILLTPO-KDKVBRYT     PIC S9              COMP-3.                  
004200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004300     03 TILLTPO-KDORDBEK     PIC 9(2).                                    
004400*                                 ORDERBEKRÄFTELSEKOD                     
004500     03 TILLTPO-KDORDING     PIC S9              COMP-3.                  
004600*                                 UPPDATERING ORDERINGÅNG                 
004700     03 TILLTPO-KDORDKL      PIC S9              COMP-3.                  
004800*                                 ORDERKLASS                              
004900     03 TILLTPO-KDPRTYP      PIC X.                                       
005000*                                 TYP AV PRISTILLÄMPNING                  
005100     03 TILLTPO-KDPRODSL     PIC S9(3)           COMP-3.                  
005200*                                 PRODUKTSLAG                             
005300     03 TILLTPO-KDTPOTYP     PIC S9              COMP-3.                  
005400*                                 TYP AV TIDPLANERAD ORDER                
005500     03 TILLTPO-KDVRINFO     PIC S9              COMP-3.                  
005600*                                 PÅVERKAN I VR/DSP SYSTEM                
005700     03 TILLTPO-KVBEART      PIC S9(7)           COMP-3.                  
005800*                                 BESTÄLLT ANTAL STYCKEN                  
005900     03 TILLTPO-KVBEART-Q    PIC S9(7)           COMP-3.                  
006000*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006100     03 TILLTPO-KVQPACK-1    PIC S9(5)           COMP-3.                  
006200*                                 ANTAL I Q1 FÖRPACKNING                  
006300     03 TILLTPO-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
006400*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
006500     03 TILLTPO-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
006600*                                 ARTIKELPRIS NETTO                       
006700     03 TILLTPO-REKSIFFR     PIC S9              COMP-3.                  
006800*                                 KONTROLLSIFFRA                          
006900     03 TILLTPO-TIDISPIN     PIC S9(7)           COMP-3.                  
007000*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
007100     03 TILLTPO-TIREGDAT-TPO PIC S9(7)           COMP-3.                  
007200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007300     03 TILLTPO-TITPO        PIC S9(7)           COMP-3.                  
007400*                                 PLANERAD ORDERDATUM                     
007500     03 TILLTPO-TIREGDAT     PIC S9(7)           COMP-3.                  
007600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007700     03 TILLTPO-TIKLOCK      PIC S9(9)           COMP-3.                  
007800*                                 KLOCKSLAG (TTMMSSTH)                    
007900*** END OF VILMAII-COPY LENGTH= 138 BYTES                                 
