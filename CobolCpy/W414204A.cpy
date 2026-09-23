000100 01  204-W414204A.                                                        
000200*                                 204                                     
000300*                                 SKAPAS VID REGISTRERING                 
000400*                                 AV TPO:ER.                              
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 204-IDARTNR          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 204-BERADREF         PIC X(10).                                   
001000*                                 KUNDENS RADREFERENS                     
001100     03 204-IDDISTR          PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 204-IDKONTO          PIC S9(11)          COMP-3.                  
001400*                                 KONTO                                   
001500     03 204-IDKST            PIC X(10).                                   
001600*                                 KOSTNADSSTÄLLE                          
001700     03 204-IDKUNDNR         PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900     03 204-IDKUNDRF         PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100     03 204-IDSYSTEM         PIC X(4).                                    
002200*                                 VOLVO VCCS SYSTEMNUMMER                 
002300     03 204-KDFRAKT          PIC S9(3)           COMP-3.                  
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500     03 204-KDKVBRYT         PIC S9              COMP-3.                  
002600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002700     03 204-KDORDKL          PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900     03 204-KDTPOTYP         PIC S9              COMP-3.                  
003000*                                 TYP AV TIDPLANERAD ORDER                
003100     03 204-KDUART           PIC X.                                       
003200*                                 UNDANTAGSARTIKEL                        
003300     03 204-KDVRINFO         PIC S9              COMP-3.                  
003400*                                 PÅVERKAN I VR/DSP SYSTEM                
003500     03 204-KVBEART-Q        PIC S9(7)           COMP-3.                  
003600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003700     03 204-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
003800*                                 ARTIKELPRIS NETTO                       
003900     03 204-REKSIFFR         PIC S9              COMP-3.                  
004000*                                 KONTROLLSIFFRA                          
004100     03 204-TIREGDAT         PIC S9(7)           COMP-3.                  
004200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004300     03 204-TITPO            PIC S9(7)           COMP-3.                  
004400*                                 PLANERAD ORDERDATUM                     
004500*** END OF VILMAII-COPY LENGTH= 77 BYTES                                  
