000100 01  REGTPO-W414006.                                                      
000200*                                 SKAPAS VID REGISTRERING                 
000300*                                 AV TPO:ER.                              
000400*                                 ANVÄNDS VID TRANSAKTION-                
000500*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000600     03 REGTPO-IDPTYP        PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 REGTPO-IDARTNR       PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 REGTPO-BERADREF      PIC X(10).                                   
001100*                                 KUNDENS RADREFERENS                     
001200     03 REGTPO-IDDISTR       PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 REGTPO-IDKONTO       PIC S9(11)          COMP-3.                  
001500*                                 KONTO                                   
001600     03 REGTPO-IDKST         PIC X(10).                                   
001700*                                 KOSTNADSSTÄLLE                          
001800     03 REGTPO-IDKUNDNR      PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000     03 REGTPO-IDKUNDRF      PIC X(10).                                   
002100*                                 KUNDENS REFERENS (ORDERID)              
002200     03 REGTPO-IDSYSTEM      PIC X(4).                                    
002300*                                 VOLVO VCCS SYSTEMNUMMER                 
002400     03 REGTPO-KDFRAKT       PIC S9(3)           COMP-3.                  
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 REGTPO-KDKVBRYT      PIC S9              COMP-3.                  
002700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002800     03 REGTPO-KDORDKL       PIC S9              COMP-3.                  
002900*                                 ORDERKLASS                              
003000     03 REGTPO-KDTPOTYP      PIC S9              COMP-3.                  
003100*                                 TYP AV TIDPLANERAD ORDER                
003200     03 REGTPO-KDUART        PIC X.                                       
003300*                                 UNDANTAGSARTIKEL                        
003400     03 REGTPO-KDVRINFO      PIC S9              COMP-3.                  
003500*                                 PÅVERKAN I VR/DSP SYSTEM                
003600     03 REGTPO-KVBEART-Q     PIC S9(7)           COMP-3.                  
003700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003800     03 REGTPO-PRARTNTO      PIC S9(7)V9(2)      COMP-3.                  
003900*                                 ARTIKELPRIS NETTO                       
004000     03 REGTPO-REKSIFFR      PIC S9              COMP-3.                  
004100*                                 KONTROLLSIFFRA                          
004200     03 REGTPO-TIREGDAT-TPO  PIC S9(7)           COMP-3.                  
004300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004400     03 REGTPO-TITPO         PIC S9(7)           COMP-3.                  
004500*                                 PLANERAD ORDERDATUM                     
004600     03 REGTPO-TIREGDAT      PIC S9(7)           COMP-3.                  
004700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004800     03 REGTPO-TIKLOCK       PIC S9(9)           COMP-3.                  
004900*                                 KLOCKSLAG (TTMMSSTH)                    
005000*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
