000100 01  203-W414203A.                                                        
000200*                                 203                                     
000300*                                 SKAPAS FÖR TILLÄGG TPO:ER.              
000400*                                 ANVÄNDS VID TRANSAKTION-                
000500*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000600     03 203-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 203-BERADREF         PIC X(10).                                   
000900*                                 KUNDENS RADREFERENS                     
001000     03 203-BEVARREF         PIC X(10).                                   
001100*                                 VÅR REFERENS                            
001200     03 203-BEVOLREF         PIC X(10).                                   
001300*                                 VOLVO REFERENS                          
001400     03 203-FLINVEST         PIC X.                                       
001500*                                 BYTES INVENTERINGSFLAGGA                
001600     03 203-FLPRTILL         PIC X.                                       
001700*                                 PRISTILLÄGGS FLAGGA                     
001800     03 203-FLTILLK          PIC X.                                       
001900*                                 TILLKOMMANDE ARTIKEL ?                  
002000     03 203-IDDISTR          PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200     03 203-IDKONTO          PIC S9(11)          COMP-3.                  
002300*                                 KONTO                                   
002400     03 203-IDKST            PIC X(10).                                   
002500*                                 KOSTNADSSTÄLLE                          
002600     03 203-IDKUNDNR         PIC S9(7)           COMP-3.                  
002700*                                 KUNDNUMMER                              
002800     03 203-IDKUNDRF         PIC X(10).                                   
002900*                                 KUNDENS REFERENS (ORDERID)              
003000     03 203-IDSYSTEM         PIC X(4).                                    
003100*                                 VOLVO VCCS SYSTEMNUMMER                 
003200     03 203-KDDSP            PIC S9              COMP-3.                  
003300*                                 PÅVERKAN PÅ DSP                         
003400     03 203-KDFAKTYP         PIC X.                                       
003500*                                 FAKTURATYP                              
003600     03 203-KDFRAKT          PIC S9(3)           COMP-3.                  
003700*                                 FRAKTSÄTT DC TILL KUND                  
003800     03 203-KDKVBRYT         PIC S9              COMP-3.                  
003900*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004000     03 203-KDORDBEK         PIC 9(2).                                    
004100*                                 ORDERBEKRÄFTELSEKOD                     
004200     03 203-KDORDING         PIC S9              COMP-3.                  
004300*                                 UPPDATERING ORDERINGÅNG                 
004400     03 203-KDORDKL          PIC S9              COMP-3.                  
004500*                                 ORDERKLASS                              
004600     03 203-KDPRTYP          PIC X.                                       
004700*                                 TYP AV PRISTILLÄMPNING                  
004800     03 203-KDPRODSL         PIC S9(3)           COMP-3.                  
004900*                                 PRODUKTSLAG                             
005000     03 203-KDTPOTYP         PIC S9              COMP-3.                  
005100*                                 TYP AV TIDPLANERAD ORDER                
005200     03 203-KDVRINFO         PIC S9              COMP-3.                  
005300*                                 PÅVERKAN I VR/DSP SYSTEM                
005400     03 203-KVBEART          PIC S9(7)           COMP-3.                  
005500*                                 BESTÄLLT ANTAL STYCKEN                  
005600     03 203-KVBEART-Q        PIC S9(7)           COMP-3.                  
005700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
005800     03 203-KVQPACK-1        PIC S9(5)           COMP-3.                  
005900*                                 ANTAL I Q1 FÖRPACKNING                  
006000     03 203-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
006100*                                 ARTIKELPRIS NETTO                       
006200     03 203-REKSIFFR         PIC S9              COMP-3.                  
006300*                                 KONTROLLSIFFRA                          
006400     03 203-TIDISPIN         PIC S9(7)           COMP-3.                  
006500*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
006600     03 203-TIREGDAT         PIC S9(7)           COMP-3.                  
006700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006800     03 203-TITPO            PIC S9(7)           COMP-3.                  
006900*                                 PLANERAD ORDERDATUM                     
007000*** END OF VILMAII-COPY LENGTH= 118 BYTES                                 
