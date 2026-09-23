000100 01  W61216-CTX.                                                          
000200*                                 INLEVERASER TILL KINA-NDC               
000300*                                 INOM AKTUELL PERIOD                     
000400     03 TIAAPP               PIC S9(5)           COMP-3.                  
000500*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
000600*                                 12 PER ≈R                               
000700*                                 NUMERA ƒR DETTA "PV-PERIOD"             
000800     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
000900*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDLEVNR              PIC X(5).                                    
001300*                                 LEVERANT÷RNUMMER                        
001400     03 IDARTNR              PIC 9(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 IDLEVNR-DC           PIC X(5).                                    
001700*                                 DC LEVERANT÷R                           
001800     03 IDPTYP               PIC X(3).                                    
001900*                                 POSTTYP                                 
002000     03 KVAVIS               PIC S9(7)           COMP-3.                  
002100*                                 AVISERAT ANTAL                          
002200     03 KVANTMOT             PIC S9(7)           COMP-3.                  
002300*                                 ANTAL MOTTAGET                          
002400     03 TIINLMOT             PIC S9(7)           COMP-3.                  
002500*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
002600     03 TIINLINL             PIC S9(7)           COMP-3.                  
002700*                                 RAPPORTERINGSDATUM INLAGD (R32)         
002800     03 KDRT                 PIC 9(2).                                    
002900*                                 REDOVISNINGSTYP                         
003000*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
