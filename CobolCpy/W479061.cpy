000100 01  W479061.                                                             
000200*                                 FAKTURERADE OCH LASTADE                 
000300*                                 KOLLIN                                  
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 IDKUNDRF             PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001500*                                 PRODUKTIONSNUMMER                       
001600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 KDORDKL              PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002100*                                 FRAKTSƒTT C1-C2 TILL KUND               
002200     03 IDTRPTNR             PIC S9(3)           COMP-3.                  
002300*                                 TRANSPORTIDENTITET                      
002400     03 KDORDLOT             PIC X(2).                                    
002500*                                 ORDERLOTTSALTERNATIV                    
002600     03 IDLOTNR              PIC S9(3)           COMP-3.                  
002700*                                 VAGN-NUMMER                             
002800     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
002900*                                 BEGƒRD PACKNINGSDAG    (≈≈MMDD)         
003000     03 TIORDREG             PIC S9(7)           COMP-3.                  
003100*                                 ORDERREGISTRERINGSDATUM  ≈≈MMDD         
003200     03 TIREGTID             PIC S9(7)           COMP-3.                  
003300*                                 REGISTRERINGSTID                        
003400     03 TIUTSKR              PIC S9(7)           COMP-3.                  
003500*                                 UTSKRIFTDATUM  (≈≈MMDD)                 
003600     03 TIUTSTID             PIC S9(7)           COMP-3.                  
003700*                                 UTSKRIFTSTID (TTMMSS)                   
003800     03 TIPACKN              PIC S9(7)           COMP-3.                  
003900*                                 PACKNINGSDATUM         (≈≈MMDD)         
004000     03 TIPACTID             PIC S9(7)           COMP-3.                  
004100*                                 PACKNINGSTID  TTMMSS                    
004200     03 TIFAKT               PIC S9(7)           COMP-3.                  
004300*                                 FAKTURERINGSDATUM (≈≈MMDD)              
004400     03 TIFAKTID             PIC S9(7)           COMP-3.                  
004500*                                 FAKTURERINGSTID                         
004600     03 TILASTN              PIC S9(7)           COMP-3.                  
004700*                                 LASTNINGSDATUM         (≈≈MMDD)         
004800     03 TILASTID             PIC S9(7)           COMP-3.                  
004900*                                 LASTNINGSTID                            
005000     03 FLDIRLEV             PIC X.                                       
005100*                                 DIREKTLEVERANS ?                        
005200*** END COPY W479061     LENGTH=83                                        
