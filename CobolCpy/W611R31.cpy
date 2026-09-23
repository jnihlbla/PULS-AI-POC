000100 01  W611R31.                                                             
000200*                                 POSTTYP R31                             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 KDSORT2              PIC S9(3)           COMP-3.                  
000700*                                 SORTERINGSFÄLT                          
000800*                                 FIELD FOR SORTING PURPOSE               
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 IDPLFORM             PIC S9(3)           COMP-3.                  
001300*                                 PLATTFORMSNUMMER                        
001400*                                 PLATFORM NUMBER                         
001500     03 IDLEVNR-INL          PIC S9(5)           COMP-3.                  
001600*                                 LEVERANTÖR FÖR AKTUELL INLEV.           
001700     03 KDRT                 PIC S9(3)           COMP-3.                  
001800*                                 REDOVISNINGSTYP                         
001900*                                 TYPE OF ACCOUNTING                      
002000     03 TIAVSDAT             PIC S9(7)           COMP-3.                  
002100*                                 AVISERINGSDATUM (YYMMDD)                
002200*                                 ADVICE NOTE DATE                        
002300     03 IDKONTO              PIC S9(11)          COMP-3.                  
002400*                                 KONTO                                   
002500*                                 ACCOUNT                                 
002600     03 IDAVINR              PIC S9(7)           COMP-3.                  
002700*                                 AVI-NUMMER                              
002800*                                 ADVICE NOTE NUMBER                      
002900     03 KVAVIS               PIC S9(7)           COMP-3.                  
003000*                                 AVISERAT ANTAL                          
003100*                                 QUANTITY NOTIFIED                       
003200     03 IDORDNR              PIC S9(5)           COMP-3.                  
003300*                                 ORDERNUMMER                             
003400*                                 ORDER NUMBER                            
003500     03 IDRADNR              PIC S9(5)           COMP-3.                  
003600*                                 RADNUMMER                               
003700*                                 LINE NO                                 
003800*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
