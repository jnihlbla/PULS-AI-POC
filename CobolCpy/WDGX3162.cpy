000100 01  3162-WDGX3162.                                                       
000200*                                 BYTES RENOVATOR CONFIRMATION            
000300*                                 FYSISK NYCKEL: (DAORDREG +              
000400*                                 IDORDER + IDARTNR + IDRADNR)            
000500     03 3162-DAORDREG        PIC 9(8).                                    
000600*                                 ORDERDATUM (≈≈≈≈MMDD)                   
000700*                                 ORDER DATE (YYYYMMDD)                   
000800     03 3162-IDORDER         PIC S9(7)           COMP-3.                  
000900*                                 VOLVO PARTS ORDERNUMMER                 
001000*                                 VOLVO PARTS ORDER NUMBER                
001100     03 3162-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 3162-IDRADNR         PIC S9(5)           COMP-3.                  
001500*                                 RADNUMMER                               
001600*                                 LINE NO                                 
001700     03 3162-DAREGDAT        PIC 9(8).                                    
001800*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001900*                                 REGISTRATION DATE (YYYYMMDD)            
002000     03 3162-IDUSER          PIC X(8).                                    
002100*                                 ANVƒNDARENS SƒKERHETS ID                
002200*                                 USER SECURITY-IDENTITY                  
002300     03 3162-KVAVIS          PIC S9(7)           COMP-3.                  
002400*                                 AVISERAT ANTAL                          
002500*                                 QUANTITY NOTIFIED                       
002600     03 3162-KVAVBART        PIC S9(7)           COMP-3.                  
002700*                                 AVBOKAT ANTAL ARTIKLAR                  
002800*                                 ALLOCATED QUANTITY                      
002900     03 3162-FILLER          PIC X(4).                                    
003000     03 3162-TIREGTID        PIC S9(7)           COMP-3.                  
003100*                                 REGISTRERINGSTID                        
003200*                                 GENERAL REGISTRATION TIME               
003300*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
