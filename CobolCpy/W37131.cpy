000100 01  W37131.                                                              
000200*                                 TRANS F÷R UPPDATERING AV                
000300*                                 ARTIKELNS SALDO OCK OBJEKTETS           
000400*                                 RETURSALDO SAMT SKAPA TRANS             
000500*                                 TILL EKONOMI                            
000600*                                 (BORTTAG AV OBJEKT P.G.A. FEL)          
000700*                                                                         
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 IDDISTR              PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 IDARTNR              PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600     03 TIAAMMDD-REG         PIC S9(7)           COMP-3.                  
001700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001800     03 KDOBJEKT             PIC S9              COMP-3.                  
001900*                                 OBJEKTSKOD                              
002000     03 IDORDNR-KEY          PIC S9(5)           COMP-3.                  
002100*                                 ORDERNUMMER                             
002200     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
002300*                                 OBJEKTNUMMER                            
002400     03 KVRETUR              PIC S9(5)           COMP-3.                  
002500*                                 ANTAL I RETUR                           
002600     03 IDDC                 PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
