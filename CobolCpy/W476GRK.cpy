000100 01  GRK-W476GRK.                                                         
000200*                                 RAD F÷R FAKTURATRANSAR FR≈N BIL         
000300*                                 L-IT / GREKLAND                         
000400     03 GRK-IDDC             PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 GRK-IDDISTR          PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 GRK-IDFAKT           PIC S9(7)           COMP-3.                  
000900*                                 FAKTURANUMMER                           
001000     03 GRK-DAFINDOC         PIC 9(8).                                    
001100*                                 DOKUMENT DATUM (≈≈≈≈MMDD)               
001200     03 GRK-IDSHIPM          PIC 9(7).                                    
001300*                                 SKEPPNINGSNUMMER                        
001400     03 GRK-TISKEPPN         PIC S9(7)           COMP-3.                  
001500*                                 SKEPPNINGSDATUM  (≈≈MMDD)               
001600     03 GRK-IDKUNDNR         PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800     03 GRK-IDORDNR7         PIC S9(7)           COMP-3.                  
001900*                                 ORDERNUMMER                             
002000     03 GRK-IDKOLLI          PIC S9(5)           COMP-3.                  
002100*                                 KOLLINUMMER                             
002200     03 GRK-KDORDKL          PIC S9              COMP-3.                  
002300*                                 ORDERKLASS                              
002400     03 GRK-KDFRAKT          PIC S9(3)           COMP-3.                  
002500*                                 FRAKTSƒTT DC TILL KUND                  
002600     03 GRK-KDPRODSL         PIC S9(3)           COMP-3.                  
002700*                                 PRODUKTSLAG                             
002800     03 GRK-IDARTNR          PIC S9(9)           COMP-3.                  
002900*                                 ARTIKELNUMMER                           
003000     03 GRK-IDARTNR-TKN      PIC X(9).                                    
003100     03 GRK-KVLEVART         PIC S9(7)           COMP-3.                  
003200*                                 LEVERERAT ANTAL STYCK                   
003300     03 GRK-KDARTURS         PIC X(2).                                    
003400*                                 ARTIKELURSPRUNGSKOD                     
003500     03 GRK-IDSTATNR         PIC S9(9)           COMP-3.                  
003600*                                 STATISTISKT NUMMER                      
003700*                                 1 = NORSKT                              
003800*                                 2 = ENGELSKT                            
003900*                                 3 = BELGISKT                            
004000*                                 4 = PERUANSKT                           
004100*                                 5 = SVENSKT                             
004200*                                 6 =                                     
004300     03 GRK-VKARTNTO         PIC S9(4)V9(3)      COMP-3.                  
004400*                                 ARTIKELVIKT NETTO (KG)                  
004500     03 GRK-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
004600*                                 ARTIKELPRIS NETTO                       
004700     03 GRK-BEKUNDRF         PIC X(15).                                   
004800*                                 KUNDENS REFERENS                        
004900     03 GRK-IDVIN            PIC X(17).                                   
005000*                                 VIN ID FORDON                           
005100     03 GRK-IDVAT            PIC X(17).                                   
005200*                                 MOMSREGISTRERINGSNUMMER                 
005300*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
