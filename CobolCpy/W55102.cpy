000100 01  W55102.                                                              
000200*                                 W55102-FILCOPYTEXT                      
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
001100*                                 INKÖPSPRIS                              
001200     03 PRDIRLON-BD          PIC S9(4)V9(3)      COMP-3.                  
001300*                                 DIREKT LÖN                              
001400     03 PRDMTRL-BD           PIC S9(6)V9(3)      COMP-3.                  
001500*                                 DIREKT MATERIAL                         
001600     03 PROVRPAL-BD          PIC S9(4)V9(3)      COMP-3.                  
001700*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
001800     03 PRARTBEL             PIC S9(8)V9(5)      COMP-3.                  
001900*                                 BESTPRIS LEVERANTÖRENS VALUTA           
002000     03 KDVTH                PIC S9              COMP-3.                  
002100*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
002200     03 FILLER               PIC X(4).                                    
002300     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
002400*                                 TULLFAKTOR                              
002500     03 KDVALISO             PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
002800*                                 VALUTAKURS                              
002900     03 IDINK                PIC X(4).                                    
003000*                                 INKÖPARNUMMER                           
003100*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
