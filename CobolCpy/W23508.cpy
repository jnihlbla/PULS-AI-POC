000100 01  W23508.                                                              
000200*                                 UTFIL FÖR LEVERANSBESKED                
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDANSK               PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000     03 IDLEVNR              PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 TIBORT               PIC S9(7)           COMP-3.                  
001300*                                 BORTTAGSDATUM  (ÅÅMMDD)                 
001400     03 DALEVBSK-AVS         PIC 9(8).                                    
001500*                                 LEV. BESK. AVS. DAT.(ÅÅÅÅMMDD)          
001600     03 KVAVIS-BSKKVAR       PIC S9(7)           COMP-3.                  
001700*                                 LEV. BESK. ANT. EFTER AVBOKNING         
001800     03 KDERS                PIC S9(3)           COMP-3.                  
001900*                                 ERSÄTTNINGSKOD                          
002000     03 KVLS                 PIC S9(7)           COMP-3.                  
002100*                                 LAGERSALDO                              
002200     03 KVAKS                PIC S9(7)           COMP-3.                  
002300*                                 ANKOMSTSALDO                            
002400     03 KVROS                PIC S9(7)           COMP-3.                  
002500*                                 RESTORDERSALDO                          
002600     03 AVROPS-DAT           PIC S9(7)           COMP-3.                  
002700     03 KVAVROP              PIC S9(7)           COMP-3.                  
002800*                                 AVROPSKVANTITET                         
002900     03 KDAVT                PIC S9              COMP-3.                  
003000*                                 AVTALSMÄRKNING                          
003100     03 KVART-FORAVIS        PIC S9(7)           COMP-3.                  
003200     03 TELEVBSK-COMP        PIC X(323).                                  
003300*** END OF VILMAII-COPY LENGTH= 381 BYTES                                 
