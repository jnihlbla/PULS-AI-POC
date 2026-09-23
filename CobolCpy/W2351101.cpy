000100 01  W2351101.                                                            
000200*                                 UTFIL FÖR LEVERANSBESKED                
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDANSK               PIC S9(3)           COMP-3.                  
000700*                                 ANSKAFFARNUMMER                         
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 DALEVBSK-AVS         PIC 9(8).                                    
001100*                                 LEV. BESK. AVS. DAT.(ÅÅÅÅMMDD)          
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDLEVNR              PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 IDLEVNR-SHIP         PIC X(5).                                    
001700*                                 SKEPPANDE LEVERANTÖR                    
001800     03 TIBORT               PIC S9(7)           COMP-3.                  
001900*                                 BORTTAGSDATUM  (ÅÅMMDD)                 
002000     03 KVAVIS-BSKKVAR       PIC S9(7)           COMP-3.                  
002100*                                 LEV. BESK. ANT. EFTER AVBOKNING         
002200     03 KDERS                PIC S9(3)           COMP-3.                  
002300*                                 ERSÄTTNINGSKOD                          
002400     03 KVLS                 PIC S9(7)           COMP-3.                  
002500*                                 LAGERSALDO                              
002600     03 KVAKS                PIC S9(7)           COMP-3.                  
002700*                                 ANKOMSTSALDO                            
002800     03 KVROS                PIC S9(7)           COMP-3.                  
002900*                                 RESTORDERSALDO                          
003000     03 AVROPS-DAT           PIC S9(7)           COMP-3.                  
003100     03 KVAVROP              PIC S9(7)           COMP-3.                  
003200*                                 AVROPSKVANTITET                         
003300     03 KDAVT                PIC S9              COMP-3.                  
003400*                                 AVTALSMÄRKNING                          
003500     03 TIERSDAT-VIPS        PIC S9(5)           COMP-3.                  
003600*                                 DATUM NÄR ERS. INFO TILL VIPS           
003700     03 KVART-FORAVIS        PIC S9(7)           COMP-3.                  
003800     03 TELEVBSK-COMP        PIC X(323).                                  
003900*** END OF VILMAII-COPY LENGTH= 391 BYTES                                 
