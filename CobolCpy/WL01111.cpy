000100 01  WL01111.                                                             
000200*                                 COPYTEXT TILL CASE LABEL INBOUN         
000300*                                 D LDC                                   
000400     03 REP-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 REP-IDARTNR          PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800     03 REP-KVINLART         PIC Z(5)9.                                   
000900*                                 ANTAL I PARTIRAD                        
001000     03 REP-KDSORT           PIC X(2).                                    
001100*                                 SORT-KOD                                
001200     03 REP-TIINLMOT         PIC 9(6).                                    
001300*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
001400     03 REP-VKKOLLIN         PIC Z(4)9.9.                                 
001500*                                 KOLLI-VIKT-NETTO                        
001600     03 REP-IDLOPNRM         PIC Z(7)9.                                   
001700*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900     03 REP-ADLAGOMR         PIC 9(2).                                    
002000*                                 LAGEROMR≈DE                             
002100     03 REP-ADGANG           PIC 9(2).                                    
002200*                                 G≈NG                                    
002300     03 REP-ADPLATS          PIC 9(5).                                    
002400*                                 LAGERPLATSNUMMER                        
002500     03 REP-IDLEVNR-KOLLI    PIC X(5).                                    
002600*                                 LEVERANT÷RNUMMER KOLLI                  
002700     03 REP-IDOKOLLI         PIC Z(8)9.                                   
002800*                                 ODETTE KOLLINUMMER                      
002900*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
