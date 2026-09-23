000100 01  REQU-WL0111I1.                                                       
000200*                                 REQUEST TO PGM WL0111                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-KVRADER         PIC 9(5).                                    
000600*                                 ANTAL RADER                             
000700     03 REQU-FLAGG-POST      OCCURS 15 TIMES.                             
000800        05 REQU-IDLOPNRM     PIC 9(8).                                    
000900*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
001000*                                 (0VVDLLLLK)                             
001100        05 REQU-IDARTNR      PIC 9(8).                                    
001200*                                 ARTIKELNUMMER                           
001300        05 REQU-KVINLART-LINE                                             
001400                             PIC 9(6).                                    
001500*                                 ANTAL I PARTIRAD                        
001600        05 REQU-TIINLMOT-LINE                                             
001700                             PIC 9(6).                                    
001800*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
001900        05 REQU-VKKOLLIN     PIC 9(5)V9(1).                               
002000*                                 KOLLI-VIKT-NETTO                        
002100        05 REQU-ADLAGOMR     PIC 9(2).                                    
002200*                                 LAGEROMR≈DE                             
002300        05 REQU-ADGANG       PIC 9(2).                                    
002400*                                 G≈NG                                    
002500        05 REQU-ADPLATS      PIC 9(5).                                    
002600*                                 LAGERPLATSNUMMER                        
002700        05 REQU-KDSORT       PIC X(2).                                    
002800*                                 SORT-KOD                                
002900        05 REQU-IDLEVNR-KOLLI                                             
003000                             PIC X(5).                                    
003100*                                 LEVERANT÷RNUMMER KOLLI                  
003200        05 REQU-IDOKOLLI     PIC 9(9).                                    
003300*                                 ODETTE KOLLINUMMER                      
003400*** END OF VILMAII-COPY LENGTH= 892 BYTES                                 
