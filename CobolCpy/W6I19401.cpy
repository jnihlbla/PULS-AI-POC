000100 01  MID-W6I19401.                                                        
000200*                                 MIDCOPYTEXT TILL W60194.                
000300     03 MID-IDPRTLST         PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500     03 MID-IDPGM            PIC X(8).                                    
000600*                                 PROGRAM IDENTITET                       
000700     03 MID-KVPOST           PIC 9(7).                                    
000800*                                 RÄKNARE, ANTAL POSTER                   
000900     03 MID-FLAGG-POST       OCCURS 15 TIMES.                             
001000        05 MID-IDLOPNRM      PIC 9(8).                                    
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300        05 MID-IDARTNR       PIC 9(8).                                    
001400*                                 ARTIKELNUMMER                           
001500        05 MID-KVINLART      PIC 9(6).                                    
001600*                                 ANTAL I PARTIRAD                        
001700        05 MID-IDLEVNR-KOLLI PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER KOLLI                  
001900        05 MID-IDOKOLLI      PIC 9(9).                                    
002000*                                 ODETTE KOLLINUMMER                      
002100        05 MID-TIINLMOT      PIC 9(6).                                    
002200*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
002300        05 MID-VKKOLLIN      PIC 9(5)V9(1).                               
002400*                                 KOLLI-VIKT-NETTO                        
002500        05 MID-VKKOLLIB      PIC 9(5)V9(1).                               
002600*                                 KOLLI-VIKT-BRUTTO                       
002700        05 MID-ADLAGOMR      PIC 9(2).                                    
002800*                                 LAGEROMRÅDE                             
002900        05 MID-ADGANG        PIC 9(2).                                    
003000*                                 GÅNG                                    
003100        05 MID-ADPLATS       PIC 9(5).                                    
003200*                                 LAGERPLATSNUMMER                        
003300        05 MID-KDSORT        PIC X(2).                                    
003400*                                 SORT-KOD                                
003500        05 MID-BEFT          PIC 9(2).                                    
003600*                                 FÖRPACKNINGSTYP                         
003700*** END OF VILMAII-COPY LENGTH= 1028 BYTES                                
