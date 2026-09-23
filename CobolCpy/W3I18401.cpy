000100 01  MID-W3I18401.                                                        
000200*                                 MID-COPYTEXT FÖR W3018400               
000300     03 MID-IDFAKT-IN        PIC X(7).                                    
000400*                                 FAKTURANUMMER                           
000500     03 MID-IDKOLLI-IN       PIC X(5).                                    
000600*                                 KOLLINUMMER                             
000700     03 MID-FLINLI-IN        PIC X.                                       
000800*                                 INLAGD RAD, PARTI ELLER KOLLI           
000900     03 MID-IDDC-REC-IN      PIC X(2).                                    
001000*                                 MOTTAGANDE LAGER                        
001100     03 MID-KDPRT            PIC X(3).                                    
001200*                                 PRINTERKOD                              
001300     03 MID-INPUT            OCCURS 11 TIMES.                             
001400        05 MID-KDCMD         PIC X.                                       
001500*                                 RAD-UPPDATERINGSKOMMANDO                
001600*                                  BLANK  = INGENTING                     
001700*                                  D , B  = DELETE                        
001800*                                  R , Ä  = REPLACE                       
001900*                                  I,N,A  = INSERT                        
002000*                                  S , V  = SELECT                        
002100*                                  P , P  = PRINT                         
002200*                                  C , K  = COPY                          
002300        05 MID-IDKOLLI       PIC Z(4)9.                                   
002400*                                 KOLLINUMMER                             
002500        05 MID-IDARTNR-OBJ   PIC Z(7)9.                                   
002600*                                 OBJEKTNUMMER                            
002700        05 MID-KVANTMOT      PIC X(7).                                    
002800*                                 ANTAL MOTTAGET                          
002900     03 MID-IDKOLLI-NY       PIC X(5).                                    
003000*                                 KOLLINUMMER                             
003100     03 MID-IDARTNR-OBJ-NY   PIC X(8).                                    
003200*                                 OBJEKTNUMMER                            
003300     03 MID-KVANTMOT-NY      PIC X(7).                                    
003400*                                 ANTAL MOTTAGET                          
003500     03 MID-IDKOLLI-KLAR     PIC X(5).                                    
003600*                                 KOLLINUMMER                             
003700*** END OF VILMAII-COPY LENGTH= 274 BYTES                                 
