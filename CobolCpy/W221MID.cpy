000100 01  W221MID.                                                             
000200*                                 ODETTE-SEGMENT MID                      
000300*                                 MESSAGE IDENT.                          
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDPTYP-LTH           PIC X(3) VALUE '023'.                        
000810*                                 LÄNGD PÅ FÄLT                           
000900     03 IDLPLAN              PIC X(17).                                   
001000*                                 LEVRANSPLANE-ID      TAG 1004           
001100     03 TILPLAN              PIC 9(6).                                    
001200*                                 LEVERANSPLANDATUM    TAG 2007           
001300*** END COPY W221MID     LENGTH=29    OLD LENGTH=29                       
