000100 01  W221PDI.                                                             
000200*                                 ODETTE-SEGMENT PDI                      
000300*                                 PREV DELIVERY INSTR                     
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '017'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 IDLPLAN              PIC X(17).                                   
001000*                                 LEVRANSPLANE-ID      TAG 1004           
001300*** END COPY W221PDI     LENGTH=23    OLD LENGTH=23                       
