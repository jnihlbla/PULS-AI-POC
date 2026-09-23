000100 01  W221BDT.                                                             
000200*                                 ODETTE-SEGMENT BDT                      
000300*                                 BUYER  DETAILS                          
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '017'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 IDBUYER              PIC X(17).                                   
001000*                                 KUNDIDENTITET TAG 3303                  
001200*                                                                         
001300*** END COPY W221BDT     LENGTH=23    OLD LENGTH=23                       
