000100 01  W221PDN.                                                             
000200*                                 ODETTE-SEGMENT PDN                      
000300*                                 PREV DESPATCH NOTES                     
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '036'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 IDNRINL              PIC X(8).                                    
001000*                                 ID-NUMMER-INLEVERANS   TAG 1128         
001200     03 TISENINL             PIC 9(6).                                    
001201*                                 DATUM SENASTE INLEV    TAG 2219         
001210     03 TISENINL-ALPHA       REDEFINES TISENINL                           
001220                             PIC X(6).                                    
001310     03 FILLER               PIC X(1) VALUE SPACE.                        
001320*                                 FILLER                                  
001400     03 KVSENINL             PIC S9(10).                                  
001401*                                 KVANTITET              TAG 6270         
001410     03 KVSENINL-ALPHA       REDEFINES KVSENINL                           
001420                             PIC X(10).                                   
001510     03 FILLER               PIC X(11) VALUE SPACE.                       
001520*                                 EJ UTNYTTJAT FÄLT     (TAG 6872)        
001600*** END COPY W221PDN     LENGTH=42    OLD LENGTH=42                       
