000100 01  W221ARI.                                                             
000200*                                 ODETTE-SEGMENT ARI                      
000300*                                 ADDITIONAL RELEASE                      
000400*                                 INFO                                    
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '013'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 KDRLSTID             PIC 9.                                       
001000*                                 RELEASE TYPE CODE    TAG 7903           
001200     03 TIYYMMDD-LPFROM      PIC 9(6).                                    
001210*                                 GÄLLANDEDAT LEV.PLAN TAG 2069           
001220     03 FILLER               PIC X(6) VALUE SPACE.                        
001230*                                                      TAG 2073           
001300*                                                                         
001400*** END COPY W221ARICC0  LENGTH=19    OLD LENGTH=13                       
