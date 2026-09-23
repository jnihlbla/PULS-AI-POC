000100 01  W221001.                                                             
000200*                                 FILE HEADER RECORD                      
000300*                                 AMOS                                    
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '073'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 IDSECN               PIC X(4).                                    
001000*                                 SÄNDANDE KOMM.NODE                      
001300     03 IDRECN               PIC X(4).                                    
001400*                                 MOTTAGANDE KOMM.NODE                    
001410     03 IDFILE               PIC X(8).                                    
001420*                                 VIRT. FILNAMN                           
001430     03 TIFILE-DAT           PIC 9(6).                                    
001440*                                 DATUM FÖR ÖVERF. YYMMDD                 
001450     03 TIFILE-KL            PIC 9(6).                                    
001460*                                 TID FÖR ÖVERF.   TTHHSS                 
001470     03 FILLER               PIC X(45) VALUE SPACE.                       
001480*                                 FILLER                                  
001500*** END COPY W221001     LENGTH=79    OLD LENGTH=79                       
