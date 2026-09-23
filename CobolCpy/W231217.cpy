000100 01  W231217.                                                             
000200*                                 RO-DATA PER ARTIKEL                     
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 CLAGERDEL            OCCURS 2 TIMES.                              
000800        05 TIRODAT           PIC S9(5)           COMP-3.                  
000900*                                 RESTORDERDATUM      TIRODAT-002         
001000*                                 (AAVVD)                                 
001100        05 KVRORAD-0-4       PIC S9(7)           COMP-3.                  
001200*                                 ANTAL RO-RADER 0 - 4 VECKOR             
001300        05 KVRORAD-5-8       PIC S9(7)           COMP-3.                  
001400*                                 ANTAL RO-RADER 5 - 8 VECKOR             
001500        05 KVRORAD-9         PIC S9(7)           COMP-3.                  
001600*                                 ANTAL RO-RADER 9 VECKOR ELLER           
001700*                                 ÄLDRE                                   
001800*** END COPY W231217CC0  LENGTH=38                                        
