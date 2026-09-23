000010*** EDIT ALLOWED                                                          
000100*     SENASTE UPPDATERING         75036       8.23.13.7                   
000200*                            *** POSTLÄNGD 80               ***           
000300 01  M117.                                                                
000400*    ***  R31  FÖRPP.TYP 10-69,   OMKOSTNADER = 0            ***          
000500*    ***          FÖRP.TYP EJ 10-69,OMKOSTNADER > 0            ***        
000600     03  FILLER          PIC X(12)   VALUE '   FÖRP.TYP '.                
000700     03  BEFT            PIC 99      DISPLAY.                             
000800     03  FILLER          PIC X(5)    VALUE '  LÖN'.                       
000900     03  PRDIRLON        PIC Z(3)9V999   DISPLAY.                         
001000     03  FILLER          PIC X(6)    VALUE '  MTRL'.                      
001100     03  PRDMTRL         PIC Z(5)9V999   DISPLAY.                         
001200     03  FILLER          PIC X(5)    VALUE '  ÖVR'.                       
001300     03  PROVRPAL        PIC Z(3)9V999   DISPLAY.                         
001400     03  FILLER          PIC X(6)    VALUE '  VTH '.                      
001500     03  KDVTH           PIC 9       DISPLAY.                             
001600     03  FILLER          PIC X(20)   VALUE SPACE.                         
001700*** END COPY W211M117   LENGTH=0                                          
