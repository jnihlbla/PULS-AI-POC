000100 01  W221DEL.                                                             
000200*                                 ODETTE-SEGMENT DEL                      
000300*                                 DELIVERY DETAILS                        
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '051'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 TISTART              PIC 9(6).                                    
001000*                                 STARTTID YYMMDD   TAG 2803              
001100     03 FILLER               PIC X(4) VALUE SPACE.                        
001200*                                 STARTTID HHMM     TAG 2002              
001300     03 TISTOPP              PIC X(6) VALUE SPACE.                        
001400*                                 STOPPTID YYMMDD   TAG 2805              
001500     03 FILLER               PIC X(1) VALUE SPACE.                        
001600*                                 FILLER                                  
001700     03 KVART-AVROP          PIC S9(15).                                  
001800*                                 AVROPSANTAL       TAG 6060              
001810     03 FILLER               PIC X(17) VALUE SPACE.                       
001820*                                                   TAG 1310              
001900     03 KDDELTYP             PIC X VALUE SPACE.                           
002000*                                 TYP AV AVROP      TAG 7803              
002200     03 KDDELIND             PIC 9.                                       
002300*                                 AVROP STATUS IND. TAG 6811              
002500*** END COPY W221DEL     LENGTH=57    OLD LENGTH=36                       
