000010*** EDIT ALLOWED                                                          
000100************************************************************              
000200*                                                                         
000300*    TABELL ÖVER INTERNA/FIKTIVA LEVERANTÖRER (ELLER LIKNANDE)            
000400*    SOM INTE SKALL UT PÅ LEVERANSPLAN (POSTEN),                          
000500*    UTAN EV. PAPPERSUTSKRIFTER STYRS TILL ANSKAFFAREN                    
000600*    (INGÅR I PGM W20103, W20163 OCH W22122)                              
000600*                 W20139, W22952)                                         
000700*                                                                         
000800*    OBS VID UTÖKNING: NY VALUE-RAD I TABELL                              
000900*                      ÄNDRA OCCURS                                       
001000*                      ÄNDRA VALUE MAX-IX-LEV                             
001100************************************************************              
001200                                                                          
001300 01  TABELL-FIKTIVA-LEVERANTORER.                                         
001401     03  FILLER             PIC X(5) VALUE '1010'.                        
001401     03  FILLER             PIC X(5) VALUE '3320'.                        
001401     03  FILLER             PIC X(5) VALUE 'R5YYB'.                       
001403     03  FILLER             PIC X(5) VALUE '3641'.                        
001403     03  FILLER             PIC X(5) VALUE 'DLJFA'.                       
001404     03  FILLER             PIC X(5) VALUE '3649'.                        
001404     03  FILLER             PIC X(5) VALUE 'DLJGA'.                       
001405     03  FILLER             PIC X(5) VALUE '3786'.                        
001406     03  FILLER             PIC X(5) VALUE '3899'.                        
001406     03  FILLER             PIC X(5) VALUE 'AH1KA'.                       
001410     03  FILLER             PIC X(5) VALUE '8261'.                        
001500     03  FILLER             PIC X(5) VALUE '8265'.                        
001600     03  FILLER             PIC X(5) VALUE '9996'.                        
001610     03  FILLER             PIC X(5) VALUE '9997'.                        
001700     03  FILLER             PIC X(5) VALUE '9998'.                        
001720     03  FILLER             PIC X(5) VALUE '14985'.                       
001720     03  FILLER             PIC X(5) VALUE 'H387D'.                       
001720     03  FILLER             PIC X(5) VALUE '21327'.                       
001720     03  FILLER             PIC X(5) VALUE 'BJPMD'.                       
001720     03  FILLER             PIC X(5) VALUE '51935'.                       
001720     03  FILLER             PIC X(5) VALUE 'R4J4E'.                       
001800                                                                          
001900 01  FILLER  REDEFINES TABELL-FIKTIVA-LEVERANTORER.                       
002000     03  FIKTIV-LEV OCCURS 21 PIC X(5).                                   
002100                                                                          
002200 01  LEV-INDEX.                                                           
002300     03  IX-LEV             PIC S9(3) COMP-3  VALUE +1.                   
002400     03  MAX-IX-LEV         PIC S9(3) COMP-3  VALUE +21.                  
