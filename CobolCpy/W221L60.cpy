000010*** EDIT ALLOWED                                                          
000100 01      W222L141.                                                        
000200******************************************************************        
000300*** LÄNKAREA FÖR IMS-CALL FÖR PGM W22214 MOT HÄNDELSEREGISTER    *        
000400******************************************************************        
000500     03  KDCALL                  PIC S9(3)           COMP-3.              
000600         88  LAES-ROT                VALUE +141.                          
000700         88  LAES-DELETE-DATA-02     VALUE +142.                          
000800         88  LAES-DELETE-DATA-03     VALUE +143.                          
000900*                                                                         
001000     03  FLAGGA-ANROP            PIC X(1).                                
001100         88  POST-FINNS              VALUE 'J'.                           
001200         88  POST-SAKNAS             VALUE 'N'.                           
001300*                                                                         
001400     03  IDHTYP                  PIC X(4).                                
001500*                        *** HÄNDELSEKOD           ***                    
001600     03  IO-AREA                 PIC X(24).                               
001700*                        *** SEGMENTAREA           ***                    
001800*** END COPY W222L141C0  LENGTH=31    OLD LENGTH=31                       
