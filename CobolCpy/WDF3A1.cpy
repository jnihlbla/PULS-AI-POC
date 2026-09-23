000100 01  HLGA-WDF3A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDF301             
000300*                                 FYSISK NYCKEL: WDF3A1KY                 
000400*                                 (IDLANDX2, DADATUM-HELG)                
000500*                                 SECONDARY KEY: WDF3ASEQ                 
000600*                                 (IDLANDX2, DADATUM-HELG)                
000700     03 HLGA-IDLANDX2        PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 2-LETTER CODE FOR COUNTRY               
001000     03 HLGA-DADATUM-HELG    PIC 9(8).                                    
001100*                                 HELGDAGAR (ÅÅÅÅMMDD)                    
001200*                                 HOLIDAYS (YYYYMMDD)                     
001300     03 HLGA-FLHELG          PIC X.                                       
001400*                                 NATIONELL HELGDAG = J                   
001500*                                 PUBLIC HOLIDAY = J                      
001600*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
