000100 01  LEV-W6F101.                                                          
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 KVALITET                                
000400*                                 FYSISK NYCKEL: IDLEVNR                  
000500     03 LEV-IDLEVNR          PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000800     03 LEV-FLSKPLOT         PIC X.                                       
000900*                                 SKIPLOT KONTROLL ?                      
001000*                                 SKIPLOT INSPECTION  ?                   
001100     03 LEV-IDMAIL           OCCURS 3 TIMES                               
001200                             PIC X(60).                                   
001300*                                 MAIL ADRESS                             
001400*                                 MAIL ADDRESS                            
001500     03 LEV-IDLEVFAX         OCCURS 4 TIMES                               
001600                             PIC X(16).                                   
001700*                                 TELEFAXNUMMER TILL LEVERANTÖR           
001800*                                 TELEFAX NUMBER TO THE SUPPLIER          
001900     03 LEV-FILLER           PIC X(12).                                   
002000*** END OF VILMAII-COPY LENGTH= 262 BYTES                                 
