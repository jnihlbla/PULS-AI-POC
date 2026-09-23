000100 01  MID-W4I79201.                                                        
000200*                                 MIDCOPYTEXT TILL W40792.                
000300     03 MID-KVPOST           PIC 9(7).                                    
000400*                                 RÄKNARE, ANTAL POSTER                   
000500*                                 RECORD COUNTER                          
000600     03 MID-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 MID-R31-POST         OCCURS 24 TIMES.                             
001000        05 MID-TIREGDAT      PIC 9(6).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200*                                 REGISTRATION DATE (YYMMDD)              
001300        05 MID-TIKLOCK       PIC 9(8).                                    
001400*                                 KLOCKSLAG (TTMMSSTH)                    
001500*                                 TIME OF DAY (HHMMSSTH)                  
001600*** END OF VILMAII-COPY LENGTH= 345 BYTES                                 
