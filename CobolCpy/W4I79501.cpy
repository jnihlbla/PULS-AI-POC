000100 01  MID-W4I79501.                                                        
000200*                                 MIDCOPYTEXT TILL W40795.                
000300     03 MID-IDPRTLST         PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500*                                 LOGICAL PRINTER+LIST IDENTITY           
000600     03 MID-IDPGM            PIC X(8).                                    
000700*                                 PROGRAM IDENTITET                       
000800*                                 PROGRAM INTENTITY                       
000900     03 MID-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MID-KVPOST           PIC 9(7).                                    
001300*                                 RÄKNARE, ANTAL POSTER                   
001400*                                 RECORD COUNTER                          
001500     03 MID-RT-POST          OCCURS 14 TIMES.                             
001600        05 MID-IDILIST       PIC X(5).                                    
001700*                                 INLÄGGNINGSLISTEIDENTITET               
001800*                                 REPORTINGLIST-IDENTITY                  
001900*** END OF VILMAII-COPY LENGTH= 95 BYTES                                  
