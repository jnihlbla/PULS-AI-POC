000100 01  REQU-WL0155I1.                                                       
000200*                                 REQUEST TO PGM WL0155                   
000300     03 REQU-L155-IDDC       PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-L155-IDPGM      PIC X(8).                                    
000700*                                 PROGRAM IDENTITET                       
000800*                                 PROGRAM INTENTITY                       
000900     03 REQU-L155-KVRADER    PIC 9(5).                                    
001000*                                 ANTAL RADER                             
001100*                                 NUMBER OF LINES                         
001200     03 REQU-INPUT           OCCURS 500 TIMES.                            
001300*                                 INMATNINGSFÄLT                          
001400        05 REQU-L155-IDILIST PIC 9(5).                                    
001500*                                 INLÄGGNINGSLISTEIDENTITET               
001600*                                 REPORTINGLIST-IDENTITY                  
001700*** END OF VILMAII-COPY LENGTH= 2515 BYTES                                
