000100 01  RESP-W40691O1.                                                       
000200*                                 RESPONSE FROM PGM 40691                 
000300     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
000400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
000500*                                 EDAN.                                   
000600     03 RESP-TABELLRAD       OCCURS 1 TO 500 TIMES                        
000700                             DEPENDING ON RESP-KVRADER-MAX1.              
000800*                                 GRUPP MED TABELLRADER                   
000900        05 RESP-IDDC         PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 RESP-IDTRPTNR     PIC 9(3).                                    
001200*                                 TRANSPORTIDENTITET                      
001300        05 RESP-TIRFSDAT     PIC 9(6).                                    
001400*                                 KLART F÷R TRANSPORT ≈≈MMDD              
001500        05 RESP-KVKOLLI-TRPT PIC 9(4).                                    
001600*                                 ANTAL KOLLI TRANSPORT                   
001700*** END OF VILMAII-COPY LENGTH= 7505 BYTES                                
