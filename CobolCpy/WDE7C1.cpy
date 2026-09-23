000100 01  SEQC-WDE7C1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE711             
000300*                                 TIREGDAT INGÅNG                         
000400*                                 INDEX FINNS NÄR KDSTASKLI = R           
000500*                                 FYSISK NYCKEL: WDE7C1KY                 
000600*                                 (TIREGDAT+IDDC+IDKOLLI-SAMP)            
000700*                                 SECONDARY KEY: WDE7CSEQ                 
000800*                                 (TIREGDAT+IDDC+IDKOLLI-SAMP)            
000900*                                                                         
001000     03 SEQC-TIREGDAT        PIC S9(7)           COMP-3.                  
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200*                                 REGISTRATION DATE (YYMMDD)              
001300     03 SEQC-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQC-IDKOLLI-SAMP    PIC S9(5)           COMP-3.                  
001700*                                 SAMPACKNINGSKOLLINUMMER                 
001800*                                 MIXED PACKING CASE NUMBER               
001900*** END OF VILMAII-COPY LENGTH= 9 BYTES                                   
