000100 01  KAMP-WDM201.                                                         
000200*                                 KAMPANJREGISTER                         
000300*                                 KAMPANJER                               
000400*                                 FYSISK NYCKEL: WDM201KY                 
000500*                                 (IDKAMPRF + IDDC)                       
000600*                                                                         
000700     03 KAMP-IDKAMPRF        PIC S9(7)           COMP-3.                  
000800*                                 KAMPANJREFERENS                         
000900*                                 CAMPAIGN REFERENCE                      
001000     03 KAMP-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 KAMP-TISTADAT        PIC S9(7)           COMP-3.                  
001400*                                 GENERELLT STARTDATUM                    
001500*                                 GENERAL START DATE                      
001600     03 KAMP-TISTODAT        PIC S9(7)           COMP-3.                  
001700*                                 GENERELLT STOPPDATUM                    
001800*                                 GENERAL STOP DATE YYMMDD                
001900     03 KAMP-TIREGDAT        PIC S9(7)           COMP-3.                  
002000*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002100*                                 REGISTRATION DATE (YYMMDD)              
002200*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
