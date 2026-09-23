000100 01  SEQA-WDM6A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDM601             
000300*                                 BYTESREGISTER LAGRING RETURER           
000400*                                 FYSISK NYCKEL: WDM6A1KY                 
000500*                                 (KDBYTSTA + IDDC + DAREGDAT             
000600*                                  + IDDISTR + IDBYTRAP)                  
000700*                                 SEKUNDÄR NYCKEL: WDM6ASEQ               
000800*                                 (KDBYTSTA + IDDC + DAREGDAT             
000900*                                  + IDDISTR + IDBYTRAP)                  
001000*                                 SÖKBEGREPP: IDKUNDNR                    
001100     03 SEQA-KDBYTSTA-RAPP   PIC X.                                       
001200*                                 STATUSKOD BYTESOBJEKT                   
001300*                                 STATUSCODE EXCH CORES                   
001400     03 SEQA-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SEQA-DAREGDAT        PIC 9(8).                                    
001800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001900*                                 REGISTRATION DATE (YYYYMMDD)            
002000     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200*                                 DISTRICT NUMBER                         
002300     03 SEQA-IDBYTRAP        PIC S9(7)           COMP-3.                  
002400*                                 RAPPORTNUMMER  BYTES                    
002500*                                 REPORTNUMBER   EXCHANGE                 
002600     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
002700*                                 KUNDNUMMER                              
002800*                                 CUSTOMER NO                             
002900*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
