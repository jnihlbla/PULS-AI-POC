000100 01  SEQD-WDM6D1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDM601             
000300*                                 BYTESREGISTER LAGRING RETURER           
000400*                                 FYSISK NYCKEL: WDM6D1KY                 
000500*                                 (IDDC + IDDISTR + KDBYTSTA +            
000600*                                  DAREGDAT + IDBYTRAP)                   
000700*                                 SEKUNDÄR NYCKEL: WDM6DSEQ               
000800*                                 (IDDC + IDDISTR + KDBYTSTA +            
000900*                                  DAREGDAT + IDBYTRAP)                   
001000     03 SEQD-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQD-IDDISTR         PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 SEQD-KDBYTSTA-RAPP   PIC X.                                       
001700*                                 STATUSKOD BYTESOBJEKT                   
001800*                                 STATUSCODE EXCH CORES                   
001900     03 SEQD-DAREGDAT        PIC 9(8).                                    
002000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002100*                                 REGISTRATION DATE (YYYYMMDD)            
002200     03 SEQD-IDBYTRAP        PIC S9(7)           COMP-3.                  
002300*                                 RAPPORTNUMMER  BYTES                    
002400*                                 REPORTNUMBER   EXCHANGE                 
002500     03 SEQD-IDKUNDNR        PIC S9(7)           COMP-3.                  
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
