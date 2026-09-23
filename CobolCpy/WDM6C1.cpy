000100 01  SEQC-WDM6C1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDM601             
000300*                                 BYTESREGISTER LAGRING RETURER           
000400*                                 FYSISK NYCKEL: WDM6C1KY                 
000500*                                 (IDDC + IDDISTR + KDBYTSTA +            
000600*                                  DAANKDAG + IDBYTRAP)                   
000700*                                 SEKUNDÄR NYCKEL: WDM6CSEQ               
000800*                                 (IDDC + IDDISTR + KDBYTSTA +            
000900*                                  DAANKDAG + IDBYTRAP)                   
001000     03 SEQC-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQC-IDDISTR         PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 SEQC-KDBYTSTA-RAPP   PIC X.                                       
001700*                                 STATUSKOD BYTESOBJEKT                   
001800*                                 STATUSCODE EXCH CORES                   
001900     03 SEQC-DAANKDAG        PIC 9(8).                                    
002000*                                 ANKOMSTDAG                              
002100*                                 RECEIVING DATE                          
002200     03 SEQC-IDBYTRAP        PIC S9(7)           COMP-3.                  
002300*                                 RAPPORTNUMMER  BYTES                    
002400*                                 REPORTNUMBER   EXCHANGE                 
002500     03 SEQC-IDKUNDNR        PIC S9(7)           COMP-3.                  
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
