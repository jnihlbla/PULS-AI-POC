000100 01  SEQB-WDM6B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDM601             
000300*                                 BYTESREGISTER LAGRING RETURER           
000400*                                 FYSISK NYCKEL: WDM6B1KY                 
000500*                                 (IDDC + KDBYTSTA + DAANKDAG             
000600*                                  + IDDISTR + IDBYTRAP)                  
000700*                                 SEKUNDÄR NYCKEL: WDM6BSEQ               
000800*                                 (IDDC + KDBYTSTA + DAANKDAG             
000900*                                  + IDDISTR + IDBYTRAP)                  
001000*                                 SÖKBEGREPP: IDDC, KDBYTSTA,             
001100*                                 DAANKDAG, IDDISTR, IDBYTRAP,            
001200*                                 IDKUNDNR                                
001300     03 SEQB-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQB-KDBYTSTA-RAPP   PIC X.                                       
001700*                                 STATUSKOD BYTESOBJEKT                   
001800*                                 STATUSCODE EXCH CORES                   
001900     03 SEQB-DAANKDAG        PIC 9(8).                                    
002000*                                 ANKOMSTDAG                              
002100*                                 RECEIVING DATE                          
002200     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500     03 SEQB-IDBYTRAP        PIC S9(7)           COMP-3.                  
002600*                                 RAPPORTNUMMER  BYTES                    
002700*                                 REPORTNUMBER   EXCHANGE                 
002800     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
002900*                                 KUNDNUMMER                              
003000*                                 CUSTOMER NO                             
003100*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
