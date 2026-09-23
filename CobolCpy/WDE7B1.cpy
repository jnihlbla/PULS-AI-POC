000100 01  SEQB-WDE7B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE721             
000300*                                 KOLLI INGÅNG                            
000400*                                 FYSISK NYCKEL: WDE7B1KY                 
000500*                                 (IDDISTR,IDKUNDNR,IDORDNR7,             
000600*                                  IDKOLLI)                               
000700*                                 SECONDARY KEY: WDE7BSEQ                 
000800*                                 (IDDISTR,IDKUNDNR,IDORDNR7,             
000900*                                  IDKOLLI)                               
001000*                                                                         
001100     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 SEQB-IDORDNR7        PIC 9(7).                                    
001800*                                 ORDERNUMMER                             
001900*                                 ORDER NUMBER                            
002000     03 SEQB-IDKOLLI         PIC S9(5)           COMP-3.                  
002100*                                 KOLLINUMMER                             
002200*                                 CASE NUMBER                             
002300     03 SEQB-IDKOLLI-SAMP    PIC S9(5)           COMP-3.                  
002400*                                 SAMPACKNINGSKOLLINUMMER                 
002500*                                 MIXED PACKING CASE NUMBER               
002600     03 SEQB-IDTRPTNR        PIC S9(3)           COMP-3.                  
002700*                                 TRANSPORTIDENTITET                      
002800*                                 TRANSPORT IDENTITY                      
002900     03 SEQB-IDDC            PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100*                                 WAREHOUSE IDENTIFIER                    
003200*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
