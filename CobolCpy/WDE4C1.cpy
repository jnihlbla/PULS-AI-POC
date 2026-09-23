000100 01  SEQC-WDE4C1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE411             
000300*                                 EJ PACKNINGSRAPPORTERADE                
000400*                                 ARTIKLAR.                               
000500*                                 FYSISK NYCKEL: WDE4C1KY                 
000600*                                  (IDARTNR, IDPRODNR, IDPURAD)           
000700*                                 SECINDARY KEY: WDE4CSEQ                 
000800*                                  (IDARTNR)                              
000900     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 SEQC-IDPRODNR        PIC S9(7)           COMP-3.                  
001300*                                 PRODUKTIONSNUMMER                       
001400*                                 PRODUCTION NUMBER                       
001500     03 SEQC-IDPURAD         PIC S9(5)           COMP-3.                  
001600*                                 RADNUMMER PÅ PACKUNDERLAG               
001700*                                 LINENO IN PACKINGDOCUMENT               
001800     03 SEQC-IDGMTREF.                                                    
001900*                                 GODSMOTTAGAREREFERENS                   
002000*                                 GOODS RECEIVER REFERENS                 
002100        05 SEQC-IDDISTR      PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400        05 SEQC-IDKUNDNR     PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700        05 SEQC-IDKUNDRF-GRP.                                             
002800*                                 KUNDENS REFERENS (ORDERID)              
002900*                                 CUSTOMER REFERENCE (ORDER ID)           
003000           07 SEQC-IDKUNDRF  PIC X(10).                                   
003100*                                 KUNDENS REFERENS (ORDERID)              
003200*                                 CUSTOMER REFERENCE (ORDER ID)           
003300           07 SEQC-IDORDNR5-FILLER REDEFINES SEQC-IDKUNDRF.               
003400              09 SEQC-IDORDNR5                                            
003500                             PIC 9(5).                                    
003600*                                 ORDERNUMMER                             
003700*                                 ORDER NUMBER                            
003800              09 FILLER      PIC X(5).                                    
003900           07 SEQC-IDORDNR7-FILLER REDEFINES SEQC-IDKUNDRF.               
004000              09 SEQC-IDORDNR7                                            
004100                             PIC 9(7).                                    
004200*                                 ORDERNUMMER                             
004300*                                 ORDER NUMBER                            
004400              09 FILLER      PIC X(3).                                    
004500     03 SEQC-IDPLKLST        PIC S9(3)           COMP-3.                  
004600*                                 PLOCKLISTNUMMER                         
004700*                                 PICKING LIST NUMBER                     
004800     03 SEQC-KDRADSTA        PIC S9              COMP-3.                  
004900*                                 STATUS PÅ ORDERRAD                      
005000*                                 ORDERLINE STATUS                        
005100*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
