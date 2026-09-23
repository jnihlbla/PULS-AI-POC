000100 01  SEQF-WDE4F1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE411             
000300*                                 RADER PACKADE I KOLLI                   
000400*                                 FYSISK NYCKEL: WDE4F1KY                 
000500*                                 (IDPRODNR, IDKOLLI, IDGMTREF,           
000600*                                  IDPURAD, IDPLKLST)                     
000700*                                 SECONDARY KEY: WDE4FSEQ                 
000800*                                 (IDPRODNR, IDKOLLI)                     
000900     03 SEQF-IDPRODNR        PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100*                                 PRODUCTION NUMBER                       
001200     03 SEQF-IDKOLLI         PIC S9(5)           COMP-3.                  
001300*                                 KOLLINUMMER                             
001400*                                 CASE NUMBER                             
001500     03 SEQF-IDGMTREF.                                                    
001600*                                 GODSMOTTAGAREREFERENS                   
001700*                                 GOODS RECEIVER REFERENS                 
001800        05 SEQF-IDDISTR      PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000*                                 DISTRICT NUMBER                         
002100        05 SEQF-IDKUNDNR     PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400        05 SEQF-IDKUNDRF-GRP.                                             
002500*                                 KUNDENS REFERENS (ORDERID)              
002600*                                 CUSTOMER REFERENCE (ORDER ID)           
002700           07 SEQF-IDKUNDRF  PIC X(10).                                   
002800*                                 KUNDENS REFERENS (ORDERID)              
002900*                                 CUSTOMER REFERENCE (ORDER ID)           
003000           07 SEQF-IDORDNR5-FILLER REDEFINES SEQF-IDKUNDRF.               
003100              09 SEQF-IDORDNR5                                            
003200                             PIC 9(5).                                    
003300*                                 ORDERNUMMER                             
003400*                                 ORDER NUMBER                            
003500              09 FILLER      PIC X(5).                                    
003600           07 SEQF-IDORDNR7-FILLER REDEFINES SEQF-IDKUNDRF.               
003700              09 SEQF-IDORDNR7                                            
003800                             PIC 9(7).                                    
003900*                                 ORDERNUMMER                             
004000*                                 ORDER NUMBER                            
004100              09 FILLER      PIC X(3).                                    
004200     03 SEQF-IDPURAD         PIC S9(5)           COMP-3.                  
004300*                                 RADNUMMER PÅ PACKUNDERLAG               
004400*                                 LINENO IN PACKINGDOCUMENT               
004500     03 SEQF-IDPLKLST        PIC S9(3)           COMP-3.                  
004600*                                 PLOCKLISTNUMMER                         
004700*                                 PICKING LIST NUMBER                     
004800     03 SEQF-KVLEVART        PIC S9(7)           COMP-3.                  
004900*                                 LEVERERAT ANTAL STYCK                   
005000*                                 DELIVERED QUANTITY                      
005100*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
