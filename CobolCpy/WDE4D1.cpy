000100 01  SEQD-WDE4D1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE411             
000300*                                 RADER MED FYSISKA AVVIKELSER            
000400*                                 FYSISK NYCKEL: WDE4D1KY                 
000500*                                 (FLFYSAVV, IDGMTREF, IDPRODNR,          
000600*                                  IDPLKLST, IDPURAD)                     
000700*                                 SECONDARY KEY: WDE4DSEQ                 
000800*                                 (FLFYSAVV)                              
000900     03 SEQD-FLFYSAVV        PIC X.                                       
001000*                                 FLAGGA FYSISKA AVVIKELSER               
001100*                                 PHYSICAL DEVIATION FLAG                 
001200     03 SEQD-IDGMTREF.                                                    
001300*                                 GODSMOTTAGAREREFERENS                   
001400*                                 GOODS RECEIVER REFERENS                 
001500        05 SEQD-IDDISTR      PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800        05 SEQD-IDKUNDNR     PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000*                                 CUSTOMER NO                             
002100        05 SEQD-IDKUNDRF-GRP.                                             
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400           07 SEQD-IDKUNDRF  PIC X(10).                                   
002500*                                 KUNDENS REFERENS (ORDERID)              
002600*                                 CUSTOMER REFERENCE (ORDER ID)           
002700           07 SEQD-IDORDNR5-FILLER REDEFINES SEQD-IDKUNDRF.               
002800              09 SEQD-IDORDNR5                                            
002900                             PIC 9(5).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200              09 FILLER      PIC X(5).                                    
003300           07 SEQD-IDORDNR7-FILLER REDEFINES SEQD-IDKUNDRF.               
003400              09 SEQD-IDORDNR7                                            
003500                             PIC 9(7).                                    
003600*                                 ORDERNUMMER                             
003700*                                 ORDER NUMBER                            
003800              09 FILLER      PIC X(3).                                    
003900     03 SEQD-IDPRODNR        PIC S9(7)           COMP-3.                  
004000*                                 PRODUKTIONSNUMMER                       
004100*                                 PRODUCTION NUMBER                       
004200     03 SEQD-IDPLKLST        PIC S9(3)           COMP-3.                  
004300*                                 PLOCKLISTNUMMER                         
004400*                                 PICKING LIST NUMBER                     
004500     03 SEQD-IDPURAD         PIC S9(5)           COMP-3.                  
004600*                                 RADNUMMER PÅ PACKUNDERLAG               
004700*                                 LINENO IN PACKINGDOCUMENT               
004800     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
004900*                                 ARTIKELNUMMER                           
005000*                                 PART NUMBER                             
005100     03 SEQD-REKSIFFR        PIC S9              COMP-3.                  
005200*                                 KONTROLLSIFFRA                          
005300*                                 PART NO CHECK DIGIT                     
005400     03 SEQD-KVLEVART        PIC S9(7)           COMP-3.                  
005500*                                 LEVERERAT ANTAL STYCK                   
005600*                                 DELIVERED QUANTITY                      
005700*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
