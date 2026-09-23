000100 01  BKOLLI-WDE221.                                                       
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 FAKTURA -> BILL-IT                      
000400*                                 KOLLIINFO                               
000500*                                 FYSISIK NYCKEL: WDE221KY                
000600*                                 (IDPRODNR, IDKOLLI)                     
000700     03 BKOLLI-IDPRODNR      PIC S9(7)           COMP-3.                  
000800*                                 PRODUKTIONSNUMMER                       
000900*                                 PRODUCTION NUMBER                       
001000     03 BKOLLI-IDKOLLI       PIC S9(5)           COMP-3.                  
001100*                                 KOLLINUMMER                             
001200*                                 CASE NUMBER                             
001300     03 BKOLLI-IDGMTREF.                                                  
001400*                                 GODSMOTTAGAREREFERENS                   
001500*                                 GOODS RECEIVER REFERENS                 
001600        05 BKOLLI-IDDISTR    PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900        05 BKOLLI-IDKUNDNR   PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200        05 BKOLLI-IDKUNDRF-GRP.                                           
002300*                                 KUNDENS REFERENS (ORDERID)              
002400*                                 CUSTOMER REFERENCE (ORDER ID)           
002500           07 BKOLLI-IDKUNDRF                                             
002600                             PIC X(10).                                   
002700*                                 KUNDENS REFERENS (ORDERID)              
002800*                                 CUSTOMER REFERENCE (ORDER ID)           
002900           07 BKOLLI-IDORDNR5-FILLER REDEFINES BKOLLI-IDKUNDRF.           
003000              09 BKOLLI-IDORDNR5                                          
003100                             PIC 9(5).                                    
003200*                                 ORDERNUMMER                             
003300*                                 ORDER NUMBER                            
003400              09 FILLER      PIC X(5).                                    
003500           07 BKOLLI-IDORDNR7-FILLER REDEFINES BKOLLI-IDKUNDRF.           
003600              09 BKOLLI-IDORDNR7                                          
003700                             PIC 9(7).                                    
003800*                                 ORDERNUMMER                             
003900*                                 ORDER NUMBER                            
004000              09 FILLER      PIC X(3).                                    
004100     03 BKOLLI-FLOVRLEV      PIC X.                                       
004200*                                 ÖVERLEVERANS                            
004300*                                 OVER DELIVERY                           
004400     03 BKOLLI-KDFAKTYP      PIC X.                                       
004500*                                 FAKTURATYP                              
004600*                                 INVOICE TYPE                            
004700     03 BKOLLI-KDORDKL       PIC S9              COMP-3.                  
004800*                                 ORDERKLASS                              
004900*                                 ORDER CLASS                             
005000     03 BKOLLI-KDPRSTA       PIC X.                                       
005100*                                 STATUS PRISFRÅGA                        
005200*                                 STATUS PRICE QUESTION                   
005300     03 BKOLLI-VKORDBTO-KOLLI                                             
005400                             PIC S9(6)V9(1)      COMP-3.                  
005500*                                 ORDERVIKT BRUTTO PER KOLLI              
005600*                                 ORDER WEIGHT GROSS PER CASE             
005700     03 BKOLLI-BEKUNDRF      PIC X(15).                                   
005800*                                 KUNDENS REFERENS                        
005900*                                 CUSTOMERS REFERENCE                     
006000     03 BKOLLI-FLCROSS       PIC X.                                       
006100*                                 CROSS-DOCKING FLAGGA                    
006200*                                 CROSS DOCKING FLAG                      
006300*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
