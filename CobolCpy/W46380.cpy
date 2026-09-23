000100 01  W46380.                                                              
000200*                                 NOT DELIVERED DIRECT DELIVERIES         
000300*                                 FROM VENDOR TO RETAILER                 
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 IDGMTREF.                                                         
000900*                                 GODSMOTTAGAREREFERENS                   
001000*                                 GOODS RECEIVER REFERENS                 
001100        05 IDDISTR           PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700        05 IDKUNDRF-GRP.                                                  
001800*                                 KUNDENS REFERENS (ORDERID)              
001900*                                 CUSTOMER REFERENCE (ORDER ID)           
002000           07 IDKUNDRF       PIC X(10).                                   
002100*                                 KUNDENS REFERENS (ORDERID)              
002200*                                 CUSTOMER REFERENCE (ORDER ID)           
002300           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
002400              09 IDORDNR5    PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700              09 FILLER      PIC X(5).                                    
002800           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002900              09 IDORDNR7    PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200              09 FILLER      PIC X(3).                                    
003300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
003400*                                 PRODUKTIONSNUMMER                       
003500*                                 PRODUCTION NUMBER                       
003600     03 IDARTNR              PIC S9(9)           COMP-3.                  
003700*                                 ARTIKELNUMMER                           
003800*                                 PART NUMBER                             
003900     03 KVBEART              PIC S9(7)           COMP-3.                  
004000*                                 BESTÄLLT ANTAL STYCKEN                  
004100*                                 ORDERED QUANTITY                        
004200     03 KVLEVART             PIC S9(7)           COMP-3.                  
004300*                                 LEVERERAT ANTAL STYCK                   
004400*                                 DELIVERED QUANTITY                      
004500     03 TIUTSKR              PIC S9(7)           COMP-3.                  
004600*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
004700*                                 PRINTING DATE  (YYMMDD)                 
004800     03 FLJANEJ              PIC X.                                       
004900*                                 JA/NEJ-FLAGGA                           
005000*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
