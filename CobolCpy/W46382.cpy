000100 01  W46382.                                                              
000200*                                 ---NEW VERSION---                       
000300*                                 NOT DELIVERED DIRECT DELIVERIES         
000400*                                 FROM VENDOR TO RETAILER                 
000500*                                 ---NEW VERSION---                       
000600*                                                                         
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDGMTREF.                                                         
001100*                                 GODSMOTTAGAREREFERENS                   
001200*                                 GOODS RECEIVER REFERENS                 
001300        05 IDDISTR           PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900        05 IDKUNDRF-GRP.                                                  
002000*                                 KUNDENS REFERENS (ORDERID)              
002100*                                 CUSTOMER REFERENCE (ORDER ID)           
002200           07 IDKUNDRF       PIC X(10).                                   
002300*                                 KUNDENS REFERENS (ORDERID)              
002400*                                 CUSTOMER REFERENCE (ORDER ID)           
002500           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
002600              09 IDORDNR5    PIC 9(5).                                    
002700*                                 ORDERNUMMER                             
002800*                                 ORDER NUMBER                            
002900              09 FILLER      PIC X(5).                                    
003000           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
003100              09 IDORDNR7    PIC 9(7).                                    
003200*                                 ORDERNUMMER                             
003300*                                 ORDER NUMBER                            
003400              09 FILLER      PIC X(3).                                    
003500     03 IDLEVNR              PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003800     03 IDPRODNR             PIC S9(7)           COMP-3.                  
003900*                                 PRODUKTIONSNUMMER                       
004000*                                 PRODUCTION NUMBER                       
004100     03 IDARTNR              PIC S9(9)           COMP-3.                  
004200*                                 ARTIKELNUMMER                           
004300*                                 PART NUMBER                             
004400     03 KDORDKL              PIC S9              COMP-3.                  
004500*                                 ORDERKLASS                              
004600*                                 ORDER CLASS                             
004700     03 KVBEART              PIC S9(7)           COMP-3.                  
004800*                                 BESTÄLLT ANTAL STYCKEN                  
004900*                                 ORDERED QUANTITY                        
005000     03 KVLEVART             PIC S9(7)           COMP-3.                  
005100*                                 LEVERERAT ANTAL STYCK                   
005200*                                 DELIVERED QUANTITY                      
005300     03 TIUTSKR              PIC S9(7)           COMP-3.                  
005400*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
005500*                                 PRINTING DATE  (YYMMDD)                 
005600     03 IDPURAD              PIC S9(5)           COMP-3.                  
005700*                                 RADNUMMER PÅ PACKUNDERLAG               
005800*                                 LINENO IN PACKINGDOCUMENT               
005900     03 KDANNULL             PIC X.                                       
006000*                                 CANCELLATION CODE                       
006100*                                 CANCELLATION CODE                       
006200     03 TIUTSTID             PIC S9(7)           COMP-3.                  
006300*                                 UTSKRIFTSTID (TTMMSS)                   
006400*                                 TIME OF PRINTING (HHMMSS)               
006500     03 FLJANEJ              PIC X.                                       
006600*                                 JA/NEJ-FLAGGA                           
006700     03 TISKEPPN-DDC         PIC S9(7)           COMP-3.                  
006800*                                 SKEPPNINGSDATUM DLEV (ÅÅMMDD)           
006900*                                 SHIPPING DATE DIR.LEV. (YYMMDD)         
007000*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
