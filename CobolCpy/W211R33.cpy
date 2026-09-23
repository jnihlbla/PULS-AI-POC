000100 01  W211R33.                                                             
000200*                                 POSTTYP R33                             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 KDSORT2              PIC S9(3)           COMP-3.                  
000700*                                 SORTERINGSFÄLT                          
000800*                                 FIELD FOR SORTING PURPOSE               
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 IDLEVNR-INL          PIC X(5).                                    
001300*                                 LEVERANTÖR FÖR AKTUELL INLEV.           
001400     03 KDRT                 PIC S9(3)           COMP-3.                  
001500*                                 REDOVISNINGSTYP                         
001600*                                 TYPE OF ACCOUNTING                      
001700     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
001800*                                 AVISERINGSDATUM (YYMMDD)                
001900*                                 ADVICE NOTE DATE                        
002000     03 IDANALYS             PIC X(12).                                   
002100*                                 ANALYSNUMMER                            
002200*                                 ANALYSIS NUMBER                         
002300     03 IDKONTO              PIC S9(11)          COMP-3.                  
002400*                                 KONTO                                   
002500*                                 ACCOUNT                                 
002600     03 IDKST                PIC X(10).                                   
002700*                                 KOSTNADSSTÄLLE                          
002800*                                 COST CENTRE                             
002900     03 IDAVINR              PIC S9(7)           COMP-3.                  
003000*                                 AVI-NUMMER                              
003100*                                 ADVICE NOTE NUMBER                      
003200     03 KVAVIS               PIC S9(7)           COMP-3.                  
003300*                                 AVISERAT ANTAL                          
003400*                                 QUANTITY NOTIFIED                       
003500     03 IDDC                 PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700*                                 WAREHOUSE IDENTIFIER                    
003800     03 IDGMTREF.                                                         
003900*                                 GODSMOTTAGAREREFERENS                   
004000*                                 GOODS RECEIVER REFERENS                 
004100        05 IDDISTR           PIC S9(5)           COMP-3.                  
004200*                                 DISTRIKTNUMMER                          
004300*                                 DISTRICT NUMBER                         
004400        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
004500*                                 KUNDNUMMER                              
004600*                                 CUSTOMER NO                             
004700        05 IDKUNDRF-GRP.                                                  
004800*                                 KUNDENS REFERENS (ORDERID)              
004900*                                 CUSTOMER REFERENCE (ORDER ID)           
005000           07 IDKUNDRF       PIC X(10).                                   
005100*                                 KUNDENS REFERENS (ORDERID)              
005200*                                 CUSTOMER REFERENCE (ORDER ID)           
005300           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
005400              09 IDORDNR5    PIC 9(5).                                    
005500*                                 ORDERNUMMER                             
005600*                                 ORDER NUMBER                            
005700              09 FILLER      PIC X(5).                                    
005800           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
005900              09 IDORDNR7    PIC 9(7).                                    
006000*                                 ORDERNUMMER                             
006100*                                 ORDER NUMBER                            
006200              09 FILLER      PIC X(3).                                    
006300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
006400*                                 PRODUKTIONSNUMMER                       
006500*                                 PRODUCTION NUMBER                       
006600     03 IDFAKT               PIC S9(7)           COMP-3.                  
006700*                                 FAKTURANUMMER                           
006800*                                 INVOICE NO.                             
006900     03 IDSUPREF             PIC X(10).                                   
007000*                                 LEVERANTöRSREF.                         
007100*                                 SUPPLIER REF.                           
007200*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
