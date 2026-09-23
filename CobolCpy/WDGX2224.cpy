000100 01  2224-WDGX2224.                                                       
000200*                                 LARMKÖ                                  
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (TISENBEK + KDLARM)                     
000500     03 2224-TISENBEK.                                                    
000600*                                 SENASTE BEKRÄFTELSETIDPUNKT             
000700*                                 LATEST CONFIRMATION DATE                
000800        05 2224-TISENBEK-DAG PIC S9(7)           COMP-3.                  
000900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001000*                                 YEAR - MONTH - DAY  (YYMMDD)            
001100        05 2224-TISENBEK-KL  PIC S9(7)           COMP-3.                  
001200*                                 TIM - MIN - SEK   (HHMMSS)              
001300*                                 HOUR - MINUTE - SEC (HHMMSS)            
001400     03 2224-KDLARM          PIC S9(3)           COMP-3.                  
001500*                                 LARMORSAKSKOD                           
001600*                                 ALARM REASON CODE                       
001700     03 2224-IDARTNR         PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000     03 2224-IDDC            PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200*                                 WAREHOUSE IDENTIFIER                    
002300     03 2224-FLNYLARM        PIC X.                                       
002400*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
002500*                                 NEW PARTALARM REGISTRATED               
002600     03 2224-IDGMTREF.                                                    
002700*                                 GODSMOTTAGAREREFERENS                   
002800*                                 GOODS RECEIVER REFERENS                 
002900        05 2224-IDDISTR      PIC S9(5)           COMP-3.                  
003000*                                 DISTRIKTNUMMER                          
003100*                                 DISTRICT NUMBER                         
003200        05 2224-IDKUNDNR     PIC S9(7)           COMP-3.                  
003300*                                 KUNDNUMMER                              
003400*                                 CUSTOMER NO                             
003500        05 2224-IDKUNDRF-GRP.                                             
003600*                                 KUNDENS REFERENS (ORDERID)              
003700*                                 CUSTOMER REFERENCE (ORDER ID)           
003800           07 2224-IDKUNDRF  PIC X(10).                                   
003900*                                 KUNDENS REFERENS (ORDERID)              
004000*                                 CUSTOMER REFERENCE (ORDER ID)           
004100           07 2224-IDORDNR5-FILLER REDEFINES 2224-IDKUNDRF.               
004200              09 2224-IDORDNR5                                            
004300                             PIC 9(5).                                    
004400*                                 ORDERNUMMER                             
004500*                                 ORDER NUMBER                            
004600              09 FILLER      PIC X(5).                                    
004700           07 2224-IDORDNR7-FILLER REDEFINES 2224-IDKUNDRF.               
004800              09 2224-IDORDNR7                                            
004900                             PIC 9(7).                                    
005000*                                 ORDERNUMMER                             
005100*                                 ORDER NUMBER                            
005200              09 FILLER      PIC X(3).                                    
005300     03 2224-IDLOPNR         PIC S9(3)           COMP-3.                  
005400*                                 LÖPNUMMER                               
005500*                                 SEQUENCE NUMBER                         
005600     03 2224-TIREGDAT        PIC S9(7)           COMP-3.                  
005700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005800*                                 REGISTRATION DATE (YYMMDD)              
005900     03 2224-IDTRANS         PIC X(4).                                    
006000*                                 BILDNUMMER                              
006100*                                 SCREEN NUMBER                           
006200     03 2224-KDMFSFOR        PIC X.                                       
006300*                                 TYP AV MFS-FORMAT                       
006400*                                 1 = W-FORMAT  2 = N-FORMAT              
006500*                                 TYPE OF MFS FORMAT                      
006600     03 2224-IDKR            PIC 9(5).                                    
006700*                                 KONTROLLRAPPORT NUMMER                  
006800*                                 INSPECTION REPORT NUMBER                
006900     03 2224-IDLEVNR         PIC X(5).                                    
007000*                                 LEVERANTÖRNUMMER                        
007100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007200     03 2224-FILLER          PIC X(14).                                   
007300*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
