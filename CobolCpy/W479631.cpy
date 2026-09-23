000100 01  W479631.                                                             
000200*                                 OUTBOUND INVOICED PER DISTRICT          
000300*                                 OUTBOUND INVOICED PER DISTRICT          
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600*                                 AFP FORMS RECORD TYPE                   
000700     03 KDMFUP               PIC X(2).                                    
000800*                                 RAPPORTGRUPP  MA/CN/PF/NA               
000900*                                 REPORT GROUP  MA/CN/PF/NA               
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 ADCITY               PIC X(20).                                   
001400     03 TIAAVV               PIC 9(4).                                    
001500*                                 ≈R - VECKA  (≈≈VV)                      
001600*                                 YEAR - WEEK  (YYWW)                     
001700     03 TIAARP REDEFINES TIAAVV                                           
001800                             PIC 9(4).                                    
001900*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
002000*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
002100*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002200*                                 12 PER YEAR, ALSO LOGISTICS PER         
002300     03 IDDISTR              PIC Z(3)9.                                   
002400*                                 DISTRIKTNUMMER                          
002500*                                 DISTRICT NUMBER                         
002600     03 IDKUNDNR             PIC Z(5)9.                                   
002700*                                 KUNDNUMMER                              
002800*                                 CUSTOMER NO                             
002900     03 KDORDKL              PIC 9.                                       
003000*                                 ORDERKLASS                              
003100*                                 ORDER CLASS                             
003200     03 SULEVANT             PIC Z(8)9.                                   
003300*                                 SUMMA LEVERERAT ANTAL                   
003400*                                 AV 1 ARTIKEL                            
003500*                                 SUMMARY DELIVERED OF AN ITEM            
003600     03 SURADER              PIC Z(8)9.                                   
003700*                                 TOTALT ANTAL RADER                      
003800*                                 TOTAL NUMBER OF LINES                   
003900     03 SUKOLLI              PIC Z(8)9.                                   
004000*                                 SUMMA ANTAL KOLLI                       
004100*                                 ACCUM NBR OF CASES                      
004200     03 KVORDER              PIC Z(6)9.                                   
004300*                                 ANTAL ORDER                             
004400*                                 QUANTITY OF ORDERS                      
004500     03 VKORDBTO-KOLLI       PIC Z(7)9.9.                                 
004600*                                 ORDERVIKT BRUTTO PER KOLLI              
004700*                                 ORDER WEIGHT GROSS PER CASE             
004800     03 SUACKSTD             PIC Z(8)9.9(2).                              
004900*                                 ACKUMULERAD FSG TILL STD PRIS           
005000*                                 ACKUMULATED SALES AT STD PRICE          
005100*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
