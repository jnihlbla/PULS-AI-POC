000100 01  W42823.                                                              
000200*                                 UPPFÖLJNINGS FIL                        
000300*                                 LEVERANSANMÄRKNINGAR                    
000400*                                 SELECTED EXTRACT ABOUT                  
000500*                                 DESCREPANCYS              .             
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 IDLANDX2             PIC X(2).                                    
001000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001100*                                 2-LETTER CODE FOR COUNTRY               
001200     03 ADCITY               PIC X(20).                                   
001300     03 KDANMORS-001         PIC X(2).                                    
001400*                                 ORSAK TILL LEVERANSANMÄRKNING           
001500*                                 DISCREPANCY REPORT REASON CODE          
001600     03 SUART-WEEK           PIC 9(5).                                    
001700*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
001800*                                 QUANTITY PARTS PER TYPE                 
001900     03 SUART-YEAR           PIC 9(5).                                    
002000*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
002100*                                 QUANTITY PARTS PER TYPE                 
002200     03 SURADER-WEEK         PIC 9(9).                                    
002300*                                 TOTALT ANTAL RADER                      
002400*                                 TOTAL NUMBER OF LINES                   
002500     03 SURADER-YEAR         PIC 9(9).                                    
002600*                                 TOTALT ANTAL RADER                      
002700*                                 TOTAL NUMBER OF LINES                   
002800     03 TIAAVV               PIC 9(4).                                    
002900*                                 ÅR - VECKA  (ÅÅVV)                      
003000*                                 YEAR - WEEK  (YYWW)                     
003100     03 TIAAPP               PIC 9(4).                                    
003200*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
003300*                                 12 PER ÅR                               
003400*                                 YEAR - PLANNING PERIOD (YYPP)           
003500*                                 12 PER YEAR                             
003600*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
