000100 01  LAK-WDD401.                                                          
000200*                                 LARMKÖ LEVERANSPRECISION                
000300*                                 FYSISK NYCKEL WDD401KY:                 
000400*                                 (DAREGDAT-9KOMP+TIKLOCK-9KOMP)          
000500*                                                                         
000600     03 LAK-DAREGDAT-9KOMPL  PIC 9(8).                                    
000700*                                 DATUMETS 9-KOMPLEMENT                   
000800*                                 DATES 9-COMPLEMENT                      
000900     03 LAK-TIKLOCK-9KOMPL   PIC S9(9)           COMP-3.                  
001000*                                 TID LAGRAT SOM 9-KOMPLEMENT             
001100*                                 TIME SAVED AS 9-COMPLEMENT              
001200     03 LAK-IDARTNR          PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 LAK-IDDC             PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 LAK-IDANSK           PIC S9(3)           COMP-3.                  
001900*                                 ANSKAFFARNUMMER                         
002000*                                 PROCURER NO.                            
002100     03 LAK-IDLEVNR          PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002400     03 LAK-KDLARM           PIC S9(3)           COMP-3.                  
002500*                                 LARMORSAKSKOD                           
002600*                                 ALARM REASON CODE                       
002700     03 LAK-FLNYLARM         PIC X.                                       
002800*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
002900*                                 NEW PARTALARM REGISTRATED               
003000     03 LAK-KVAVIS           PIC S9(7)           COMP-3.                  
003100*                                 AVISERAT ANTAL                          
003200*                                 QUANTITY NOTIFIED                       
003300     03 LAK-KVAVROP          PIC S9(7)           COMP-3.                  
003400*                                 AVROPSKVANTITET                         
003500     03 LAK-TIAAMMDD         PIC S9(7)           COMP-3.                  
003600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003700*                                 YEAR - MONTH - DAY  (YYMMDD)            
003800     03 LAK-TIAAMMDD-AVS     PIC S9(7)           COMP-3.                  
003900*                                 AVSÄNDNINGSDATUM                        
004000     03 LAK-FILLER           PIC X(6).                                    
004100*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
