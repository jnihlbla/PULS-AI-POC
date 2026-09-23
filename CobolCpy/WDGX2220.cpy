000100 01  2220-WDGX2220.                                                       
000200*                                 LARMKÖ LEVERANSPRECISION                
000300*                                 FYSISK NYCKEL KY2220:                   
000400*                                 (DAREGDAT- + TIKLOCK-9KOMPL)            
000500     03 2220-DAREGDAT-9KOMPL PIC 9(8).                                    
000600*                                 DATUMETS 9-KOMPLEMENT                   
000700*                                 DATES 9-COMPLEMENT                      
000800     03 2220-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
000900*                                 TID LAGRAT SOM 9-KOMPLEMENT             
001000*                                 TIME SAVED AS 9-COMPLEMENT              
001100     03 2220-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 2220-IDANSK          PIC S9(3)           COMP-3.                  
001500*                                 ANSKAFFARNUMMER                         
001600*                                 PROCURER NO.                            
001700     03 2220-IDLEVNR         PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002000     03 2220-KDLARM          PIC S9(3)           COMP-3.                  
002100*                                 LARMORSAKSKOD                           
002200*                                 ALARM REASON CODE                       
002300     03 2220-FLNYLARM        PIC X.                                       
002400*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
002500*                                 NEW PARTALARM REGISTRATED               
002600     03 2220-KVAVIS          PIC S9(7)           COMP-3.                  
002700*                                 AVISERAT ANTAL                          
002800*                                 QUANTITY NOTIFIED                       
002900     03 2220-KVAVROP         PIC S9(7)           COMP-3.                  
003000*                                 AVROPSKVANTITET                         
003100     03 2220-TIAAMMDD        PIC S9(7)           COMP-3.                  
003200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003300*                                 YEAR - MONTH - DAY  (YYMMDD)            
003400*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
