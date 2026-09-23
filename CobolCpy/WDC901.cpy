000100 01  KART-WDC901.                                                         
000200*                                 NYA ARTIKLAR FRÅN VCC                   
000300*                                 TILL KINA                               
000400*                                 FYSISK NYCKEL: WDC901KY                 
000500*                                 ( IDDC + IDARTNR )                      
000600     03 KART-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 KART-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 KART-IDANSK          PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400*                                 PROCURER NO.                            
001500     03 KART-KDANSKQ         PIC X.                                       
001600*                                 KOD FÖR ANSKAFFARE KÖ                   
001700*                                 PROCURER QUEUE CODE                     
001800     03 KART-KVPROG          PIC S9(7)           COMP-3.                  
001900*                                 ÅRSPROGNOS                              
002000*                                 PROGNOS OF THE YEAR                     
002100     03 KART-TIINKOP         PIC S9(7)           COMP-3.                  
002200*                                 DATUM NÄR INKÖP BEGÄRS                  
002300*                                 DATE FOR PURCHASE REQUEST               
002400     03 KART-TILEVBEG        PIC S9(7)           COMP-3.                  
002500*                                 DATUM NÄR LEVERANS BEGÄRS               
002600*                                 DATE FOR DELIVER REQUEST                
002700     03 KART-TIMOTSI         PIC S9(7)           COMP-3.                  
002800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002900*                                 YEAR - MONTH - DAY  (YYMMDD)            
003000     03 KART-TIREGDAT        PIC S9(7)           COMP-3.                  
003100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003200*                                 REGISTRATION DATE (YYMMDD)              
003300     03 KART-FILLER          PIC X(10).                                   
003400*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
