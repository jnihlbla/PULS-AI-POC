000100 01  NDC-WDF116.                                                          
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 HEMTAGNINGSREGLER NDC                   
000400*                                 FYSISK NYCKEL: IDDC                     
000500     03 NDC-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 NDC-KVDAGAR-AVIAVV   PIC S9(3)           COMP-3.                  
000900*                                 TOLERANSAVVIKELSE FÖRAVISERING          
001000*                                 ACCEPTED DEVIATON PRE ADVICES           
001100     03 NDC-KVDAGAR-INLAVV   PIC S9(3)           COMP-3.                  
001200*                                 TOLERANSAVVIKELSE INLEVERANS            
001300*                                 ACCEPTED DEVIATON GOODS RECEIVI         
001400*                                 NG                                      
001500     03 NDC-KVDAGAR-TBT      PIC S9(3)           COMP-3.                  
001600*                                 TOT ANTAL DAGAR HEMTAGNINGSTID          
001700*                                 TOT NUMBER OF DAYS SUPPLY-TIME          
001800     03 NDC-KVDAGAR-TT       PIC S9(3)           COMP-3.                  
001900*                                 DAGAR TULL- OCH TRANSPORT-TID           
002000     03 NDC-KVVECKOR-LVAR    PIC S9(2)V9(1)      COMP-3.                  
002100*                                 VARIANS I LEDTIDEN                      
002200*                                                                         
002300     03 NDC-TILEVDAG         OCCURS 5 TIMES                               
002400                             PIC S9              COMP-3.                  
002500*                                 AVSÄNDNINGSDAG INOM VECKA               
002600*                                 DELIVERY WEEK DAY                       
002700     03 NDC-IDANSK-PG        OCCURS 8 TIMES                               
002800                             PIC S9(3)           COMP-3.                  
002900*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
003000*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
