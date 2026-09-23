000100 01  RESP-WL0137O1.                                                       
000200*                                 COPYTEXT FÖR MOD WL0137O1               
000300*                                                                         
000400*                                                                         
000500*                                                                         
000600     03 RESP-IDDC-KEY        PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 RESP-IDKVAINF-KEY    PIC 9(2).                                    
001300*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
001400*                                 LINENO FOR QUALITY CONTROL TEXT         
001500     03 RESP-TIREGDAT-KEY    PIC 9(6).                                    
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700*                                 REGISTRATION DATE (YYMMDD)              
001800     03 RESP-IDDC2-KEY       PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 RESP-SHOW-ALL-KEY    PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300*                                 GENERAL FLAG                            
002400     03 RESP-TIREGDAT-NEXT   PIC 9(7).                                    
002500*                                 DATUMETS 9-KOMPLEMENT                   
002600*                                 DATES 9-COMPLEMENT                      
002700     03 RESP-TIKLOCK-NEXT    PIC 9(9).                                    
002800*                                 TID LAGRAT SOM 9-KOMPLEMENT             
002900*                                 TIME SAVED AS 9-COMPLEMENT              
003000     03 RESP-IN-RAD.                                                      
003100*                                 LINES                                   
003200        05 RESP-IDDC-IN      PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400*                                 WAREHOUSE IDENTIFIER                    
003500        05 RESP-TISTADAT-IN  PIC 9(6).                                    
003600*                                 GENERELLT STARTDATUM                    
003700*                                 GENERAL START DATE                      
003800        05 RESP-TISTODAT-IN  PIC 9(6).                                    
003900*                                 GENERELLT STOPPDATUM                    
004000*                                 GENERAL STOP DATE YYMMDD                
004100        05 RESP-KVANTAL-IN   PIC 9(7).                                    
004200*                                 ANTAL                                   
004300*                                 NUMBER                                  
004400        05 RESP-KVAVV-KVAL-IN                                             
004500                             PIC 9(7).                                    
004600*                                 ANTALSAVVIKELSE KVALITET                
004700*                                 QUANTITYDEVIATION QUALITY               
004800        05 RESP-KVART-SKROT-IN                                            
004900                             PIC 9(7).                                    
005000*                                 ANTAL SKROTADE ARTIKLAR                 
005100*                                 QUANTITY INSPECTED PARTS                
005200        05 RESP-KVART-RET-IN PIC 9(7).                                    
005300*                                 ANTAL ARTIKLAR I RETUR                  
005400*                                 QUANTITY INSPECTED PARTS                
005500        05 RESP-KVART-KJUST-IN                                            
005600                             PIC 9(7).                                    
005700*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
005800*                                 QUANTITY INSPECTED PARTS                
005900        05 RESP-BEINIT-IN    PIC X(3).                                    
006000*                                 INITIALER FÖR EN PERSON                 
006100*                                 INITIALS FOR A PERSON                   
006200     03 RESP-KVRADER         PIC Z(4)9.                                   
006300*                                 ANTAL RADER                             
006400*                                 NUMBER OF LINES                         
006500     03 RESP-RAD             OCCURS 500 TIMES.                            
006600*                                 LINES                                   
006700        05 RESP-IDDC         PIC X(2).                                    
006800*                                 IDENTIFIERARE LAGER                     
006900*                                 WAREHOUSE IDENTIFIER                    
007000        05 RESP-TISTADAT     PIC 9(6).                                    
007100*                                 GENERELLT STARTDATUM                    
007200*                                 GENERAL START DATE                      
007300        05 RESP-TISTODAT     PIC 9(6).                                    
007400*                                 GENERELLT STOPPDATUM                    
007500*                                 GENERAL STOP DATE YYMMDD                
007600        05 RESP-KVANTAL      PIC Z(7).                                    
007700*                                 ANTAL                                   
007800*                                 NUMBER                                  
007900        05 RESP-KVAVV-KVAL   PIC Z(6)9.                                   
008000*                                 ANTALSAVVIKELSE KVALITET                
008100*                                 QUANTITYDEVIATION QUALITY               
008200        05 RESP-KVART-SKROT  PIC Z(6)9.                                   
008300*                                 ANTAL SKROTADE ARTIKLAR                 
008400*                                 QUANTITY INSPECTED PARTS                
008500        05 RESP-KVART-RET    PIC Z(6)9.                                   
008600*                                 ANTAL ARTIKLAR I RETUR                  
008700*                                 QUANTITY INSPECTED PARTS                
008800        05 RESP-KVART-KJUST  PIC Z(6)9.                                   
008900*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
009000*                                 QUANTITY INSPECTED PARTS                
009100        05 RESP-BEINIT       PIC X(3).                                    
009200*                                 INITIALER FÖR EN PERSON                 
009300*                                 INITIALS FOR A PERSON                   
009400        05 RESP-KVLS         PIC Z(5)9.                                   
009500*                                 LAGERSALDO                              
009600*                                 STOCK BALANCE                           
009700        05 RESP-KDLEVSP      PIC Z9.                                      
009800*                                 SPÄRRKOD LEVERANS                       
009900*                                 DELIVERY BLOCKING CODE                  
010000        05 RESP-KVSPARR-KVAL PIC Z(5)9.                                   
010100*                                 SPÄRRAT ANTAL KVALITETSFEL              
010200*                                 BLOCKED QUANTITY QUALITY ERROR          
010300        05 RESP-FL-DC-INFO   PIC X.                                       
010400*                                 ALLMÄN FLAGGA                           
010500*                                 GENERAL FLAG                            
010600*** END OF VILMAII-COPY LENGTH= 33594 BYTES                               
