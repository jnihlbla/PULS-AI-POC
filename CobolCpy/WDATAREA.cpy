000100 01  DAT-WDATAREA.                                                        
000200*                                 PARAMETRAR TILL WDATKONV                
000300*                                 F÷R KONVERTERING AV DATUM               
000400*                                 P≈ OLIKA FORMAT.                        
000500*                                 EXEMPEL P≈ ANROP:                       
000600*                                 MOVE "AAMMDD" TO DAT-KDDATFORM          
000700*                                 MOVE TIAAMMDD TO DAT-I-TIDATUM          
000800*                                 CALL WDATKONV USING                     
000900*                                      DAT-KDDATFORM,                     
001000*                                      DAT-I-TIDATUM,                     
001100*                                      DAT-O-TIDATUM,                     
001200*                                      DAT-KDSVAR                         
001300*                                                                         
001400*                                 RESULTAT ERH≈LLS I                      
001500*                                 DAT-O-TIDATUM.                          
001600*                                 STATUSKOD I DAT-KDSVAR.                 
001700*                                 -------------------------------         
001800*                                 PARAMETERS TO WDATKONV FOR              
001900*                                 CONVERTERING DATES OF VARYING           
002000*                                 FORMATS.                                
002100*                                 EXAMPLE OF CALL:                        
002200*                                 MOVE "AAMMDD" TO DAT-KDDATFORM          
002300*                                 MOVE TIAAMMDD TO DAT-I-TIDATUM          
002400*                                 CALL WDATKONV USING                     
002500*                                      DAT-KDDATFORM,                     
002600*                                      DAT-I-TIDATUM,                     
002700*                                      DAT-O-TIDATUM,                     
002800*                                      DAT-KDSVAR                         
002900*                                                                         
003000*                                 RESULT IS PLACED IN                     
003100*                                 DAT-O-TIDATUM.                          
003200*                                 RETURN CODE IN DAT-KDSVAR.              
003300*                                 -------------------------------         
003400     03 DAT-KDDATFORM        PIC X(6).                                    
003500*                                 FORMAT P≈ DATUMFƒLT                     
003600*                                 TILL≈TNA VƒRDEN:                        
003700*                                 "IDAG  " INGET DATUM I FƒLTET           
003800*                                          MASKINENS DATUM TAS.           
003900*                                 "AAMMDD" ≈R-M≈NAD-DAG.                  
004000*                                 "AADDD " ≈R-DAGNR.                      
004100*                                 "AAVVD " ≈R-VECKA-DAGNR.                
004200*                                 "AAVV  " ≈R-VECKA.                      
004300*                                 "AAP   " ≈R-PLANERINGSPERIOD            
004400*                                          8 PERIODER PER ≈R              
004500*                                 "AAPP  " ≈R-PLANERINGSPERIOD            
004600*                                          12 PERIODER PER ≈R             
004700*                                 "AARP  " ≈R-REDOV.PERIOD.               
004800*                                          12 PERIODER PER ≈R             
004900*                                 FORMAT OF DATE FIELD                    
005000*                                 VALID CODE VALUES:                      
005100*                                 "IDAG  " NO DATE IN FIELD               
005200*                                          MACHINE DATE IS USED.          
005300*                                 "AAMMDD" YEAR-MONTH-DAY                 
005400*                                 "AADDD " YEAR-DAY NO.                   
005500*                                 "AAVVD " YEAR-WEEK-DAY NO.              
005600*                                 "AAVV  " YEAR-WEEK.                     
005700*                                 "AAP   " YEAR-PLANNING PERIOD           
005800*                                          8 PERIODES PER YEAR            
005900*                                 "AAPP  " YEAR-PLANNING PERIOD           
006000*                                          12 PERIODES PER YEAR           
006100*                                 "AARP  " YEAR-ACCOUNT. PERIOD           
006200*                                          12 PERIODES PER YEAR           
006300     03 DAT-I-TIDATUM        PIC 9(6).                                    
006400*                                 DATUM ENLIGT KDDATFORM                  
006500*                                 DATE AS SPECIFIED BY KDDATFORM          
006600     03 DAT-O-TIDATUM.                                                    
006700        05 DAT-TIAAMMDD      PIC 9(6).                                    
006800*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
006900*                                 YEAR - MONTH - DAY  (YYMMDD)            
007000        05 DAT-TIAAMMDD-GRP REDEFINES DAT-TIAAMMDD.                       
007100           07 DAT-TIAA       PIC 9(2).                                    
007200*                                 ≈R    (≈≈)                              
007300*                                 YEAR  (YY)                              
007400           07 DAT-TIMM       PIC 9(2).                                    
007500*                                 M≈NAD (MM)                              
007600*                                 MONTH (MM)                              
007700           07 DAT-TIDD       PIC 9(2).                                    
007800*                                 DAG I M≈NAD (DD)                        
007900*                                 DAY OF MONTH (DD)                       
008000        05 DAT-TIAAVVD       PIC 9(5).                                    
008100*                                 ≈R - VECKA - DAG   (≈≈VVD)              
008200*                                 YEAR - WEEK - DAY  (YYWWD)              
008300        05 DAT-TIAAVVD-GRP REDEFINES DAT-TIAAVVD.                         
008400           07 DAT-TIAAVV-GRP.                                             
008500              09 DAT-TIAA-VECKA                                           
008600                             PIC 9(2).                                    
008700*                                 ≈R    (≈≈)                              
008800*                                 YEAR  (YY)                              
008900              09 DAT-TIVV    PIC 9(2).                                    
009000*                                 VECKA  (VV)                             
009100*                                 WEEK   (WW)                             
009200           07 DAT-TID        PIC 9.                                       
009300*                                 DAGNUMMER I VECKA (M≈NDAG = 1)          
009400*                                 DAY NO. IN WEEK   (MONDAY = 1)          
009500        05 DAT-TIAADDD       PIC 9(5).                                    
009600*                                 ≈R - DAGNUMMER    (≈≈DDD)               
009700*                                 YEAR - DAY NUMBER   (YYDDD)             
009800        05 DAT-TIAADDD-GRP REDEFINES DAT-TIAADDD.                         
009900           07 DAT-TIAA-DAGNR PIC 9(2).                                    
010000*                                 ≈R    (≈≈)                              
010100*                                 YEAR  (YY)                              
010200           07 DAT-TIDDD      PIC 9(3).                                    
010300*                                 DAGNUMMER  (DDD)                        
010400*                                 DAY NUMBER  (DDD)                       
010500        05 DAT-TIAAP         PIC 9(3).                                    
010600*                                 ≈R - PLANERINGSPERIOD (≈≈P)             
010700*                                 8 PER ≈R                                
010800*                                 YEAR - PLANNING PERIOD (YYP)            
010900*                                 8 PER YEAR                              
011000        05 DAT-TIAAP-GRP REDEFINES DAT-TIAAP.                             
011100           07 DAT-TIAA-PPER  PIC 9(2).                                    
011200*                                 ≈R    (≈≈)                              
011300*                                 YEAR  (YY)                              
011400           07 DAT-TIP        PIC 9.                                       
011500*                                 PLANERINGSPERIOD (P)                    
011600*                                 8 PER ≈R                                
011700*                                 PLANNING PERIOD  (P)                    
011800*                                 8 PER YEAR                              
011900        05 DAT-TISEKEL       PIC 9(2).                                    
012000*                                 SEKEL I ≈RTALET                         
012100*                                 CENTURY                                 
012200        05 DAT-KVVIPER       PIC 9.                                       
012300*                                 ANT VECKOR I REDOVISNINGSPERIOD         
012400*                                 NUMBER OF WEEKS IN A PERIOD             
012500        05 DAT-TIAAPP        PIC 9(4).                                    
012600*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
012700*                                 12 PER ≈R                               
012800*                                 YEAR - PLANNING PERIOD (YYPP)           
012900*                                 12 PER YEAR                             
013000        05 DAT-TIAAPP-GRP REDEFINES DAT-TIAAPP.                           
013100           07 DAT-TIAA-PPPER PIC 9(2).                                    
013200*                                 ≈R    (≈≈)                              
013300*                                 YEAR  (YY)                              
013400           07 DAT-TIPP       PIC 9(2).                                    
013500*                                 PLANERINGSPERIOD (PP)                   
013600*                                 12 PER ≈R                               
013700*                                 PLANNING PERIOD  (PP)                   
013800*                                 12 PER YEAR                             
013900        05 DAT-TIAARP        PIC 9(4).                                    
014000*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
014100*                                 12 PER ≈R                               
014200*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
014300*                                 12 PER YEAR                             
014400        05 DAT-TIAARP-GRP REDEFINES DAT-TIAARP.                           
014500           07 DAT-TIAA-RPPER PIC 9(2).                                    
014600*                                 ≈R    (≈≈)                              
014700*                                 YEAR  (YY)                              
014800           07 DAT-TIRP       PIC 9(2).                                    
014900*                                 REDOVISNINGSPERIOD                      
015000*                                 12 PER ≈R                               
015100*                                 ACCOUNTING PERIOD                       
015200*                                 12 PER YEAR                             
015300        05 DAT-TISEKDAT      PIC S9(7)           COMP-3.                  
015400*                                 ≈RTAL MED 1 I F÷RSTA F÷R 2000           
015500*                                 CENTURY 20 = 1XXXXXX                    
015600     03 DAT-KDSVAR           PIC X.                                       
015700      88 DAT-KDSVAR-OK       VALUE ' '.                                   
015800      88 DAT-KDSVAR-FEL      VALUE 'F'.                                   
015900*                                                       KDSVAR-88         
016000*                                 SVARSKOD FR≈N SUBPROGRAM                
016100*                                                       KDSVAR-88         
016200*                                 RETURN CODE FROM SUBPROGRAM             
016300     03 DAT-FILLER           PIC X(17).                                   
016400*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
