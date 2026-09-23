000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W3302600.                                                 
000400 AUTHOR.        RONNY STENHOLM                                            
000500     DATE-WRITTEN.  SEPT 1995.                                            
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAM ÅT ARTIKELSTATISTIKEN.                                   
001000*        BERÄKNAR BRUTTOVINSTEN PER AF1 - AF5 SAMT SKAPAR TRANSAR         
001100*        PRISOMRÅDESNIVÅ FÖR ATT LADDA DB2-BAS MED.                       
001200*                                                                         
001300*        DENNA SUMMERINGSNIVÅ ÄR:                                         
001400*        - ARTIKEL/PRISOMRÅDE (FSG5)                                      
001500*              POSTER SKRIVS PÅ UTFIL W33032.                             
001600*                                                                         
001700*        PROGRAMMET                                                       
001800*        BERÄKAR BRUTTOVINSTEN SAMT SKRIVER EN TRANS PÅ FSG5-NIVÅ.        
001900*                                                                         
002000*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*    --- INFIL:                                                           
002900*           --- SEKUNDÄRREGISTER:                                         
003000     SELECT W33026                       ASSIGN TO W33026D1.              
003100                                                                          
003200*    --- UTFILER:                                                         
003300*           --- FÖR LADDNING AV DB2-BAS:                                  
003400     SELECT W33032                       ASSIGN TO W33026D2.              
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W33026                                                               
004100     LABEL RECORD   STANDARD                                              
004200     RECORDING      F                                                     
004300     BLOCK CONTAINS 0.                                                    
004400                                                                          
004500*    -COPY W33026   -L.                                                   
004600     EJECT                                                                
004700 FD  W33032                                                               
004800     LABEL RECORD   STANDARD                                              
004900     RECORDING      F                                                     
005000     BLOCK CONTAINS 0.                                                    
005100                                                                          
005200*01  W33032-POST -COPY FSG5   -L.                                         
005300     SKIP3                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500     SKIP2                                                                
005501                                                                          
005510*    -- CHECKED BY WY2000                                                 
005600 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3302600'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900                                                                          
006000 77  INFIL-POST-SW               PIC X(1)    VALUE 'J'.                   
006100     88  INFIL-POST-SAKNAS                   VALUE 'N'.                   
006200                                                                          
006300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006400 77  SPAR-SUTOTBV-PER        PIC S9(11)V9(2)     COMP-3.                  
006500 77  SPAR-SUTOTBV-AAR        PIC S9(11)V9(2)     COMP-3.                  
006600 77  SPAR-SUTOTBV-FAAR       PIC S9(11)V9(2)     COMP-3.                  
006700 77  SPAR-SUTOTBV-RAAR       PIC S9(11)V9(2)     COMP-3.                  
006800 77  SPAR-SUTOTBV-FRAAR      PIC S9(11)V9(2)     COMP-3.                  
006900                                                                          
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     SKIP3                                                                
007400*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
007500*                                                                         
007600*01  -COPY W0005 -PRE  POSTSUM-                                           
007700                                                                          
007800 01  W33026-TRANSID.                                                      
007900     03  FILLER                 PIC X(6)  VALUE 'W33026'.                 
008000     03  FILLER                 PIC X(8)  VALUE 'W33026D1'.               
008100     03  FILLER                 PIC X(4)  VALUE ' IN '.                   
008200 01  W33032-TRANSID.                                                      
008300     03  FILLER                 PIC X(6)  VALUE 'W33032'.                 
008400     03  FILLER                 PIC X(8)  VALUE 'W33026D2'.               
008500     03  FILLER                 PIC X(4)  VALUE 'FSG5'.                   
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)  VALUE 'IN-AREA-START'.        
008800*01  AREA    -PRE IN-  -COPY W33026                                       
008900     EJECT                                                                
009000*01  AREA    -PRE FSG5-    -COPY FSG5                                     
009100     EJECT                                                                
009200 PROCEDURE DIVISION.                                                      
009300                                                                          
009400     PERFORM A-INIT                                                       
009500                                                                          
009600     PERFORM S01-LAS-INFIL                                                
009700     PERFORM UNTIL INFIL-POST-SAKNAS                                      
009800       INITIALIZE FSG5-AREA                                               
009900       MOVE IN-IDARTNR    TO FSG5-IDARTNR                                 
010000       MOVE IN-IDPROMR    TO FSG5-IDPROMR                                 
010100       PERFORM UNTIL INFIL-POST-SAKNAS                                    
010200              OR IN-IDARTNR NOT = FSG5-IDARTNR                            
010300              OR IN-IDPROMR NOT = FSG5-IDPROMR                            
010400                                                                          
010500         PERFORM C-SUMMERA                                                
010600         PERFORM D-ADDERA                                                 
010700         PERFORM S01-LAS-INFIL                                            
010800                                                                          
010900       END-PERFORM                                                        
014174       PERFORM S02-SKRIV-FSG5                                             
014176     END-PERFORM                                                          
014180                                                                          
014200     PERFORM Z-FINIT                                                      
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800                                                                          
014900     OPEN INPUT  W33026                                                   
015000          OUTPUT W33032                                                   
015100                                                                          
015200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015300     .                                                                    
015400     EJECT                                                                
015500 C-SUMMERA SECTION.                                                       
015600                                                                          
015700     MOVE IN-IDARTNR             TO FSG5-IDARTNR                          
015800     MOVE IN-IDPROMR             TO FSG5-IDPROMR                          
015900     SUBTRACT IN-SUARTSJK-PER FROM IN-SUARTFSG-PER                        
016000                             GIVING SPAR-SUTOTBV-PER                      
016100     SUBTRACT IN-SUARTSJK-AAR FROM IN-SUARTFSG-AAR                        
016200                             GIVING SPAR-SUTOTBV-AAR                      
016300     SUBTRACT IN-SUARTSJK-FAAR FROM IN-SUARTFSG-FAAR                      
016400                             GIVING SPAR-SUTOTBV-FAAR                     
016500     SUBTRACT IN-SUARTSJK-RAAR FROM IN-SUARTFSG-RAAR                      
016600                             GIVING SPAR-SUTOTBV-RAAR                     
016700     SUBTRACT IN-SUARTSJK-FRAAR FROM IN-SUARTFSG-FRAAR                    
016800                             GIVING SPAR-SUTOTBV-FRAAR                    
016900                                                                          
017000     .                                                                    
017100     EJECT                                                                
017200 D-ADDERA SECTION.                                                        
017300                                                                          
017400     ADD IN-SUARTFSG-PER         TO FSG5-SUARTFSG-PER                     
017500     ADD IN-SUARTFSG-AAR         TO FSG5-SUARTFSG-AAR                     
017600     ADD IN-SUARTFSG-RAAR-SPEC   TO FSG5-SUARTFSG-RAAR-SPEC               
017700     ADD IN-SUARTFSG-RAAR-RAB    TO FSG5-SUARTFSG-RAAR-RAB                
017800     ADD IN-SUARTFSG-RAAR-MAN    TO FSG5-SUARTFSG-RAAR-MAN                
017900     ADD IN-SUARTFSG-RAAR-KRE    TO FSG5-SUARTFSG-RAAR-KRE                
018000     ADD IN-SUARTFSG-FAAR        TO FSG5-SUARTFSG-FAAR                    
018100     ADD IN-SUARTFSG-RAAR        TO FSG5-SUARTFSG-RAAR                    
018200     ADD IN-SUARTFSG-FRAAR       TO FSG5-SUARTFSG-FRAAR                   
018300     ADD IN-SULEVANT-PER         TO FSG5-SULEVANT-PER                     
018400     ADD IN-SULEVANT-AAR         TO FSG5-SULEVANT-AAR                     
018500     ADD IN-SULEVANT-RAAR-SPEC   TO FSG5-SULEVANT-RAAR-SPEC               
018600     ADD IN-SULEVANT-RAAR-RAB    TO FSG5-SULEVANT-RAAR-RAB                
018700     ADD IN-SULEVANT-RAAR-MAN    TO FSG5-SULEVANT-RAAR-MAN                
018800     ADD IN-SULEVANT-RAAR-KRE    TO FSG5-SULEVANT-RAAR-KRE                
018900     ADD IN-SULEVANT-FAAR        TO FSG5-SULEVANT-FAAR                    
019000     ADD IN-SULEVANT-RAAR        TO FSG5-SULEVANT-RAAR                    
019100     ADD IN-SULEVANT-FRAAR       TO FSG5-SULEVANT-FRAAR                   
019200     ADD SPAR-SUTOTBV-PER        TO FSG5-SUTOTBV-PER                      
019300     ADD SPAR-SUTOTBV-AAR        TO FSG5-SUTOTBV-AAR                      
019400     ADD SPAR-SUTOTBV-FAAR       TO FSG5-SUTOTBV-FAAR                     
019500     ADD SPAR-SUTOTBV-RAAR       TO FSG5-SUTOTBV-RAAR                     
019600     ADD SPAR-SUTOTBV-FRAAR      TO FSG5-SUTOTBV-FRAAR                    
019700     .                                                                    
019800                                                                          
019900     EJECT                                                                
020000 Z-FINIT SECTION.                                                         
020100                                                                          
020200     CLOSE W33026                                                         
020300           W33032                                                         
020400                                                                          
020500     MOVE 'S'      TO POSTSUM-OPKOD                                       
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     .                                                                    
020800     SKIP3                                                                
020900 S01-LAS-INFIL        SECTION.                                            
021000                                                                          
021100     READ W33026       INTO IN-AREA                                       
021200       AT END                                                             
021300          MOVE NEJ TO INFIL-POST-SW                                       
021400     END-READ                                                             
021500                                                                          
021600     IF NOT INFIL-POST-SAKNAS                                             
021700         MOVE W33026-TRANSID TO POSTSUM-TRANSID                           
021800         CALL POSTSUM USING POSTSUM-PARM                                  
021900     END-IF                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 S02-SKRIV-FSG5 SECTION.                                                  
022300                                                                          
022400     IF   FSG5-IDPROMR NOT = SPACE                                        
022500       WRITE W33032-POST FROM FSG5-AREA                                   
022600                                                                          
022700       MOVE W33032-TRANSID TO POSTSUM-TRANSID                             
022800       CALL POSTSUM USING POSTSUM-PARM                                    
022900     END-IF                                                               
023000     .                                                                    
