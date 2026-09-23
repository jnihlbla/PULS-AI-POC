000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W0301200.                                                 
000400 AUTHOR.        JAN MELANDER OCH SVANTE BJÖRKBERG                         
000500 DATE-WRITTEN.  DEC 1985 JAN O SVANTE                                     
000600                DEC 1986 LARS C                                           
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        SKAPAR ETT DATUMKORT INNEHÅLLANDE DATUM                          
001200*        PERIOD (START,SLUT), VECKA (START,SLUT),                         
001300*        SAMT UPPGIFTER OM BL.A:                                          
001400*        VECKO-START -SLUT, PERIOD-START -SLUT,                           
001500*        MÅNADSLUT, BOKFÖRINGSPERIODSLUT ETC.                             
001600*                                                                         
001700*        VIDARE FÖRKLARINGAR SE COPTEXTEN DATUMKORT (WDATKORTC0)          
001800*                                                                         
001900*                                                                         
002000*    SUBPROGRAM 1:                                                        
002100*        WDATKONV - DAT-WDATAREA, PROGRAMMET KONVERTERAR DATUM            
002200*                 TILL OLIKA FORMAT.                                      
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 - VID FELAKTIG ANGIVNING AV DATUM.                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*--- INFILER:                                                             
003400                                                                          
003500     SELECT IN-SOPCAL                ASSIGN TO  W03012D1.                 
003600     SKIP2                                                                
003700*--- UTFILER:                                                             
003800                                                                          
003900     SELECT UT-DATUMKORT             ASSIGN TO  W03012D2.                 
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400 FD  IN-SOPCAL                                                            
004500     LABEL RECORD   STANDARD                                              
004600     RECORDING      F                                                     
004700     BLOCK CONTAINS 0.                                                    
004800     SKIP2                                                                
004900 01  FILLER                      PIC X(250).                              
005000     SKIP2                                                                
005100 FD  UT-DATUMKORT                                                         
005200     LABEL RECORD   STANDARD                                              
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*    -COPY WDATKORT                                                       
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
005901*    - CHECKED BY WY2000                                                  
005910     SKIP3                                                                
006000*                                                                         
006100 01  TABELLER.                                                            
006200     02  TABELL  OCCURS 25  INDEXED BY IX-TAB PIC X(13).                  
006300*                                                                         
006400 01  RETURKODER.                                                          
006500*                                                                         
006600     03  RKOD                    PIC S9(4)   COMP SYNC VALUE ZERO.        
006700     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP SYNC VALUE +16.         
006800                                                                          
006900*                                                                         
007000 01  DAGENS-DATUM                PIC X(6).                                
007100 01  AARDELNING.                                                          
007200     02 FILLER                   PIC X(1).                                
007300     02 DEL-K-AAR                PIC S9(1).                               
007400*                                                                         
007500 01  EOF-SWITCHAR.                                                        
007600     02 IN-SOPCAL-EOF            PIC X(1)  VALUE 'N'.                     
007700*                                                                         
007800 01  GENERELLA-KONSTANTER.                                                
007900     02 JA                       PIC X(1)  VALUE 'J'.                     
008000     02 NEJ                      PIC X(1)  VALUE 'N'.                     
008100*                                                                         
008200                                                                          
008300 01  TEMP-AAR                    PIC 9(2).                                
008400 01  TEMP-VECKA                  PIC 9(2).                                
008500 01  TEMP-DAT-TIP                PIC 9(1).                                
008600 01  TEMP-DAT-TIVV               PIC 9(2).                                
008700 01  TEMP-DAT-TIMM               PIC 9(2).                                
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009400     EJECT                                                                
009500 01  SOPCAL-AREA.                                                         
009600*                                                                         
009700     02  DAT-AREA                PIC 9(6)    VALUE ZERO.                  
009800     02  TEXT-AREA               PIC X(244)  VALUE SPACE.                 
009900     EJECT                                                                
010000*    -COPY WDATAREA                                                       
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010500 01  EXEC-PARM.                                                           
010600     03 LAENGD                  PIC S9(4) COMP.                           
010700     03 PARM-DATUM              PIC X(6).                                 
010800     EJECT                                                                
010900 PROCEDURE DIVISION USING EXEC-PARM.                                      
011000                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300     PERFORM S11-LAS-IN-SOPCAL                                            
011400     PERFORM B-CHECK-PARM-DATUM                                           
011600     PERFORM UNTIL DAGENS-DATUM = DAT-AREA                                
011610       PERFORM S11-LAS-IN-SOPCAL                                          
011620     END-PERFORM                                                          
011700     PERFORM C-FLYTTA-BERAKNA-UTFIL                                       
011800     PERFORM D-FLYTTA-KALENDEWINFO                                        
011900     PERFORM Z-FINIT                                                      
012000     MOVE RKOD TO RETURN-CODE                                             
012100     GOBACK                                                               
012200     .                                                                    
012300     EJECT                                                                
012400 A-INIT SECTION.                                                          
012500                                                                          
012600     OPEN INPUT  IN-SOPCAL                                                
012700     OPEN OUTPUT UT-DATUMKORT                                             
012800     MOVE SPACE                  TO DATUMKORT                             
012900     .                                                                    
013000     EJECT                                                                
013100 B-CHECK-PARM-DATUM SECTION.                                              
013200     EVALUATE LAENGD                                                      
013300     WHEN 0                                                               
013400        ACCEPT DAGENS-DATUM FROM DATE                                     
013500     WHEN 6                                                               
013600        IF PARM-DATUM IS NUMERIC                                          
013700           MOVE PARM-DATUM       TO DAGENS-DATUM                          
013800        ELSE                                                              
013900           DISPLAY 'DATUM ANGIVEN I JCL:EN ÄR EJ NUMERISK.'               
014000           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
014100        END-IF                                                            
014200     WHEN OTHER                                                           
014300        DISPLAY 'DATUM ANGIVEN I JCL:EN ÄR INTE 6 POS LÅNG.'              
014400        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
014500     END-EVALUATE                                                         
014600     EJECT                                                                
014700     .                                                                    
014800 C-FLYTTA-BERAKNA-UTFIL SECTION.                                          
014900                                                                          
015000     MOVE 'DATE9'              TO DATUM                                   
015100     MOVE 'WDATUM'             TO FAELT2                                  
015200     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
015300     MOVE DAGENS-DATUM         TO DAT-I-TIDATUM                           
015400                                                                          
015500     CALL WDATKONV USING DAT-KDDATFORM                                    
015600                         DAT-I-TIDATUM                                    
015700                         DAT-O-TIDATUM                                    
015800                         DAT-KDSVAR                                       
015900                                                                          
016000     MOVE DAT-TIAA             TO D-AAR                                   
016100     MOVE DAT-TIMM             TO D-MAANAD                                
016200     MOVE DAT-TIDD             TO D-DAG                                   
016300     MOVE DAT-TIVV             TO D-VECKA                                 
016400     MOVE DAT-TID              TO D-DAGNR                                 
016500     MOVE DAT-TIP              TO D-PERIOD                                
016600     MOVE DAT-TIAA             TO AARDELNING                              
016700     MOVE DEL-K-AAR            TO K-AAR                                   
016800     MOVE DAT-TIVV             TO K-VECKA                                 
016900     MOVE DAT-TIP              TO K-PERIOD                                
017000     MOVE DAT-TIAADDD          TO D-IOCSDAT                               
017100     EJECT                                                                
017200     MOVE 'AAP'                TO DAT-KDDATFORM                           
017300     MOVE DAT-TIAAP            TO DAT-I-TIDATUM                           
017400     CALL WDATKONV USING DAT-KDDATFORM                                    
017500                         DAT-I-TIDATUM                                    
017600                         DAT-O-TIDATUM                                    
017700                         DAT-KDSVAR                                       
017800                                                                          
017900                                                                          
018000     IF DAT-TIAA-VECKA  NOT = DAT-TIAA-PPER                               
018100*    -- OLIKA ÅRTAL PÅ PERIODEN OCH PERIODENS 1:A VECKA                   
018200*    -- (1:A VECKAN STARTADE FÖRRA ÅRET)                                  
018300          IF D-VECKA > 30                                                 
018400*        -- DET ÄR SISTA DAGARNA PÅ ÅRET                                  
018500             MOVE 1 TO K-STATVECKA                                        
018600          ELSE                                                            
018700*        -- DET ÄR I BÖRJAN PÅ ÅRET                                       
018800            MOVE D-VECKA  TO K-STATVECKA                                  
018900          END-IF                                                          
019000     ELSE                                                                 
019100        COMPUTE K-STATVECKA = D-VECKA - DAT-TIVV + 1                      
019200        IF D-PERIOD = 5                                                   
019300           EVALUATE K-STATVECKA                                           
019400             WHEN 0                                                       
019500*               -- (10 TRUNKERAT TILL 0)                                  
019600               MOVE 6              TO K-STATVECKA                         
019700             WHEN 3 THRU 7                                                
019800               MOVE 3              TO K-STATVECKA                         
019900             WHEN 8 THRU 9                                                
020000               COMPUTE K-STATVECKA = K-STATVECKA - 4                      
020100           END-EVALUATE                                                   
020200        END-IF                                                            
020300        IF K-STATVECKA > 6                                                
020400           MOVE 6                 TO K-STATVECKA                          
020500        END-IF                                                            
020600     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 D-FLYTTA-KALENDEWINFO SECTION.                                           
021000                                                                          
021100     UNSTRING TEXT-AREA DELIMITED BY ALL SPACE INTO TABELL(1)             
021200          TABELL(2) TABELL(3) TABELL(4) TABELL(5) TABELL(6)               
021300          TABELL(7) TABELL(8) TABELL(9) TABELL(10) TABELL(11)             
021400          TABELL(12) TABELL(13) TABELL(14) TABELL(15) TABELL(16)          
021500          TABELL(17) TABELL(18) TABELL(19) TABELL(20) TABELL(21)          
021600          TABELL(22) TABELL(23) TABELL(24) TABELL(25)                     
021700                                                                          
021800     SET IX-TAB TO 1                                                      
021900     SEARCH TABELL                                                        
022000     AT END                                                               
022100       MOVE 0  TO  K-VECKASLUT                                            
022200     WHEN TABELL(IX-TAB) = 'WEEK-1'                                       
022300       MOVE 1  TO     K-VECKASLUT                                         
022400     END-SEARCH                                                           
022500     SKIP2                                                                
022600                                                                          
022700     SET IX-TAB TO 1                                                      
022800     SEARCH TABELL                                                        
022900       AT END                                                             
023000       MOVE '0' TO K-VECKOSTART                                           
023100     WHEN TABELL(IX-TAB) = 'WEEK1'                                        
023200       MOVE '1' TO    K-VECKOSTART                                        
023300     END-SEARCH                                                           
023400     SKIP2                                                                
023500                                                                          
023600     SET IX-TAB TO 1                                                      
023700     SEARCH TABELL                                                        
023800       AT END                                                             
023900       MOVE '0' TO K-MANADSLUT                                            
024000     WHEN TABELL(IX-TAB) = 'MONTH-1'                                      
024100       MOVE '1' TO    K-MANADSLUT                                         
024200     END-SEARCH                                                           
024300     EJECT                                                                
024400                                                                          
024500     SET IX-TAB TO 1                                                      
024600     SEARCH TABELL                                                        
024700       AT END                                                             
024800       MOVE 0  TO  K-PERSLUT                                              
024900     WHEN TABELL(IX-TAB) = 'PROGRAM-1'                                    
025000       MOVE 1  TO     K-PERSLUT                                           
025100     END-SEARCH                                                           
025200     SKIP2                                                                
025300                                                                          
025400     SET IX-TAB TO 1                                                      
025500     SEARCH TABELL                                                        
025600       AT END                                                             
025700       MOVE 0  TO  K-NYPER                                                
025800     WHEN TABELL(IX-TAB) = 'PROGRAM1'                                     
025900       MOVE 1  TO     K-NYPER                                             
026000     END-SEARCH                                                           
026100     SKIP2                                                                
026200                                                                          
026300     SET IX-TAB TO 1                                                      
026400     SEARCH TABELL                                                        
026500       AT END                                                             
026600       MOVE '0' TO K-PERSLUT-BOKF                                         
026700     WHEN TABELL(IX-TAB) = 'ACCOUNT-1'                                    
026800       MOVE '1' TO    K-PERSLUT-BOKF                                      
026900     END-SEARCH                                                           
027000     SKIP2                                                                
027100     .                                                                    
027200 Z-FINIT SECTION.                                                         
027300                                                                          
027400     WRITE DATUMKORT                                                      
027500     CLOSE IN-SOPCAL                                                      
027600     CLOSE UT-DATUMKORT                                                   
027700     .                                                                    
027800     EJECT                                                                
027900 S11-LAS-IN-SOPCAL SECTION.                                               
028000                                                                          
028100     READ IN-SOPCAL INTO SOPCAL-AREA                                      
028200       AT END                                                             
028300           MOVE JA TO IN-SOPCAL-EOF                                       
028400     END-READ                                                             
028500     .                                                                    
