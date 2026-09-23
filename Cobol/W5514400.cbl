000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5514400.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   94/05/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER NER WDF1 OCH SORTERAR UTFILEN PÅ IDLEVNR.                  
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- FIL MED TULLFAKTOR                                         
002600     SELECT W55144                     ASSIGN TO W55144D1.                
002700     SKIP2                                                                
002800*          --- SORTERINGSFIL                                              
002900     SELECT SORTFIL                    ASSIGN TO W55144DS.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W55144                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900*01  POST -COPY W55144 -PRE  UT-  -L.                                     
004000     SKIP3                                                                
004100 SD  SORTFIL                                                              
004200     RECORDING       F                                                    
004300     SKIP2                                                                
004400*01  POST -COPY W55144 -PRE  SORT-                                        
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W5514400'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  FLAGGA-02-SEG               PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
005600     88  SKRIV-POST                          VALUE 'J'.                   
005700     EJECT                                                                
005800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES DAGENS-DATUM.                                       
006000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006300     EJECT                                                                
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     SKIP2                                                                
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
007600 01  FELTEXT.                                                             
007700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
008500                                 'SORTWS-AREA-START'.                     
008600     SKIP2                                                                
008700                                                                          
008800*01  AREA -COPY W55144     -PRE SORTWS-                                   
008900     SKIP2                                                                
009000 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
009100     EJECT                                                                
009200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009300*                                                                         
009400     SKIP2                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700 01  NYCKLAR-TILL-DLI.                                                    
009800     03  W-IDLEVNR-X.                                                     
009900         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
010000     SKIP2                                                                
010100*    --- STATUS-KOD FRÅN IMS                                              
010200 01  STATUS-WS                   PIC XX.                                  
010300     88  SEGMENT-FINNS                       VALUE '  '.                  
010400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010500     SKIP2                                                                
010600 01  GODK-STATUSKODER.                                                    
010700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     SKIP3                                                                
010900 01  SSA1                        PIC X(64).                               
011000 01  SSA2                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNKTIONSKODER                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011700     SKIP3                                                                
011800 01  DLI-IO-AREA.                                                         
011900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012000     SKIP3                                                                
012100     03  WLLEVA01 REDEFINES IO-AREA.                                      
012200*        05  -COPY WDF101  -PRE LEVA01-                                   
012300     SKIP3                                                                
012400     03  WLLEVA02 REDEFINES IO-AREA.                                      
012500*        05  -COPY WDF102  -PRE LEVA02-                                   
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900     SKIP2                                                                
013000*01  -COPY W0008  -PRE LEVA-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013300 PROCEDURE DIVISION  USING LEVA-PCB.                                      
013400     ENTRY 'DLITCBL' USING LEVA-PCB.                                      
013500                                                                          
013600     SKIP2                                                                
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     SORT SORTFIL ASCENDING KEY SORT-IDLEVNR                              
014000                  INPUT PROCEDURE B-SORT-INPUT                            
014100                  GIVING W55144                                           
014200                                                                          
014300     IF SORT-RETURN NOT = 0                                               
014400       MOVE SORT-RETURN TO SORT-RETURN-X                                  
014500       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
014600           DELIMITED BY SIZE                                              
014700           INTO FELTEXT-STR                                               
014800       DISPLAY FELTEXT                                                    
014900       PERFORM S99-ABEND                                                  
015000     ELSE                                                                 
015100       PERFORM Z-FINIT                                                    
015200                                                                          
015300       MOVE ZERO TO RETURN-CODE                                           
015400       GOBACK                                                             
015500     END-IF                                                               
015600                                                                          
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000     SKIP2                                                                
016100     ACCEPT DAGENS-DATUM  FROM DATE                                       
016200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016300     PERFORM AA-NOLLSTAELL-UTPOST                                         
016400     .                                                                    
016500     EJECT                                                                
016600 AA-NOLLSTAELL-UTPOST SECTION.                                            
016700     SKIP2                                                                
016800     MOVE ZERO TO  SORTWS-KDVALLEV                                        
016900                  SORTWS-RETULF-1                                         
017000                  SORTWS-RETULF-2                                         
017100                  SORTWS-TITULF                                           
017200     MOVE SPACE TO SORTWS-IDLEVNR                                         
017300     .                                                                    
017400     EJECT                                                                
017500 B-SORT-INPUT SECTION.                                                    
017600     SKIP2                                                                
017700     PERFORM IMS-GET-WDF1                                                 
017800     PERFORM UNTIL SEGMENT-SLUT                                           
017900       EVALUATE LEVA-SEG-NAME-FB                                          
018000         WHEN 'WDF101  '                                                  
018100           IF SKRIV-POST AND FLAGGA-02-SEG = JA                           
018200             PERFORM S31-RELEASE-W55144                                   
018300             PERFORM AA-NOLLSTAELL-UTPOST                                 
018400           END-IF                                                         
018500           IF LEVA01-LEV-IDLEVNR NOT = SPACE                              
018600             MOVE LEVA01-LEV-IDLEVNR     TO SORTWS-IDLEVNR                
018700             MOVE JA                     TO SKRIV-SW                      
018800           ELSE                                                           
018900             MOVE NEJ TO SKRIV-SW                                         
019000           END-IF                                                         
019100           MOVE NEJ TO FLAGGA-02-SEG                                      
019200         WHEN 'WDF102  '                                                  
019300           IF LEVA02-TULL-IDLANDX2 = 'SE'                                 
019400             IF SKRIV-POST                                                
019500               MOVE LEVA02-TULL-KDVALLEV   TO SORTWS-KDVALLEV             
019600               MOVE LEVA02-TULL-TITULF     TO SORTWS-TITULF               
019700               MOVE LEVA02-TULL-RETULF-1   TO SORTWS-RETULF-1             
019800               MOVE LEVA02-TULL-RETULF-2   TO SORTWS-RETULF-2             
019810               MOVE LEVA02-TULL-IDLANDX2   TO SORTWS-IDLANDX2             
019900             END-IF                                                       
020000             MOVE JA TO FLAGGA-02-SEG                                     
020010           END-IF                                                         
020100       END-EVALUATE                                                       
020200       PERFORM IMS-GET-WDF1                                               
020300     END-PERFORM                                                          
020400     IF SKRIV-POST                                                        
020500       PERFORM S31-RELEASE-W55144                                         
020600     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 Z-FINIT SECTION.                                                         
021000     SKIP2                                                                
021100     MOVE 'S' TO POSTSUM-OPKOD                                            
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     .                                                                    
021400     EJECT                                                                
021500 S31-RELEASE-W55144 SECTION.                                              
021600     SKIP2                                                                
021700     RELEASE SORT-POST FROM SORTWS-AREA                                   
021800                                                                          
021900     MOVE 'WDF1'  TO POSTSUM-TRANSTYP                                     
022000     MOVE 'W55143' TO POSTSUM-FDNAMN                                      
022100     MOVE 'W55143D1' TO POSTSUM-DDNAMN2                                   
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300     .                                                                    
022400     EJECT                                                                
022500 S99-ABEND SECTION.                                                       
022600     SKIP2                                                                
022700     SKIP2                                                                
022800     MOVE 'S' TO POSTSUM-OPKOD                                            
022900     CALL POSTSUM USING POSTSUM-PARM                                      
023000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
023100     .                                                                    
023200     EJECT                                                                
023300* --- IMS SEKTIONER ---                                                   
023400     SKIP3                                                                
023500     EJECT                                                                
023600 IMS-GET-WDF1   SECTION.                                                  
023700     SKIP2                                                                
023800     CALL CBLTDLI USING GN LEVA-PCB DLI-IO-AREA                           
023900     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
024000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
024100     PERFORM IMS-STATUSKONTROLL                                           
024200     .                                                                    
024300     EJECT                                                                
024400 IMS-STATUSKONTROLL SECTION.                                              
024500     SKIP2                                                                
024600     SET STATUS-IX TO 1                                                   
024700     SEARCH GODK-STATUS                                                   
024800       AT END                                                             
024900         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
025000         DISPLAY FELTEXT                                                  
025100         CALL FELLOG                                                      
025200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025300         CONTINUE                                                         
025400     END-SEARCH                                                           
025500     .                                                                    
