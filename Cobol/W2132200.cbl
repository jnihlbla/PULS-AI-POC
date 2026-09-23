000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2132200.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   10/12/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LEVERANTÖRREGISTER PÅ 'PRÄNT'                                    
000900*        KATASTROFRUTIN                                                   
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDF1                                       
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
002500*          --- LEVERANTÖRREGISTER                                         
002600     SELECT W21323                     ASSIGN TO W21322D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W21323                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  UT-POST  -COPY  W21323   -L.                                         
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W2132200'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES DAGENS-DATUM.                                       
004600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004900 01  ANTAL-ATT                   PIC S9(3).                               
005000 01  ANTAL-RAD                   PIC S9(3).                               
005100 01  ANTAL-SID                   PIC S9(5).                               
005200 01  IX                          PIC S9(3).                               
005300 01  FL-SKRIV-RADER              PIC X   VALUE 'N'.                       
005400 01  FL-RAD-DEL1-OK              PIC X   VALUE 'N'.                       
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  UT-AREA-START               PIC X(24)   VALUE                        
007800                                 'UT-AREA-START  '.                       
007900     SKIP2                                                                
008000 01  UT-AREA.                                                             
008100*    03  -COPY   W21323    -PRE  UT-                                      
008200                                                                          
008300                                                                          
008400     EJECT                                                                
008500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000     03  W-IDLEVNR-X.                                                     
009100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
009200     03  W-IDLEVSUF-X.                                                    
009300         05  W-IDLEVSUF          PIC S9(1)   VALUE ZERO COMP-3.           
009400     03  W-IDATTENT-X.                                                    
009500         05  W-IDATTENT          PIC S9(3)   VALUE ZERO COMP-3.           
009600     SKIP2                                                                
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FINNS                       VALUE '  '.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF1'.                        
011300 01  DLI-IO-WDF1.                                                         
011400     03  WDF1-AREA            PIC X(280).                                 
011500     03  FILLER   REDEFINES WDF1-AREA.                                    
011600*        05 -COPY  WDF101                                                 
011700     03  FILLER   REDEFINES WDF1-AREA.                                    
011800*        05 -COPY  WDF106                                                 
011900     03  FILLER   REDEFINES WDF1-AREA.                                    
012000*        05 -COPY  WDF107                                                 
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400                                                                          
012500*01  -COPY W0008  -PRE WDF1-                                              
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING WDF1-PCB.                                      
012900 MAIN SECTION.                                                            
013000     ENTRY 'DLITCBL' USING WDF1-PCB.                                      
013100                                                                          
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     MOVE NEJ      TO FL-SKRIV-RADER                                      
013600     MOVE ZERO     TO ANTAL-RAD                                           
013700                      ANTAL-ATT                                           
013800     PERFORM IMS-GET-WDF1                                                 
013900     PERFORM UNTIL SEGMENT-SAKNAS                                         
014000       EVALUATE WDF1-SEG-NAME-FB                                          
014100         WHEN 'WDF101'                                                    
014200           IF FL-SKRIV-RADER = JA                                         
014300              PERFORM S01-SKRIV-RADER                                     
014400           END-IF                                                         
014500           MOVE ZERO           TO ANTAL-ATT                               
014600           PERFORM S05-NOLLA                                              
014700           MOVE LEV-IDLEVNR    TO UT-IDLEVNR                              
014800           MOVE JA             TO FL-SKRIV-RADER                          
014900         WHEN 'WDF106'                                                    
015000           PERFORM B-LEVDATA                                              
015100         WHEN 'WDF107'                                                    
015200           PERFORM C-ATT                                                  
015300       END-EVALUATE                                                       
015400       PERFORM IMS-GET-WDF1                                               
015500     END-PERFORM                                                          
015600     PERFORM Z-FINIT                                                      
015700                                                                          
015800     MOVE ZERO TO RETURN-CODE                                             
015900     GOBACK                                                               
016000     .                                                                    
016100     EJECT                                                                
016200 A-INIT SECTION.                                                          
016300                                                                          
016400     OPEN OUTPUT W21323                                                   
016500                                                                          
016600     ACCEPT DAGENS-DATUM  FROM DATE                                       
016700     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
016800     .                                                                    
016900     EJECT                                                                
017000 B-LEVDATA SECTION.                                                       
017100                                                                          
017200        MOVE ADR-BELEV         TO UT-BELEV                                
017300        MOVE ADR-ADLEV-RAD1    TO UT-ADLEV-RAD1                           
017400        MOVE ADR-ADLEV-RAD2    TO UT-ADLEV-RAD2                           
017500        MOVE ADR-ADLEV-ORT     TO UT-ADLEV-ORT                            
017600        MOVE ADR-ADLEVLND      TO UT-ADLEVLND                             
017700        MOVE ADR-IDLEVTLF      TO UT-IDLEVTLF                             
017900     .                                                                    
018000     EJECT                                                                
018100 C-ATT     SECTION.                                                       
018200                                                                          
018210     IF ATT-BELEV = SPACE AND                                             
018220        ATT-IDLEVTLF-KLEV = SPACE AND                                     
018230        ATT-IDMAIL = SPACE AND                                            
018240        ATT-TENOTE = SPACE                                                
018250        CONTINUE                                                          
018260     ELSE                                                                 
018300       IF ANTAL-ATT < +2                                                  
018400          IF ANTAL-ATT = ZERO                                             
018500                MOVE ATT-IDATTENT      TO UT-IDATTENT(1)                  
018600                MOVE ATT-BELEV         TO UT-ATT-BELEV(1)                 
018700                MOVE ATT-IDLEVTLF-KLEV TO UT-IDLEVTLF-KLEV(1)             
018710                MOVE ATT-IDMAIL        TO UT-IDMAIL(1)                    
018720                MOVE ATT-TENOTE        TO UT-TENOTE(1)                    
018800          END-IF                                                          
018900          IF ANTAL-ATT = +1                                               
019000                MOVE ATT-IDATTENT      TO UT-IDATTENT(2)                  
019010                MOVE ATT-BELEV         TO UT-ATT-BELEV(2)                 
019020                MOVE ATT-IDLEVTLF-KLEV TO UT-IDLEVTLF-KLEV(2)             
019030                MOVE ATT-IDMAIL        TO UT-IDMAIL(2)                    
019040                MOVE ATT-TENOTE        TO UT-TENOTE(2)                    
019200          END-IF                                                          
019300          ADD +1  TO ANTAL-ATT                                            
019400       END-IF                                                             
019410     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 Z-FINIT SECTION.                                                         
019800                                                                          
019900     IF FL-SKRIV-RADER = JA                                               
020000        PERFORM S01-SKRIV-RADER                                           
020100     END-IF                                                               
020200     CLOSE W21323                                                         
020300     SKIP2                                                                
020400     MOVE 'S' TO POSTSUM-OPKOD                                            
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600     .                                                                    
020700     EJECT                                                                
020800 S01-SKRIV-RADER  SECTION.                                                
020900                                                                          
021000     PERFORM S11-SKRIV-W21323                                             
021100     .                                                                    
021200     EJECT                                                                
021300 S05-NOLLA        SECTION.                                                
021400                                                                          
021500     MOVE SPACE             TO UT-BELEV                                   
021600     MOVE SPACE             TO UT-ADLEV-RAD1                              
021700     MOVE SPACE             TO UT-ADLEV-RAD2                              
021800     MOVE SPACE             TO UT-ADLEV-ORT                               
021900     MOVE SPACE             TO UT-ADLEVLND                                
022000     MOVE SPACE             TO UT-IDLEVTLF                                
022200     MOVE ZERO              TO UT-IDATTENT(1)                             
022210                               UT-IDATTENT(2)                             
022300     MOVE SPACE             TO UT-ATT-BELEV(1)                            
022320                               UT-ATT-BELEV(2)                            
022330                               UT-IDLEVTLF-KLEV(1)                        
022331                               UT-IDLEVTLF-KLEV(2)                        
022340                               UT-IDMAIL(1)                               
022341                               UT-IDMAIL(2)                               
022350                               UT-TENOTE(1)                               
022360                               UT-TENOTE(2)                               
023200     .                                                                    
023300     EJECT                                                                
023400 S11-SKRIV-W21323 SECTION.                                                
023500                                                                          
023600     WRITE UT-POST FROM UT-AREA                                           
023700                                                                          
023800     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
023900     MOVE 'W21323'   TO POSTSUM-FDNAMN                                    
024000     MOVE 'W21322D1' TO POSTSUM-DDNAMN2                                   
024100     CALL POSTSUM USING POSTSUM-PARM                                      
024200     .                                                                    
024300     EJECT                                                                
024400 S99-ABEND SECTION.                                                       
024500                                                                          
024600     SKIP2                                                                
024700     MOVE 'S' TO POSTSUM-OPKOD                                            
024800     CALL POSTSUM USING POSTSUM-PARM                                      
024900     CALL ABEND USING RKOD-ABEND                                          
025000     .                                                                    
025100     EJECT                                                                
025200* --- IMS SEKTIONER ---                                                   
025300                                                                          
025400                                                                          
025500 IMS-GET-WDF1   SECTION.                                                  
025600                                                                          
025700     CALL CBLTDLI USING GN WDF1-PCB DLI-IO-WDF1                           
025800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
025900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
026000     PERFORM IMS-STATUSKONTROLL                                           
026100     .                                                                    
026200     EJECT                                                                
026300 IMS-STATUSKONTROLL SECTION.                                              
026400                                                                          
026500     SET STATUS-IX TO 1                                                   
026600     SEARCH GODK-STATUS                                                   
026700       AT END                                                             
026800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026900           DELIMITED BY SIZE INTO FELTEXT                                 
027000         DISPLAY FELTEXT                                                  
027100         CALL FELLOG                                                      
027200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027300         CONTINUE                                                         
027400     END-SEARCH                                                           
027500     .                                                                    
