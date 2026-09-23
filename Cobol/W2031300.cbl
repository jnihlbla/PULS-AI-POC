000100 PROCESS DYNAM                                                            
001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W2031300.                                                
001600 AUTHOR.         NIHLBLAD JOHAN.                                          
001700 DATE-WRITTEN.   03/03/18.                                                
001800 DATE-COMPILED.                                                           
001810                                                                          
001900                                                                          
002000*    FUNCTION:                                                            
002100*        OVERVIEW FOR CAMPAIGNS                                           
002200*                                                                         
002301*        THE PROGRAM UPDATES TABLE TP1GRP                                 
002310*        THE PROGRAM UPDATES TABLE TP1KAMP                                
002320*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W2T313                                              
002700*        MID:         W2I31301                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        MOD:         W2O31301                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W2031300'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004410 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
004500                                                                          
004600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
004601*    --- INDEX FOR SCROLL LINES                                           
004602 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004610 77  MAX-INDX                    PIC S9(4)  VALUE +7    COMP SYNC.        
004620 01  IX                          PIC S9(7)  VALUE ZERO  COMP-3.           
004630 01  IX-ANTAL                    PIC S9(7)  VALUE ZERO  COMP-3.           
004700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005100                                                                          
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'J'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
005410                                                                          
005411 77  GRP-SW                      PIC X       VALUE 'J'.                   
005412     88  GRP-OK                              VALUE 'J'.                   
005413     88  GRP-WRONG                           VALUE 'N'.                   
005414                                                                          
005420 77  CMD-SW                      PIC X       VALUE 'J'.                   
005430     88  CMD-OK                              VALUE 'J'.                   
005440                                                                          
005450 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005460     88  INDATA-OK                           VALUE 'J'.                   
005470     88  INDATA-WRONG                        VALUE 'N'.                   
005500                                                                          
005510 77  INPUT-SW                    PIC X       VALUE 'J'.                   
005520     88  INPUT-OK                            VALUE 'J'.                   
005530     88  INPUT-WRONG                         VALUE 'N'.                   
005531                                                                          
005532 77  NEWPOST-SW                   PIC X       VALUE 'J'.                  
005533     88  NEWPOST-OK                           VALUE 'J'.                  
005534     88  NEWPOST-WRONG                        VALUE 'N'.                  
005535                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  OWN-MID                             VALUE '2313'.                
005800     88  2317-MID                            VALUE '2317'.                
005900     88  GOOD-MID                            VALUE '2313' '2314'          
006000                                                   '2315' '2316'.         
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006910     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007200     EJECT                                                                
007210*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
007220*01  -COPY WDATAREA                                                       
007300*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
008000 01  MESSAGE-CODES.                                                       
008010     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008020     03  CONFLICT                PIC X(3)    VALUE '002'.                 
008030     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008040     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008050     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008060     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008070     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
008080     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008090     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008091     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
008092     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
008093     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
008094     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008095     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
008096     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
008097     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
008100     EJECT                                                                
008200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008900*                                                                         
009310*    WS AREA                                                              
009362                                                                          
009424 01  WS-IDLOPNR-IDKAMP           PIC X(7)       VALUE SPACE.              
009425 01  WS-IDLOPNR-IDKAMP-NUM       PIC 9(7)       VALUE ZERO.               
009426 01  W-IDLOPNR-KAMP              PIC S9(7)      VALUE ZERO COMP-3.        
009427 01  W-IDKAMP                    PIC X(7)       VALUE SPACE.              
009428 01  WS-IDKAMP                   PIC X(7)       VALUE SPACE.              
009429 01  WS-IDKAMP-DEL               PIC X(7)       VALUE SPACE.              
009430 01  W-IDKAMP-GRP                PIC S9(7)      VALUE ZERO COMP-3.        
009431 01  W-KDKAMP                    PIC X          VALUE SPACE.              
009432 01  W-RERESPRT                  PIC 9(3)       VALUE ZERO.               
009433 01  WS-RERESPRT2                PIC 9(3)       VALUE ZERO.               
009434 01  WS-RERESPRT-UPDATE          PIC S9(1)V9(2) VALUE ZERO COMP-3.        
009435 01  WS-KVKAMP-CARS-TOT          PIC S9(7)      VALUE ZERO COMP-3.        
009436 01  WS-TISTADAT-KAMP            PIC S9(7)      VALUE ZERO COMP-3.        
009437 01  WS-TISTODAT-KAMP            PIC S9(7)      VALUE ZERO COMP-3.        
009438 01  W-TISTADAT-KAMP            PIC  9(6)      VALUE ZERO.                
009439 01  W-TISTODAT-KAMP            PIC  9(6)      VALUE ZERO.                
009441 01  W-IDARTNR                   PIC S9(9)      VALUE ZERO COMP-3.        
009442 01 FILLER                       PIC X(16)   VALUE                        
009443                                             'WS-DB2-SEKTION'.            
009444 01  WS-DB2-SEKTION              PIC X(27) VALUE SPACE.                   
009445 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
009446                                                                          
009447 01  WS-DATUM                    PIC 9(8)    VALUE ZERO.                  
009448 01  FILLER   REDEFINES WS-DATUM.                                         
009449     03 WS-SEKEL                 PIC 9(2).                                
009450     03 WS-DATUM-AAR             PIC 9(2).                                
009451     03 WS-DATUM-MAN             PIC 9(2).                                
009452     03 WS-DATUM-DAG             PIC 9(2).                                
009453                                                                          
009460*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W2I31301                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400     03  MOD REDEFINES MSG-AREA.                                          
010500*      05  -COPY W2O31301                                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010800     SKIP3                                                                
010801 01  W-BEKAMNOT-TP1GRP            PIC X(158) VALUE SPACE.                 
010810 01  FILLER  REDEFINES W-BEKAMNOT-TP1GRP.                                 
010820     03 NOTE1-TP1GRP              PIC X(79).                              
010830     03 NOTE2-TP1GRP              PIC X(79).                              
010850     SKIP3                                                                
010851 01  W-BEKAMNOT-TP1KAMP           PIC X(158).                             
010860 01  FILLER  REDEFINES W-BEKAMNOT-TP1KAMP.                                
010870     03 NOTE1-TP1KAMP             PIC X(79).                              
010880     03 NOTE2-TP1KAMP             PIC X(79).                              
010910 01  SAVE-AREA.                                                           
010920     03  SAVE-IDTRANS              PIC X(4)    VALUE '2313'.              
010930     03  SAVE-IDKAMP-ENTER         PIC X(7).                              
010940     03  SAVE-IDKAMP-NEXT          PIC X(7).                              
010950     03  WS-IDKAMP-GRP             PIC 9(7)    VALUE ZERO.                
010960     03  KAMP-QS.                                                         
010970         05 WS-KDKAMP            OCCURS 7  PIC X   VALUE SPACE.           
010980         05 WS-IDKAMP-RAD-DEL    OCCURS 7  PIC X(7) VALUE SPACE.          
010990     03  TID-QS.                                                          
010991         05 WS-TISTADAT-KAMP-RAD OCCURS 7  PIC 9(6) VALUE ZERO.           
010992         05 WS-TISTODAT-KAMP-RAD OCCURS 7  PIC 9(6) VALUE ZERO.           
010993     03  WS-RERESPRT                       PIC 9(3) VALUE ZERO.           
010994     03  WS-NOTE1                          PIC X(79) VALUE SPACE.         
010995     03  WS-NOTE2                          PIC X(79) VALUE SPACE.         
010996     EJECT                                                                
010997*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  KEYS-TO-DLI.                                                         
011601*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011602     03  W-IDKAMP-MIN-X.                                                  
011603         05  W-IDKAMP-MIN     PIC X(7).                                   
011604                                                                          
011605     03  W-IDKAMP-GRP-MIN-X.                                              
011606         05  W-IDKAMP-GRP-MIN   PIC S9(7)        COMP-3.                  
011610                                                                          
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNCTION CODES                                               
013100*01  -COPY W0003                                                          
013201     EJECT                                                                
013202 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
013203       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013204                                                                          
013205 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
013206 01  DB2-WS.                                                              
013207     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
013208         88  CURSOR-OK                       VALUE 000.                   
013209         88  LINES-FOUND                     VALUE 000.                   
013210         88  LINES-MISSING                   VALUE 100.                   
013211         88  RESOURCE-WRONG                  VALUE 904.                   
013212     03  GOOD-SQLCODECODES.                                               
013213         05  GOOD-SQLCODE OCCURS 5                                        
013220             INDEXED BY SQLCODE-IX PIC 9(3).                              
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013701     EJECT                                                                
013702 01  FILLER                      PIC X(16)  VALUE 'TP1GRP-AREA'.          
013703                                                                          
013704*01  -COPY TP1GRP -PRE TP1GRP-                                            
013705     EJECT                                                                
013706 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
013707                                                                          
013710*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
013720 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
013730*01  -COPY TP1ARTK  -PRE TP1ARTK-                                         
013801     EJECT                                                                
013802     EXEC SQL INCLUDE TP1GRP END-EXEC.                                    
013803     EJECT                                                                
013810     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
013811     EJECT                                                                
013830     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014200*01  -COPY W0008   -PRE WDP7-                                             
014300     05  FILLER                  PIC X.                                   
014400 EJECT                                                                    
014500                                                                          
014601 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB.                              
014602 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB.                              
014700                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FOUND                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-CHECK-KEYS                                               
015300       IF KEYS-OK                                                         
015400           IF MFS-UPDATE                                                  
015500              PERFORM G-CHECK-INPUT                                       
015502              IF INDATA-OK                                                
015504                 PERFORM H-UPDATE                                         
015505              END-IF                                                      
015506           ELSE                                                           
015507              IF MFS-FIRST                                                
015509                PERFORM C-FIRST-PAGE                                      
015512              ELSE                                                        
015513                IF MFS-NEXT                                               
015515                  PERFORM D-NEXT-PAGE                                     
015516                ELSE                                                      
015520                  PERFORM E-SAME-PAGE                                     
015522                END-IF                                                    
015523              END-IF                                                      
015530           END-IF                                                         
015800         PERFORM F-READ-SHOW-INFO                                         
015900       END-IF                                                             
015910       MOVE ALL '+'          TO MSGI-WMSGINIT                             
015920       MOVE '001'            TO MSGI-KDCALL                               
015930       MOVE MSG-SIGNON-USERID                                             
015940                             TO MSGI-IDUSER                               
015950       MOVE MSG-LTERM-NAME                                                
015960                             TO MSGI-IDLTERM-USER                         
015970       MOVE '2313'           TO MSGI-IDTRANS                              
015980       MOVE W-IDKAMP         TO MSGI-IDKAMP                               
015990       MOVE W-IDKAMP-GRP                                                  
015991                             TO MSGI-IDKAMP-GRP                           
015992       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
015993                                                                          
015994       MOVE '002'      TO MSGI-KDCALL                                     
015995       MOVE MSG-LTERM-NAME                                                
015996                             TO MSGI-IDLTERM-USER                         
015997       MOVE MSG-SIGNON-USERID                                             
015998                             TO MSGI-IDUSER                               
015999       MOVE '2313'   TO MSGI-IDTRANS                                      
016000       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
016001       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
016010                                                                          
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31301-CTX + 4                  
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DOUBLE-TRANSACTIONS                                           
017400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I31301                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I31301                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W2O313N1' TO MFS-IDMOD                                         
018900     MOVE '2313' TO MOD-IDTRANS                                           
019000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF OWN-MID OR HELP-MID                                               
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
019800     MOVE FUNCTION  CURRENT-DATE(1:8)  TO DAGENS-DATUM                    
019801                                                                          
019810     INITIALIZE GOOD-SQLCODECODES                                         
020000     .                                                                    
020100     EJECT                                                                
020200 B-CHECK-KEYS SECTION.                                                    
020210     MOVE SPACE TO  SAVE-IDKAMP-ENTER                                     
020211     MOVE SPACE TO  SAVE-IDKAMP-NEXT                                      
020300                                                                          
020310     MOVE MFS-ERASE-FIELD TO MOD-IDKAMP-IN                                
020320                             MOD-IDKAMP-GRP-IN                            
020330                                                                          
020400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020500     MOVE '001'             TO MSGI-KDCALL                                
020600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020800     MOVE '2313'            TO MSGI-IDTRANS                               
020900     IF OWN-MID OR 2317-MID                                               
021017         MOVE MID-IDKAMP-IN         TO MSGI-IDKAMP                        
021018         MOVE MID-IDKAMP-GRP-IN     TO MSGI-IDKAMP-GRP                    
021100     END-IF                                                               
021400                                                                          
021410     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021411     IF OWN-MID OR 2317-MID                                               
021413     AND MSGI-SPAR-AREA(1:4) = '2313'                                     
021415       MOVE MSGI-SPAR-AREA        TO SAVE-AREA                            
021416     END-IF                                                               
021420                                                                          
021500*    - LANGUAGE TO BE USED BY MEDKONV                                     
021510     MOVE +2    TO SPRAK-IX                                               
021600     MOVE 'GB ' TO MED-IDSKYLT                                            
021700                                                                          
021800     MOVE YES TO KEYS-SW                                                  
021900                                                                          
021901     MOVE MSGI-IDKAMP             TO W-IDKAMP                             
021903                                     MOD-IDKAMP-UT                        
021904                                                                          
021906                                                                          
021907     MOVE MSGI-IDKAMP-GRP         TO W-IDKAMP-GRP                         
021908                                     WS-IDKAMP-GRP                        
021909     INSPECT WS-IDKAMP-GRP REPLACING ALL SPACE BY ZERO                    
021910     MOVE WS-IDKAMP-GRP      TO MOD-IDKAMP-GRP-UT                         
021911     INSPECT MOD-IDKAMP-GRP-UT REPLACING LEADING ZERO BY SPACE            
021912                                                                          
021913     IF OWN-MID                                                           
021914       IF MID-IDKAMP-IN = ALL '+'                                         
021915         CONTINUE                                                         
021916       ELSE                                                               
021917         MOVE '7'            TO MFS-IDPFK                                 
021918         MOVE SPACE          TO MFS-KDTRTYP                               
021919         MOVE ZERO           TO W-IDKAMP-GRP                              
021920       END-IF                                                             
021921                                                                          
021922       IF MID-IDKAMP-GRP-IN = ALL '+'                                     
021923         CONTINUE                                                         
021924       ELSE                                                               
021925         MOVE '7'            TO MFS-IDPFK                                 
021926         MOVE SPACE          TO MFS-KDTRTYP                               
021927         MOVE SPACE          TO WS-IDKAMP                                 
021928                                  W-IDKAMP                                
021929       END-IF                                                             
021930                                                                          
021932       IF  MID-IDKAMP-IN NOT = ALL '+'                                    
021933       AND MID-IDKAMP-GRP-IN NOT = ALL '+'                                
021934          MOVE NOO                TO KEYS-SW                              
021937       END-IF                                                             
021938     END-IF                                                               
021939                                                                          
021940     IF W-IDKAMP-GRP NOT NUMERIC                                          
021941        MOVE NOO                  TO KEYS-SW                              
021942     END-IF                                                               
021950                                                                          
021951*                                                                         
021960*    IF  W-IDKAMP = SPACE                                                 
021970*    AND W-IDKAMP-GRP = ZERO                                              
021980*       MOVE NOO                  TO KEYS-SW                              
021990*    END-IF                                                               
022200                                                                          
022300     IF KEYS-WRONG                                                        
022400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022500       CALL WMEDKONV USING MED-WMEDAREA                                   
022600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022700       PERFORM MFS-ERASE-FIELD-IN                                         
022800       PERFORM MFS-ERASE-FIELD-OUT                                        
022900     END-IF                                                               
023000     .                                                                    
023101     EJECT                                                                
023102 C-FIRST-PAGE SECTION.                                                    
023105                                                                          
023106     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023107     CALL WMEDKONV USING MED-WMEDAREA                                     
023108     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
023110                                                                          
023111     MOVE SPACE TO  SAVE-IDKAMP-ENTER                                     
023112                    SAVE-IDKAMP-NEXT                                      
023114                                                                          
023115     PERFORM MFS-ERASE-FIELD-IN                                           
023116     .                                                                    
023117     EJECT                                                                
023118 D-NEXT-PAGE SECTION.                                                     
023119     MOVE SPACE TO  SAVE-IDKAMP-ENTER                                     
023128     .                                                                    
023129     EJECT                                                                
023130 E-SAME-PAGE SECTION.                                                     
023131                                                                          
023132     IF SAVE-IDTRANS = '2313' OR '0551'                                   
023135       IF MID-INPUT = ALL '+'                                             
023136         PERFORM MFS-ERASE-FIELD-IN                                       
023137       ELSE                                                               
023138         PERFORM MFS-READ-IN-AGAIN                                        
023139         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
023140         CALL WMEDKONV USING MED-WMEDAREA                                 
023141         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
023143         PERFORM EA-MID-INDATA-TO-MOD                                     
023144       END-IF                                                             
023145     ELSE                                                                 
023146       PERFORM MFS-ERASE-FIELD-IN                                         
023147     END-IF                                                               
023148     .                                                                    
023149     EJECT                                                                
023150 EA-MID-INDATA-TO-MOD SECTION.                                            
023151                                                                          
023152* * * * * FÖR VARJE MID-FÄLT                                              
023153* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
023154* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
023155     MOVE +1    TO INDX                                                   
023156                                                                          
023157     PERFORM UNTIL INDX > MAX-INDX                                        
023158       IF MID-CMD (INDX) = ALL '+'                                        
023159          MOVE MFS-ERASE-FIELD       TO MOD-CMD (INDX)                    
023160       ELSE                                                               
023161          MOVE MID-CMD (INDX)        TO MOD-CMD (INDX)                    
023162       END-IF                                                             
023163                                                                          
023164       IF MID-TISTADAT-KAMP-RAD (INDX) = ALL '+'                          
023165          MOVE MFS-ERASE-FIELD    TO MOD-TISTADAT-KAMP-RAD (INDX)         
023166       ELSE                                                               
023167          MOVE MID-TISTADAT-KAMP-RAD (INDX)                               
023168                                  TO MOD-TISTADAT-KAMP-RAD (INDX)         
023169       END-IF                                                             
023170                                                                          
023171       IF MID-TISTODAT-KAMP-RAD (INDX) = ALL '+'                          
023172          MOVE MFS-ERASE-FIELD    TO MOD-TISTODAT-KAMP-RAD (INDX)         
023173       ELSE                                                               
023174          MOVE MID-TISTODAT-KAMP-RAD (INDX)                               
023175                                  TO MOD-TISTODAT-KAMP-RAD (INDX)         
023176       END-IF                                                             
023177       ADD +1 TO INDX                                                     
023178     END-PERFORM                                                          
023179                                                                          
023180     IF MID-RERESPRT-IN   = ALL '+'                                       
023181        MOVE MFS-ERASE-FIELD         TO MOD-RERESPRT-IN                   
023182     ELSE                                                                 
023183        MOVE MID-RERESPRT-IN         TO MOD-RERESPRT-IN                   
023184     END-IF                                                               
023185                                                                          
023186     IF MID-NOTE1      = ALL '+'                                          
023187        MOVE MFS-ERASE-FIELD         TO MOD-NOTE1                         
023188     ELSE                                                                 
023189        MOVE MID-NOTE1               TO MOD-NOTE1                         
023190     END-IF                                                               
023191                                                                          
023192     IF MID-NOTE2      = ALL '+'                                          
023193        MOVE MFS-ERASE-FIELD         TO MOD-NOTE2                         
023194     ELSE                                                                 
023195        MOVE MID-NOTE2               TO MOD-NOTE2                         
023196     END-IF                                                               
023197                                                                          
023198     IF MID-KDKAMP       = ALL '+'                                        
023199        MOVE MFS-ERASE-FIELD         TO MOD-KDKAMP                        
023200     ELSE                                                                 
023201        MOVE MID-KDKAMP              TO MOD-KDKAMP                        
023202     END-IF                                                               
023203                                                                          
023204     IF MID-TISTADAT-KAMP = ALL '+'                                       
023205        MOVE MFS-ERASE-FIELD         TO MOD-TISTADAT-KAMP                 
023206     ELSE                                                                 
023207        MOVE MID-TISTADAT-KAMP       TO MOD-TISTADAT-KAMP                 
023208     END-IF                                                               
023209                                                                          
023210     IF MID-TISTODAT-KAMP = ALL '+'                                       
023211        MOVE MFS-ERASE-FIELD         TO MOD-TISTODAT-KAMP                 
023212     ELSE                                                                 
023213        MOVE MID-TISTODAT-KAMP       TO MOD-TISTODAT-KAMP                 
023214     END-IF                                                               
023215                                                                          
023220     .                                                                    
023300     EJECT                                                                
023400 F-READ-SHOW-INFO SECTION.                                                
023405     IF  SAVE-IDKAMP-ENTER     = SPACE                                    
023406     AND SAVE-IDKAMP-NEXT      = SPACE                                    
023407       CONTINUE                                                           
023408     ELSE                                                                 
023409       IF SAVE-IDKAMP-ENTER     NOT = SPACE                               
023410         MOVE SAVE-IDKAMP-ENTER                                           
023411                             TO W-IDKAMP                                  
023412       ELSE                                                               
023414         MOVE SAVE-IDKAMP-NEXT                                            
023415                             TO W-IDKAMP                                  
023416       END-IF                                                             
023417     END-IF                                                               
023419     IF W-IDKAMP NOT = SPACE                                              
023422****    CAMPAIGN NUMBER                                                   
023423        PERFORM FA-READ-SHOW-TP1KAMP                                      
023424     ELSE                                                                 
023425****    CAMPAIGN GROUP                                                    
023427        IF W-IDKAMP-GRP NOT = ZERO                                        
023428          PERFORM FB-READ-SHOW-TP1GRP                                     
023429        ELSE                                                              
023430          MOVE SPACE         TO MOD-TEMFSINF                              
023431        END-IF                                                            
023432     END-IF                                                               
023433     .                                                                    
023434     EJECT                                                                
023440 FA-READ-SHOW-TP1KAMP SECTION.                                            
023500                                                                          
023610     PERFORM DB2-SEARCH-TP1KAMP                                           
023613     IF SQLCODE = ZERO                                                    
023615       IF TP1KAMP-IDKAMP-GRP > ZERO                                       
023616         IF SAVE-IDKAMP-ENTER  = SPACE                                    
023617         AND SAVE-IDKAMP-NEXT  = SPACE                                    
023618           MOVE SPACE  TO W-IDKAMP                                        
023619         END-IF                                                           
023621         MOVE TP1KAMP-IDKAMP-GRP                                          
023622                       TO W-IDKAMP-GRP                                    
023623                          WS-IDKAMP-GRP                                   
023624         MOVE WS-IDKAMP-GRP                                               
023625                       TO MOD-IDKAMP-GRP-UT                               
023626         INSPECT MOD-IDKAMP-GRP-UT                                        
023627                       REPLACING LEADING ZERO BY SPACE                    
023628         PERFORM DB2-SELECT-TP1GRP                                        
023629         IF LINES-FOUND                                                   
023630           IF TP1GRP-RERESPRT NOT = 1                                     
023631             MOVE TP1GRP-RERESPRT TO WS-RERESPRT-UPDATE                   
023632             MULTIPLY TP1GRP-RERESPRT BY 100                              
023633                      GIVING W-RERESPRT                                   
023634             MOVE W-RERESPRT      TO MOD-RERESPRT-UT                      
023635                                     WS-RERESPRT                          
023636           ELSE                                                           
023637             MOVE 100  TO MOD-RERESPRT-UT                                 
023638                          WS-RERESPRT                                     
023639           END-IF                                                         
023640           PERFORM DB2-SELECT-TP1KAMP-SUM                                 
023641           MOVE WS-KVKAMP-CARS-TOT   TO MOD-KVKAMP-CARS-TOT               
023642           MOVE TP1GRP-BEKAMNOT         TO W-BEKAMNOT-TP1GRP              
023643           MOVE NOTE1-TP1GRP            TO MOD-NOTE1                      
023644                                           WS-NOTE1                       
023645           MOVE NOTE2-TP1GRP            TO MOD-NOTE2                      
023646                                           WS-NOTE2                       
023647         ELSE                                                             
023648           MOVE MFS-ERASE-FIELD         TO MOD-RERESPRT-UT                
023649                                             MOD-NOTE1                    
023650                                             MOD-NOTE2                    
023651         END-IF                                                           
023652         MOVE +1 TO INDX                                                  
023653         PERFORM DB2-OPEN-TP1KAMP-CRS1                                    
023654         IF SQLCODE = ZERO                                                
023655           PERFORM DB2-FETCH-TP1KAMP-CRS1                                 
023656         END-IF                                                           
023657         PERFORM UNTIL INDX > MAX-INDX OR SQLCODE > ZERO                  
023658           IF INDX = 1                                                    
023659             MOVE TP1KAMP-IDKAMP  TO SAVE-IDKAMP-ENTER                    
023660             MOVE TP1KAMP-KDKAMP  TO MOD-KDKAMP-RAD                       
023661           END-IF                                                         
023662           MOVE TP1KAMP-IDKAMP    TO MOD-IDKAMP-RAD (INDX)                
023663                                     WS-IDKAMP-RAD-DEL (INDX)             
023664           MOVE TP1KAMP-KVKAMP-CARS TO MOD-KVKAMP-CARS-RAD (INDX)         
023665           MOVE TP1KAMP-TISTADAT-KAMP                                     
023666                                  TO MOD-TISTADAT-KAMP-RAD (INDX)         
023667                                     WS-TISTADAT-KAMP-RAD (INDX)          
023668           MOVE TP1KAMP-TISTODAT-KAMP                                     
023669                                  TO MOD-TISTODAT-KAMP-RAD (INDX)         
023670                                     WS-TISTODAT-KAMP-RAD (INDX)          
023671           MOVE TP1KAMP-KDKAMP    TO WS-KDKAMP (INDX)                     
023672           PERFORM DB2-FETCH-TP1KAMP-CRS1                                 
023673           ADD +1                 TO INDX                                 
023674         END-PERFORM                                                      
023675         MOVE SPACE          TO W-IDKAMP                                  
023676                                MOD-IDKAMP-UT                             
023677         PERFORM UNTIL INDX > MAX-INDX                                    
023678           MOVE MFS-ERASE-FIELD TO MOD-IDKAMP-RAD (INDX)                  
023679                                   WS-KDKAMP (INDX)                       
023680                                   MOD-KVKAMP-CARS-RAD (INDX)             
023681                                   MOD-CMD (INDX)                         
023682                                   MOD-TISTADAT-KAMP-RAD (INDX)           
023683                                   MOD-TISTODAT-KAMP-RAD (INDX)           
023684           MOVE SPACE           TO WS-IDKAMP-RAD-DEL (INDX)               
023685           MOVE MFS-CLOSE-FIELD TO MOD-CMD-ATTR (INDX)                    
023686                                 MOD-TISTADAT-KAMP-RAD-ATTR (INDX)        
023687                                 MOD-TISTODAT-KAMP-RAD-ATTR (INDX)        
023688           ADD +1                 TO INDX                                 
023689         END-PERFORM                                                      
023690         IF SQLCODE = ZERO                                                
023691           MOVE TP1KAMP-IDKAMP    TO SAVE-IDKAMP-NEXT                     
023692           MOVE INF-MORE-INFO-EXISTS                                      
023693                                 TO MED-IDMFSFEL                          
023694           CALL WMEDKONV USING MED-WMEDAREA                               
023695           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
023696         ELSE                                                             
023697           MOVE SPACE        TO SAVE-IDKAMP-NEXT                          
023698         END-IF                                                           
023699*                                                                         
023700       ELSE                                                               
023702         MOVE ZERO                TO MOD-IDKAMP-GRP-UT                    
023703                                     WS-IDKAMP-GRP                        
023704         INSPECT MOD-IDKAMP-GRP-UT                                        
023705                       REPLACING LEADING ZERO BY SPACE                    
023706         MOVE +1                  TO INDX                                 
023707         MOVE TP1KAMP-IDKAMP      TO MOD-IDKAMP-RAD (INDX)                
023708                                     WS-IDKAMP-RAD-DEL (INDX)             
023709                                     SAVE-IDKAMP-ENTER                    
023710         MOVE TP1KAMP-KVKAMP-CARS TO MOD-KVKAMP-CARS-RAD (INDX)           
023711                                     MOD-KVKAMP-CARS-TOT                  
023712         MOVE TP1KAMP-TISTADAT-KAMP                                       
023713                                  TO MOD-TISTADAT-KAMP-RAD (INDX)         
023714                                     WS-TISTADAT-KAMP-RAD (INDX)          
023715         MOVE TP1KAMP-TISTODAT-KAMP                                       
023716                                  TO MOD-TISTODAT-KAMP-RAD (INDX)         
023717                                     WS-TISTODAT-KAMP-RAD (INDX)          
023718         MOVE TP1KAMP-KDKAMP      TO WS-KDKAMP (INDX)                     
023719                                     MOD-KDKAMP-RAD                       
023720         MOVE +2                  TO INDX                                 
023721         PERFORM UNTIL INDX > MAX-INDX                                    
023722           MOVE MFS-ERASE-FIELD TO MOD-IDKAMP-RAD (INDX)                  
023723                                   WS-KDKAMP (INDX)                       
023724                                   MOD-KVKAMP-CARS-RAD (INDX)             
023725                                   MOD-CMD (INDX)                         
023726                                   MOD-TISTADAT-KAMP-RAD (INDX)           
023727                                   MOD-TISTODAT-KAMP-RAD (INDX)           
023728           MOVE SPACE           TO WS-IDKAMP-RAD-DEL (INDX)               
023729           MOVE MFS-CLOSE-FIELD TO MOD-CMD-ATTR (INDX)                    
023730                                 MOD-TISTADAT-KAMP-RAD-ATTR (INDX)        
023731                                 MOD-TISTODAT-KAMP-RAD-ATTR (INDX)        
023732           ADD +1                 TO INDX                                 
023733         END-PERFORM                                                      
023734         IF TP1KAMP-RERESPRT NOT = 1                                      
023735           MOVE TP1KAMP-RERESPRT TO WS-RERESPRT-UPDATE                    
023736           MULTIPLY TP1KAMP-RERESPRT BY 100                               
023737                    GIVING W-RERESPRT                                     
023738           MOVE W-RERESPRT        TO MOD-RERESPRT-UT                      
023739                                     WS-RERESPRT                          
023740         ELSE                                                             
023741           MOVE 100  TO MOD-RERESPRT-UT                                   
023742                        WS-RERESPRT                                       
023743         END-IF                                                           
023745         MOVE TP1KAMP-BEKAMNOT    TO W-BEKAMNOT-TP1KAMP                   
023746         MOVE NOTE1-TP1KAMP       TO MOD-NOTE1                            
023747                                     WS-NOTE1                             
023748         MOVE NOTE2-TP1KAMP       TO MOD-NOTE2                            
023749                                     WS-NOTE2                             
023750       END-IF                                                             
023751     ELSE                                                                 
023752       MOVE +1 TO INDX                                                    
023753       PERFORM UNTIL INDX > MAX-INDX                                      
023754         MOVE MFS-ERASE-FIELD TO MOD-IDKAMP-RAD (INDX)                    
023755                                 MOD-KVKAMP-CARS-RAD (INDX)               
023756                                 MOD-CMD (INDX)                           
023757                                 MOD-TISTADAT-KAMP-RAD (INDX)             
023758                                 MOD-TISTODAT-KAMP-RAD (INDX)             
023765         MOVE SPACE             TO WS-IDKAMP-RAD-DEL (INDX)               
023766         MOVE MFS-CLOSE-FIELD TO MOD-CMD-ATTR (INDX)                      
023767                               MOD-TISTADAT-KAMP-RAD-ATTR (INDX)          
023768                               MOD-TISTODAT-KAMP-RAD-ATTR (INDX)          
023769         ADD +1                   TO INDX                                 
023770       END-PERFORM                                                        
023771     MOVE MFS-ERASE-FIELD     TO MOD-RERESPRT-IN                          
023772                                 MOD-NOTE1                                
023773                                 MOD-NOTE2                                
023774                                 MOD-KDKAMP                               
023775                                 MOD-TISTADAT-KAMP                        
023776                                 MOD-TISTODAT-KAMP                        
023777                                                                          
023778     MOVE MFS-CLOSE-FIELD     TO MOD-RERESPRT-ATTR                        
023779                                 MOD-NOTE1-ATTR                           
023780                                 MOD-NOTE2-ATTR                           
023781                                 MOD-KDKAMP-ATTR                          
023782                                 MOD-TISTADAT-KAMP-ATTR                   
023783                                 MOD-TISTODAT-KAMP-ATTR                   
023784                                                                          
023785     END-IF                                                               
023786     .                                                                    
023790     EJECT                                                                
023800                                                                          
025798 FB-READ-SHOW-TP1GRP SECTION.                                             
025799                                                                          
025836     MOVE +1 TO INDX                                                      
025837     PERFORM DB2-OPEN-TP1KAMP-CRS1                                        
025838     IF SQLCODE = ZERO                                                    
025839       PERFORM DB2-FETCH-TP1KAMP-CRS1                                     
025840     END-IF                                                               
025841                                                                          
025842     IF SQLCODE > ZERO                                                    
025843       MOVE INF-URVAL-SAKNAS                                              
025844                             TO MED-IDMFSFEL                              
025845       CALL WMEDKONV USING MED-WMEDAREA                                   
025846       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
025847       MOVE +1 TO INDX                                                    
025848       PERFORM UNTIL INDX > MAX-INDX                                      
025849         MOVE MFS-ERASE-FIELD TO MOD-IDKAMP-RAD (INDX)                    
025850                                 MOD-KVKAMP-CARS-RAD (INDX)               
025851                                 MOD-CMD (INDX)                           
025852                                 MOD-TISTADAT-KAMP-RAD (INDX)             
025853                                 MOD-TISTODAT-KAMP-RAD (INDX)             
025854         MOVE SPACE             TO WS-IDKAMP-RAD-DEL (INDX)               
025855         MOVE MFS-CLOSE-FIELD TO MOD-CMD-ATTR (INDX)                      
025856                               MOD-TISTADAT-KAMP-RAD-ATTR (INDX)          
025857                               MOD-TISTODAT-KAMP-RAD-ATTR (INDX)          
025858         ADD +1                   TO INDX                                 
025859       END-PERFORM                                                        
025860     MOVE MFS-ERASE-FIELD     TO MOD-RERESPRT-IN                          
025861                                 MOD-NOTE1                                
025862                                 MOD-NOTE2                                
025863                                 MOD-KDKAMP                               
025864                                 MOD-TISTADAT-KAMP                        
025865                                 MOD-TISTODAT-KAMP                        
025866                                                                          
025867     MOVE MFS-CLOSE-FIELD     TO MOD-RERESPRT-ATTR                        
025868                                 MOD-NOTE1-ATTR                           
025869                                 MOD-NOTE2-ATTR                           
025870                                 MOD-KDKAMP-ATTR                          
025871                                 MOD-TISTADAT-KAMP-ATTR                   
025872                                 MOD-TISTODAT-KAMP-ATTR                   
025873                                                                          
025874                                                                          
025875     ELSE                                                                 
025876       MOVE TP1KAMP-IDKAMP TO SAVE-IDKAMP-ENTER                           
025877       MOVE SPACES         TO MOD-IDKAMP-UT                               
025878                                                                          
025879       PERFORM UNTIL INDX > MAX-INDX OR SQLCODE > ZERO                    
025880         IF INDX = 1                                                      
025881           MOVE TP1KAMP-IDKAMP    TO SAVE-IDKAMP-ENTER                    
025882           MOVE TP1KAMP-KDKAMP    TO MOD-KDKAMP-RAD                       
025883         END-IF                                                           
025884         MOVE TP1KAMP-IDKAMP      TO MOD-IDKAMP-RAD (INDX)                
025885                                     WS-IDKAMP-RAD-DEL (INDX)             
025886         MOVE TP1KAMP-KVKAMP-CARS TO MOD-KVKAMP-CARS-RAD (INDX)           
025887         MOVE TP1KAMP-TISTADAT-KAMP                                       
025888                                 TO MOD-TISTADAT-KAMP-RAD (INDX)          
025889                                    WS-TISTADAT-KAMP-RAD (INDX)           
025890         MOVE TP1KAMP-TISTODAT-KAMP                                       
025891                                 TO MOD-TISTODAT-KAMP-RAD (INDX)          
025892                                    WS-TISTODAT-KAMP-RAD (INDX)           
025893         MOVE TP1KAMP-KDKAMP      TO WS-KDKAMP (INDX)                     
025894         PERFORM DB2-FETCH-TP1KAMP-CRS1                                   
025895         ADD +1                   TO INDX                                 
025896       END-PERFORM                                                        
025897       PERFORM UNTIL INDX > MAX-INDX                                      
025898         MOVE MFS-ERASE-FIELD TO MOD-IDKAMP-RAD (INDX)                    
025899                                 WS-KDKAMP (INDX)                         
025900                                 MOD-KVKAMP-CARS-RAD (INDX)               
025901                                 MOD-CMD (INDX)                           
025902                                 MOD-TISTADAT-KAMP-RAD (INDX)             
025903                                 MOD-TISTODAT-KAMP-RAD (INDX)             
025904           MOVE SPACE           TO WS-IDKAMP-RAD-DEL (INDX)               
025905         MOVE MFS-CLOSE-FIELD TO MOD-CMD-ATTR (INDX)                      
025906                               MOD-TISTADAT-KAMP-RAD-ATTR (INDX)          
025907                               MOD-TISTODAT-KAMP-RAD-ATTR (INDX)          
025908         ADD +1                   TO INDX                                 
025909       END-PERFORM                                                        
025910                                                                          
025911       IF SQLCODE = ZERO                                                  
025912         MOVE TP1KAMP-IDKAMP TO SAVE-IDKAMP-NEXT                          
025913         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
025914         CALL WMEDKONV USING MED-WMEDAREA                                 
025915         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
025916       ELSE                                                               
025917         MOVE SPACE          TO SAVE-IDKAMP-NEXT                          
025918       END-IF                                                             
025919       PERFORM DB2-SELECT-TP1GRP                                          
025920       IF LINES-FOUND                                                     
025921         IF TP1GRP-RERESPRT NOT = 1                                       
025922           MOVE TP1GRP-RERESPRT TO WS-RERESPRT-UPDATE                     
025923           MULTIPLY TP1GRP-RERESPRT BY 100                                
025924                    GIVING W-RERESPRT                                     
025925           MOVE W-RERESPRT        TO MOD-RERESPRT-UT                      
025926                                     WS-RERESPRT                          
025927         ELSE                                                             
025928           MOVE 100  TO MOD-RERESPRT-UT                                   
025929                        WS-RERESPRT                                       
025930         END-IF                                                           
025931         PERFORM DB2-SELECT-TP1KAMP-SUM                                   
025932         MOVE WS-KVKAMP-CARS-TOT     TO MOD-KVKAMP-CARS-TOT               
025933         MOVE TP1GRP-BEKAMNOT           TO W-BEKAMNOT-TP1GRP              
025934         MOVE NOTE1-TP1GRP              TO MOD-NOTE1                      
025935                                           WS-NOTE1                       
025936         MOVE NOTE2-TP1GRP              TO MOD-NOTE2                      
025937                                           WS-NOTE2                       
025938       ELSE                                                               
025939         MOVE MFS-ERASE-FIELD           TO MOD-RERESPRT-UT                
025940                                           MOD-NOTE1                      
025941                                           MOD-NOTE2                      
025942         MOVE INF-URVAL-SAKNAS                                            
025943                               TO MED-IDMFSFEL                            
025944         CALL WMEDKONV USING MED-WMEDAREA                                 
025945         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
025946       END-IF                                                             
025947                                                                          
025948       PERFORM DB2-CLOSE-TP1KAMP-CRS1                                     
025949     END-IF                                                               
025950     .                                                                    
025960     EJECT                                                                
026001 G-CHECK-INPUT SECTION.                                                   
026002                                                                          
026003     MOVE YES    TO INDATA-SW                                             
026004     MOVE NOO    TO CMD-SW                                                
026005                                                                          
026006     MOVE +1 TO INDX                                                      
026007     PERFORM UNTIL INDX > MAX-INDX                                        
026009       IF MID-CMD (INDX) NOT = ALL '+'                                    
026010          MOVE YES TO CMD-SW                                              
026012       END-IF                                                             
026013       IF MID-TISTADAT-KAMP-RAD (INDX) NOT = ALL '+'                      
026014          MOVE YES TO INPUT-SW                                            
026016       END-IF                                                             
026017       IF MID-TISTODAT-KAMP-RAD (INDX) NOT = ALL '+'                      
026018          MOVE YES TO INPUT-SW                                            
026020       END-IF                                                             
026021       ADD +1    TO INDX                                                  
026022     END-PERFORM                                                          
026023                                                                          
026024     IF MID-RERESPRT-IN = ALL '+'                                         
026025     AND MID-NOTE1   = ALL '+'                                            
026026     AND MID-NOTE2   = ALL '+'                                            
026027     AND MID-KDKAMP  = ALL '+'                                            
026028     AND MID-TISTADAT-KAMP = ALL '+'                                      
026029     AND MID-TISTODAT-KAMP = ALL '+'                                      
026030     AND NOT CMD-OK                                                       
026031     AND INPUT-WRONG                                                      
026033       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
026034       CALL WMEDKONV USING MED-WMEDAREA                                   
026035       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026036       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
026037       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
026038       MOVE NOO TO INDATA-SW                                              
026039       MOVE NOO                  TO NEWPOST-SW                            
026040     ELSE                                                                 
026042       IF CMD-OK                                                          
026044         MOVE +1 TO INDX                                                  
026045         PERFORM UNTIL INDX > MAX-INDX                                    
026046           IF MID-CMD (INDX) NOT = '+'                                    
026048             MOVE NOO                  TO NEWPOST-SW                      
026049             IF MID-CMD (INDX) = 'D' OR 'B'                               
026051               IF WS-KDKAMP (INDX) = 'S' OR 'Q'                           
026053                 MOVE NOO              TO NEWPOST-SW                      
026054                 MOVE MFS-ALPHA-FIELD-OK TO MOD-CMD-ATTR (INDX)           
026055               ELSE                                                       
026056                 MOVE 'DELETE ONLY FOR S OR Q CAMPAIGNS'                  
026057                       TO MOD-TEMFSINF                                    
026058                 MOVE MFS-ALPHA-FIELD-WRONG TO MOD-CMD-ATTR (INDX)        
026059                 MOVE NOO              TO INDATA-SW                       
026060               END-IF                                                     
026061             ELSE                                                         
026062               MOVE 'ONLY D OR B ALLOWED' TO MOD-TEMFSINF                 
026063               MOVE MFS-ALPHA-FIELD-WRONG TO MOD-CMD-ATTR (INDX)          
026064               MOVE NOO              TO INDATA-SW                         
026065             END-IF                                                       
026066           END-IF                                                         
026067           ADD +1 TO INDX                                                 
026068         END-PERFORM                                                      
026069       END-IF                                                             
026070                                                                          
026071       MOVE +1 TO INDX                                                    
026072       PERFORM UNTIL INDX > MAX-INDX                                      
026073         IF MID-TISTADAT-KAMP-RAD (INDX) NOT = ALL '+'                    
026075            MOVE NOO                   TO NEWPOST-SW                      
026076            IF WS-KDKAMP (INDX) NOT = 'W'                                 
026077              INSPECT MID-TISTADAT-KAMP-RAD (INDX)                        
026078                      REPLACING LEADING SPACE BY ZERO                     
026079              IF MID-TISTADAT-KAMP-RAD (INDX) NUMERIC                     
026081                 MOVE MID-TISTADAT-KAMP-RAD (INDX)                        
026082                                         TO WS-DATUM                      
026083                 MOVE 'AAMMDD' TO DAT-KDDATFORM                           
026084                 MOVE WS-DATUM TO DAT-I-TIDATUM                           
026085                                                                          
026086                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
026087                                     DAT-O-TIDATUM DAT-KDSVAR             
026088                 MOVE DAT-TISEKEL  TO WS-SEKEL                            
026089                 MOVE DAT-TIAAMMDD TO WS-DATUM (3:6)                      
026090                                                                          
026091                 IF (DAT-KDSVAR-OK)                                       
026092                   CONTINUE                                               
026093*                  IF WS-DATUM >= DAGENS-DATUM                            
026095*                    MOVE NOO          TO NEWPOST-SW                      
026096*                    MOVE MFS-NUM-FIELD-OK                                
026097*                         TO MOD-TISTADAT-KAMP-RAD-ATTR (INDX)            
026099*                  ELSE                                                   
026100*                    MOVE 'DATE BEFORE TODAYS DATE'                       
026101*                                          TO MOD-TEMFSINF                
026102*                    MOVE NOO          TO INDATA-SW                       
026103*                    MOVE MFS-NUM-FIELD-WRONG                             
026104*                         TO MOD-TISTADAT-KAMP-RAD-ATTR (INDX)            
026105*                    END-IF                                               
026106                 ELSE                                                     
026107                   MOVE 'WRONG DATE'                                      
026108                                         TO MOD-TEMFSINF                  
026109                   MOVE NOO            TO INDATA-SW                       
026110                   MOVE MFS-NUM-FIELD-WRONG                               
026111                        TO MOD-TISTADAT-KAMP-RAD-ATTR (INDX)              
026112                 END-IF                                                   
026113              ELSE                                                        
026114                 MOVE NOO              TO INDATA-SW                       
026115                 MOVE MFS-NUM-FIELD-WRONG                                 
026116                      TO MOD-TISTADAT-KAMP-RAD-ATTR (INDX)                
026117              END-IF                                                      
026118            ELSE                                                          
026119              MOVE 'NO UPDATE OF START FOR QW90'                          
026120                   TO MOD-TEMFSINF                                        
026121              MOVE NOO                 TO INDATA-SW                       
026122              MOVE MFS-NUM-FIELD-WRONG                                    
026123                   TO MOD-TISTADAT-KAMP-RAD-ATTR (INDX)                   
026124            END-IF                                                        
026125         END-IF                                                           
026126         IF MID-TISTODAT-KAMP-RAD (INDX) NOT = ALL '+'                    
026128            MOVE NOO                   TO NEWPOST-SW                      
026129            IF WS-KDKAMP (INDX) NOT = 'W'                                 
026130              IF MID-TISTODAT-KAMP-RAD (INDX) NUMERIC                     
026132                 MOVE MID-TISTODAT-KAMP-RAD (INDX)                        
026133                                         TO WS-DATUM                      
026134                 MOVE 'AAMMDD' TO DAT-KDDATFORM                           
026135                 MOVE WS-DATUM TO DAT-I-TIDATUM                           
026136                                                                          
026137                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
026138                                     DAT-O-TIDATUM DAT-KDSVAR             
026139                 MOVE DAT-TISEKEL  TO WS-SEKEL                            
026140                 MOVE DAT-TIAAMMDD TO WS-DATUM (3:6)                      
026141                                                                          
026142                 IF (DAT-KDSVAR-OK)                                       
026143*                                                                         
026144*                  IF WS-DATUM >= DAGENS-DATUM                            
026146                    IF WS-DATUM (3:6) >=                                  
026147                       WS-TISTADAT-KAMP-RAD (INDX)                        
026149                       MOVE NOO        TO NEWPOST-SW                      
026150                       MOVE MFS-NUM-FIELD-OK                              
026151                            TO MOD-TISTODAT-KAMP-RAD-ATTR (INDX)          
026153                    ELSE                                                  
026155                       MOVE 'DATE BEFORE START DATE'                      
026156                                             TO MOD-TEMFSINF              
026157                       MOVE NOO        TO INDATA-SW                       
026158                       MOVE MFS-NUM-FIELD-WRONG                           
026159                            TO MOD-TISTODAT-KAMP-RAD-ATTR (INDX)          
026160                    END-IF                                                
026161*                  ELSE                                                   
026163*                    MOVE 'DATE BEFORE TODAYS DATE'                       
026164*                                          TO MOD-TEMFSINF                
026165*                    MOVE NOO          TO INDATA-SW                       
026166*                    MOVE MFS-NUM-FIELD-WRONG                             
026167*                         TO MOD-TISTODAT-KAMP-RAD-ATTR (INDX)            
026168*                    END-IF                                               
026169                 ELSE                                                     
026170                   IF WS-DATUM > ZERO                                     
026171                     MOVE 'WRONG DATE'                                    
026172                                       TO MOD-TEMFSINF                    
026173                     MOVE NOO          TO INDATA-SW                       
026174                     MOVE MFS-NUM-FIELD-WRONG                             
026175                        TO MOD-TISTODAT-KAMP-RAD-ATTR (INDX)              
026176                   END-IF                                                 
026177                 END-IF                                                   
026178              ELSE                                                        
026179                 MOVE NOO              TO INDATA-SW                       
026180                 MOVE MFS-NUM-FIELD-WRONG                                 
026181                      TO MOD-TISTODAT-KAMP-RAD-ATTR (INDX)                
026182              END-IF                                                      
026183            ELSE                                                          
026184              MOVE 'NO UPDATE OF STOP FOR QW90'                           
026185                   TO MOD-TEMFSINF                                        
026186              MOVE NOO                 TO INDATA-SW                       
026187              MOVE MFS-NUM-FIELD-WRONG                                    
026188                   TO MOD-TISTODAT-KAMP-RAD-ATTR (INDX)                   
026189            END-IF                                                        
026190         END-IF                                                           
026191         ADD +1 TO INDX                                                   
026192       END-PERFORM                                                        
026193                                                                          
026194       IF MID-RERESPRT-IN NOT = ALL '+'                                   
026195          MOVE NOO                     TO NEWPOST-SW                      
026196          INSPECT MID-RERESPRT-IN REPLACING LEADING SPACES BY ZERO        
026197          IF MID-RERESPRT-IN NUMERIC                                      
026198             IF WS-KDKAMP (1) = 'W'                                       
026199               MOVE MID-RERESPRT-IN TO WS-RERESPRT2                       
026200               IF WS-RERESPRT2 > 100                                      
026201                  MOVE 'OVER 100% NOT ALLOWED' TO MOD-TEMFSINF            
026202                  MOVE MFS-NUM-FIELD-WRONG TO MOD-RERESPRT-ATTR           
026203                  MOVE NOO TO INDATA-SW                                   
026204               ELSE                                                       
026205                  MOVE NOO             TO NEWPOST-SW                      
026206                  MOVE MFS-NUM-FIELD-OK TO MOD-RERESPRT-ATTR              
026207               END-IF                                                     
026208             ELSE                                                         
026209                MOVE 'UPDATE OF RESP.RATE ONLY FOR QW90'                  
026210                     TO MOD-TEMFSINF                                      
026211                MOVE MFS-NUM-FIELD-WRONG TO MOD-RERESPRT-ATTR             
026212                MOVE NOO TO INDATA-SW                                     
026213             END-IF                                                       
026214          ELSE                                                            
026215             MOVE NOO     TO INDATA-SW                                    
026216             MOVE MFS-NUM-FIELD-WRONG TO MOD-RERESPRT-ATTR                
026217          END-IF                                                          
026218       END-IF                                                             
026219       IF MID-NOTE1 = ALL '+'                                             
026220          CONTINUE                                                        
026221       ELSE                                                               
026222          MOVE NOO         TO NEWPOST-SW                                  
026223       END-IF                                                             
026224                                                                          
026225       IF MID-NOTE2 = ALL '+'                                             
026226          CONTINUE                                                        
026227       ELSE                                                               
026228          MOVE NOO         TO NEWPOST-SW                                  
026229       END-IF                                                             
026230                                                                          
026231       IF NEWPOST-OK                                                      
026232         IF MID-KDKAMP = 'Q' OR 'S'                                       
026233            MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDKAMP-ATTR                 
026234         ELSE                                                             
026235            MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                    
026236            MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDKAMP-ATTR                 
026237            MOVE NOO                   TO INDATA-SW                       
026238         END-IF                                                           
026239         IF MID-TISTADAT-KAMP NOT = ALL '+'                               
026240           INSPECT MID-TISTADAT-KAMP REPLACING LEADING                    
026241                                     SPACE BY ZERO                        
026242           IF MID-TISTADAT-KAMP NUMERIC                                   
026243             MOVE MID-TISTADAT-KAMP                                       
026244                                     TO WS-DATUM                          
026245             MOVE 'AAMMDD' TO DAT-KDDATFORM                               
026246             MOVE WS-DATUM TO DAT-I-TIDATUM                               
026247                                                                          
026248             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
026249                                 DAT-O-TIDATUM DAT-KDSVAR                 
026250             MOVE DAT-TISEKEL      TO WS-SEKEL                            
026251             MOVE DAT-TIAAMMDD     TO WS-DATUM (3:6)                      
026252                                      W-TISTADAT-KAMP                     
026253                                                                          
026254             IF (DAT-KDSVAR-OK)                                           
026255*                                                                         
026256*              IF WS-DATUM >= DAGENS-DATUM                                
026257                 MOVE MFS-NUM-FIELD-OK                                    
026258                           TO MOD-TISTADAT-KAMP-ATTR                      
026259*              ELSE                                                       
026260*                MOVE 'DATE BEFORE TODAYS DATE'                           
026261*                                      TO MOD-TEMFSINF                    
026262*                MOVE NOO              TO INDATA-SW                       
026263*                MOVE MFS-NUM-FIELD-WRONG                                 
026264*                     TO MOD-TISTADAT-KAMP-ATTR                           
026265*                END-IF                                                   
026266             ELSE                                                         
026267               MOVE 'WRONG DATE'                                          
026268                                     TO MOD-TEMFSINF                      
026269               MOVE NOO                TO INDATA-SW                       
026270               MOVE MFS-NUM-FIELD-WRONG                                   
026271                    TO MOD-TISTADAT-KAMP-ATTR                             
026272             END-IF                                                       
026273           ELSE                                                           
026274              MOVE NOO    TO INDATA-SW                                    
026275              MOVE MFS-NUM-FIELD-WRONG TO MOD-TISTADAT-KAMP-ATTR          
026276           END-IF                                                         
026277         ELSE                                                             
026278           MOVE 'START MUST BE SPECIFIED' TO MOD-TEMFSINF                 
026279           MOVE NOO       TO INDATA-SW                                    
026280           MOVE MFS-NUM-FIELD-WRONG TO MOD-TISTADAT-KAMP-ATTR             
026281         END-IF                                                           
026282         IF MID-TISTODAT-KAMP NOT = ALL '+'                               
026283           INSPECT MID-TISTODAT-KAMP REPLACING LEADING                    
026284                                     SPACE BY ZERO                        
026285           IF MID-TISTODAT-KAMP NUMERIC                                   
026286             MOVE MID-TISTODAT-KAMP                                       
026287                                     TO WS-DATUM                          
026288             MOVE 'AAMMDD' TO DAT-KDDATFORM                               
026289             MOVE WS-DATUM TO DAT-I-TIDATUM                               
026290                                                                          
026291             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
026292                                 DAT-O-TIDATUM DAT-KDSVAR                 
026293             MOVE DAT-TISEKEL      TO WS-SEKEL                            
026294             MOVE DAT-TIAAMMDD     TO WS-DATUM (3:6)                      
026295                                                                          
026296             IF (DAT-KDSVAR-OK)                                           
026297*                                                                         
026298*              IF WS-DATUM >= DAGENS-DATUM                                
026299                 IF WS-DATUM (3:6) >= W-TISTADAT-KAMP                     
026300                    MOVE MFS-NUM-FIELD-OK                                 
026301                           TO MOD-TISTODAT-KAMP-ATTR                      
026302                 ELSE                                                     
026303                    MOVE 'DATE BEFORE START DATE'                         
026304                                          TO MOD-TEMFSINF                 
026305                    MOVE NOO           TO INDATA-SW                       
026306                    MOVE MFS-NUM-FIELD-WRONG                              
026307                         TO MOD-TISTODAT-KAMP-ATTR                        
026308                 END-IF                                                   
026309*              ELSE                                                       
026310*                MOVE 'DATE BEFORE TODAYS DATE'                           
026311*                                      TO MOD-TEMFSINF                    
026312*                MOVE NOO              TO INDATA-SW                       
026313*                MOVE MFS-NUM-FIELD-WRONG                                 
026314*                     TO MOD-TISTODAT-KAMP-ATTR                           
026315*              END-IF                                                     
026316             ELSE                                                         
026317               IF WS-DATUM > ZERO                                         
026318                 MOVE 'WRONG DATE'                                        
026319                                     TO MOD-TEMFSINF                      
026320                 MOVE NOO              TO INDATA-SW                       
026321                 MOVE MFS-NUM-FIELD-WRONG                                 
026322                    TO MOD-TISTODAT-KAMP-ATTR                             
026323               END-IF                                                     
026324             END-IF                                                       
026325           END-IF                                                         
026326         END-IF                                                           
026327       END-IF                                                             
026328       IF INDATA-WRONG                                                    
026329         IF MED-IDMFSFEL = SPACE                                          
026330           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
026331         END-IF                                                           
026332         CALL WMEDKONV USING MED-WMEDAREA                                 
026333         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
026334         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
026335         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
026336       END-IF                                                             
026337     END-IF                                                               
026338     .                                                                    
026339     EJECT                                                                
026340                                                                          
026341 H-UPDATE SECTION.                                                        
026342                                                                          
026343     IF NEWPOST-OK                                                        
026344        PERFORM HA-UPDATE-NEWPOST                                         
026345     END-IF                                                               
026346                                                                          
026347     IF NEWPOST-WRONG AND CMD-OK                                          
026348        PERFORM HB-DELETE-POST                                            
026349     END-IF                                                               
026350                                                                          
026351     IF NEWPOST-WRONG AND NOT CMD-OK                                      
026352        PERFORM HC-UPDATE-POST                                            
026353     END-IF                                                               
026354                                                                          
026355     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
026356     CALL WMEDKONV USING MED-WMEDAREA                                     
026357     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
026358     PERFORM MFS-ERASE-FIELD-IN                                           
026359     .                                                                    
026360     EJECT                                                                
026361                                                                          
026362 HA-UPDATE-NEWPOST SECTION.                                               
026363     PERFORM DB2-SELECT-MAX-TP1KAM-NEWPOST                                
026364     ADD +1                      TO W-IDLOPNR-KAMP                        
026365     MOVE W-IDLOPNR-KAMP         TO WS-IDLOPNR-IDKAMP-NUM                 
026366     MOVE W-IDLOPNR-KAMP         TO WS-IDKAMP                             
026367     INSPECT WS-IDLOPNR-IDKAMP-NUM REPLACING LEADING ZERO BY SPACE        
026368     MOVE WS-IDLOPNR-IDKAMP-NUM  TO WS-IDLOPNR-IDKAMP                     
026369     INSPECT WS-IDLOPNR-IDKAMP                                            
026370             TALLYING IX FOR LEADING SPACE                                
026371     COMPUTE IX = IX + 1                                                  
026372     COMPUTE IX-ANTAL  = 8 - IX                                           
026373     MOVE WS-IDLOPNR-IDKAMP (IX : IX-ANTAL)                               
026374                                 TO W-IDKAMP                              
026375                                    MOD-IDKAMP-UT                         
026376     MOVE ZERO                   TO W-IDKAMP-GRP                          
026377     MOVE SPACE                  TO MOD-IDKAMP-GRP-UT                     
026378     MOVE MID-TISTADAT-KAMP      TO WS-TISTADAT-KAMP                      
026379     IF MID-TISTODAT-KAMP NOT = ALL '+'                                   
026380       MOVE MID-TISTODAT-KAMP    TO WS-TISTODAT-KAMP                      
026381     END-IF                                                               
026382     PERFORM DB2-INSERT-TP1KAMP-NEWPOST                                   
026383     MOVE SPACE                  TO SAVE-IDKAMP-ENTER                     
026384     MOVE SPACE                  TO SAVE-IDKAMP-NEXT                      
026385     .                                                                    
026386     EJECT                                                                
026387                                                                          
026388 HB-DELETE-POST SECTION.                                                  
026389                                                                          
026390     MOVE +1 TO INDX                                                      
026391     PERFORM UNTIL INDX > MAX-INDX                                        
026392       IF MID-CMD (INDX) = 'D' OR 'B'                                     
026393         MOVE WS-IDKAMP-RAD-DEL (INDX) TO WS-IDKAMP-DEL                   
026394         PERFORM DB2-DELETE-TP1KAMP                                       
026395                                                                          
026396         MOVE ZERO           TO W-IDARTNR                                 
026397         PERFORM DB2-DCL-OPN-CRS-TP1ARTK                                  
026398         IF SQLCODE = ZERO                                                
026399           PERFORM DB2-FETCH-TP1ARTK                                      
026400         END-IF                                                           
026401         PERFORM UNTIL SQLCODE > ZERO                                     
026402           PERFORM DB2-DELETE-TP1ARTK                                     
026403           PERFORM DB2-FETCH-TP1ARTK                                      
026404         END-PERFORM                                                      
026405         PERFORM DB2-CLOSE-TP1ARTK-CRS                                    
026406       END-IF                                                             
026407       ADD +1 TO INDX                                                     
026408     END-PERFORM                                                          
026409     MOVE SPACE                  TO SAVE-IDKAMP-ENTER                     
026410     MOVE SPACE                  TO SAVE-IDKAMP-NEXT                      
026411     MOVE SPACE                  TO MOD-IDKAMP-GRP-UT                     
026412     MOVE SPACE                  TO MOD-IDKAMP-UT                         
026413     .                                                                    
026414     EJECT                                                                
026415                                                                          
026416 HC-UPDATE-POST SECTION.                                                  
026417     IF WS-IDKAMP-GRP = ZERO                                              
026418        PERFORM HCA-UPDATE-TP1KAMP-POST                                   
026419     ELSE                                                                 
026420        PERFORM HCB-UPDATE-TP1GRP-POST                                    
026421     END-IF                                                               
026422     .                                                                    
026423     EJECT                                                                
026424                                                                          
026425 HCA-UPDATE-TP1KAMP-POST SECTION.                                         
026426     IF (MID-RERESPRT-IN NOT = ALL '+')                                   
026427        MOVE MID-RERESPRT-IN TO W-RERESPRT                                
026428        COMPUTE WS-RERESPRT-UPDATE =                                      
026429               W-RERESPRT / 100                                           
026430     ELSE                                                                 
026431        MOVE WS-RERESPRT TO W-RERESPRT                                    
026432        COMPUTE WS-RERESPRT-UPDATE =                                      
026433               W-RERESPRT / 100                                           
026434     END-IF                                                               
026435     IF (MID-NOTE1    NOT = ALL '+')                                      
026436        MOVE MID-NOTE1                 TO NOTE1-TP1KAMP                   
026437     ELSE                                                                 
026438        MOVE WS-NOTE1                 TO NOTE1-TP1KAMP                    
026439     END-IF                                                               
026440     IF (MID-NOTE2    NOT = ALL '+')                                      
026441        MOVE MID-NOTE2                 TO NOTE2-TP1KAMP                   
026442     ELSE                                                                 
026443        MOVE WS-NOTE2                 TO NOTE2-TP1KAMP                    
026444     END-IF                                                               
026445     MOVE W-BEKAMNOT-TP1KAMP           TO TP1KAMP-BEKAMNOT                
026446     PERFORM DB2-UPDATE-TP1KAMP-RERESPRT                                  
026447     MOVE +1 TO INDX                                                      
026448     PERFORM UNTIL INDX > MAX-INDX                                        
026449       IF WS-IDKAMP-RAD-DEL (INDX) NOT = SPACE                            
026450         MOVE WS-IDKAMP-RAD-DEL (INDX)  TO W-IDKAMP                       
026451         IF (MID-TISTADAT-KAMP-RAD(INDX) NOT = ALL '+')                   
026452           MOVE MID-TISTADAT-KAMP-RAD (INDX) TO                           
026453                                            TP1KAMP-TISTADAT-KAMP         
026454         ELSE                                                             
026455           MOVE WS-TISTADAT-KAMP-RAD (INDX) TO                            
026456                                            TP1KAMP-TISTADAT-KAMP         
026457         END-IF                                                           
026458         IF (MID-TISTODAT-KAMP-RAD(INDX) NOT = ALL '+')                   
026459           MOVE MID-TISTODAT-KAMP-RAD (INDX)  TO                          
026460                                            TP1KAMP-TISTODAT-KAMP         
026461         ELSE                                                             
026462           MOVE WS-TISTODAT-KAMP-RAD (INDX) TO                            
026463                                            TP1KAMP-TISTODAT-KAMP         
026464         END-IF                                                           
026465         PERFORM DB2-UPDATE-TP1KAMP-CRS4                                  
026466       END-IF                                                             
026467       ADD +1 TO INDX                                                     
026468     END-PERFORM                                                          
026469     .                                                                    
026470     EJECT                                                                
026471 HCB-UPDATE-TP1GRP-POST SECTION.                                          
026472     IF (MID-RERESPRT-IN NOT = ALL '+')                                   
026473        MOVE MID-RERESPRT-IN TO W-RERESPRT                                
026474        COMPUTE WS-RERESPRT-UPDATE =                                      
026475               W-RERESPRT / 100                                           
026476     ELSE                                                                 
026477        MOVE WS-RERESPRT TO W-RERESPRT                                    
026478        COMPUTE WS-RERESPRT-UPDATE =                                      
026479               W-RERESPRT / 100                                           
026480     END-IF                                                               
026481     IF (MID-NOTE1    NOT = ALL '+')                                      
026482        MOVE MID-NOTE1                 TO NOTE1-TP1GRP                    
026483     ELSE                                                                 
026484        MOVE WS-NOTE1                 TO NOTE1-TP1GRP                     
026485     END-IF                                                               
026486     IF (MID-NOTE2    NOT = ALL '+')                                      
026487        MOVE MID-NOTE2                 TO NOTE2-TP1GRP                    
026488     ELSE                                                                 
026489        MOVE WS-NOTE2                 TO NOTE2-TP1GRP                     
026490     END-IF                                                               
026491     MOVE W-BEKAMNOT-TP1GRP            TO TP1GRP-BEKAMNOT                 
026492     PERFORM DB2-UPDATE-TP1GRP                                            
026493     MOVE +1 TO INDX                                                      
026494     PERFORM UNTIL INDX > MAX-INDX                                        
026495       MOVE WS-IDKAMP-RAD-DEL (INDX)    TO W-IDKAMP                       
026496       IF WS-IDKAMP-RAD-DEL (INDX) NOT = SPACE                            
026497         IF (MID-TISTADAT-KAMP-RAD(INDX) NOT = ALL '+')                   
026498           MOVE MID-TISTADAT-KAMP-RAD (INDX) TO                           
026499                                            TP1KAMP-TISTADAT-KAMP         
026500         ELSE                                                             
026501           MOVE WS-TISTADAT-KAMP-RAD (INDX) TO                            
026502                                            TP1KAMP-TISTADAT-KAMP         
026503         END-IF                                                           
026504         IF (MID-TISTODAT-KAMP-RAD(INDX) NOT = ALL '+')                   
026505           MOVE MID-TISTODAT-KAMP-RAD (INDX) TO                           
026506                                            TP1KAMP-TISTODAT-KAMP         
026507         ELSE                                                             
026508           MOVE WS-TISTODAT-KAMP-RAD (INDX) TO                            
026509                                            TP1KAMP-TISTODAT-KAMP         
026510         END-IF                                                           
026511         PERFORM DB2-UPDATE-TP1KAMP-CRS3                                  
026512       END-IF                                                             
026513       CONTINUE                                                           
026514       ADD +1 TO INDX                                                     
026515     END-PERFORM                                                          
026516     .                                                                    
026517     EJECT                                                                
026518                                                                          
026519 MFS-ERASE-FIELD-OUT SECTION.                                             
026520                                                                          
026521     MOVE +1 TO INDX                                                      
026522     PERFORM UNTIL INDX > MAX-INDX                                        
026523       MOVE MFS-ERASE-FIELD  TO MOD-IDKAMP-RAD (INDX)                     
026524                                MOD-KVKAMP-CARS-RAD (INDX)                
026525       ADD +1 TO INDX                                                     
026526     END-PERFORM                                                          
026527     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                              
026528     MOVE MFS-ERASE-FIELD    TO MOD-RERESPRT-UT                           
026529                                MOD-KDKAMP-RAD                            
026530*    --- ALLA UTDATA-FÄLT                                                 
026531     .                                                                    
026540     SKIP3                                                                
026700 MFS-ERASE-FIELD-IN SECTION.                                              
026800                                                                          
026900*    --- ALLA INDATA-FÄLT                                                 
026910     MOVE +1 TO INDX                                                      
026920     PERFORM UNTIL INDX > MAX-INDX                                        
027000       MOVE MFS-ERASE-FIELD TO MOD-CMD (INDX)                             
027100                               MOD-TISTADAT-KAMP-RAD (INDX)               
027110                               MOD-TISTODAT-KAMP-RAD (INDX)               
027120       ADD +1 TO INDX                                                     
027130     END-PERFORM                                                          
027140     MOVE MFS-ERASE-FIELD   TO MOD-RERESPRT-IN                            
027141                               MOD-NOTE1                                  
027142                               MOD-NOTE2                                  
027143                               MOD-KDKAMP                                 
027150                               MOD-TISTADAT-KAMP                          
027160                               MOD-TISTODAT-KAMP                          
027200     .                                                                    
027300     EJECT                                                                
027400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
027500                                                                          
027600*    --- ALLA UTDATA-FÄLT                                                 
027710*    --- INCL SCROLL KEYS AND LINEDATA                                    
027720     MOVE +1 TO INDX                                                      
027730     PERFORM UNTIL INDX > MAX-INDX                                        
027740       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDKAMP-RAD (INDX)              
027750                                       MOD-KVKAMP-CARS-RAD (INDX)         
027770       ADD +1 TO INDX                                                     
027780     END-PERFORM                                                          
028011     .                                                                    
028012     SKIP2                                                                
028300 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
028400                                                                          
028500*    --- ALLA INDATA-FÄLT                                                 
028710     MOVE +1 TO INDX                                                      
028720     PERFORM UNTIL INDX > MAX-INDX                                        
028730       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-CMD (INDX)                      
028740                                      MOD-TISTADAT-KAMP-RAD (INDX)        
028750                                      MOD-TISTODAT-KAMP-RAD (INDX)        
028760       ADD +1 TO INDX                                                     
028770     END-PERFORM                                                          
028780     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-RERESPRT-IN                     
028790                                      MOD-NOTE1                           
028791                                      MOD-NOTE2                           
028792                                      MOD-KDKAMP                          
028793                                      MOD-TISTADAT-KAMP                   
028794                                      MOD-TISTODAT-KAMP                   
028800     .                                                                    
028900     EJECT                                                                
029000 MFS-FORM-ATTR SECTION.                                                   
029100                                                                          
029200*    --- ALL INDATA-FIELDS                                                
029210     MOVE +1 TO INDX                                                      
029220     PERFORM UNTIL INDX > MAX-INDX                                        
029230       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-CMD (INDX)                     
029240                                      MOD-TISTADAT-KAMP-RAD (INDX)        
029250                                      MOD-TISTODAT-KAMP-RAD (INDX)        
029260       ADD +1 TO INDX                                                     
029270     END-PERFORM                                                          
029280     MOVE MFS-FORMAT-DEFAULT-ATTR  TO MOD-RERESPRT-IN                     
029290                                      MOD-NOTE1                           
029291                                      MOD-NOTE2                           
029292                                      MOD-KDKAMP                          
029293                                      MOD-TISTADAT-KAMP                   
029294                                      MOD-TISTODAT-KAMP                   
029500     .                                                                    
029600     SKIP2                                                                
029700 MFS-READ-IN-AGAIN SECTION.                                               
029800                                                                          
029900*    --- ALL INDATA-FIELDS                                                
029910     MOVE +1 TO INDX                                                      
029920     PERFORM UNTIL INDX > MAX-INDX                                        
029930       MOVE MFS-ADD-READ-FIELD      TO MOD-CMD (INDX)                     
029940                                      MOD-TISTADAT-KAMP-RAD (INDX)        
029950                                      MOD-TISTODAT-KAMP-RAD (INDX)        
029960       ADD +1 TO INDX                                                     
029970     END-PERFORM                                                          
030000     MOVE MFS-ADD-READ-FIELD TO MOD-RERESPRT-ATTR                         
030130                                MOD-KDKAMP-ATTR                           
030140                                MOD-TISTADAT-KAMP-ATTR                    
030150                                MOD-TISTODAT-KAMP-ATTR                    
030200     .                                                                    
030300     EJECT                                                                
030400* --- IMS SECTIONS ---                                                    
030500     SKIP3                                                                
030600 IMS-GET-MSG SECTION.                                                     
030700                                                                          
030800     MOVE '  QC' TO GOOD-STATUSCODES                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSCHECK                                              
031200     .                                                                    
031300     SKIP3                                                                
031400 IMS-INSERT-MSG SECTION.                                                  
031500                                                                          
031900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032000     MOVE SPACE TO GOOD-STATUSCODES                                       
032100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032300     PERFORM IMS-STATUSCHECK                                              
032400     .                                                                    
032600     EJECT                                                                
032700 IMS-STATUSCHECK SECTION.                                                 
032800                                                                          
032900     SET STATUS-IX TO 1                                                   
033000     SEARCH GOOD-STATUS                                                   
033100       AT END                                                             
033200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033300         DELIMITED BY SIZE INTO ERROR-TEXT                                
033400         CALL FELLOG                                                      
033500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
033901     EJECT                                                                
033902 DB2-SEARCH-TP1KAMP SECTION.                                              
033903     MOVE 'DB2-SEARCH-TP1KAMP   ' TO  WS-DB2-SEKTION                      
033904     MOVE 000100  TO GOOD-SQLCODECODES                                    
033905     EXEC SQL                                                             
033906       SELECT IDKAMP                                                      
033907             ,KVKAMP_CARS                                                 
033909             ,TISTADAT_KAMP                                               
033910             ,TISTODAT_KAMP                                               
033911             ,RERESPRT                                                    
033912             ,KDKAMP                                                      
033913             ,BEKAMNOT                                                    
033915             ,IDKAMP_GRP                                                  
033916             ,IDLOPNR_KAMP                                                
033930                                                                          
033932       INTO :TP1KAMP-IDKAMP                                               
033934           ,:TP1KAMP-KVKAMP-CARS                                          
033935           ,:TP1KAMP-TISTADAT-KAMP                                        
033936           ,:TP1KAMP-TISTODAT-KAMP                                        
033937           ,:TP1KAMP-RERESPRT                                             
033938           ,:TP1KAMP-KDKAMP                                               
033939           ,:TP1KAMP-BEKAMNOT                                             
033940           ,:TP1KAMP-IDKAMP-GRP                                           
033941           ,:TP1KAMP-IDLOPNR-KAMP                                         
033942                                                                          
033943       FROM TP1KAMP                                                       
033944                                                                          
033945       WHERE IDKAMP = :W-IDKAMP                                           
033946     END-EXEC                                                             
033947     MOVE SQLCODE      TO SQLCODE-WS                                      
033948     PERFORM DB2-STATUS-CHECK                                             
033949     .                                                                    
033950     EJECT                                                                
033951                                                                          
033952 DB2-SELECT-TP1KAMP-SUM SECTION.                                          
033953     MOVE 'DB2-SELECT-TP1KAMP-SUM' TO  WS-DB2-SEKTION                     
033954     MOVE 000100  TO GOOD-SQLCODECODES                                    
033955     EXEC SQL                                                             
033956       SELECT SUM(KVKAMP_CARS)                                            
033957                                                                          
033958       INTO :WS-KVKAMP-CARS-TOT                                           
033959                                                                          
033960       FROM TP1KAMP                                                       
033961                                                                          
033962       WHERE IDKAMP_GRP = :W-IDKAMP-GRP                                   
033963     END-EXEC                                                             
033964     MOVE SQLCODE      TO SQLCODE-WS                                      
033965     PERFORM DB2-STATUS-CHECK                                             
033966     .                                                                    
033967     EJECT                                                                
033968                                                                          
033969 DB2-OPEN-TP1KAMP-CRS1 SECTION.                                           
033970     MOVE 'DB2-OPEN-TP1KAMP-CRS1' TO  WS-DB2-SEKTION                      
033971                                                                          
033972     EXEC SQL DECLARE TP1KAMP-CRS CURSOR FOR                              
033973         SELECT  IDKAMP                                                   
033974                ,KVKAMP_CARS                                              
033975                ,TISTADAT_KAMP                                            
033976                ,TISTODAT_KAMP                                            
033977                ,RERESPRT                                                 
033978                ,KDKAMP                                                   
033979                                                                          
033980         FROM    TP1KAMP                                                  
033981                                                                          
033982         WHERE   IDKAMP_GRP = :W-IDKAMP-GRP                               
033983           AND   IDKAMP >= :W-IDKAMP                                      
033984                                                                          
033985         ORDER BY IDKAMP                                                  
033986                                                                          
033987     END-EXEC                                                             
033988                                                                          
033989     MOVE SQLCODE TO SQLCODE-WS                                           
033990     MOVE 000     TO GOOD-SQLCODECODES                                    
033991     EXEC SQL OPEN TP1KAMP-CRS END-EXEC                                   
033992     PERFORM DB2-STATUS-CHECK                                             
033993     .                                                                    
033994     EJECT                                                                
033995 DB2-FETCH-TP1KAMP-CRS1 SECTION.                                          
033996     MOVE 'DB2-FETCH-TP1KAMP-CRS1' TO  WS-DB2-SEKTION                     
033997     MOVE 000100  TO GOOD-SQLCODECODES                                    
033998     EXEC SQL FETCH TP1KAMP-CRS INTO                                      
033999                :TP1KAMP-IDKAMP                                           
034000               ,:TP1KAMP-KVKAMP-CARS                                      
034001               ,:TP1KAMP-TISTADAT-KAMP                                    
034002               ,:TP1KAMP-TISTODAT-KAMP                                    
034003               ,:TP1KAMP-RERESPRT                                         
034004               ,:TP1KAMP-KDKAMP                                           
034005                                                                          
034006     END-EXEC                                                             
034007                                                                          
034008     MOVE SQLCODE TO SQLCODE-WS                                           
034009     PERFORM DB2-STATUS-CHECK                                             
034010     .                                                                    
034011     SKIP3                                                                
034012                                                                          
034013 DB2-CLOSE-TP1KAMP-CRS1 SECTION.                                          
034014     MOVE 'DB2-CLOSE-TP1KAMP-CRS1' TO  WS-DB2-SEKTION                     
034015                                                                          
034016     EXEC SQL CLOSE TP1KAMP-CRS END-EXEC                                  
034017     .                                                                    
034018     EJECT                                                                
034019 DB2-OPEN-TP1KAMP-CRS2 SECTION.                                           
034020     MOVE 'DB2-OPEN-TP1KAMP-CRS2' TO  WS-DB2-SEKTION                      
034021                                                                          
034022     EXEC SQL DECLARE TP1KAMP-CRS2 CURSOR FOR                             
034023         SELECT  IDKAMP                                                   
034024                ,KVKAMP_CARS                                              
034025                ,TISTADAT_KAMP                                            
034026                ,TISTODAT_KAMP                                            
034027                ,RERESPRT                                                 
034028                ,BEKAMNOT                                                 
034029                ,KDKAMP                                                   
034030                                                                          
034031         FROM    TP1KAMP                                                  
034032                                                                          
034033         WHERE   IDKAMP = :W-IDKAMP                                       
034034                                                                          
034035         ORDER BY IDKAMP                                                  
034036                                                                          
034037                                                                          
034038     END-EXEC                                                             
034039                                                                          
034040     MOVE SQLCODE TO SQLCODE-WS                                           
034041     MOVE 000     TO GOOD-SQLCODECODES                                    
034042     EXEC SQL OPEN TP1KAMP-CRS2 END-EXEC                                  
034043     PERFORM DB2-STATUS-CHECK                                             
034044     .                                                                    
034045     EJECT                                                                
034046 DB2-FETCH-TP1KAMP-CRS2 SECTION.                                          
034047     MOVE 'DB2-FETCH-TP1KAMP-CRS2' TO  WS-DB2-SEKTION                     
034048     SKIP2                                                                
034049     MOVE 000100  TO GOOD-SQLCODECODES                                    
034050     EXEC SQL FETCH TP1KAMP-CRS2 INTO                                     
034051                :TP1KAMP-IDKAMP                                           
034052               ,:TP1KAMP-KVKAMP-CARS                                      
034053               ,:TP1KAMP-TISTADAT-KAMP                                    
034054               ,:TP1KAMP-TISTODAT-KAMP                                    
034055               ,:TP1KAMP-RERESPRT                                         
034056               ,:TP1KAMP-BEKAMNOT                                         
034057               ,:TP1KAMP-KDKAMP                                           
034058                                                                          
034059                                                                          
034060     END-EXEC                                                             
034061                                                                          
034062     MOVE SQLCODE TO SQLCODE-WS                                           
034063     PERFORM DB2-STATUS-CHECK                                             
034064     .                                                                    
034065     SKIP3                                                                
034066                                                                          
034067 DB2-CLOSE-TP1KAMP-CRS2 SECTION.                                          
034068     MOVE 'DB2-CLOSE-TP1KAMP-CRS2' TO  WS-DB2-SEKTION                     
034069                                                                          
034070     EXEC SQL CLOSE TP1KAMP-CRS2 END-EXEC                                 
034071     .                                                                    
034072     EJECT                                                                
034073 DB2-OPEN-TP1KAMP-CRS3 SECTION.                                           
034074     MOVE 'DB2-OPEN-TP1KAMP-CRS3' TO  WS-DB2-SEKTION                      
034075                                                                          
034076     MOVE 000100  TO GOOD-SQLCODECODES                                    
034077     EXEC SQL DECLARE TP1KAMP-CRS3 CURSOR FOR                             
034078         SELECT  IDKAMP                                                   
034079                ,TISTADAT_KAMP                                            
034080                ,TISTODAT_KAMP                                            
034081                                                                          
034082         FROM    TP1KAMP                                                  
034083                                                                          
034084         WHERE   IDKAMP_GRP = :W-IDKAMP-GRP                               
034085                                                                          
034086         ORDER BY IDKAMP                                                  
034087                                                                          
034088                                                                          
034089     END-EXEC                                                             
034090                                                                          
034091     MOVE SQLCODE TO SQLCODE-WS                                           
034092     MOVE 000     TO GOOD-SQLCODECODES                                    
034093     EXEC SQL OPEN TP1KAMP-CRS3 END-EXEC                                  
034094     PERFORM DB2-STATUS-CHECK                                             
034095     .                                                                    
034096     EJECT                                                                
034097 DB2-FETCH-TP1KAMP-CRS3 SECTION.                                          
034098     MOVE 'DB2-FETCH-TP1KAMP-CRS3' TO  WS-DB2-SEKTION                     
034099     MOVE 000100  TO GOOD-SQLCODECODES                                    
034100     EXEC SQL FETCH TP1KAMP-CRS3 INTO                                     
034101                :TP1KAMP-IDKAMP                                           
034102               ,:TP1KAMP-TISTADAT-KAMP                                    
034103               ,:TP1KAMP-TISTODAT-KAMP                                    
034104                                                                          
034105     END-EXEC                                                             
034106                                                                          
034107     MOVE SQLCODE TO SQLCODE-WS                                           
034108     PERFORM DB2-STATUS-CHECK                                             
034109     .                                                                    
034110     SKIP3                                                                
034111                                                                          
034112 DB2-UPDATE-TP1KAMP-CRS3 SECTION.                                         
034113     MOVE 'DB2-UPDATE-TP1KAMP-CRS3' TO  WS-DB2-SEKTION                    
034114                                                                          
034115     MOVE 000     TO GOOD-SQLCODECODES                                    
034116     EXEC SQL                                                             
034117         UPDATE TP1KAMP                                                   
034118             SET TISTADAT_KAMP = :TP1KAMP-TISTADAT-KAMP                   
034119               , TISTODAT_KAMP = :TP1KAMP-TISTODAT-KAMP                   
034120         WHERE   IDKAMP        = :W-IDKAMP                                
034121     END-EXEC                                                             
034122                                                                          
034123     MOVE SQLCODE TO SQLCODE-WS                                           
034124     PERFORM DB2-STATUS-CHECK                                             
034125     .                                                                    
034126     EJECT                                                                
034127                                                                          
034128 DB2-CLOSE-TP1KAMP-CRS3 SECTION.                                          
034129     MOVE 'DB2-CLOSE-TP1KAMP-CRS3' TO  WS-DB2-SEKTION                     
034130                                                                          
034131     EXEC SQL CLOSE TP1KAMP-CRS3 END-EXEC                                 
034132     .                                                                    
034133     EJECT                                                                
034134*                                                                         
034135 DB2-OPEN-TP1KAMP-CRS4 SECTION.                                           
034136     MOVE 'DB2-OPEN-TP1KAMP-CRS4' TO  WS-DB2-SEKTION                      
034137                                                                          
034138     MOVE 000100  TO GOOD-SQLCODECODES                                    
034139     EXEC SQL DECLARE TP1KAMP-CRS4 CURSOR FOR                             
034140         SELECT  IDKAMP                                                   
034141                ,TISTADAT_KAMP                                            
034142                ,TISTODAT_KAMP                                            
034143                                                                          
034144         FROM    TP1KAMP                                                  
034145                                                                          
034146         WHERE   IDKAMP = :W-IDKAMP                                       
034147                                                                          
034148         ORDER BY IDKAMP                                                  
034149                                                                          
034150                                                                          
034151     END-EXEC                                                             
034152                                                                          
034153     MOVE SQLCODE TO SQLCODE-WS                                           
034154     MOVE 000     TO GOOD-SQLCODECODES                                    
034155     EXEC SQL OPEN TP1KAMP-CRS4 END-EXEC                                  
034156     PERFORM DB2-STATUS-CHECK                                             
034157     .                                                                    
034158     EJECT                                                                
034159 DB2-FETCH-TP1KAMP-CRS4 SECTION.                                          
034160     MOVE 'DB2-FETCH-TP1KAMP-CRS4' TO  WS-DB2-SEKTION                     
034161     MOVE 000100  TO GOOD-SQLCODECODES                                    
034162     EXEC SQL FETCH TP1KAMP-CRS4 INTO                                     
034163                :TP1KAMP-IDKAMP                                           
034164               ,:TP1KAMP-TISTADAT-KAMP                                    
034165               ,:TP1KAMP-TISTODAT-KAMP                                    
034166                                                                          
034167     END-EXEC                                                             
034168                                                                          
034169     MOVE SQLCODE TO SQLCODE-WS                                           
034170     PERFORM DB2-STATUS-CHECK                                             
034171     .                                                                    
034172     SKIP3                                                                
034173                                                                          
034174 DB2-UPDATE-TP1KAMP-CRS4 SECTION.                                         
034175     MOVE 'DB2-UPDATE-TP1KAMP-CRS4' TO  WS-DB2-SEKTION                    
034176                                                                          
034177     MOVE 000     TO GOOD-SQLCODECODES                                    
034178     EXEC SQL                                                             
034179         UPDATE TP1KAMP                                                   
034180             SET TISTADAT_KAMP = :TP1KAMP-TISTADAT-KAMP                   
034181               , TISTODAT_KAMP = :TP1KAMP-TISTODAT-KAMP                   
034182         WHERE   IDKAMP        = :W-IDKAMP                                
034183     END-EXEC                                                             
034184                                                                          
034185     MOVE SQLCODE TO SQLCODE-WS                                           
034186     PERFORM DB2-STATUS-CHECK                                             
034187     .                                                                    
034188     EJECT                                                                
034189                                                                          
034190 DB2-CLOSE-TP1KAMP-CRS4 SECTION.                                          
034191     MOVE 'DB2-CLOSE-TP1KAMP-CRS4' TO  WS-DB2-SEKTION                     
034192                                                                          
034193     EXEC SQL CLOSE TP1KAMP-CRS4 END-EXEC                                 
034194     .                                                                    
034195     EJECT                                                                
034196*                                                                         
034197 DB2-SELECT-MAX-TP1KAM-NEWPOST SECTION.                                   
034198     MOVE 'DB2-SELECT-MAX-TP1KAM-NEWPOST' TO  WS-DB2-SEKTION              
034199                                                                          
034200     MOVE 000100  TO GOOD-SQLCODECODES                                    
034201     EXEC SQL                                                             
034202     SELECT   MAX(TP1KAMP.IDLOPNR_KAMP)                                   
034203                                                                          
034204     INTO     :W-IDLOPNR-KAMP                                             
034205                                                                          
034206     FROM     TP1KAMP                                                     
034207                                                                          
034208     END-EXEC                                                             
034209     MOVE SQLCODE TO SQLCODE-WS                                           
034210     PERFORM DB2-STATUS-CHECK                                             
034211     .                                                                    
034212     EJECT                                                                
034213                                                                          
034214 DB2-INSERT-TP1KAMP-NEWPOST SECTION.                                      
034215     MOVE 'DB2-INSERT-TP1KAM-NEWPOST' TO  WS-DB2-SEKTION                  
034216                                                                          
034217     MOVE 000     TO GOOD-SQLCODECODES                                    
034218     EXEC SQL                                                             
034219       INSERT INTO TP1KAMP                                                
034220          (IDKAMP                                                         
034221          ,TISTADAT_KAMP                                                  
034222          ,TISTODAT_KAMP                                                  
034223          ,KDKAMP                                                         
034224          ,IDLOPNR_KAMP)                                                  
034225       VALUES (:W-IDKAMP                                                  
034226              ,:WS-TISTADAT-KAMP                                          
034227              ,:WS-TISTODAT-KAMP                                          
034228              ,:MID-KDKAMP                                                
034229              ,:W-IDLOPNR-KAMP)                                           
034230     END-EXEC                                                             
034231     MOVE SQLCODE TO SQLCODE-WS                                           
034232     PERFORM DB2-STATUS-CHECK                                             
034233     .                                                                    
034234     EJECT                                                                
034235                                                                          
034236 DB2-UPDATE-TP1KAMP-RERESPRT SECTION.                                     
034237     MOVE 'DB2-UPDATE-TP1KAM-RERESPRT' TO  WS-DB2-SEKTION                 
034238                                                                          
034239     MOVE 000     TO GOOD-SQLCODECODES                                    
034240     EXEC SQL                                                             
034241         UPDATE TP1KAMP                                                   
034242         SET RERESPRT      = :WS-RERESPRT-UPDATE                          
034243           , BEKAMNOT      = :TP1KAMP-BEKAMNOT                            
034244                                                                          
034245         WHERE IDKAMP = :W-IDKAMP                                         
034246     END-EXEC                                                             
034247                                                                          
034248     MOVE SQLCODE TO SQLCODE-WS                                           
034249     PERFORM DB2-STATUS-CHECK                                             
034250     .                                                                    
034251     EJECT                                                                
034252                                                                          
034253 DB2-DELETE-TP1KAMP SECTION.                                              
034254     MOVE 'DB2-DELETE-TP1KAMP' TO  WS-DB2-SEKTION                         
034255     MOVE 000   TO GOOD-SQLCODECODES                                      
034256     EXEC SQL                                                             
034257         DELETE FROM TP1KAMP                                              
034258         WHERE IDKAMP = :WS-IDKAMP-DEL                                    
034259     END-EXEC                                                             
034260                                                                          
034261     MOVE SQLCODE TO SQLCODE-WS                                           
034262     PERFORM DB2-STATUS-CHECK                                             
034263     .                                                                    
034264     EJECT                                                                
034265                                                                          
034266 DB2-SELECT-TP1GRP  SECTION.                                              
034267     MOVE 'DB2-SELECT-TP1GRP' TO  WS-DB2-SEKTION                          
034268     MOVE 000100  TO GOOD-SQLCODECODES                                    
034269                                                                          
034270     EXEC SQL                                                             
034271           SELECT IDKAMP_GRP                                              
034272                 ,BEKAMNOT                                                
034273                 ,RERESPRT                                                
034274                                                                          
034275           INTO :TP1GRP-IDKAMP-GRP                                        
034276               ,:TP1GRP-BEKAMNOT                                          
034277               ,:TP1GRP-RERESPRT                                          
034278                                                                          
034279           FROM   TP1GRP                                                  
034280                                                                          
034281           WHERE IDKAMP_GRP = :W-IDKAMP-GRP                               
034282     END-EXEC                                                             
034283     MOVE SQLCODE TO SQLCODE-WS                                           
034284     PERFORM DB2-STATUS-CHECK                                             
034285     .                                                                    
034286     EJECT                                                                
034287                                                                          
034288 DB2-OPEN-TP1GRP-CRS  SECTION.                                            
034289     MOVE 'DB2-OPEN-TP1GRP-CRS' TO  WS-DB2-SEKTION                        
034290                                                                          
034291     MOVE 000100  TO GOOD-SQLCODECODES                                    
034292                                                                          
034293     EXEC SQL DECLARE TP1GRP-CRS CURSOR FOR                               
034294                                                                          
034295           SELECT  IDKAMP_GRP                                             
034296                  ,BEKAMNOT                                               
034297                  ,RERESPRT                                               
034298                                                                          
034299                                                                          
034300           FROM    TP1GRP                                                 
034301           WHERE   IDKAMP_GRP = :W-IDKAMP-GRP                             
034302           ORDER BY IDKAMP_GRP                                            
034303     END-EXEC                                                             
034304                                                                          
034305     MOVE 000100  TO GOOD-SQLCODECODES                                    
034306     EXEC SQL OPEN TP1GRP-CRS END-EXEC                                    
034307                                                                          
034308     .                                                                    
034309     SKIP3                                                                
034310 DB2-FETCH-TP1GRP-CRS  SECTION.                                           
034311     MOVE 'DB2-FETCH-TP1GRP-CRS' TO  WS-DB2-SEKTION                       
034312     MOVE 000100  TO GOOD-SQLCODECODES                                    
034313     EXEC SQL                                                             
034314         FETCH TP1GRP-CRS INTO                                            
034315                          :TP1GRP-IDKAMP-GRP                              
034316                         ,:TP1GRP-BEKAMNOT                                
034317                         ,:TP1GRP-RERESPRT                                
034318     END-EXEC                                                             
034319                                                                          
034320     MOVE SQLCODE TO SQLCODE-WS                                           
034321     PERFORM DB2-STATUS-CHECK                                             
034322     .                                                                    
034323     SKIP3                                                                
034324 DB2-UPDATE-TP1GRP SECTION.                                               
034325     MOVE 'DB2-UPDATE-TP1GRP' TO  WS-DB2-SEKTION                          
034326                                                                          
034327     MOVE 000     TO GOOD-SQLCODECODES                                    
034328     EXEC SQL                                                             
034329         UPDATE TP1GRP                                                    
034330         SET RERESPRT      = :WS-RERESPRT-UPDATE                          
034331           , BEKAMNOT      = :TP1GRP-BEKAMNOT                             
034332                                                                          
034333         WHERE IDKAMP_GRP = :W-IDKAMP-GRP                                 
034334     END-EXEC                                                             
034335                                                                          
034336     MOVE SQLCODE TO SQLCODE-WS                                           
034337     PERFORM DB2-STATUS-CHECK                                             
034338     .                                                                    
034339     EJECT                                                                
034340                                                                          
034341 DB2-CLOSE-TP1GRP-CRS  SECTION.                                           
034342     MOVE 'DB2-CLOSE-TP1GRP-CRS' TO  WS-DB2-SEKTION                       
034343                                                                          
034344     EXEC SQL CLOSE TP1GRP-CRS END-EXEC                                   
034345     .                                                                    
034347     EJECT                                                                
034348 DB2-DCL-OPN-CRS-TP1ARTK SECTION.                                         
034349     MOVE 'DB2-DCL-OPN-CRS-TP1ARTK' TO  WS-DB2-SEKTION                    
034350*    DISPLAY WS-DB2-SEKTION                                               
034351* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
034352     EXEC SQL DECLARE TP1ARTK-CRS CURSOR FOR                              
034353              SELECT IDKAMP                                               
034354                     ,IDARTNR                                             
034360              FROM TP1ARTK                                                
034362              WHERE IDKAMP   = :WS-IDKAMP-DEL                             
034363              AND   IDARTNR >= :W-IDARTNR                                 
034364     END-EXEC                                                             
034365     MOVE 000     TO GOOD-SQLCODECODES                                    
034367     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
034368     MOVE SQLCODE           TO SQLCODE-WS                                 
034369     PERFORM DB2-STATUS-CHECK                                             
034371     .                                                                    
034372     EJECT                                                                
034373 DB2-FETCH-TP1ARTK SECTION.                                               
034374     MOVE 'DB2-FETCH-TP1ARTK' TO  WS-DB2-SEKTION                          
034375*    DISPLAY WS-DB2-SEKTION                                               
034376     MOVE 000100  TO GOOD-SQLCODECODES                                    
034377     EXEC SQL FETCH TP1ARTK-CRS INTO                                      
034378            :TP1ARTK-IDKAMP                                               
034379           ,:TP1ARTK-IDARTNR                                              
034382     END-EXEC                                                             
034383     MOVE SQLCODE           TO SQLCODE-WS                                 
034384     PERFORM DB2-STATUS-CHECK                                             
034386     .                                                                    
034387     EJECT                                                                
034388                                                                          
034389 DB2-DELETE-TP1ARTK SECTION.                                              
034390     MOVE 'DB2-DELETE-TP1ARTK' TO  WS-DB2-SEKTION                         
034392     MOVE 000   TO GOOD-SQLCODECODES                                      
034393     EXEC SQL                                                             
034394         DELETE FROM TP1ARTK                                              
034395         WHERE IDKAMP  = :TP1ARTK-IDKAMP                                  
034396         AND   IDARTNR = :TP1ARTK-IDARTNR                                 
034397     END-EXEC                                                             
034398                                                                          
034399     MOVE SQLCODE TO SQLCODE-WS                                           
034400     PERFORM DB2-STATUS-CHECK                                             
034401     .                                                                    
034402     EJECT                                                                
034403 DB2-CLOSE-TP1ARTK-CRS SECTION.                                           
034404     MOVE 'DB2-CLOSE-TP1ARTK-CRS' TO  WS-DB2-SEKTION                      
034405*    DISPLAY WS-DB2-SEKTION                                               
034406     SKIP2                                                                
034407     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
034408     .                                                                    
034409     EJECT                                                                
034410 DB2-STATUS-CHECK  SECTION.                                               
034411                                                                          
034412     SET SQLCODE-IX TO 1                                                  
034413     SEARCH GOOD-SQLCODE                                                  
034414       AT END                                                             
034415          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
034416          DELIMITED BY SIZE INTO ERROR-TEXT                               
034417          CALL ABEND USING RKOD-ABEND-DB2                                 
034418       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
034420     END-SEARCH                                                           
034500     .                                                                    
