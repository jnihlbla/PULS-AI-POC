000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4405A00.                                                
000300 AUTHOR.         BO SVENSSON.                                             
000400 DATE-WRITTEN.   03/03/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        H S S R                                                          
000900*        LÄSER VORKÖ NY (WDA6)                                            
001000*        SKAPAR FIL TILL SALDOUPPFÖLJNING, W44064                         
001100*        SKAPAR FIL TILL RENSNING AV WDA6, W4405B                         
001110*        SKAPAR FIL TILL VOR FOLLOW UP,    W4405A                         
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- FIL TILL SALDOUPPFÖLJNING                                  
002700     SELECT W44064                     ASSIGN TO W4405AD1.                
002800     SKIP2                                                                
002900*          --- FIL MED RENSNINGSPOSTER VORKÖ NY                           
003000     SELECT W4405B                     ASSIGN TO W4405AD2.                
003010*          --- KOMPLETT FIL FÖR VOR-UPPFÖLJING                            
003020     SELECT W4405A                     ASSIGN TO W4405AD3.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W44064                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W440064 -PRE  VORS-  -L.                                  
004100     SKIP3                                                                
004200 FD  W4405B                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W4405B -PRE  VORR-  -L.                                   
004610     SKIP3                                                                
004620 FD  W4405A                                                               
004630     RECORDING       F                                                    
004640     BLOCK CONTAINS  0.                                                   
004650                                                                          
004660*01  POST -COPY WDA601 -PRE  VORE-  -L.                                   
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77  IDPGM                       PIC X(8)    VALUE 'W4405A00'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300     EJECT                                                                
005400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES DAGENS-DATUM.                                       
005600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005900 01  WS-RENS-DATUM               PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES WS-RENS-DATUM.                                      
006100     03  WS-RENS-DATUM-AAR       PIC 9(2).                                
006200     03  WS-RENS-DATUM-MAANAD    PIC 9(2).                                
006300     03  WS-RENS-DATUM-DAG       PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
007200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007300     SKIP2                                                                
007400*    --- PARAMETRAR TILL ABEND                                            
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007900     SKIP2                                                                
008000 01  FELTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700     EJECT                                                                
008800*01  -COPY WORKAREA                                                       
008900     EJECT                                                                
009000*01  -COPY WDATAREA                                                       
009100     EJECT                                                                
009200 01  VORS-AREA-START             PIC X(24)   VALUE                        
009300                                 'VORS-AREA-START  '.                     
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY W440064     -PRE VORS-                                    
009700     EJECT                                                                
009800 01  VORR-AREA-START             PIC X(24)   VALUE                        
009900                                 'VORR-AREA-START  '.                     
010000     SKIP2                                                                
010100                                                                          
010200*01  AREA -COPY W4405B     -PRE VORR-                                     
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP2                                                                
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FINNS                       VALUE '  '.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA6'.                        
012500 01  DLI-IO-WDA6.                                                         
012600*    03  -COPY WDA601                                                     
012700     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900                                                                          
013000                                                                          
013100*01  -COPY W0008  -PRE WDA6-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING WDA6-PCB.                                      
013500 MAIN SECTION.                                                            
013600     ENTRY 'DLITCBL' USING WDA6-PCB.                                      
013700                                                                          
013800     PERFORM A-INIT                                                       
013900                                                                          
014000     PERFORM IMS-GET-WDA6                                                 
014100     PERFORM UNTIL SEGMENT-SAKNAS                                         
014200       EVALUATE WDA6-SEG-NAME-FB                                          
014300         WHEN 'WDA601'                                                    
014400           PERFORM B-TILL-VORS                                            
014500           PERFORM C-TILL-VORR                                            
014510           PERFORM S13-SKRIV-W4405A                                       
014600       END-EVALUATE                                                       
014700       PERFORM IMS-GET-WDA6                                               
014800     END-PERFORM                                                          
014900     PERFORM Z-FINIT                                                      
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015600                                                                          
015700     OPEN OUTPUT W44064                                                   
015800                 W4405B                                                   
015810                 W4405A                                                   
015900                                                                          
016000     ACCEPT DAGENS-DATUM  FROM DATE                                       
016100                                                                          
016110     MOVE DAGENS-DATUM TO WS-RENS-DATUM                                   
016120     IF WS-RENS-DATUM-MAANAD > 6                                          
016130        SUBTRACT 6 FROM WS-RENS-DATUM-MAANAD                              
016140     ELSE                                                                 
016150        SUBTRACT 1 FROM WS-RENS-DATUM-AAR                                 
016160        ADD      6   TO WS-RENS-DATUM-MAANAD                              
016170     END-IF                                                               
016180                                                                          
016191     DISPLAY '*** DAGENS-DATUM  ' DAGENS-DATUM                            
016195     DISPLAY '*** WS-RENS-DATUM ' WS-RENS-DATUM                           
016200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016300                                                                          
016400     .                                                                    
016500     EJECT                                                                
016600 B-TILL-VORS SECTION.                                                     
016700                                                                          
016800     IF (VOR-KVBEART-Q NOT = VOR-KVPREAVB)                                
016900            AND (VOR-KDVORATG = '0' OR '1')                               
017000                                                                          
017100       MOVE VOR-IDARTNR     TO VORS-IDARTNR                               
017200       MOVE VOR-IDDC        TO VORS-IDDC                                  
017300                                                                          
017400       COMPUTE VORS-KVPRERO = VOR-KVBEART-Q                               
017500                            - VOR-KVPREAVB                                
017600                                                                          
017700       PERFORM S11-SKRIV-W44064                                           
017800                                                                          
017900     END-IF                                                               
018000     .                                                                    
018100     EJECT                                                                
018200 C-TILL-VORR SECTION.                                                     
018300                                                                          
018401       IF VOR-TIREGDAT-URSP < WS-RENS-DATUM                               
018402* FIX FÖR ATT RENSA GAMLA RADER                                           
018403          MOVE 010101 TO VOR-TIKLAR                                       
018404          MOVE 999    TO VORR-KVWORKD                                     
018405       ELSE                                                               
018410          IF VOR-TIKLAR > 0                                               
018500          AND VOR-KDVORATG > '1'                                          
018600            MOVE 001          TO WORK-KDCALL                              
018700            MOVE '11'         TO WORK-IDDC                                
018800            MOVE VOR-TIKLAR   TO WORK-TIAAMMDD-FOM                        
018900            MOVE DAGENS-DATUM TO WORK-TIAAMMDD-TOM                        
019000                                                                          
019100            CALL WORKDAY USING                                            
019200                 WORK-KDCALL                                              
019300                 WORK-DATE-AREA                                           
019400                 WORK-KDSVAR                                              
019500                                                                          
019600            IF WORK-KDSVAR = SPACE                                        
019700              MOVE WORK-KVWORKD TO VORR-KVWORKD                           
019800            ELSE                                                          
019900              MOVE 0          TO VORR-KVWORKD                             
020000            END-IF                                                        
020100          ELSE                                                            
020200            MOVE 0            TO VORR-KVWORKD                             
020300          END-IF                                                          
020310       END-IF                                                             
020400                                                                          
020500       MOVE VOR-IDDISTR       TO VORR-IDDISTR                             
020600       MOVE VOR-IDKUNDNR      TO VORR-IDKUNDNR                            
020700       MOVE VOR-IDKUNDRF      TO VORR-IDKUNDRF                            
020800       MOVE VOR-TIREGDAT-URSP TO VORR-TIREGDAT-URSP                       
020900       MOVE VOR-IDARTNR       TO VORR-IDARTNR                             
021000       MOVE VOR-TIREGTID-URSP TO VORR-TIREGTID-URSP                       
021100       MOVE VOR-TIREGDAT-AVV  TO VORR-TIREGDAT-AVV                        
021200       MOVE VOR-TIREGTID-AVV  TO VORR-TIREGTID-AVV                        
021300       MOVE VOR-TIKLAR        TO VORR-TIKLAR                              
021400                                                                          
021500       PERFORM S12-SKRIV-W4405B                                           
021600                                                                          
021700     .                                                                    
021800     EJECT                                                                
021900 Z-FINIT SECTION.                                                         
022000     CLOSE W44064                                                         
022100           W4405B                                                         
022110           W4405A                                                         
022200     SKIP2                                                                
022300     MOVE 'S' TO POSTSUM-OPKOD                                            
022400     CALL POSTSUM USING POSTSUM-PARM                                      
022500     .                                                                    
022600     EJECT                                                                
022700 S11-SKRIV-W44064 SECTION.                                                
022800                                                                          
022900     WRITE VORS-POST FROM VORS-AREA                                       
023000                                                                          
023100     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
023200     MOVE 'W44064' TO POSTSUM-FDNAMN                                      
023300     MOVE 'W4405AD1' TO POSTSUM-DDNAMN2                                   
023400     CALL POSTSUM USING POSTSUM-PARM                                      
023500     .                                                                    
023600     EJECT                                                                
023700 S12-SKRIV-W4405B SECTION.                                                
023800                                                                          
023900     WRITE VORR-POST FROM VORR-AREA                                       
024000                                                                          
024100     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
024200     MOVE 'W4405B' TO POSTSUM-FDNAMN                                      
024300     MOVE 'W4405AD2' TO POSTSUM-DDNAMN2                                   
024400     CALL POSTSUM USING POSTSUM-PARM                                      
024500     .                                                                    
024600     EJECT                                                                
024610 S13-SKRIV-W4405A SECTION.                                                
024620                                                                          
024630     WRITE VORE-POST FROM DLI-IO-WDA6                                     
024640                                                                          
024650     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
024660     MOVE 'W4405A' TO POSTSUM-FDNAMN                                      
024670     MOVE 'W4405AD3' TO POSTSUM-DDNAMN2                                   
024680     CALL POSTSUM USING POSTSUM-PARM                                      
024690     .                                                                    
024691     EJECT                                                                
024700 S99-ABEND SECTION.                                                       
024800                                                                          
024900     SKIP2                                                                
025000     MOVE 'S' TO POSTSUM-OPKOD                                            
025100     CALL POSTSUM USING POSTSUM-PARM                                      
025200     CALL ABEND USING RKOD-ABEND                                          
025300     .                                                                    
025400     EJECT                                                                
025500* --- IMS SEKTIONER ---                                                   
025600                                                                          
025700                                                                          
025800 IMS-GET-WDA6   SECTION.                                                  
025900                                                                          
026000     CALL CBLTDLI USING GN WDA6-PCB DLI-IO-WDA6                           
026100     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
026200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
026300     PERFORM IMS-STATUSKONTROLL                                           
026400     .                                                                    
026500     EJECT                                                                
026600 IMS-STATUSKONTROLL SECTION.                                              
026700                                                                          
026800     SET STATUS-IX TO 1                                                   
026900     SEARCH GODK-STATUS                                                   
027000       AT END                                                             
027100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027200           DELIMITED BY SIZE INTO FELTEXT                                 
027300         DISPLAY FELTEXT                                                  
027400         CALL FELLOG                                                      
027500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027600         CONTINUE                                                         
027700     END-SEARCH                                                           
027800     .                                                                    
