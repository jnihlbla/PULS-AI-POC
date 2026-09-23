000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2246000.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   13/10/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKAPAR EN FIL MED LEV.NR / DC OM 2206-FLLEVVB         
000900*        = JA OCH DET ÄR VECKOKÖRNING ELLER OM 2206-FLLEVPLP = JA         
001000*        OCH DET ÄR PERIODKÖRNING.                                        
001100*                                                                         
001200*        PARAMETERKORT LÄSES IN: VECKOKÖRNING / PERIODSLUT.               
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDR2 (H-TYP 2205)                          
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000*    2013-10-08  E-TRACKER: 10205391                                      
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- PARAMETERKORT IN                                           
003100     SELECT PARMIN                     ASSIGN TO W22460D1.                
003200     SKIP2                                                                
003300*          --- LEVNR SOM SKALL GE LEVPLANER FÖR KINA                      
003400     SELECT W22460                     ASSIGN TO W22460D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  PARMIN                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300     SKIP2                                                                
004400 01  FILLER                  PIC X(80).                                   
004500     SKIP3                                                                
004600 FD  W22460                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W22460 -PRE  UT-  -L.                                     
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W2246000'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005800 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005900     EJECT                                                                
006000                                                                          
006100 01  SW-UT-IDLEVNR               PIC X       VALUE 'N'.                   
006200     SKIP2                                                                
006210 01  DAGENS-DAGNR                PIC 9(1) VALUE ZERO.                     
006220                                                                          
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006800     EJECT                                                                
006810 01  FILLER                      PIC X(16)   VALUE 'WWDC99 '.             
006820*01  -COPY WWDC99                                                         
006830     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007410     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007601     EJECT                                                                
007610*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
007620                                                                          
007630*01  -COPY WDATAREA.                                                      
007640     EJECT                                                                
007700*    --- PARAMETRAR TILL ABEND                                            
007800                                                                          
007900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008200     SKIP2                                                                
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL DATKORT                                          
008800*                                                                         
008900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22460'.              
009000     SKIP2                                                                
009100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009200     SKIP2                                                                
009300*01  -COPY WDATKORT                                                       
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL POSTSUM                                          
009600*                                                                         
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800     EJECT                                                                
009900 01  UT-AREA-START               PIC X(24)   VALUE                        
010000                                 'UT-AREA-START  '.                       
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY W22460     -PRE UT-                                       
010400     SKIP3                                                                
010500 01  PARM-AREA.                                                           
010600     03  KORTYP             PIC X(03)  VALUE SPACE.                       
010700     03  PARM-IDDC-FOM      PIC X(02)  VALUE SPACE.                       
010800     03  PARM-IDDC-TOM      PIC X(02)  VALUE SPACE.                       
010900     03  FILLER             PIC X(73)  VALUE SPACE.                       
011000     EJECT                                                                
011100                                                                          
011200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  NYCKLAR-TILL-DLI.                                                    
011800     03  W-WDGXKEY-2205-X.                                                
011900         05  FILLER              PIC X(4)    VALUE '2205'.                
012000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
012100                                                                          
012200     03  W-IDDC-MIN-X.                                                    
012300         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
012410                                                                          
012420     03  W-IDDC-MAX-X.                                                    
012430         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
012440                                                                          
012500     SKIP2                                                                
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FINNS                       VALUE '  '.                  
012900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013100     SKIP2                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR201'.                      
014300 01  DLI-IO-WDR201.                                                       
014400*    03  -COPY WDGX01                                                     
014500     EJECT                                                                
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
014700 01  DLI-IO-WDGX2206.                                                     
014800*    03  -COPY WDGX2206                                                   
014900     EJECT                                                                
015000 LINKAGE SECTION.                                                         
015100                                                                          
015200                                                                          
015300*01  -COPY W0008  -PRE WDR2-                                              
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING WDR2-PCB.                                      
015700 MAIN SECTION.                                                            
015800     ENTRY 'DLITCBL' USING WDR2-PCB.                                      
015900                                                                          
016000                                                                          
016100     PERFORM A-INIT                                                       
016200                                                                          
016300     PERFORM IMS-GU-WDR201-2205                                           
016301                                                                          
016310     MOVE PARM-IDDC-FOM TO W-IDDC-MIN                                     
016320     MOVE PARM-IDDC-TOM TO W-IDDC-MAX                                     
016400     PERFORM IMS-GNP-WDGX2206                                             
016500                                                                          
016600     PERFORM UNTIL SEGMENT-SAKNAS                                         
017100        IF 2206-KDEDI NOT = 'T'                                           
017200*          (TEST-LEV SKALL EJ MED)                                        
017300*          KÖRTYP BESTÄMMER URVAL  (PERIOD /VECKO)                        
017400           IF KORTYP = 'PER' AND                                          
017500              2206-FLLEVPLP  = JA                                         
017600              MOVE 2206-IDLEVNR      TO UT-IDLEVNR                        
017700              MOVE 2206-IDDC         TO UT-IDDC                           
017800              MOVE JA                TO SW-UT-IDLEVNR                     
017900           END-IF                                                         
018000           IF KORTYP = 'VEC' AND                                          
018100              2206-FLLEVVB  = JA                                          
018101              MOVE 2206-IDDC            TO WS-IDDC                        
018104              IF (2206-KDVECKOSL= 'D')                                    
018105             AND ((NDC-CN AND DAGENS-DAGNR = 1) OR                        
018106                  (NDC-US AND DAGENS-DAGNR = 2))                          
018107                 MOVE 2206-IDLEVNR      TO UT-IDLEVNR                     
018108                 MOVE 2206-IDDC         TO UT-IDDC                        
018109                 MOVE JA                TO SW-UT-IDLEVNR                  
018110              ELSE                                                        
018120                 IF (2206-KDVECKOSL= 'V')                                 
018130                AND ((NDC-CN AND DAGENS-DAGNR = 4) OR                     
018140                     (NDC-US AND DAGENS-DAGNR = 5))                       
018200                    MOVE 2206-IDLEVNR      TO UT-IDLEVNR                  
018300                    MOVE 2206-IDDC         TO UT-IDDC                     
018400                    MOVE JA                TO SW-UT-IDLEVNR               
018410                 END-IF                                                   
018500              END-IF                                                      
018501           END-IF                                                         
018600           IF SW-UT-IDLEVNR = JA                                          
018700              PERFORM S11-SKRIV-W22460                                    
018800              MOVE NEJ TO SW-UT-IDLEVNR                                   
018900           END-IF                                                         
019000        END-IF                                                            
019100        PERFORM IMS-GNP-WDGX2206                                          
019200     END-PERFORM                                                          
019300                                                                          
019400                                                                          
019500     PERFORM Z-FINIT                                                      
019600                                                                          
019700     MOVE ZERO TO RETURN-CODE                                             
019800     GOBACK                                                               
019900     .                                                                    
020000     EJECT                                                                
020100 A-INIT SECTION.                                                          
020200     MOVE 'A-INIT  '    TO CURRENT-SECTION                                
020300                                                                          
020400     OPEN INPUT  PARMIN                                                   
020500     OPEN OUTPUT W22460                                                   
020600                                                                          
020650     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
020660     CALL WDATKONV USING DAT-KDDATFORM                                    
020670                         DAT-I-TIDATUM                                    
020680                         DAT-O-TIDATUM                                    
020690                         DAT-KDSVAR                                       
020691                                                                          
020692     MOVE DAT-TID     TO DAGENS-DAGNR                                     
020693                                                                          
020694                                                                          
020700*    HÄMTA DAGENS-DATUM FRÅN DATKORT                                      
020800                                                                          
020900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
021000     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
021100     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
021200     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
021300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021400                                                                          
021500     PERFORM S01-LAS-PARMIN                                               
021600                                                                          
021700     MOVE NEJ TO SW-UT-IDLEVNR                                            
021800                                                                          
021900     .                                                                    
022000     EJECT                                                                
022100 Z-FINIT SECTION.                                                         
022200     MOVE 'Z-FINIT  '   TO CURRENT-SECTION                                
022300                                                                          
022400     CLOSE PARMIN                                                         
022500           W22460                                                         
022600     SKIP2                                                                
022700     MOVE 'S' TO POSTSUM-OPKOD                                            
022800     CALL POSTSUM USING POSTSUM-PARM                                      
022900     .                                                                    
023000     EJECT                                                                
023100 S01-LAS-PARMIN   SECTION.                                                
023200     MOVE 'S01-LAS-PARMIN '   TO CURRENT-SECTION                          
023300     SKIP2                                                                
023400*    LÄS DATAKORT FRÅN JCL OM DET ÄR VECKO ELLER PERIODKÖRNING            
023500                                                                          
023600     READ PARMIN INTO PARM-AREA                                           
023700                                                                          
023800     AT END                                                               
023900        MOVE '***   PARM SAKNAS   ***'  TO FELTEXT-STR                    
024000        PERFORM S99-ABEND                                                 
024100     NOT AT END                                                           
024200        IF KORTYP NOT = 'VEC' AND                                         
024300           KORTYP NOT = 'PER'                                             
024400                                                                          
024500           MOVE  '***   PARM FELAKTIG ***'  TO FELTEXT-STR                
024600           PERFORM S99-ABEND                                              
024700        END-IF                                                            
024800                                                                          
024900        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
025000        MOVE 'W22460'   TO POSTSUM-FDNAMN                                 
025100        MOVE 'W22460D1' TO POSTSUM-DDNAMN2                                
025200        CALL POSTSUM USING POSTSUM-PARM                                   
025300     END-READ                                                             
025400     .                                                                    
025500     EJECT                                                                
025600 S11-SKRIV-W22460 SECTION.                                                
025700     MOVE 'S11-SKRIV-W22460 '   TO CURRENT-SECTION                        
025800                                                                          
025900     WRITE UT-POST FROM UT-AREA                                           
026000                                                                          
026100     MOVE 'LEV'     TO POSTSUM-TRANSTYP                                   
026200     MOVE 'W22460' TO POSTSUM-FDNAMN                                      
026300     MOVE 'W22460D2' TO POSTSUM-DDNAMN2                                   
026400     CALL POSTSUM USING POSTSUM-PARM                                      
026500     .                                                                    
026600     EJECT                                                                
026700 S99-ABEND SECTION.                                                       
026800     MOVE 'S99-ABEND '   TO CURRENT-SECTION                               
026900                                                                          
027000     DISPLAY FELTEXT                                                      
027100     DISPLAY '* *'                                                        
027200     DISPLAY '* *'                                                        
027300     DISPLAY '* *  PROGRAM ABEND'                                         
027400     DISPLAY '* *'                                                        
027500     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * * * '           
027600     DISPLAY '* *'                                                        
027700                                                                          
027800     MOVE 'S' TO POSTSUM-OPKOD                                            
027900     CALL POSTSUM USING POSTSUM-PARM                                      
028000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
028100     .                                                                    
028200     EJECT                                                                
028300* --- IMS SEKTIONER ---                                                   
028400                                                                          
028500     EJECT                                                                
028600 IMS-GU-WDR201-2205 SECTION.                                              
028700     MOVE 'IMS-GU-WDR201-2205' TO DBS-SECTION                             
028800                                                                          
028900     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
029000          DELIMITED BY SIZE INTO SSA1                                     
029100     MOVE '  '   TO GODK-STATUSKODER                                      
029200     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDR201 SSA1                    
029300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
029400     PERFORM IMS-STATUSKONTROLL                                           
029500     .                                                                    
029600     EJECT                                                                
029700 IMS-GNP-WDGX2206 SECTION.                                                
029800     MOVE 'IMS-GNP-WDGX2206 '  TO DBS-SECTION                             
029900                                                                          
029910     STRING 'WDGX2206(IDDC    >=' W-IDDC-MIN-X                            
029920                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
030000          DELIMITED BY SIZE INTO SSA1                                     
030100     MOVE '  GE'      TO GODK-STATUSKODER                                 
030200     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX2206 SSA1                 
030300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
030400     PERFORM IMS-STATUSKONTROLL                                           
030500     .                                                                    
030600     EJECT                                                                
030700 IMS-STATUSKONTROLL SECTION.                                              
030800                                                                          
030900     SET STATUS-IX TO 1                                                   
031000     SEARCH GODK-STATUS                                                   
031100       AT END                                                             
031200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031300           DELIMITED BY SIZE INTO FELTEXT                                 
031400         DISPLAY FELTEXT                                                  
031500         CALL FELLOG                                                      
031600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031700         CONTINUE                                                         
031800     END-SEARCH                                                           
031900     .                                                                    
