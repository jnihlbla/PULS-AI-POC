000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2363400.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOMPL VECKANS INLEV                                              
000900*        MED AVBOKN PÅ WDD9                                               
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDD9                                       
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
002500*          --- INLEV GÅGNA VECKAN                                         
002600     SELECT W23633                     ASSIGN TO W23634D1.                
002700     SKIP2                                                                
002800*          --- GÅNGNA VV INLEV + WDD9                                     
002900     SELECT W23635                     ASSIGN TO W23634D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W23633                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W23633      -L.                                                
004000     SKIP3                                                                
004100 FD  W23635                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W23635 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W2363400'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005101                                                                          
005110*01  -COPY WWDCKONS                                                       
005120                                                                          
005200 01  ARBETSAREOR.                                                         
005300     03  WS-IDLOPNRM             PIC 9(9).                                
005400     03  FILLER REDEFINES WS-IDLOPNRM.                                    
005500         05  FILLER              PIC 9(1).                                
005600         05  WS-VVD              PIC 9(3).                                
005700         05  FILLER REDEFINES WS-VVD.                                     
005800             07  WS-VV           PIC 9(2).                                
005900             07  FILLER          PIC 9(1).                                
006000         05  WS-LLLL             PIC 9(4).                                
006100         05  FILLER              PIC 9(1).                                
006200     03  WS-IDLOPNRM-PL          PIC 9(9).                                
006300     03  FILLER REDEFINES WS-IDLOPNRM-PL.                                 
006400         05  WS-AA-PL            PIC 9(2).                                
006500         05  WS-VVD-PL           PIC 9(3).                                
006600         05  WS-LLLL-PL          PIC 9(4).                                
006700                                                                          
006800 77  W23633-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W23633                       VALUE 'J'.                   
007000     EJECT                                                                
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600 01  DAGENS-AA                   PIC 9(2).                                
007700 01  DAGENS-VV                   PIC 9(2).                                
007800 01  FOREG-AA                    PIC 9(2).                                
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
008700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008800     SKIP2                                                                
008900*    --- PARAMETRAR TILL ABEND                                            
009000                                                                          
009100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009400     SKIP2                                                                
009500 01  FELTEXT.                                                             
009600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200     EJECT                                                                
010300*01  -COPY WDATAREA                                                       
010400     EJECT                                                                
010500 01  IN-AREA-START               PIC X(24)   VALUE                        
010600                                 'IN-AREA-START  '.                       
010700     SKIP2                                                                
010800                                                                          
010900*01  AREA -COPY W23633     -PRE IN-                                       
011000     EJECT                                                                
011100 01  UT-AREA-START               PIC X(24)   VALUE                        
011200                                 'UT-AREA-START  '.                       
011300     SKIP2                                                                
011400                                                                          
011500*01  AREA -COPY W23635     -PRE UT-                                       
011600     EJECT                                                                
011700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011800*                                                                         
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200 01  NYCKLAR-TILL-DLI.                                                    
012300     03  W-WDD901KY-X.                                                    
012400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012600     03  W-IDLEVNR-X.                                                     
012700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012800     03  W-WDD905KY-X.                                                    
012900         05  W-DAAVROP-AVS       PIC 9(6)    VALUE ZERO.                  
013000         05  W-TILEVDAG          PIC S9(1)   VALUE ZERO COMP-3.           
013100     03  W-IDLOPNRM-X.                                                    
013200         05  W-IDLOPNRM-PL       PIC S9(9)   VALUE ZERO COMP-3.           
013300     SKIP2                                                                
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013900     SKIP2                                                                
014000 01  GODK-STATUSKODER.                                                    
014100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  SSA1                        PIC X(64).                               
014400 01  SSA2                        PIC X(64).                               
014500     EJECT                                                                
014600*    --- IMS FUNKTIONSKODER                                               
014700*01  -COPY W0003                                                          
014800     EJECT                                                                
014900*    ---  DLI INPUT-OUTPUT AREA                                           
015000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
015100 01  DLI-IO-WDD901.                                                       
015200*    03  -COPY WDD901                                                     
015300     EJECT                                                                
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
015500 01  DLI-IO-WDD902.                                                       
015600*    03  -COPY WDD902                                                     
015700     EJECT                                                                
015800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
015900 01  DLI-IO-WDD905.                                                       
016000*    03  -COPY WDD905                                                     
016100     EJECT                                                                
016200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD906'.                      
016300 01  DLI-IO-WDD906.                                                       
016400*    03  -COPY WDD906                                                     
016500     EJECT                                                                
016600 LINKAGE SECTION.                                                         
016700                                                                          
016800                                                                          
016900*01  -COPY W0008  -PRE WDD9-                                              
017000     05  WDD9-IDARTNR            PIC S9(9) COMP-3.                        
017100     05  WDD9-IDDC               PIC X(2).                                
017110     05  WDD9-IDLEVNR            PIC X(5).                                
017200     05  WDD9-WDD905KY.                                                   
017300         07  WDD9-DAAVROP-AVS    PIC 9(6).                                
017400         07  WDD9-TILEVDAG       PIC S9(1) COMP-3.                        
017500     05  WDD9-IDLOPNRM           PIC S9(9) COMP-3.                        
017600     EJECT                                                                
017700 PROCEDURE DIVISION  USING WDD9-PCB.                                      
017800 MAIN SECTION.                                                            
017900     ENTRY 'DLITCBL' USING WDD9-PCB.                                      
018000                                                                          
018100*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
018200                                                                          
018300*    CALL WORKDAY USING WORKDAY                                           
018400                                                                          
018500*------------------------                                                 
018600                                                                          
018700     PERFORM A-INIT                                                       
018800                                                                          
018900     PERFORM S01-LAES-W23633                                              
019000     PERFORM UNTIL END-OF-W23633                                          
019100       MOVE IN-IDARTNR       TO W-IDARTNR                                 
019110       MOVE WC-CDC-SE        TO W-IDDC                                    
019200       MOVE IN-IDLEVNR       TO W-IDLEVNR                                 
019300       PERFORM IMS-GET-WDD902                                             
019400       IF SEGMENT-FINNS                                                   
019500          MOVE IN-IDLOPNRM      TO WS-IDLOPNRM                            
019600          MOVE WS-VVD           TO WS-VVD-PL                              
019700          MOVE WS-LLLL          TO WS-LLLL-PL                             
019800          IF WS-VV > DAGENS-VV                                            
019900             MOVE FOREG-AA      TO WS-AA-PL                               
020000          ELSE                                                            
020100             MOVE DAGENS-AA     TO WS-AA-PL                               
020200          END-IF                                                          
020300          MOVE WS-IDLOPNRM-PL   TO W-IDLOPNRM-PL                          
020400                                                                          
020500          PERFORM IMS-GET-WDD906                                          
020600          PERFORM UNTIL SEGMENT-SAKNAS                                    
020700             PERFORM B-AVB-FINNS                                          
020800             PERFORM IMS-GET-WDD906                                       
020900          END-PERFORM                                                     
021000       END-IF                                                             
021100                                                                          
021200       PERFORM S01-LAES-W23633                                            
021300     END-PERFORM                                                          
021400                                                                          
021500                                                                          
021600     PERFORM Z-FINIT                                                      
021700                                                                          
021800     MOVE ZERO TO RETURN-CODE                                             
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 A-INIT SECTION.                                                          
022300                                                                          
022400     OPEN INPUT  W23633                                                   
022500                                                                          
022600     OPEN OUTPUT W23635                                                   
022700                                                                          
022800     ACCEPT DAGENS-DATUM  FROM DATE                                       
022900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023000                                                                          
023100     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
023200     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
023300                                                                          
023400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
023500                     DAT-O-TIDATUM DAT-KDSVAR                             
023600                                                                          
023700     IF DAT-KDSVAR-OK                                                     
023800       MOVE DAT-TIVV  TO DAGENS-VV                                        
023900       MOVE DAT-TIAA  TO DAGENS-AA                                        
024000       SUBTRACT 1 FROM DAGENS-AA GIVING FOREG-AA                          
024100     ELSE                                                                 
024200       DISPLAY ' FEL I DATKONV '                                          
024300       PERFORM S99-ABEND                                                  
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 B-AVB-FINNS SECTION.                                                     
024800                                                                          
024900     MOVE IN-AREA          TO UT-AREA                                     
025000                                                                          
025100     MOVE KVAVROP-AVB      TO UT-KVAVROP-AVB                              
025200*    HÄMTA FRÅN KEY-FEEDBACK-AREA I LÄNKAREAN                             
025300     MOVE WDD9-DAAVROP-AVS TO UT-DAAVROP-AVS                              
025400     MOVE WDD9-TILEVDAG    TO UT-TILEVDAG                                 
025500                                                                          
025600     PERFORM S11-SKRIV-W23635                                             
025700     .                                                                    
025800     EJECT                                                                
025900 Z-FINIT SECTION.                                                         
026000     CLOSE W23633                                                         
026100           W23635                                                         
026200     SKIP2                                                                
026300     MOVE 'S' TO POSTSUM-OPKOD                                            
026400     CALL POSTSUM USING POSTSUM-PARM                                      
026500     .                                                                    
026600     EJECT                                                                
026700 S01-LAES-W23633  SECTION.                                                
026800     READ W23633 INTO IN-AREA                                             
026900     AT END                                                               
027000        SET END-OF-W23633 TO TRUE                                         
027100                                                                          
027200     NOT AT END                                                           
027300        MOVE 'W23633'   TO POSTSUM-FDNAMN                                 
027400        MOVE 'W23634D1' TO POSTSUM-DDNAMN2                                
027500        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
027600        CALL POSTSUM USING POSTSUM-PARM                                   
027700     END-READ                                                             
027800     .                                                                    
027900     EJECT                                                                
028000 S11-SKRIV-W23635 SECTION.                                                
028100                                                                          
028200     WRITE UT-POST FROM UT-AREA                                           
028300                                                                          
028400     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
028500     MOVE 'W23635'   TO POSTSUM-FDNAMN                                    
028600     MOVE 'W23634D2' TO POSTSUM-DDNAMN2                                   
028700     CALL POSTSUM USING POSTSUM-PARM                                      
028800     .                                                                    
028900     EJECT                                                                
029000 S99-ABEND SECTION.                                                       
029100                                                                          
029200     SKIP2                                                                
029300     MOVE 'S' TO POSTSUM-OPKOD                                            
029400     CALL POSTSUM USING POSTSUM-PARM                                      
029500     CALL ABEND USING RKOD-ABEND                                          
029600     .                                                                    
029700     EJECT                                                                
029800* --- IMS SEKTIONER ---                                                   
029900                                                                          
031000 IMS-GET-WDD902 SECTION.                                                  
031100                                                                          
031200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
031300          DELIMITED BY SIZE INTO SSA1                                     
031400     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
031500          DELIMITED BY SIZE INTO SSA2                                     
031600     MOVE '  GE' TO GODK-STATUSKODER                                      
031700     CALL CBLTDLI USING GU  WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
031800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
031900     PERFORM IMS-STATUSKONTROLL                                           
032000     .                                                                    
032100     EJECT                                                                
032200 IMS-GET-WDD905 SECTION.                                                  
032300                                                                          
032400     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X ')'                        
032500          DELIMITED BY SIZE INTO SSA1                                     
032600     MOVE '  GE' TO GODK-STATUSKODER                                      
032700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
032800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
032900     PERFORM IMS-STATUSKONTROLL                                           
033000     .                                                                    
033100     EJECT                                                                
033200 IMS-GET-WDD906 SECTION.                                                  
033300                                                                          
033400     STRING 'WDD905   '                                                   
033500          DELIMITED BY SIZE INTO SSA1                                     
033600     STRING 'WDD906  (IDLOPNRM =' W-IDLOPNRM-X ')'                        
033700          DELIMITED BY SIZE INTO SSA2                                     
033800     MOVE '  GE' TO GODK-STATUSKODER                                      
033900     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD906 SSA1 SSA2              
034000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300     EJECT                                                                
034400 IMS-STATUSKONTROLL SECTION.                                              
034500                                                                          
034600     SET STATUS-IX TO 1                                                   
034700     SEARCH GODK-STATUS                                                   
034800       AT END                                                             
034900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035000           DELIMITED BY SIZE INTO FELTEXT                                 
035100         DISPLAY FELTEXT                                                  
035200         CALL FELLOG                                                      
035300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035400         CONTINUE                                                         
035500     END-SEARCH                                                           
035600     .                                                                    
