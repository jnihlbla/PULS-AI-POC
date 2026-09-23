000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2246100.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   13/10/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*  FUNKTION:                                                              
000800*  LEVERANSPLANER TILL LEVERANTÖRER VIA EDI PER PERIOD/VECKA.             
000900*  GÄLLER BARA KINA OCH NA-USA NDC'R MED LOKAL ANSKAFFNING.               
001000*  SKAPAR EN FIL MED ARTNR/DC/LEVNR FÖR ARTIKLARMED                       
001100*  KDAVROP = 2 (GÄLLANDE AVROP).                                          
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDD9  (SB)                                 
001400*                              WDB6                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000*    2013-10-11  E-TRACKER: 10205391  RUTIN FÖR VECKO-/PERIOD-BATC        
002100*                                     BILD 2117.                          
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003001*          --- DC-FOM - DC-TOM                                            
003010     SELECT  PARMIN                    ASSIGN TO W22461D1.                
003100*          --- ARTIKLAR MED KDAVROP = 2                                   
003200     SELECT W22461                     ASSIGN TO W22461D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003710 FD  PARMIN                                                               
003720     LABEL RECORD STANDARD                                                
003730     RECORDING F                                                          
003740     BLOCK CONTAINS 0.                                                    
003750 01  FILLER                    PIC X(80).                                 
003760     SKIP3                                                                
003800 FD  W22461                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W22461 -PRE  UT-  -L.                                     
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W2246100'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005000 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005100                                                                          
005200 77  SKRIV-W22461-SW             PIC X       VALUE 'N'.                   
005300     88 SKRIV-W22461                         VALUE 'J'.                   
005400                                                                          
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008210 01  PARM-AREA.                                                           
008220     03  PARM-IDDC-FOM           PIC X(02) VALUE SPACE.                   
008230     03  PARM-IDDC-TOM           PIC X(02) VALUE SPACE.                   
008240     03  FILLER                  PIC X(76) VALUE SPACE.                   
008250     EJECT                                                                
008300 01  UT-AREA-START               PIC X(24)   VALUE                        
008400                                 'UT-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W22461     -PRE UT-                                       
008800     EJECT                                                                
008900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009000*                                                                         
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-WDD901KY-X.                                                    
009600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009800     03  W-IDLEVNR-X.                                                     
009900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
010000     03  W-WDD905KY-X.                                                    
010100         05  W-DAAVROP-AVS       PIC 9(6)    VALUE ZERO.                  
010200         05  W-TILEVDAG          PIC S9      VALUE ZERO COMP-3.           
010300                                                                          
010400     03  W-IDDC-B6-X.                                                     
010500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
010600     SKIP2                                                                
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FINNS                       VALUE '  '.                  
011000     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
011100     SKIP2                                                                
011200 01  GODK-STATUSKODER.                                                    
011300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011400     SKIP3                                                                
011500 01  SSA1                        PIC X(64).                               
011600 01  SSA2                        PIC X(64).                               
011700     EJECT                                                                
011800*    --- IMS FUNKTIONSKODER                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
012300 01  DLI-IO-AREA.                                                         
012400     03  IO-AREA                 PIC X(150).                              
012500*    03  WDD901  -COPY WDD901 -RED IO-AREA.                               
012600     SKIP3                                                                
012700*    03  WDD902  -COPY WDD902 -RED IO-AREA.                               
012800     SKIP3                                                                
012900*    03  WDD905  -COPY WDD905 -RED IO-AREA.                               
013000     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013200                                                                          
013300                                                                          
013400*01  -COPY W0008  -PRE WDD9-                                              
013500     05  FILLER                  PIC X.                                   
013600                                                                          
013700 PROCEDURE DIVISION  USING WDD9-PCB.                                      
013800 MAIN SECTION.                                                            
013900     ENTRY 'DLITCBL' USING WDD9-PCB.                                      
014000                                                                          
014100                                                                          
014200     PERFORM A-INIT                                                       
014300                                                                          
014310     PERFORM S01-LAS-PARMIN                                               
014320                                                                          
014400     PERFORM IMS-GET-WDD9                                                 
014500     PERFORM UNTIL SEGMENT-SAKNAS                                         
014600       EVALUATE WDD9-SEG-NAME-FB                                          
014700         WHEN 'WDD901'                                                    
014800           MOVE IDARTNR   TO W-IDARTNR                                    
014900           MOVE IDDC      TO W-IDDC                                       
015000                                                                          
015100           PERFORM B-KOLLA-SKRIV-W22461                                   
015200                                                                          
015300         WHEN 'WDD902'                                                    
015400           IF SKRIV-W22461                                                
015500             MOVE IDLEVNR   TO W-IDLEVNR                                  
015600           END-IF                                                         
015700         WHEN 'WDD905'                                                    
015800           IF SKRIV-W22461                                                
015900             IF KDAVROP = 2                                               
016000               IF W-IDARTNR = UT-IDARTNR AND                              
016100                  W-IDDC    = UT-IDDC    AND                              
016200                  W-IDLEVNR = UT-IDLEVNR                                  
016300                                                                          
016400                  CONTINUE                                                
016500               ELSE                                                       
016600                 MOVE W-IDARTNR  TO UT-IDARTNR                            
016700                 MOVE W-IDDC     TO UT-IDDC                               
016800                 MOVE W-IDLEVNR  TO UT-IDLEVNR                            
016900                                                                          
017000                 PERFORM S11-SKRIV-W22461                                 
017100               END-IF                                                     
017200             END-IF                                                       
017300           END-IF                                                         
017400       END-EVALUATE                                                       
017500       PERFORM IMS-GET-WDD9                                               
017600     END-PERFORM                                                          
017700     PERFORM Z-FINIT                                                      
017800                                                                          
017900     MOVE ZERO TO RETURN-CODE                                             
018000     GOBACK                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 A-INIT SECTION.                                                          
018400                                                                          
018410     OPEN  INPUT PARMIN                                                   
018500     OPEN OUTPUT W22461                                                   
018600                                                                          
018700     ACCEPT DAGENS-DATUM  FROM DATE                                       
018800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018900                                                                          
019000     MOVE ZERO  TO UT-IDARTNR                                             
019100     MOVE SPACE TO UT-IDDC                                                
019200     MOVE SPACE TO UT-IDLEVNR                                             
019400                                                                          
019500     .                                                                    
019600     EJECT                                                                
019700 B-KOLLA-SKRIV-W22461 SECTION.                                            
019800     MOVE 'B-KOLLA-SKRIV-W22461'  TO CURRENT-SECTION                      
019900                                                                          
020500     IF W-IDDC < PARM-IDDC-FOM                                            
020600     OR W-IDDC > PARM-IDDC-TOM                                            
021000       MOVE NEJ  TO SKRIV-W22461-SW                                       
021100     ELSE                                                                 
021200       MOVE JA   TO SKRIV-W22461-SW                                       
021300     END-IF                                                               
021400                                                                          
021500     .                                                                    
021600     EJECT                                                                
021700 Z-FINIT SECTION.                                                         
021800     CLOSE PARMIN                                                         
021810           W22461                                                         
021900     SKIP2                                                                
022000     MOVE 'S' TO POSTSUM-OPKOD                                            
022100     CALL POSTSUM USING POSTSUM-PARM                                      
022200     .                                                                    
022300     EJECT                                                                
022310 S01-LAS-PARMIN   SECTION.                                                
022320                                                                          
022330     READ PARMIN INTO PARM-AREA                                           
022340     .                                                                    
022350     EJECT                                                                
022400 S11-SKRIV-W22461 SECTION.                                                
022500     MOVE 'S11-SKRIV-W22461 '    TO CURRENT-SECTION                       
022600                                                                          
022700     WRITE UT-POST   FROM UT-AREA                                         
022800                                                                          
022900     MOVE 'REC'     TO POSTSUM-TRANSTYP                                   
023000     MOVE 'W22461' TO POSTSUM-FDNAMN                                      
023100     MOVE 'W22461D2' TO POSTSUM-DDNAMN2                                   
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     .                                                                    
023400     EJECT                                                                
023500 S99-ABEND SECTION.                                                       
023600                                                                          
023700     SKIP2                                                                
023800     MOVE 'S' TO POSTSUM-OPKOD                                            
023900     CALL POSTSUM USING POSTSUM-PARM                                      
024000     CALL ABEND USING RKOD-ABEND                                          
024100     .                                                                    
024200     EJECT                                                                
024300* --- IMS SEKTIONER ---                                                   
024400                                                                          
024500                                                                          
024600 IMS-GET-WDD9   SECTION.                                                  
024700     MOVE 'IMS-GET-WDD9 '    TO DBS-SECTION                               
024800                                                                          
024900     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-AREA                           
025000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
025100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
025200     PERFORM IMS-STATUSKONTROLL                                           
025300     .                                                                    
025400     EJECT                                                                
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
