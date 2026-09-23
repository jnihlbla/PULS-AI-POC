000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2343000.                                                
000400*AUTHOR.         HENRIK ARONSSON.                                         
000500*DATE-WRITTEN.   APRIL 93.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER HÄNDELESEREGISTER OCH SKAPAR FIL MED AVROP                 
001100*        FÖR ARTIKLAR MED LV-LEVERANTÖRNR SOM FÅTT                        
001200*        GODKÄNDA/ÄNDRADE/..ETC.  LEVERANSPLANER PÅ 2103-BILDEN.          
001300*        INFO HÄMTAS FRÅN LEVERANSPLANEREGISTRET.                         
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLXXCZ (WDR5)                              
001600*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- FIL MED ARTIKEL + AVROP                                    
003100     SELECT W23430                     ASSIGN TO W23430D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W23430                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100*01  POST -COPY W23430 -PRE  UT-  -L.                                     
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004500                                                                          
004600*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W2343000'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
004901                                                                          
004910*01  -COPY WWDCKONS                                                       
004920                                                                          
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600                                                                          
005700 01  WS-DAAVROP-AVS              PIC 9(6).                                
005800 01  FILLER  REDEFINES WS-DAAVROP-AVS.                                    
005900     03  WS-DAAVROP-SS           PIC 9(2).                                
006000     03  WS-DAAVROP-AAVV         PIC 9(4).                                
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
007100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  UT-AREA-START               PIC X(24)   VALUE                        
008300                                 'UT-AREA-START  '.                       
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W23430     -PRE UT-                                       
008700     EJECT                                                                
008800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*                                                                         
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  NYCKLAR-TILL-DLI.                                                    
009400                                                                          
009500     03  W-WDGXKEY-2245-X.                                                
009600         05  W-IDHTYP-2245       PIC X(4)    VALUE '2245'.                
009700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
009800                                                                          
009900     03  W-WDD901KY-X.                                                    
010000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010200                                                                          
010300     03  W-IDLEVNR-X.                                                     
010400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
010500                                                                          
010600     03  W-KDAVROP-2-X.                                                   
010700         05  W-KDAVROP-2         PIC S9      VALUE +2 COMP-3.             
010800     SKIP2                                                                
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FINNS                       VALUE '  '.                  
011200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011400     SKIP2                                                                
011500 01  GODK-STATUSKODER.                                                    
011600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01  SSA1                        PIC X(64).                               
011900 01  SSA2                        PIC X(64).                               
012000     EJECT                                                                
012100*    --- IMS FUNKTIONSKODER                                               
012200*01  -COPY W0003                                                          
012300     EJECT                                                                
012400*    ---  DLI INPUT-OUTPUT AREA                                           
012500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012600     SKIP3                                                                
012700 01  DLI-IO-AREA.                                                         
012800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012900     SKIP3                                                                
013000     03  WLINLB01 REDEFINES IO-AREA.                                      
013100*        05  -COPY WDD901  -PRE INLB-                                     
013200     SKIP3                                                                
013300     03  WLINLB11 REDEFINES IO-AREA.                                      
013400*        05  -COPY WDD902  -PRE INLB-                                     
013500     SKIP3                                                                
013600     03  WLINLB23 REDEFINES IO-AREA.                                      
013700*        05  -COPY WDD905  -PRE INLB-                                     
013800     SKIP3                                                                
013900     03  WLXXCZ11 REDEFINES IO-AREA.                                      
014000*        05  -COPY WDGX2246  -PRE XXCZ-                                   
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE INLB-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800*01  -COPY W0008  -PRE XXCZ-                                              
014900     05  FILLER                  PIC X.                                   
015000     EJECT                                                                
015100 PROCEDURE DIVISION  USING INLB-PCB XXCZ-PCB.                             
015200     ENTRY 'DLITCBL' USING INLB-PCB XXCZ-PCB.                             
015300                                                                          
015400     SKIP2                                                                
015500     PERFORM A-INIT                                                       
015600                                                                          
015700     PERFORM IMS-GU-XXCZ01                                                
015800     PERFORM IMS-GNP-XXCZ11                                               
015900                                                                          
016000     PERFORM UNTIL SEGMENT-SAKNAS                                         
016100                                                                          
016200       PERFORM B-BEHANDLA-ARTIKEL                                         
016300                                                                          
016400       PERFORM IMS-GNP-XXCZ11                                             
016500     END-PERFORM                                                          
016600                                                                          
016700     PERFORM Z-FINIT                                                      
016800                                                                          
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-INIT SECTION.                                                          
017400                                                                          
017500     OPEN OUTPUT W23430                                                   
017600     .                                                                    
017700     EJECT                                                                
017800 B-BEHANDLA-ARTIKEL SECTION.                                              
017900*****************************************************************         
018000* LÄS GÄLLANDE PLANER PÅ WDD9 FÖR ARTIKEL/LEVERANTÖR (LV)       *         
018100* OCH SKAPA UTFIL                                               *         
018200*****************************************************************         
018300                                                                          
018400     MOVE XXCZ-2246-IDARTNR TO W-IDARTNR                                  
018410     MOVE WC-CDC-SE         TO W-IDDC                                     
018500     MOVE XXCZ-2246-IDLEVNR TO W-IDLEVNR                                  
018600                                                                          
018700     PERFORM IMS-GU-INLB11                                                
018800     IF SEGMENT-FINNS                                                     
018900       PERFORM IMS-GNP-INLB23-GAELLANDE                                   
019000       IF SEGMENT-FINNS                                                   
019100         PERFORM UNTIL SEGMENT-SAKNAS                                     
019200                                                                          
019300           PERFORM BA-SKAPA-SKRIV-UTPOST                                  
019400           PERFORM IMS-GNP-INLB23-GAELLANDE                               
019500                                                                          
019600         END-PERFORM                                                      
019700       ELSE                                                               
019800* ----   ALLA AVROP RENSADE                                               
019900         PERFORM BB-SKAPA-SKRIV-UTPOST-RENSAD                             
020000       END-IF                                                             
020100     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 BA-SKAPA-SKRIV-UTPOST SECTION.                                           
020500                                                                          
020600     MOVE W-IDARTNR        TO UT-IDARTNR                                  
020700     MOVE INLB-DAAVROP-AVS TO WS-DAAVROP-AVS                              
020800     MOVE WS-DAAVROP-AAVV  TO UT-TIAVROP-AVS                              
020900     MOVE INLB-KVAVROP     TO UT-KVAVROP                                  
021000                                                                          
021100     PERFORM S11-SKRIV-W23430                                             
021200     .                                                                    
021300     EJECT                                                                
021400 BB-SKAPA-SKRIV-UTPOST-RENSAD SECTION.                                    
021500*****************************************************************         
021600* SKAPAR EN POST FÖR ATT VISA ATT ALLA AVROP RENSATS            *         
021700*****************************************************************         
021800                                                                          
021900     MOVE W-IDARTNR        TO UT-IDARTNR                                  
022000     MOVE +9999            TO UT-TIAVROP-AVS                              
022100     MOVE ZERO             TO UT-KVAVROP                                  
022200                                                                          
022300     PERFORM S11-SKRIV-W23430                                             
022400     .                                                                    
022500     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700                                                                          
022800     CLOSE W23430                                                         
022900                                                                          
023000     MOVE 'S' TO POSTSUM-OPKOD                                            
023100     CALL POSTSUM USING POSTSUM-PARM                                      
023200     .                                                                    
023300     EJECT                                                                
023400 S11-SKRIV-W23430 SECTION.                                                
023500                                                                          
023600     WRITE UT-POST FROM UT-AREA                                           
023700                                                                          
023800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
023900     MOVE 'W23430'   TO POSTSUM-FDNAMN                                    
024000     MOVE 'W23430D1' TO POSTSUM-DDNAMN2                                   
024100     CALL POSTSUM USING POSTSUM-PARM                                      
024200     .                                                                    
024300     EJECT                                                                
024400 S99-ABEND SECTION.                                                       
024500                                                                          
024600     MOVE 'S' TO POSTSUM-OPKOD                                            
024700     CALL POSTSUM USING POSTSUM-PARM                                      
024800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
024900     .                                                                    
025000     EJECT                                                                
025100* --- IMS SEKTIONER ---                                                   
025200     SKIP3                                                                
025300     EJECT                                                                
025400 IMS-GU-INLB11 SECTION.                                                   
025500                                                                          
025600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
025700          DELIMITED BY SIZE INTO SSA1                                     
025800     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
025900          DELIMITED BY SIZE INTO SSA2                                     
026000     MOVE '  GE' TO GODK-STATUSKODER                                      
026100     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
026200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
026300     PERFORM IMS-STATUSKONTROLL                                           
026400     .                                                                    
026500     EJECT                                                                
026600 IMS-GNP-INLB23-GAELLANDE SECTION.                                        
026700                                                                          
026800     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-2-X ')'                       
026900          DELIMITED BY SIZE INTO SSA1                                     
027000     MOVE '  GE' TO GODK-STATUSKODER                                      
027100     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
027200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
027300     PERFORM IMS-STATUSKONTROLL                                           
027400     .                                                                    
027500     EJECT                                                                
027600 IMS-GU-XXCZ01 SECTION.                                                   
027700                                                                          
027800     STRING 'WLXXCZ01(WDGXKEY  =' W-WDGXKEY-2245-X ')'                    
027900          DELIMITED BY SIZE INTO SSA1                                     
028000     MOVE '  ' TO GODK-STATUSKODER                                        
028100     CALL CBLTDLI USING GU XXCZ-PCB DLI-IO-AREA SSA1                      
028200     MOVE XXCZ-STATUS-CODE TO STATUS-WS                                   
028300     PERFORM IMS-STATUSKONTROLL                                           
028400     .                                                                    
028500     EJECT                                                                
028600 IMS-GNP-XXCZ11 SECTION.                                                  
028700                                                                          
028800     MOVE 'WLXXCZ11 ' TO SSA1                                             
028900     MOVE '  GE' TO GODK-STATUSKODER                                      
029000     CALL CBLTDLI USING GNP XXCZ-PCB DLI-IO-AREA SSA1                     
029100     MOVE XXCZ-STATUS-CODE TO STATUS-WS                                   
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     .                                                                    
029400     EJECT                                                                
029500 IMS-STATUSKONTROLL SECTION.                                              
029600                                                                          
029700     SET STATUS-IX TO 1                                                   
029800     SEARCH GODK-STATUS                                                   
029900       AT END                                                             
030000         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
030100           DELIMITED BY SIZE INTO FELTEXT-STR                             
030200         DISPLAY FELTEXT                                                  
030300         CALL FELLOG                                                      
030400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030500         CONTINUE                                                         
030600     END-SEARCH                                                           
030700     .                                                                    
