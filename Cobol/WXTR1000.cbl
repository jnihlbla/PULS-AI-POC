000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WXTR1000.                                                
000400 AUTHOR.         STEFANO GIOBBI.                                          
000500 DATE-WRITTEN.   94/08/23.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT SB.                                            
001000*        DET LÄSER NED WDH101 OCH -11 TILL EN SEKVENSIELL FIL.            
001100*        EN POST PER ARTIKEL SKAPAS, ENDAST CDC-INFORMATION.              
001200*        FILEN BLIR ETT PRIMÄREXTRAKT I VIOS.                             
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLINVA (WDH1)                              
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INVENTERINGSSTATISTIK PER ARTIKEL, ENDAST C1               
002300     SELECT WXTRK6                     ASSIGN TO WXTR10D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  WXTRK6                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  POST -COPY WXTRK6 -PRE  INV-  -L.                                    
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'WXTR1000'.            
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  SW-SKRIV-POST               PIC X       VALUE 'N'.                   
004200 77  SW-ENDAST-WDH101            PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES DAGENS-DATUM.                                       
004600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004900     EJECT                                                                
005000 01  WS-FIX-DATUM.                                                        
005100     03  WS-FIX-TISEGKEY     PIC 9(9).                                    
005200     03  WS-FILLER1 REDEFINES WS-FIX-TISEGKEY.                            
005300         05 WS-FILLER1-1-2   PIC 9(2).                                    
005400         05 WS-TISEGKEY-3-8  PIC 9(6).                                    
005500         05 WS-FILLER1-9     PIC 9(1).                                    
005600     03  WS-FILLER2 REDEFINES WS-FIX-TISEGKEY.                            
005700         05 WS-TISEGKEY-1-8  PIC 9(8).                                    
005800         05 WS-FILLER2-9     PIC 9(1).                                    
005900       EJECT                                                              
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     SKIP2                                                                
006700*    --- PARAMETRAR TILL ABEND                                            
006800                                                                          
006900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500     EJECT                                                                
007600     SKIP2                                                                
007700 01  SPAR-AREA.                                                           
007800     03  SPAR-IDDC               PIC X(2)    VALUE SPACE.                 
007900     03  SPAR-SEG-NAME-FB        PIC X(8)    VALUE SPACE.                 
008000*                                                                         
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  INV-AREA-START              PIC X(24)   VALUE                        
008600                                 'INV-AREA-START  '.                      
008700     SKIP2                                                                
008800                                                                          
008900*01  AREA -COPY WXTRK6     -PRE INV-                                      
009000     EJECT                                                                
009100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009200*                                                                         
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDDC-B6-X.                                                     
009800         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
009900                                                                          
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FINNS                       VALUE '  '.                  
010300     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
010400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010500                                                                          
010600 01  GODK-STATUSKODER.                                                    
010700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800                                                                          
010900 01  SSA1                        PIC X(64).                               
011000 01  SSA2                        PIC X(64).                               
011100     EJECT                                                                
011200                                                                          
011300*    --- IMS FUNKTIONSKODER                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600                                                                          
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011900                                                                          
012000 01  DLI-IO-AREA.                                                         
012100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012200                                                                          
012300     03  WLINVA01 REDEFINES IO-AREA.                                      
012400*        05  -COPY WDH101  -PRE WDH101-                                   
012500     EJECT                                                                
012600                                                                          
012700     03  WLINVA11 REDEFINES IO-AREA.                                      
012800*        05  -COPY WDH111  -PRE WDH111-                                   
012900     EJECT                                                                
013000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013100 01   DLI-IO-AREA-B601.                                                   
013200*     03  -COPY WDB601                                                    
013300     EJECT                                                                
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0008  -PRE INVA-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000*01  -COPY W0008      -PRE WDB6-                                          
014100     05  FILLER                  PIC X.                                   
014200     EJECT                                                                
014300 PROCEDURE DIVISION  USING INVA-PCB WDB6-PCB.                             
014400     ENTRY 'DLITCBL' USING INVA-PCB WDB6-PCB.                             
014500                                                                          
014600     PERFORM A-INIT                                                       
014700     PERFORM IMS-GN-WDH1                                                  
014800     MOVE    NEJ TO SW-SKRIV-POST                                         
014900                                                                          
015000     PERFORM UNTIL SEGMENT-SLUT                                           
015100       EVALUATE TRUE                                                      
015200                                                                          
015300       WHEN INVA-SEG-NAME-FB = 'WDH101  '                                 
015400         IF SPAR-SEG-NAME-FB = 'WDH101  '                                 
015500           PERFORM S11-SKRIV-WXTRK6                                       
015600         END-IF                                                           
015700         PERFORM B-INITIERA-INVENTERINGS-DATA                             
015800         PERFORM C-BEHANDLA-WDH101-DATA                                   
015900                                                                          
016000       WHEN INVA-SEG-NAME-FB = 'WDH111  '                                 
016100         MOVE WDH111-INV-IDDC TO SPAR-IDDC                                
016200                                 W-IDDC-B6                                
016300         PERFORM IMS-GU-WDB601                                            
016400         IF SEGMENT-FINNS                                                 
016500         AND DCS-CDC                                                      
016600           PERFORM D-BEHANDLA-WDH111-DATA                                 
016700           MOVE    JA TO SW-SKRIV-POST                                    
016800         END-IF                                                           
016900       END-EVALUATE                                                       
017000                                                                          
017100       MOVE SPAR-IDDC      TO W-IDDC-B6                                   
017200       PERFORM IMS-GU-WDB601                                              
017300       IF SW-SKRIV-POST    = JA                                           
017400       OR  (SW-SKRIV-POST  = NEJ                                          
017500       AND (SEGMENT-FINNS                                                 
017600       AND (DCS-SDC OR DCS-NDC-NA OR DCS-NDC-PF OR                        
017700            DCS-NDC-OTHERS OR DCS-NDC-SA))                                
017800       AND  SPAR-SEG-NAME-FB = 'WDH101  ')                                
017900         PERFORM S11-SKRIV-WXTRK6                                         
018000       END-IF                                                             
018100                                                                          
018200       MOVE INVA-SEG-NAME-FB TO SPAR-SEG-NAME-FB                          
018300                                                                          
018400       PERFORM IMS-GN-WDH1                                                
018500       MOVE    NEJ   TO SW-SKRIV-POST                                     
018600       MOVE    SPACE TO SPAR-IDDC                                         
018700     END-PERFORM                                                          
018800                                                                          
018900     PERFORM Z-FINIT                                                      
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     OPEN   OUTPUT       WXTRK6                                           
019800                                                                          
019900     ACCEPT DAGENS-DATUM FROM DATE                                        
020000     MOVE   IDPGM        TO   POSTSUM-PROGNAMN                            
020100     .                                                                    
020200     EJECT                                                                
020300 B-INITIERA-INVENTERINGS-DATA SECTION.                                    
020400                                                                          
020500     MOVE ZERO  TO INV-IDARTNR                                            
020600     MOVE ZERO  TO INV-KDINVKAT                                           
020700     MOVE ZERO  TO INV-KVJUSTKV                                           
020800     MOVE ZERO  TO INV-TIM-INV                                            
020900     MOVE SPACE TO INV-TEINVANM                                           
021000     .                                                                    
021100     EJECT                                                                
021200 C-BEHANDLA-WDH101-DATA SECTION.                                          
021300                                                                          
021400     MOVE WDH101-ART-IDARTNR  TO INV-IDARTNR                              
021500     .                                                                    
021600     EJECT                                                                
021700 D-BEHANDLA-WDH111-DATA SECTION.                                          
021800                                                                          
021900     MOVE WDH111-INV-KDINVKAT TO INV-KDINVKAT                             
022000     MOVE WDH111-INV-KVJUSTKV TO INV-KVJUSTKV                             
022100     MOVE WDH111-INV-TISEGKEY TO WS-FIX-TISEGKEY                          
022200     MOVE WS-TISEGKEY-3-8     TO INV-TIM-INV                              
022300     MOVE WDH111-INV-TEINVANM TO INV-TEINVANM                             
022400     .                                                                    
022500     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700                                                                          
022800     CLOSE WXTRK6                                                         
022900                                                                          
023000     MOVE 'S'     TO    POSTSUM-OPKOD                                     
023100     CALL POSTSUM USING POSTSUM-PARM                                      
023200     .                                                                    
023300     EJECT                                                                
023400 S11-SKRIV-WXTRK6 SECTION.                                                
023500                                                                          
023600     WRITE INV-POST   FROM  INV-AREA                                      
023700                                                                          
023800     MOVE 'INV'       TO    POSTSUM-TRANSTYP                              
023900     MOVE 'WXTRK6'    TO    POSTSUM-FDNAMN                                
024000     MOVE 'WXTRK6D1'  TO    POSTSUM-DDNAMN2                               
024100     CALL  POSTSUM    USING POSTSUM-PARM                                  
024200     .                                                                    
024300     EJECT                                                                
024400 IMS-GN-WDH1 SECTION.                                                     
024500                                                                          
024600     CALL    CBLTDLI           USING GN  INVA-PCB  DLI-IO-AREA            
024700     MOVE    INVA-STATUS-CODE  TO    STATUS-WS                            
024800     MOVE    '  GAGKGB'        TO    GODK-STATUSKODER                     
024900     PERFORM IMS-STATUSKONTROLL                                           
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025300 IMS-GU-WDB601    SECTION.                                                
025400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
025500          DELIMITED BY SIZE INTO SSA1                                     
025600     MOVE '  GE' TO GODK-STATUSKODER                                      
025700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
025800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
025900     PERFORM IMS-STATUSKONTROLL                                           
026000     .                                                                    
026100     EJECT                                                                
026200 IMS-STATUSKONTROLL SECTION.                                              
026300                                                                          
026400     SET    STATUS-IX TO 1                                                
026500     SEARCH GODK-STATUS                                                   
026600       AT END                                                             
026700         MOVE ' - - -STATUSKOD EJ = BLANK-BLANK, GE ELLER GB'             
026800                        TO FELTEXT                                        
026900         DISPLAY FELTEXT                                                  
027000         CALL    FELLOG                                                   
027100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027200         CONTINUE                                                         
027300     END-SEARCH                                                           
027400     .                                                                    
