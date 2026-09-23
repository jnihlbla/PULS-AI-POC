000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6128700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/08/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        NEDLÄSNING WDL6 PERIOD                                           
000900*                                                                         
000910*                                                                         
001000                                                                          
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*          --- NEDLÄSNING WDL6 R32 PERIOD                                 
001900     SELECT UTFIL                      ASSIGN TO W61287D1.                
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP2                                                                
002300 FILE SECTION.                                                            
002400     SKIP3                                                                
002500 FD  UTFIL                                                                
002600     RECORDING       F                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900*01  POST -COPY W61207 -PRE  UT-  -L.                                     
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(8)    VALUE 'W6128700'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003510 77  WS-IDDC-IX                  PIC 9(3)    VALUE ZERO.                  
003520 77  MAX-IDDC-IX                 PIC 9(3)    VALUE ZERO.                  
003600 01  DAGENS-DATUM-PERIOD         PIC 9(4)    VALUE ZERO.                  
003700 01  DAGENS-DATUM-VECKA          PIC 9(4)    VALUE ZERO.                  
003800 01  FILLER REDEFINES DAGENS-DATUM-VECKA.                                 
003900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004000     03  DAGENS-DATUM-VV         PIC 9(2).                                
004100                                                                          
004200 01    WS-IDDC-TABELL.                                                    
004300    03 WS-VALID-IDDC  OCCURS 200.                                         
004400       05 TAB-IDDC            PIC X(2)       VALUE SPACE.                 
004500       05 TAB-KDDC            PIC X(2)       VALUE SPACE.                 
004510       05 TAB-FLBINNUT        PIC X          VALUE SPACE.                 
004600                                                                          
004700                                                                          
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
005300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005500                                                                          
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900     EJECT                                                                
006000*    --- PARAMETRAR TILL DATKORT                                          
006100*                                                                         
006200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61287'.              
006300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006400*01  -COPY WDATKORT                                                       
006500     EJECT                                                                
006600*01  -COPY WDATAREA                                                       
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007200 01  UT-AREA-START               PIC X(24)   VALUE                        
007300                                 'UT-AREA-START  '.                       
007400                                                                          
007500*01  AREA -COPY W61207     -PRE UT-                                       
007600     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000                                                                          
008100 01  NYCKLAR-TILL-DLI.                                                    
008200     03  W-IDARTNR-X.                                                     
008300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008400                                                                          
008500     03  W-IDDC-B6-X.                                                     
008600         05 W-IDDC-B6                  PIC X(2).                          
008700                                                                          
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
009200                                                                          
009300 01  GODK-STATUSKODER.                                                    
009400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009500                                                                          
009600 01  SSA1                        PIC X(160).                              
009700     EJECT                                                                
009800*    --- IMS FUNKTIONSKODER                                               
009900*01  -COPY W0003                                                          
010000     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6'.                        
010300 01  DLI-IO-WDL6.                                                         
010400     03 IO-AREA     PIC X(600) VALUE SPACE.                               
010500         03 DLI-IO-WDL601 REDEFINES IO-AREA.                              
010600*            05 -COPY WDL601                                              
010700     EJECT                                                                
010800         03 DLI-IO-WDL611 REDEFINES IO-AREA.                              
010900*            05 -COPY WDL611                                              
011000     EJECT                                                                
011100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
011200 01   DLI-IO-AREA-B601.                                                   
011300*     03  -COPY WDB601                                                    
011400                                                                          
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W0008  -PRE WDL6-                                              
011800     05  FILLER                  PIC X.                                   
011900                                                                          
012000*01  -COPY W0008  -PRE WDB6-                                              
012100     05  FILLER                  PIC X.                                   
012200     EJECT                                                                
012300 PROCEDURE DIVISION  USING WDL6-PCB WDB6-PCB.                             
012400 MAIN SECTION.                                                            
012500     ENTRY 'DLITCBL' USING WDL6-PCB WDB6-PCB.                             
012600                                                                          
012700     PERFORM A-INIT                                                       
012800                                                                          
012900     PERFORM IMS-GET-WDL6                                                 
013000     PERFORM UNTIL SEGMENT-SAKNAS                                         
013100       EVALUATE WDL6-SEG-NAME-FB                                          
013200         WHEN 'WDL601'                                                    
013300           MOVE ART-IDARTNR TO UT-IDARTNR                                 
013400         WHEN 'WDL611'                                                    
013500           PERFORM B-URVAL                                                
013600       END-EVALUATE                                                       
013700       PERFORM IMS-GET-WDL6                                               
013800     END-PERFORM                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500 A-INIT SECTION.                                                          
014600                                                                          
014700     OPEN OUTPUT UTFIL                                                    
014800                                                                          
014900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015000     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
015100     MOVE D-VECKA  TO DAGENS-DATUM-VV                                     
015200     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
015300                                                                          
015400     MOVE 'AAVV'             TO DAT-KDDATFORM                             
015500     MOVE DAGENS-DATUM-VECKA TO DAT-I-TIDATUM                             
015600     CALL WDATKONV USING DAT-KDDATFORM                                    
015700                         DAT-I-TIDATUM                                    
015800                         DAT-O-TIDATUM                                    
015900                         DAT-KDSVAR                                       
016000     MOVE DAT-TIAARP TO DAGENS-DATUM-PERIOD                               
016100     PERFORM AA-LADDA-DC-TABELL                                           
016200     .                                                                    
016300     EJECT                                                                
016310 AA-LADDA-DC-TABELL SECTION.                                              
016320                                                                          
016330     INITIALIZE WS-IDDC-TABELL                                            
016340     MOVE +1 TO WS-IDDC-IX                                                
016350                MAX-IDDC-IX                                               
016360     PERFORM IMS-GN-WDB601                                                
016370     PERFORM UNTIL SEGMENT-SAKNAS                                         
016390          MOVE DCS-IDDC     TO TAB-IDDC(WS-IDDC-IX)                       
016391          MOVE DCS-KDDC     TO TAB-KDDC(WS-IDDC-IX)                       
016392          MOVE DCS-FLBINNUT TO TAB-FLBINNUT(WS-IDDC-IX)                   
016396          ADD +1 TO WS-IDDC-IX                                            
016397                    MAX-IDDC-IX                                           
016398          IF WS-IDDC-IX > 200                                             
016399             MOVE 'DC-TABELLEN FULL' TO FELTEXT                           
016400             CALL FELLOG                                                  
016401          END-IF                                                          
016403        PERFORM IMS-GN-WDB601                                             
016404     END-PERFORM                                                          
016405     .                                                                    
016406     EJECT                                                                
016410 B-URVAL SECTION.                                                         
016500                                                                          
016520     MOVE +1 TO WS-IDDC-IX                                                
016530     PERFORM UNTIL (INL-IDDC = TAB-IDDC (WS-IDDC-IX))                     
016540                    OR WS-IDDC-IX > MAX-IDDC-IX                           
016550       ADD +1 TO WS-IDDC-IX                                               
016560     END-PERFORM                                                          
016570     IF WS-IDDC-IX > MAX-IDDC-IX                                          
016571        MOVE 'INL-IDDC UTANFÖR TABELL ELLER FELAKTIGT' TO FELTEXT         
016572        DISPLAY 'INL-IDDC = ' INL-IDDC                                    
016573        DISPLAY 'ART-IDARTNR = ' ART-IDARTNR                              
016574        CALL FELLOG                                                       
016580     END-IF                                                               
016600*    MOVE INL-IDDC TO W-IDDC-B6                                           
016700*    PERFORM IMS-GU-WDB601                                                
016800     IF TAB-FLBINNUT (WS-IDDC-IX) = JA                                    
016900        IF INL-IDPTYP = 'R32'                                             
017000           IF INL-IDUSER-003 = SPACE                                      
017100              CONTINUE                                                    
017200           ELSE                                                           
017300              MOVE 'AAMMDD'     TO DAT-KDDATFORM                          
017400              MOVE INL-TIINLINL TO DAT-I-TIDATUM                          
017500              CALL WDATKONV USING DAT-KDDATFORM                           
017600                                  DAT-I-TIDATUM                           
017700                                  DAT-O-TIDATUM                           
017800                                  DAT-KDSVAR                              
017900                                                                          
018000              IF DAT-KDSVAR-OK                                            
018100                 IF DAT-TIAARP = DAGENS-DATUM-PERIOD                      
018200                    IF INL-KVANTMOT = ZERO                                
018300                    AND INL-KVART-SKROT = ZERO                            
018400                        CONTINUE                                          
018500                    ELSE                                                  
018600                       MOVE INL-ADLAGOMR    TO UT-ADLAGOMR                
018700                       MOVE INL-IDDC        TO UT-IDDC                    
018800                       MOVE INL-IDUSER-003  TO UT-IDUSER-003              
018900                       MOVE INL-KVANTMOT    TO UT-KVANTMOT                
019000                       MOVE INL-KVAVIS      TO UT-KVAVIS                  
019100                       MOVE INL-KVART-SKROT TO UT-KVART-SKROT             
019200                       MOVE INL-TIINLINL    TO UT-TIINLINL                
019300                       PERFORM S11-SKRIV-UTFIL                            
019400                    END-IF                                                
019500                 END-IF                                                   
019600              END-IF                                                      
019700           END-IF                                                         
019800        END-IF                                                            
019900     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 Z-FINIT SECTION.                                                         
020300                                                                          
020400     CLOSE UTFIL                                                          
020500                                                                          
020600     MOVE 'S' TO POSTSUM-OPKOD                                            
020700     CALL POSTSUM USING POSTSUM-PARM                                      
020800     .                                                                    
020900     SKIP3                                                                
021000 S11-SKRIV-UTFIL SECTION.                                                 
021100                                                                          
021200     WRITE UT-POST FROM UT-AREA                                           
021300                                                                          
021400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
021500     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
021600     MOVE 'W61287D1' TO POSTSUM-DDNAMN2                                   
021700     CALL POSTSUM USING POSTSUM-PARM                                      
021800     .                                                                    
021900     EJECT                                                                
022000* --- IMS SEKTIONER ---                                                   
022100                                                                          
022200 IMS-GET-WDL6 SECTION.                                                    
022300     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-WDL6                           
022400     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
022500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022600     PERFORM IMS-STATUSKONTROLL                                           
022700     .                                                                    
022800     SKIP3                                                                
024000                                                                          
024010 IMS-GN-WDB601    SECTION.                                                
024020     MOVE 'WDB601  ' TO SSA1                                              
024030     MOVE '  GB'     TO GODK-STATUSKODER                                  
024040     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
024050     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
024060     PERFORM IMS-STATUSKONTROLL                                           
024070     .                                                                    
024080     EJECT                                                                
024100 IMS-STATUSKONTROLL SECTION.                                              
024200     SET STATUS-IX TO 1                                                   
024300     SEARCH GODK-STATUS                                                   
024400       AT END                                                             
024500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024600           DELIMITED BY SIZE INTO FELTEXT                                 
024700         DISPLAY FELTEXT                                                  
024800         CALL FELLOG                                                      
024900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025000         CONTINUE                                                         
025100     END-SEARCH                                                           
025200     .                                                                    
