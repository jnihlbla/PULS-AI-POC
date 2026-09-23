000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6120700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/08/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DAGLIG NEDLÄSNING WDL6                                           
000900*        MEN FÖRST, LÄS IGENOM HELA DC-BASEN WDB6                         
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- DAGLIG NEDLÄSNING WDL6 R32                                 
002000     SELECT UTFIL                      ASSIGN TO W61207D1.                
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP2                                                                
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD  UTFIL                                                                
002700     RECORDING       F                                                    
002800     BLOCK CONTAINS  0.                                                   
002900                                                                          
003000*01  POST -COPY W61207 -PRE  UT-  -L.                                     
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W6120700'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 01    WS-IDDC-TABELL.                                                    
003900    03 WS-VALID-IDDC  OCCURS 100 INDEXED BY WS-IDDC-IX.                   
004000       05 WS-IDDC             PIC X(2).                                   
004100       05 WS-KDDC             PIC X(2).                                   
004200          88 WS-CDC           VALUE 'C '.                                 
004300          88 WS-CDC-TR        VALUE 'TR'.                                 
004400          88 WS-SDC           VALUE 'S '.                                 
004500          88 WS-NDC-NA        VALUE 'NA'.                                 
004600          88 WS-NDC-PF        VALUE 'NP'.                                 
004610          88 WS-NDC-OTHERS    VALUE 'NX'.                                 
004610          88 WS-NDC-SA        VALUE 'NS'.                                 
004700          88 WS-DDC           VALUE 'D '.                                 
004800       05 WS-IDLANDX2         PIC X(2).                                   
004900       05 WS-FLBINNUT         PIC X.                                      
005000                                                                          
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600                                                                          
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200                                                                          
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL DATKORT                                          
006800*                                                                         
006900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61207'.              
007000 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
007100*01  -COPY WDATKORT                                                       
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  UT-AREA-START               PIC X(24)   VALUE                        
007800                                 'UT-AREA-START  '.                       
007900                                                                          
008000*01  AREA -COPY W61207     -PRE UT-                                       
008100     EJECT                                                                
008200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500                                                                          
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-IDARTNR-X.                                                     
008800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008900                                                                          
009000*    --- STATUS-KOD FRÅN IMS                                              
009100 01  STATUS-WS                   PIC XX.                                  
009200     88  SEGMENT-FINNS                       VALUE '  '.                  
009300     88  BASEN-SLUT                          VALUE 'GB'.                  
009400                                                                          
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(160).                              
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6'.                        
010500 01  DLI-IO-WDL6.                                                         
010600     03 IO-AREA     PIC X(600) VALUE SPACE.                               
010700         03 DLI-IO-WDL601 REDEFINES IO-AREA.                              
010800*            05 -COPY WDL601                                              
010900     EJECT                                                                
011000         03 DLI-IO-WDL611 REDEFINES IO-AREA.                              
011100*            05 -COPY WDL611                                              
011200     EJECT                                                                
011300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
011400 01   DLI-IO-AREA-B601.                                                   
011500*     03  -COPY WDB601                                                    
011600                                                                          
011700 LINKAGE SECTION.                                                         
011800                                                                          
011900*01  -COPY W0008  -PRE WDL6-                                              
012000     05  FILLER                  PIC X.                                   
012100                                                                          
012200*01  -COPY W0008  -PRE WDB6-                                              
012300     05  FILLER                  PIC X.                                   
012400     EJECT                                                                
012500 PROCEDURE DIVISION  USING WDL6-PCB WDB6-PCB.                             
012600 MAIN SECTION.                                                            
012700     ENTRY 'DLITCBL' USING WDL6-PCB WDB6-PCB.                             
012800                                                                          
012900     PERFORM A-INIT                                                       
013000                                                                          
013100     PERFORM IMS-GET-WDL6                                                 
013200     PERFORM UNTIL BASEN-SLUT                                             
013300       EVALUATE WDL6-SEG-NAME-FB                                          
013400         WHEN 'WDL601'                                                    
013500           MOVE ART-IDARTNR TO UT-IDARTNR                                 
013600         WHEN 'WDL611'                                                    
013700           PERFORM B-URVAL                                                
013800       END-EVALUATE                                                       
013900       PERFORM IMS-GET-WDL6                                               
014000     END-PERFORM                                                          
014100                                                                          
014200     PERFORM Z-FINIT                                                      
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800                                                                          
014900     OPEN OUTPUT UTFIL                                                    
015000                                                                          
015100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015200     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
015300     MOVE D-MAANAD TO DAGENS-DATUM-MAANAD                                 
015400     MOVE D-DAG    TO DAGENS-DATUM-DAG                                    
015500     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
015600                                                                          
015700*** LÄS IN HELA DC-BASEN WDB6 OCH FYLL I DC-TABELL                        
015800     INITIALIZE WS-IDDC-TABELL                                            
015900     SET WS-IDDC-IX TO +1                                                 
016000     PERFORM IMS-GN-WDB601                                                
016100     PERFORM UNTIL BASEN-SLUT                                             
016200        MOVE DCS-IDDC             TO WS-IDDC    (WS-IDDC-IX)              
016300        MOVE DCS-KDDC             TO WS-KDDC    (WS-IDDC-IX)              
016400        MOVE DCS-FLBINNUT         TO WS-FLBINNUT(WS-IDDC-IX)              
016500        MOVE DCS-IDLANDX2         TO WS-IDLANDX2(WS-IDDC-IX)              
016600        PERFORM IMS-GN-WDB601                                             
016700        SET WS-IDDC-IX UP BY +1                                           
016800        IF WS-IDDC-IX > 100                                               
016900           MOVE 'DC-TABELLEN FULL' TO FELTEXT                             
017000           CALL FELLOG                                                    
017100        END-IF                                                            
017200     END-PERFORM                                                          
017300     .                                                                    
017400     EJECT                                                                
017500 B-URVAL SECTION.                                                         
017600                                                                          
017700*** FÖRST, KOLLA I DC-TABELL OM DC ÄR AKTUELLT                            
017800     IF NOT WS-IDDC (WS-IDDC-IX) = INL-IDDC                               
017900        SET WS-IDDC-IX TO 1                                               
018000        SEARCH WS-VALID-IDDC                                              
018100          AT END                                                          
018200         STRING 'DC SAKNAS PÅ WDB6  ' INL-IDDC                            
018300           DELIMITED BY SIZE INTO FELTEXT                                 
018400         CALL FELLOG                                                      
018500          WHEN WS-IDDC (WS-IDDC-IX) = INL-IDDC CONTINUE                   
018600        END-SEARCH                                                        
018700     END-IF                                                               
018800                                                                          
018900     IF WS-FLBINNUT(WS-IDDC-IX) = JA                                      
019000        IF INL-IDPTYP = 'R32'                                             
019100           AND INL-TIINLINL = DAGENS-DATUM                                
019200           IF INL-IDUSER-003 NOT = SPACE                                  
019300              IF INL-KVANTMOT = ZERO AND INL-KVART-SKROT = 0              
019400                 CONTINUE                                                 
019600              ELSE                                                        
019700                 MOVE INL-ADLAGOMR    TO UT-ADLAGOMR                      
019800                 MOVE INL-IDDC        TO UT-IDDC                          
019900                 MOVE INL-IDUSER-003  TO UT-IDUSER-003                    
020000                 MOVE INL-KVANTMOT    TO UT-KVANTMOT                      
020100                 MOVE INL-KVAVIS      TO UT-KVAVIS                        
020200                 MOVE INL-KVART-SKROT TO UT-KVART-SKROT                   
020300                 MOVE INL-TIINLINL    TO UT-TIINLINL                      
020400                 PERFORM S11-SKRIV-UTFIL                                  
020500              END-IF                                                      
020600           END-IF                                                         
020700        END-IF                                                            
020800     END-IF                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 Z-FINIT SECTION.                                                         
021200                                                                          
021300     CLOSE UTFIL                                                          
021400                                                                          
021500     MOVE 'S' TO POSTSUM-OPKOD                                            
021600     CALL POSTSUM USING POSTSUM-PARM                                      
021700     .                                                                    
021800     SKIP3                                                                
021900 S11-SKRIV-UTFIL SECTION.                                                 
022000                                                                          
022100     WRITE UT-POST FROM UT-AREA                                           
022200                                                                          
022300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
022400     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
022500     MOVE 'W61207D1' TO POSTSUM-DDNAMN2                                   
022600     CALL POSTSUM USING POSTSUM-PARM                                      
022700     .                                                                    
022800     EJECT                                                                
022900* --- IMS SEKTIONER ---                                                   
023000                                                                          
023100 IMS-GET-WDL6 SECTION.                                                    
023200     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-WDL6                           
023300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
023400     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
023500     PERFORM IMS-STATUSKONTROLL                                           
023600     .                                                                    
023700     SKIP3                                                                
023800 IMS-GN-WDB601    SECTION.                                                
023900     MOVE 'WDB601  ' TO SSA1                                              
024000     MOVE '  GB'     TO GODK-STATUSKODER                                  
024100     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
024200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
024300     PERFORM IMS-STATUSKONTROLL                                           
024400     .                                                                    
024500 IMS-STATUSKONTROLL SECTION.                                              
024600     SET STATUS-IX TO 1                                                   
024700     SEARCH GODK-STATUS                                                   
024800       AT END                                                             
024900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025000           DELIMITED BY SIZE INTO FELTEXT                                 
025100         DISPLAY FELTEXT                                                  
025200         CALL FELLOG                                                      
025300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025400         CONTINUE                                                         
025500     END-SEARCH                                                           
025600     .                                                                    
