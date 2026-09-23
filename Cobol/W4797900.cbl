000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4797900.                                                
000400*AUTHOR.         BOO HAMMARIN CGL.                                        
000500*DATE-WRITTEN.   92/03/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER WDE601-611 OCH SELEKTERAR RAKT AV TILL TVÅ             
001100*                            SB-FILER                                     
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
002500*          --- UTFIL                                                      
002600     SELECT W47924                    ASSIGN TO W47979D1.                 
002810     SELECT W47925X                   ASSIGN TO W47979D2.                 
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W47924                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W47924 -PRE  W47924-  -L.                                 
003900     EJECT                                                                
005110 FD  W47925X                                                              
005120     RECORDING       F                                                    
005130     BLOCK CONTAINS  0.                                                   
005140     SKIP2                                                                
005150*01  POST -COPY W47925X -PRE  W47925X- -L.                                
005160     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300     SKIP2                                                                
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600 77  IDPGM                       PIC X(8)    VALUE 'W4797900'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  PGM-POS                     PIC X(24)   VALUE SPACE.                 
006000 77  FILLER                      PIC X(8)    VALUE 'ZZZZZZZZ'.            
006100 77  POST-RAKNARE                PIC 9(5).                                
006200                                                                          
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
007600     SKIP2                                                                
007700*    --- PARAMETRAR TILL ABEND                                            
007800                                                                          
007900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008100     SKIP2                                                                
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500     EJECT                                                                
008600*    ------- PARAMETRAR TILL DATUMKORT                                    
008700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47979'.              
008800     SKIP2                                                                
008900 01  DATUMKORT-ID            PIC X(6)   VALUE '000001'.                   
009000                                                                          
009100*    -COPY WDATKORT                                                       
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL POSTSUM                                          
009400*                                                                         
009500*01  -COPY W0005   -PRE  POSTSUM-                                         
009600     EJECT                                                                
009700 01  UT-AREA-START               PIC X(24)   VALUE                        
009800                                 'UT-AREA-START  '.                       
009900     SKIP2                                                                
010000                                                                          
010100*01  AREA -COPY W47924    -PRE U24-                                       
010200     EJECT                                                                
010410*01  AREA -COPY W47925X   -PRE U25X-                                      
010420     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900                                                                          
011000 01 NYCKLAR-TILL-DLI.                                                     
011100    03 W-WDE4F1KY-MIN-X.                                                  
011200      05 W-IDPRODNR-MIN    PIC S9(7) VALUE +0 COMP-3.                     
011300      05 W-IDKOLLI-MIN     PIC S9(5) VALUE +0 COMP-3.                     
011400      05 FILLER            PIC X(22) VALUE LOW-VALUE.                     
011500                                                                          
011600    03 W-WDE4F1KY-MAX-X.                                                  
011700      05 W-IDPRODNR-MAX    PIC S9(7) VALUE +0 COMP-3.                     
011800      05 W-IDKOLLI-MAX     PIC S9(5) VALUE +0 COMP-3.                     
011900      05 FILLER            PIC X(22) VALUE HIGH-VALUE.                    
011910                                                                          
011920    03  W-IDSHIPM-X.                                                      
011930        05  W-IDSHIPM      PIC  9(7)   VALUE ZERO.                        
012000     SKIP3                                                                
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  FILLER                      PIC X(08)   VALUE 'STATUSWS'.            
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012410     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     88  BASEN-SLUT                          VALUE 'GB'.                  
012600     SKIP2                                                                
012700 01  GODK-STATUSKODER.                                                    
012800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     SKIP3                                                                
013000 01  SSA1                        PIC X(128).                              
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013700     SKIP3                                                                
013800 01  DLI-IO-AREA.                                                         
013900     03  IO-AREA                 PIC X(352)  VALUE SPACE.                 
014000     03  IO-E601                 REDEFINES IO-AREA.                       
014100*      05  -COPY WDE601                                                   
014200     SKIP2                                                                
014300     03  IO-E611                 REDEFINES IO-AREA.                       
014400*      05  -COPY WDE611                                                   
014500     EJECT                                                                
014600 01  FILLER                  PIC X(16) VALUE 'WDE4F1-AREA'.               
014700 01  DLI-IO-E4F1.                                                         
014800*    03  -COPY WDE4F1                                                     
014900     EJECT                                                                
014910 01  FILLER                  PIC X(16) VALUE 'WDE101-AREA'.               
014920 01  DLI-IO-E101.                                                         
014930*    03  -COPY WDE101                                                     
014940     EJECT                                                                
015000 LINKAGE SECTION.                                                         
015100*01  -COPY W0008  -PRE WDE6-                                              
015200     05  FILLER                  PIC X.                                   
015300     EJECT                                                                
015400*01  -COPY W0008  -PRE WDE4F-                                             
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015610*01  -COPY W0008  -PRE WDE1-                                              
015620     05  FILLER                  PIC X.                                   
015630     EJECT                                                                
015700 PROCEDURE DIVISION  USING WDE6-PCB WDE4F-PCB WDE1-PCB.                   
015800     ENTRY 'DLITCBL' USING WDE6-PCB WDE4F-PCB WDE1-PCB.                   
015900                                                                          
016000     PERFORM A-INIT                                                       
016100     PERFORM IMS-GET-WDE6                                                 
016200                                                                          
016300     PERFORM UNTIL BASEN-SLUT                                             
016400        EVALUATE WDE6-SEG-NAME-FB                                         
016500        WHEN 'WDE601  '                                                   
016600           PERFORM B-FLYTTA-WDE601-INFO                                   
016700        WHEN 'WDE611  '                                                   
016800           PERFORM C-FLYTTA-WDE611-INFO                                   
016900                                                                          
017000           MOVE KOLLI-IDKOLLI       TO W-IDKOLLI-MIN                      
017100                                       W-IDKOLLI-MAX                      
017200           PERFORM IMS-GU-WDE4F1                                          
017300           PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                     
017400             PERFORM E-FLYTTA-WDE4-INFO                                   
017900                                                                          
017901             IF KOLLI-TIFAKT = DAGENS-DATUM                               
017903                PERFORM F-KOMPLETTERA-WDE1-INFO                           
017904             ELSE                                                         
017905                MOVE ZERO TO U25X-TISKPTID                                
017906                             U25X-TISKEPPN                                
017909             END-IF                                                       
017910                                                                          
017911             IF (KOLLI-TIPACKN = DAGENS-DATUM                             
017912                 AND KOLLI-KDKOLSTA > ZERO)                               
017913             OR KOLLI-TIFAKT  = DAGENS-DATUM                              
017914                PERFORM S13-SKRIV-W47925X                                 
017920             END-IF                                                       
017930                                                                          
018000             PERFORM S12-SKRIV-W47924                                     
018100             PERFORM IMS-GN-WDE4F1                                        
018200           END-PERFORM                                                    
018300        END-EVALUATE                                                      
018400        PERFORM IMS-GET-WDE6                                              
018500     END-PERFORM                                                          
018600                                                                          
018700     PERFORM Z-FINIT                                                      
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT SECTION.                                                          
019400                                                                          
019500     OPEN OUTPUT W47924                                                   
019710                 W47925X                                                  
019800                                                                          
019900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020000                                                                          
020100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020200                                                                          
020300     MOVE D-AAR                TO  DAGENS-DATUM-AAR                       
020400     MOVE D-MAANAD             TO  DAGENS-DATUM-MAANAD                    
020500     MOVE D-DAG                TO  DAGENS-DATUM-DAG                       
021000*    FIXA DATUM FÖR OMKÖRNING                                             
021010*    MOVE 160423 TO DAGENS-DATUM                                          
021020*                                                                         
021100     DISPLAY 'DAGENS-DATUM=' DAGENS-DATUM                                 
021200     .                                                                    
021300     EJECT                                                                
021400 B-FLYTTA-WDE601-INFO SECTION.                                            
021500                                                                          
021600     MOVE VORD-IDPRODNR   TO U24-IDPRODNR                                 
021710                             U25X-IDPRODNR                                
021800                             W-IDPRODNR-MIN                               
021900                             W-IDPRODNR-MAX                               
022000     MOVE VORD-IDDC       TO U24-IDDC                                     
022100     MOVE VORD-DABEGPAC (3:6)  TO U25X-TIBEGPAC                           
022110                                                                          
022120*    MOVE VORD-TIUTSKR    TO U25X-TIUTSKR                                 
022130*    MOVE VORD-TIUTSTID   TO U25X-TIUTSTID                                
022200     .                                                                    
022300     EJECT                                                                
022400 C-FLYTTA-WDE611-INFO SECTION.                                            
022500                                                                          
022600     MOVE KOLLI-IDFAKT    TO U24-IDFAKT                                   
022710                             U25X-IDFAKT                                  
022800     MOVE KOLLI-IDKOLLI   TO U25X-IDKOLLI                                 
022900     MOVE KOLLI-TIPACKN   TO U25X-TIPACKN                                 
022920     MOVE KOLLI-TIPACTID  TO U25X-TIPACTID                                
023000     MOVE KOLLI-IDPLOCK   TO U25X-IDPLOCK                                 
023100     MOVE KOLLI-KDKOLSTA  TO U24-KDKOLSTA                                 
023110     MOVE KOLLI-DASUPREF  TO U25X-DASUPREF                                
023111     MOVE KOLLI-IDLBBET   TO U25X-IDLBBET                                 
023115     MOVE KOLLI-IDSHIPM   TO U25X-IDSHIPM                                 
023116     IF U25X-IDSHIPM NOT NUMERIC                                          
023117        MOVE ZERO         TO U25X-IDSHIPM                                 
023118     END-IF                                                               
023119     MOVE KOLLI-KDKOLLI   TO U25X-KDKOLLI                                 
023120     MOVE KOLLI-KDVIA     TO U25X-KDVIA                                   
023121     MOVE KOLLI-TISUPTID  TO U25X-TISUPTID                                
023130     MOVE KOLLI-TILASTN   TO U25X-TILASTN                                 
023140     MOVE KOLLI-TILASTID  TO U25X-TILASTID                                
023150     MOVE KOLLI-TIFAKT    TO U25X-TIFAKT                                  
023160     MOVE KOLLI-TIFAKTID  TO U25X-TIFAKTID                                
023200     .                                                                    
023300     EJECT                                                                
023400 E-FLYTTA-WDE4-INFO SECTION.                                              
023500                                                                          
023600     MOVE SEQF-IDDISTR    TO U24-IDDISTR                                  
023700     MOVE SEQF-IDKUNDNR   TO U24-IDKUNDNR                                 
023800     MOVE SEQF-IDKUNDRF   TO U24-IDKUNDRF                                 
023900     MOVE SEQF-IDPLKLST   TO U24-IDPLKLST                                 
024000     MOVE SEQF-IDPURAD    TO U24-IDPURAD                                  
024100     MOVE SEQF-KVLEVART   TO U24-KVLEVART                                 
024200     .                                                                    
024300     EJECT                                                                
024400 F-KOMPLETTERA-WDE1-INFO SECTION.                                         
024500                                                                          
024503     MOVE U25X-IDSHIPM     TO W-IDSHIPM                                   
024505     PERFORM IMS-GU-WDE101                                                
024507     IF SEGMENT-FINNS                                                     
024508        MOVE SHIP-TISKEPPN TO U25X-TISKEPPN                               
024509        MOVE SHIP-TISKPTID TO U25X-TISKPTID                               
024510     ELSE                                                                 
024511        MOVE ZERO            TO U25X-TISKEPPN                             
024512                                U25X-TISKPTID                             
024513     END-IF                                                               
024516     .                                                                    
024520     EJECT                                                                
024530 Z-FINIT SECTION.                                                         
024540                                                                          
024600     CLOSE W47924                                                         
024810           W47925X                                                        
024900                                                                          
025000     MOVE 'S' TO POSTSUM-OPKOD                                            
025100     CALL POSTSUM USING POSTSUM-PARM                                      
025200     .                                                                    
025300     EJECT                                                                
026400 S12-SKRIV-W47924 SECTION.                                                
026500                                                                          
026600     WRITE W47924-POST FROM U24-AREA                                      
026700     MOVE 'U24'      TO POSTSUM-TRANSTYP                                  
026800     MOVE 'W47924'   TO POSTSUM-FDNAMN                                    
026900     MOVE 'W47979D1' TO POSTSUM-DDNAMN2                                   
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027800     .                                                                    
027900     EJECT                                                                
027910 S13-SKRIV-W47925X SECTION.                                               
027920                                                                          
027930     IF  KOLLI-TIPACKN = DAGENS-DATUM                                     
027940     AND KOLLI-KDKOLSTA > ZERO                                            
027941                                                                          
027950         IF KOLLI-TIFAKT  = DAGENS-DATUM                                  
027960            MOVE 'PF ' TO U25X-IDPTYP                                     
027970         ELSE                                                             
027980            MOVE 'P  ' TO U25X-IDPTYP                                     
027981         END-IF                                                           
027982     ELSE                                                                 
027983         MOVE 'F  '    TO U25X-IDPTYP                                     
027984     END-IF                                                               
027990     WRITE W47925X-POST FROM U25X-AREA                                    
027991     MOVE 'U25X'     TO POSTSUM-TRANSTYP                                  
027992     MOVE 'W47925X'  TO POSTSUM-FDNAMN                                    
027993     MOVE 'W47979D2' TO POSTSUM-DDNAMN2                                   
027994     CALL POSTSUM USING POSTSUM-PARM                                      
027995                                                                          
027996     .                                                                    
027997     EJECT                                                                
028000 IMS-GET-WDE6 SECTION.                                                    
028100                                                                          
028200     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
028300     CALL CBLTDLI USING GN WDE6-PCB DLI-IO-AREA                           
028400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
028500     PERFORM IMS-STATUSKONTROLL                                           
028600     .                                                                    
028700     SKIP2                                                                
028800 IMS-GU-WDE4F1                 SECTION.                                   
028900     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
029000                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
029100            DELIMITED BY SIZE INTO SSA1                                   
029200     MOVE '  GE' TO GODK-STATUSKODER                                      
029300     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
029400     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
029500     PERFORM IMS-STATUSKONTROLL                                           
029600     .                                                                    
029700     SKIP3                                                                
029800 IMS-GN-WDE4F1                 SECTION.                                   
029900     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
030000                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
030100            DELIMITED BY SIZE INTO SSA1                                   
030200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
030300     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-E4F1 SSA1                     
030400     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
030500     PERFORM IMS-STATUSKONTROLL                                           
030600     .                                                                    
030700     SKIP3                                                                
030710 IMS-GU-WDE101 SECTION.                                                   
030720                                                                          
030730     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
030740          DELIMITED BY SIZE INTO SSA1                                     
030750     MOVE '  GE' TO GODK-STATUSKODER                                      
030760     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-E101 SSA1                      
030770     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
030780     PERFORM IMS-STATUSKONTROLL                                           
030790     .                                                                    
030791     EJECT                                                                
030800 IMS-STATUSKONTROLL SECTION.                                              
030900                                                                          
031000     SET STATUS-IX TO 1                                                   
031100     SEARCH GODK-STATUS                                                   
031200       AT END                                                             
031300         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
031400         DISPLAY FELTEXT                                                  
031500         CALL FELLOG                                                      
031600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031700         CONTINUE                                                         
031800     END-SEARCH                                                           
031900     .                                                                    
