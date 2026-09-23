000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2710900.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   99/01/19.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        REFILLRENSNING AV ORDERFÖRSLAG PÅ WDE3                           
001100*                                                                         
001200*        FILER SKAPAS:                                                    
001300*          - FIL MED FÖRSLAG SOM SKA RENSAS (W27113)                      
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLORDL (WDE3)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- RENSNINGS-FIL                                              
003000     SELECT W27113                     ASSIGN TO W27109D2.                
003100     SKIP2                                                                
003200*          --- IN-FIL MED IDDC                                            
003300     SELECT W271DC                    ASSIGN TO W27109D4.                 
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W27113                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY W27113 -PRE RENS-     -L.                                 
004400                                                                          
004500 FD  W271DC                                                               
004600     LABEL RECORD STANDARD                                                
004700     RECORDING F                                                          
004800     BLOCK CONTAINS 0.                                                    
004900                                                                          
005000 01  FILLER                  PIC X(80).                                   
005100                                                                          
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400     SKIP2                                                                
005500*    -COPY WY2000W1                                                       
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(8)    VALUE 'W2710900'.            
005800*                                                                         
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100     SKIP2                                                                
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500     EJECT                                                                
006600                                                                          
006700 01  ARBETSFALT.                                                          
006800     03 BEORDRAT-DC             PIC X(2) VALUE SPACE.                     
006900                                                                          
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500                                                                          
007600     EJECT                                                                
007700 01  ARBETSFAELT.                                                         
007800     03  WS-TID                  PIC 9(8).                                
007900     03  WS-IDORDNR              PIC 9(7).                                
008000     03  WS-IDORDNR-FLYG         PIC 9(7).                                
008100     03  WS-IDORDNR-EVENING      PIC 9(7).                                
008200     03  WS-TID-FLYG             PIC 9(2).                                
008300     03  WS-TID-EVENING          PIC 9(2).                                
008400     03  WS-TID-TIMME            PIC 9(2).                                
008500*                                                                         
008600     03  WS-IDKUNDNR             PIC 9(6).                                
008700*                                                                         
008800     03  WS-KDFRAKT              PIC 9(2).                                
008900*                                                                         
009000     03  WS-KDORDKL              PIC 9.                                   
009100*                                                                         
009200                                                                          
009300     03  WS-ADLAGOMR-9      PIC  9(2)     VALUE ZERO.                     
009400                                                                          
009500     03 WS-ADART-X.                                                       
009600        05 WS-ADLAGOMR-X    PIC  9(2).                                    
009700        05 WS-ADGANG-X      PIC  9(2).                                    
009800        05 WS-ADPLATS-X     PIC  X(5)     VALUE SPACE.                    
009900        05 FILLER           PIC  X(1)     VALUE SPACE.                    
010000* OM ÄNDRING AV LAYOUT PÅ WS-ADART-X GÖRS MÅSTE MOTSVARANDE               
010100* ÄNDRING GÖRAS I PGM W40376.                                             
010200                                                                          
010700* BELOW FLAGS SHOULD BE RETAINED UNTIL PROPOSAL REVIEWED                  
010800* VALUES IN FIELD KDREFTXT IN WDE3                                        
010900     03 WS-CHECK-FLAG          PIC 9(02).                                 
011000        88 RETAIN-FLAG         VALUE 02 18 19 21 22 40 45 76.             
011100     EJECT                                                                
011200                                                                          
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400*                                                                         
011500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011900     EJECT                                                                
012000*    --- PARAMETRAR TILL DATKONV                                          
012100*                                                                         
012200*01  -COPY WDATAREA                                                       
012300     EJECT                                                                
012400*    --- PARAMETRAR TILL POSTSUM                                          
012500*                                                                         
012600*01  -COPY W0005   -PRE  POSTSUM-                                         
012700     EJECT                                                                
012800 01  RENS-AREA-START             PIC X(24)   VALUE                        
012900                                             'RENS-AREA-START'.           
013000     SKIP2                                                                
013100                                                                          
013200*01  AREA -COPY W27113      -PRE RENS-                                    
013300*                                                                         
013400     EJECT                                                                
013500 01  LOKAL-AREA-START             PIC X(24)   VALUE                       
013600                                             'LOKAL-AREA-START'.          
013700     SKIP2                                                                
013800                                                                          
013900*01  AREA -COPY W27115      -PRE LOKAL-                                   
014000*                                                                         
014100     EJECT                                                                
014200 01  IN-AREA-START               PIC X(24)   VALUE                        
014300                                             'IN-AREA-START'.             
014400     SKIP2                                                                
014500                                                                          
014600                                                                          
014700 01  DC-POST.                                                             
014800     03  DC-PARAMETER-TIME      PIC X(2).                                 
014900     03  FILLER                 PIC X(78).                                
015000                                                                          
016000                                                                          
016100*      --- VALID IDDC CODES                                               
016200*                                                                         
016300*01    -COPY WWDC99                                                       
016400       EJECT                                                              
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800     SKIP3                                                                
016900 01  NYCKLAR-TILL-DLI.                                                    
017000     03 W-WDE301-X.                                                       
017100         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
017200         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
017300         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
017400         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
017500         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
017600                                                                          
017700     03 W-WDE301KY-MIN-X.                                                 
017800         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
017900         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
018000         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
018100         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
018200         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
018300                                                                          
018400     03 W-WDE301KY-MAX-X.                                                 
018500         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
018600         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE ZERO COMP-3.             
018700         05  W-KDREFTYP-MAX      PIC X     VALUE SPACE.                   
018800         05  W-IDARTNR-MAX       PIC S9(9)                                
018900                                         VALUE +999999999 COMP-3.         
019000         05  W-IDDISTR-MIN       PIC S9(5) VALUE +99999 COMP-3.           
019100                                                                          
019200     03  W-IDDC-B6-X.                                                     
019300         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
019400                                                                          
019501     03  W-IDDC-B616-X.                                                   
019601         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
019701                                                                          
019801     03  W-IDDC-X.                                                        
019901         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
020001                                                                          
020101     03  W-IDARTNR-X.                                                     
020201         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020301                                                                          
020302     03  W-KDSEGKEY-X.                                                    
020303         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
020304                                                                          
020401     SKIP2                                                                
020501*    --- STATUS-KOD FRÅN IMS                                              
020601 01  STATUS-WS                   PIC XX.                                  
020701     88  SEGMENT-FINNS                       VALUE '  '.                  
020801     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020901     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021001     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021101     88  IMS-EJ-OK                           VALUE 'XD'.                  
021201     SKIP2                                                                
021301 01  GODK-STATUSKODER.                                                    
021401     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021501     SKIP3                                                                
021601 01  SSA1                        PIC X(128).                              
021701 01  SSA2                        PIC X(64).                               
021801     EJECT                                                                
021901*    --- IMS FUNKTIONSKODER                                               
022001*01  -COPY W0003                                                          
022101     EJECT                                                                
022201*    ---  DLI INPUT-OUTPUT AREA                                           
022301 01  FILLER                      PIC X(16)   VALUE 'MMI-IO-AREA'.         
022401     SKIP3                                                                
022501 01  DLI-IO-AREA.                                                         
022601     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
022701     SKIP3                                                                
022801     03  WLORDL01 REDEFINES IO-AREA.                                      
022901*        05  -COPY WDE301                                                 
023001     EJECT                                                                
023101 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
023201 01   DLI-IO-AREA-B616.                                                   
023301*     03  -COPY WDB616                                                    
023401                                                                          
023501 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
023601 01   DLI-IO-AREA-K711.                                                   
023701*     03  -COPY WDK711                                                    
023801                                                                          
023802 01  FILLER               PIC X(16)   VALUE 'WDK611 AREA'.                
023803 01   DLI-IO-AREA-K611.                                                   
023804*     03  -COPY WDK611                                                    
023805                                                                          
023901     EJECT                                                                
024001 LINKAGE SECTION.                                                         
024101                                                                          
024201*01  -COPY W0009   -PRE MSG-                                              
024301     EJECT                                                                
024401*01  -COPY W0008  -PRE WDE3-                                              
024501     05  FILLER                  PIC X.                                   
024601     EJECT                                                                
024701*01  -COPY W0008      -PRE WDB6-                                          
024801     05  FILLER                  PIC X.                                   
024901     EJECT                                                                
025001*01  -COPY W0008      -PRE WDK7-                                          
025101     05  FILLER                  PIC X.                                   
025201     EJECT                                                                
025202*01  -COPY W0008      -PRE WDK6-                                          
025203     05  FILLER                  PIC X.                                   
025204     EJECT                                                                
025301 PROCEDURE DIVISION  USING WDE3-PCB WDB6-PCB WDK7-PCB WDK6-PCB.           
025401     ENTRY 'DLITCBL' USING WDE3-PCB WDB6-PCB WDK7-PCB WDK6-PCB.           
025501                                                                          
025601     PERFORM A-INIT                                                       
025701     PERFORM IMS-GET-WDE3                                                 
025801     PERFORM UNTIL SEGMENT-SLUT                                           
025901       EVALUATE WDE3-SEG-NAME-FB                                          
026001         WHEN 'WDE301  '                                                  
026101           MOVE REF-IDDC TO W-IDDC-B6                                     
026201                            W-IDDC                                        
026202           MOVE REF-IDARTNR TO W-IDARTNR                                  
026203           IF REF-IDDC = '11'                                             
026204             PERFORM IMS-GU-WDK611-REF                                    
026205             MOVE CLAG-IDDC-REF  TO W-IDDC-B616                           
026206           ELSE                                                           
026301             PERFORM IMS-GU-WDK711-REF                                    
026302             IF SLAG-IDDC-REF = SPACE                                     
026303               MOVE 5              TO REF-TIREFBAT                        
026303               MOVE '11'           TO W-IDDC-B616                         
026304             ELSE                                                         
026305               MOVE SLAG-IDDC-REF  TO W-IDDC-B616                         
026306             END-IF                                                       
026307           END-IF                                                         
026401           IF SEGMENT-FINNS                                               
026601             PERFORM IMS-GU-WDB616                                        
026701***********W271V2     WEEKLY                                              
026801             IF (DC-PARAMETER-TIME = '99'                                 
026901***********W271D*   DAILY                                                 
027000             OR (DC-PARAMETER-TIME = REF-TIREFBAT))                       
027300             AND (( REF-KDREFTYP = 'B' OR                                 
027400                    REF-KDREFTYP = 'A' OR                                 
027500                    REF-KDREFTYP = 'C' OR                                 
027600                    REF-KDREFTYP = 'L' )                                  
027700                                                 AND                      
027800                    REF-KDREFORS = 'P')                                   
027900**                 THE SUPERCEEDING MSGS ON 2372 AND 2382 SHOULD          
028000**                 REMAIN UNTIL THE PROPOSALS ARE REVIEWED AND DO         
028100**                 NOT DELETE FROM WDE3 HERE FOR                          
028200**                 KDREFTXT = 02,18,19,21,22,40,45,76                     
028300                   MOVE REF-KDREFTXT    TO WS-CHECK-FLAG                  
028400                   IF RETAIN-FLAG                                         
028500                     CONTINUE                                             
028600                   ELSE                                                   
028700                     PERFORM S60-FLYTTA-TILL-RENSPOST                     
028800                     PERFORM S13-SKRIV-W27113                             
028900                   END-IF                                                 
029000             END-IF                                                       
029100           END-IF                                                         
029200       END-EVALUATE                                                       
029300       PERFORM IMS-GET-WDE3                                               
029400     END-PERFORM                                                          
029500                                                                          
029600     PERFORM Z-FINIT                                                      
029700                                                                          
029800     MOVE ZERO TO RETURN-CODE                                             
029900     GOBACK                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 A-INIT SECTION.                                                          
030300     SKIP2                                                                
030400                                                                          
030500     OPEN INPUT W271DC                                                    
030600                                                                          
030700     PERFORM S20-LAES-W271DC                                              
030800                                                                          
031200     CLOSE W271DC                                                         
031300                                                                          
031400     OPEN OUTPUT W27113                                                   
031500                                                                          
031600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
031700                                                                          
031800     ACCEPT DAGENS-DATUM FROM DATE                                        
031900                                                                          
032000     .                                                                    
032100     EJECT                                                                
032200                                                                          
032300                                                                          
032400                                                                          
032500                                                                          
032600 Z-FINIT SECTION.                                                         
032700                                                                          
032800                                                                          
032900     CLOSE W27113                                                         
033000     SKIP2                                                                
033100     MOVE 'S' TO POSTSUM-OPKOD                                            
033200     CALL POSTSUM USING POSTSUM-PARM                                      
033300     .                                                                    
033400     EJECT                                                                
033500                                                                          
033600                                                                          
033700                                                                          
033800                                                                          
033900 S13-SKRIV-W27113 SECTION.                                                
034000                                                                          
034100     WRITE RENS-POST FROM RENS-AREA                                       
034200                                                                          
034300     MOVE  'RENS '   TO POSTSUM-TRANSTYP                                  
034400     MOVE 'W27113 '  TO POSTSUM-FDNAMN                                    
034500     MOVE 'W27109D3' TO POSTSUM-DDNAMN2                                   
034600     CALL POSTSUM USING POSTSUM-PARM                                      
034700     .                                                                    
034800     EJECT                                                                
034900                                                                          
035000                                                                          
035100                                                                          
035200                                                                          
035300 S20-LAES-W271DC SECTION.                                                 
035400                                                                          
035500      READ W271DC   INTO DC-POST                                          
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900                                                                          
036000                                                                          
036100 S60-FLYTTA-TILL-RENSPOST SECTION.                                        
036200                                                                          
036300     MOVE REF-IDDC          TO RENS-IDDC                                  
036400     MOVE REF-IDPERSON-BUY  TO RENS-IDPERSON-BUY                          
036500     MOVE REF-KDREFTYP      TO RENS-KDREFTYP                              
036600     MOVE REF-IDARTNR       TO RENS-IDARTNR                               
036700     MOVE REF-IDDISTR       TO RENS-IDDISTR                               
036800     MOVE NEJ               TO RENS-FLREFNYO                              
036900     .                                                                    
037000     EJECT                                                                
037100                                                                          
037200                                                                          
037300                                                                          
037400                                                                          
037500* --- IMS SEKTIONER ---                                                   
037600                                                                          
037700                                                                          
037800 IMS-GET-WDE3      SECTION.                                               
037900                                                                          
038000     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-AREA                           
038100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
038200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     .                                                                    
038500     EJECT                                                                
038600                                                                          
038700 IMS-GU-WDB616    SECTION.                                                
038800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
038900          DELIMITED BY SIZE INTO SSA1                                     
039000     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
039100          DELIMITED BY SIZE INTO SSA2                                     
039200     MOVE '  ' TO GODK-STATUSKODER                                        
039300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
039400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
039500     PERFORM IMS-STATUSKONTROLL                                           
039600     .                                                                    
039700     EJECT                                                                
039800                                                                          
039901 IMS-GU-WDK711-REF SECTION.                                               
040001                                                                          
040101     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
040201          DELIMITED BY SIZE INTO SSA1                                     
040301     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
040401          DELIMITED BY SIZE INTO SSA2                                     
040501     MOVE '  GE'           TO GODK-STATUSKODER                            
040601     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2            
040701     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
040801     PERFORM IMS-STATUSKONTROLL                                           
040901     .                                                                    
041001     EJECT                                                                
041100                                                                          
041110 IMS-GU-WDK611-REF SECTION.                                               
041120                                                                          
041130     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
041140          DELIMITED BY SIZE INTO SSA1                                     
041150     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
041160          DELIMITED BY SIZE INTO SSA2                                     
041170     MOVE '  GE'           TO GODK-STATUSKODER                            
041180     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K611 SSA1 SSA2            
041190     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
041191     PERFORM IMS-STATUSKONTROLL                                           
041192     .                                                                    
041193     EJECT                                                                
041194                                                                          
041200 IMS-STATUSKONTROLL SECTION.                                              
041300     SKIP2                                                                
041400     SET STATUS-IX TO 1                                                   
041500     SEARCH GODK-STATUS                                                   
041600       AT END                                                             
041700         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
041800         DISPLAY FELTEXT                                                  
041900         CALL FELLOG                                                      
042000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042100         CONTINUE                                                         
042200     END-SEARCH                                                           
042300     .                                                                    
042400     EJECT                                                                
043000*    -COPY WY2000P1                                                       
