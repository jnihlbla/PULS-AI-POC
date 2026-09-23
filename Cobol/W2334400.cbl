001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W2334400.                                                
001300 AUTHOR.         KIHLBERG STEFAN.                                         
001400 DATE-WRITTEN.   13/03/05.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        READS WDD9 MED SB.                                               
001810*        SAMLAR KOMMANDE AVROP FÖR NDC I KINA, USA OCH CDC-SE             
001900*                                                                         
002010*        THE PROGRAM READS     WDD9                                       
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- KOMMANDE AVROP, NDC KINA, CDC-SE , USA                     
003310     SELECT W23344                     ASSIGN TO W23344D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W23344                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  RECORD -COPY W23344 -PRE  OUT-  -L.                                  
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W2334400'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004510 01  RKOD                        PIC S9(4)  VALUE +0  COMP SYNC.          
004600 01  CURRENT-SECTION             PIC X(20)   VALUE 'SPACE'.               
004610 01  WDATUM                      PIC X(6)    VALUE 'WDATUM'.              
004800     EJECT                                                                
004900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES TODAYS-DATE.                                        
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400                                                                          
005402 01  WS-DATE                     PIC 9(6)    VALUE ZERO.                  
005403 01  FILLER REDEFINES WS-DATE.                                            
005404     03  WS-DATE-YEAR            PIC 9(2).                                
005405     03  WS-DATE-MONTH           PIC 9(2).                                
005406     03  WS-DATE-DAY             PIC 9(2).                                
005407                                                                          
005408 01  FILLER              PIC X(16)   VALUE 'CURRENT PERIOD'.              
005409 01 INNEV-TIAAPP                 PIC 9(4)       VALUE ZERO.               
005410 01 INNEV-TIAAPP-GRP            REDEFINES INNEV-TIAAPP.                   
005411     03 INNEV-TIAA               PIC 9(2).                                
005412     03 INNEV-TIPP               PIC 9(2).                                
005413                                                                          
005414 01  FILLER                      PIC X(16)   VALUE 'NEXT PERIOD'.         
005415 01  NEXT-TIAAPP                 PIC 9(4)       VALUE ZERO.               
005420 01  NEXT-TIAAPP-GRP            REDEFINES NEXT-TIAAPP.                    
005430     03 NEXT-TIAA                PIC 9(2).                                
005440     03 NEXT-TIPP                PIC 9(2).                                
005450                                                                          
005460 01  NEXT-TIAAPP-STARTDAY     PIC 9(6)       VALUE ZERO.                  
005470                                                                          
005480 01  WS-TIAAPP-INL            PIC 9(4)       VALUE ZERO.                  
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006101     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006110     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006120     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
006200     SKIP2                                                                
006300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  ERROR-TEXT.                                                          
007000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007100     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
007400*                                                                         
007500 01  PROGRAM-NAME                PIC X(6)    VALUE 'W23344'.              
007600     SKIP2                                                                
007601                                                                          
007610 01  WORK-FIELDS.                                                         
007611     03 WS-IDARTNR               PIC S9(9)   VALUE ZERO COMP-3.           
007612     03 WS-IDLEVNR               PIC  X(5)   VALUE SPACE.                 
007613     03 WS-IDLEVNR-DC            PIC  X(5)   VALUE SPACE.                 
007614                                                                          
007615 77  NDC-CHINA-CDC-SE-SW            PIC X   VALUE 'N'.                    
007616     88  NDC-CHINA-CDC-SE                   VALUE 'J'.                    
007617                                                                          
007618 01  FILLER                      PIC X(16) VALUE 'DATUMKORT'.             
007619                                                                          
007620*01  DATUM   -COPY WDATKORT.                                              
007621     EJECT                                                                
007630                                                                          
008001     EJECT                                                                
008002*    --- PARAMETRAR TILL POSTSUM                                          
008003*                                                                         
008010*01  -COPY W0005   -PRE  POSTSUM-                                         
008101     EJECT                                                                
008110*01  -COPY WDATAREA                                                       
008201     EJECT                                                                
008202*01    -COPY WWDC99                                                       
008203                                                                          
008204 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
008205*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
008206*- - - - - - - - - - - - - DC-MAX OCCURS SÄTTS TILL VERKLIGT ANTAL        
008207*- - - - - - - - - - - - - I M- SEKTIONEN.                                
008208 01  IDDC-INDEX-WS.                                                       
008209     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
008210                                                                          
008211     03 WDCIX            PIC S9(3)   VALUE +0  COMP SYNC.                 
008212     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
008213                                                                          
008214 01  IDDC-TABELL.                                                         
008215     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
008216                INDEXED BY DCIX.                                          
008217        05 T-DCS.                                                         
008218          07 T-DCS-IDDC           PIC X(2).                               
008219          07 T-DCS-KDDC           PIC X(2).                               
008220          07 T-DCS-IDLEVNR-DC     PIC X(5).                               
008221          07 T-DCS-IDLEVNR-EMB    PIC X(5).                               
008222                                                                          
008223                                                                          
008224 01  FILLER              PIC X(16)   VALUE 'DC-TABELL-SORT'.              
008225*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
008226 01  TABENTRY-PARM.                                                       
008227     03  STEGLANGD               PIC S9(9) COMP.                          
008228     03  ANTAL                   PIC S9(9) COMP.                          
008229     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
008230                                                                          
008231                                                                          
008232       EJECT                                                              
008233 01  OUT-AREA-START               PIC X(24)   VALUE                       
008234                                 'OUT-AREA-START  '.                      
008235     SKIP2                                                                
008236                                                                          
008240*01  AREA -COPY W23344     -PRE OUT-                                      
008300     EJECT                                                                
008400*    --- AREAS FOR IMS-SECTIONS                                           
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  KEYS-FOR-DLI.                                                        
009001     03  W-WDD901KY-X.                                                    
009002         05  W-WDD901KY.                                                  
009003           07  W-IDARTNR-D9      PIC S9(9)   VALUE ZERO.                  
009004           07  W-IDDC-D9         PIC  X(2)   VALUE SPACE.                 
009005     03  W-IDLEVNR-X.                                                     
009010         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
009020     03  W-WDD905KY-X.                                                    
009030         05  W-WDD905KY          PIC X(7)    VALUE SPACE.                 
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FOUND                       VALUE '  '.                  
009500     88  SEGMENT-MISSING                     VALUE 'GB'.                  
009600     SKIP2                                                                
009700 01  GOOD-STATUSCODES.                                                    
009800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200     EJECT                                                                
010300*    --- IMS FUNCTION CODES                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800 01  DLI-IO-AREA.                                                         
010801     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010802     SKIP3                                                                
010803     03  WDD901 REDEFINES IO-AREA.                                        
010804*        05  -COPY WDD901  -PRE WDD901-                                   
010806     03  WDD902 REDEFINES IO-AREA.                                        
010807*        05  -COPY WDD902  -PRE WDD902-                                   
010808     03  WDD905 REDEFINES IO-AREA.                                        
010809*        05  -COPY WDD905  -PRE WDD905-                                   
011100     EJECT                                                                
011110   03  IO-AREA2                  PIC X(800).                              
011120                                                                          
011130*  03  WDB601-AREA   -COPY WDB601     -RED IO-AREA2                       
011140     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300                                                                          
011401                                                                          
011402*01  -COPY W0008  -PRE WDD9-                                              
011410     05  FILLER                  PIC X.                                   
011420*01  -COPY W0008  -PRE WDB6-                                              
011430     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING WDD9-PCB WDB6-PCB.                             
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING WDD9-PCB WDB6-PCB.                             
011700                                                                          
011900                                                                          
012000     PERFORM A-INIT                                                       
012010     PERFORM AA-SKAPA-DCTABELL                                            
012100                                                                          
012201     PERFORM IMS-GET-WDD9                                                 
012202     PERFORM UNTIL SEGMENT-MISSING                                        
012203       EVALUATE WDD9-SEG-NAME-FB                                          
012204                                                                          
012205         WHEN 'WDD901'                                                    
012206                                                                          
012207           PERFORM AA-NOLLSTALL                                           
012208           MOVE WDD901-IDDC TO WS-IDDC                                    
012210           IF NDC-CN OR CDC-SE OR NDC-US                                  
012213              MOVE WDD901-IDARTNR   TO WS-IDARTNR                         
012214              MOVE YES TO NDC-CHINA-CDC-SE-SW                             
012215           END-IF                                                         
012216                                                                          
012217         WHEN 'WDD902'                                                    
012218           IF NDC-CHINA-CDC-SE                                            
012220              MOVE WDD902-IDLEVNR   TO WS-IDLEVNR                         
012221           END-IF                                                         
012222                                                                          
012223         WHEN 'WDD905'                                                    
012224           IF NDC-CHINA-CDC-SE                                            
012226              IF WDD905-KDAVROP = 2                                       
012228                PERFORM BB-CREATE-FILE                                    
012229                PERFORM S11-WRITE-W23344                                  
012231              END-IF                                                      
012233           END-IF                                                         
012234                                                                          
012235       END-EVALUATE                                                       
012236       PERFORM IMS-GET-WDD9                                               
012240     END-PERFORM                                                          
012300     PERFORM Z-FINIT                                                      
012400                                                                          
012500     MOVE ZERO TO RETURN-CODE                                             
012600     GOBACK                                                               
012700     .                                                                    
012800     EJECT                                                                
012900 A-INIT SECTION.                                                          
013101                                                                          
013110     OPEN OUTPUT W23344                                                   
013200                                                                          
013210     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
013400     MOVE D-AAR       TO TODAYS-DATE-YEAR                                 
013500     MOVE D-MAANAD    TO TODAYS-DATE-MONTH                                
013600     MOVE D-DAG       TO TODAYS-DATE-DAY                                  
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013720                                                                          
013721* CURRENT PERIOD                                                          
013730     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
013740     MOVE TODAYS-DATE        TO DAT-I-TIDATUM                             
013750     CALL WDATKONV USING        DAT-KDDATFORM                             
013760                                DAT-I-TIDATUM                             
013770                                DAT-O-TIDATUM                             
013780                                DAT-KDSVAR                                
013790                                                                          
013800     IF DAT-KDSVAR-OK                                                     
013810       MOVE DAT-TIAAPP       TO INNEV-TIAAPP                              
013820     END-IF                                                               
013830                                                                          
013831* NEXT PERIOD                                                             
013840     MOVE INNEV-TIAAPP TO NEXT-TIAAPP                                     
013880     IF INNEV-TIPP = 12                                                   
013890        MOVE 01 TO NEXT-TIPP                                              
013891        ADD 1 TO NEXT-TIAA                                                
013892     ELSE                                                                 
013893        ADD 1 TO NEXT-TIPP                                                
013894     END-IF                                                               
013896                                                                          
013897* NEXT PERIOD STARTDAY                                                    
013899     MOVE 'AAPP'           TO DAT-KDDATFORM                               
013900     MOVE NEXT-TIAAPP      TO DAT-I-TIDATUM                               
013901     CALL WDATKONV USING        DAT-KDDATFORM                             
013902                                DAT-I-TIDATUM                             
013903                                DAT-O-TIDATUM                             
013904                                DAT-KDSVAR                                
013905                                                                          
013906     IF DAT-KDSVAR-OK                                                     
013907       MOVE DAT-TIAAMMDD     TO NEXT-TIAAPP-STARTDAY                      
013908     END-IF                                                               
013909                                                                          
013910     .                                                                    
014000     EJECT                                                                
014001 AA-NOLLSTALL SECTION.                                                    
014002                                                                          
014003     MOVE 'AA-NOLLSTALL  '   TO CURRENT-SECTION.                          
014004                                                                          
014005     MOVE ZERO               TO WS-IDARTNR                                
014006     MOVE ZERO               TO WS-IDLEVNR                                
014007     MOVE SPACE              TO WS-IDDC                                   
014008     MOVE ZERO               TO WS-DATE                                   
014009     MOVE ZERO               TO WS-TIAAPP-INL                             
014010     MOVE NOO                TO NDC-CHINA-CDC-SE-SW                       
014011     .                                                                    
014012     EJECT                                                                
014013 AA-SKAPA-DCTABELL SECTION.                                               
014020     MOVE 'AA-SKAPA-DCTABELL'  TO CURRENT-SECTION.                        
014030                                                                          
014040     SET DCIX TO +1                                                       
014050     PERFORM IMS-GN-WDB601                                                
014060                                                                          
014070     PERFORM UNTIL SEGMENT-MISSING                                        
014080       IF DCIX <= DC-MAX                                                  
014090         MOVE DCS-IDDC TO T-DCS-IDDC(DCIX)                                
014091         MOVE DCS-KDDC TO T-DCS-KDDC(DCIX)                                
014092         MOVE DCS-IDLEVNR-DC  TO T-DCS-IDLEVNR-DC(DCIX)                   
014093         MOVE DCS-IDLEVNR-EMB TO T-DCS-IDLEVNR-EMB(DCIX)                  
014094*                                                                         
014095         SET DCIX UP BY +1                                                
014096                                                                          
014097         PERFORM IMS-GN-WDB601                                            
014098       ELSE                                                               
014099                                                                          
014100           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO ERROR-TEXT-STR            
014101           DISPLAY ERROR-TEXT                                             
014110           CALL ABEND USING RKOD                                          
014120       END-IF                                                             
014130     END-PERFORM                                                          
014140                                                                          
014150*    --- SÄTTER TAKET PÅ TABELLEN                                         
014160     SET DCIX   DOWN BY +1                                                
014170     SET DC-MAX TO DCIX                                                   
014180                                                                          
014190*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
014191     MOVE DC-MAX                  TO ANTAL                                
014192     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
014193     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
014194                                                                          
014195     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
014196                  T-DCS-IDDC(1) NYCKELLANGD                               
014197     .                                                                    
014198     EJECT                                                                
014199                                                                          
014200 BB-CREATE-FILE SECTION.                                                  
014201                                                                          
014202     MOVE 'BB-CREATE-FILE' TO CURRENT-SECTION                             
014203                                                                          
014204     PERFORM BB1-FETCH-IDLEVNR-DC                                         
014205     PERFORM BB2-FETCH-TIAAPP-INL                                         
014206                                                                          
014209     MOVE WS-IDDC             TO OUT-IDDC                                 
014210     MOVE WS-IDARTNR          TO OUT-IDARTNR                              
014211     MOVE WS-IDLEVNR          TO OUT-IDLEVNR                              
014212     MOVE WS-IDLEVNR-DC       TO OUT-IDLEVNR-DC                           
014213     MOVE WDD905-KDAVROP      TO OUT-KDAVROP                              
014214     MOVE WDD905-KVAVROP      TO OUT-KVAVROP                              
014215     MOVE WDD905-TIAVRDAT-INL TO OUT-TIAVRDAT-INL                         
014216     MOVE NEXT-TIAAPP         TO OUT-TIAAPP-NEXT                          
014217     MOVE WS-TIAAPP-INL       TO OUT-TIAAPP-INL                           
014218*    CALL ABEND USING RKOD-ABEND-WITH-DUMP                                
014219     .                                                                    
014220                                                                          
014221                                                                          
014222 BB1-FETCH-IDLEVNR-DC SECTION.                                            
014223                                                                          
014224     MOVE 'BB1-FETCH-IDLEVNR-DC' TO CURRENT-SECTION.                      
014225*    MOVE  INL-IDDC TO WS-IDDC                                            
014226     SET DCIX TO +1                                                       
014227     SEARCH DC-TAB                                                        
014228        AT END                                                            
014229          MOVE NOO  TO DCS-TRAEFF                                         
014230        WHEN T-DCS-IDDC(DCIX) = WS-IDDC                                   
014231          MOVE YES  TO DCS-TRAEFF                                         
014232          CONTINUE                                                        
014233     END-SEARCH                                                           
014234                                                                          
014235     IF DCS-TRAEFF = YES                                                  
014236        IF T-DCS-KDDC(DCIX) = 'NA'                                        
014237          MOVE T-DCS-IDLEVNR-EMB(DCIX) TO WS-IDLEVNR-DC                   
014238        ELSE                                                              
014239          MOVE T-DCS-IDLEVNR-DC(DCIX)  TO WS-IDLEVNR-DC                   
014241        END-IF                                                            
014242     ELSE                                                                 
014243       DISPLAY 'IDDC ' WS-IDDC ' EJ REG PÅ WDB6'                          
014244     END-IF                                                               
014245     .                                                                    
014246     EJECT                                                                
014247                                                                          
014248 BB2-FETCH-TIAAPP-INL SECTION.                                            
014249                                                                          
014250     MOVE 'BB2-FETCH-TIAAPP-INL' TO CURRENT-SECTION.                      
014251                                                                          
014252     MOVE WDD905-TIAVRDAT-INL TO WS-DATE                                  
014253*    IF WS-DATE < NEXT-TIAAPP-STARTDAY                                    
014254       MOVE NEXT-TIAAPP TO OUT-TIAAPP-INL                                 
014255                                                                          
014256       MOVE 'AAMMDD'              TO DAT-KDDATFORM                        
014257       MOVE WDD905-TIAVRDAT-INL   TO DAT-I-TIDATUM                        
014258       CALL WDATKONV USING          DAT-KDDATFORM                         
014259                                    DAT-I-TIDATUM                         
014260                                    DAT-O-TIDATUM                         
014261                                    DAT-KDSVAR                            
014262                                                                          
014263       IF DAT-KDSVAR-OK                                                   
014264         MOVE DAT-TIAAPP          TO WS-TIAAPP-INL                        
014265       END-IF                                                             
014266*    END-IF                                                               
014267                                                                          
014268     .                                                                    
014269     EJECT                                                                
014270                                                                          
014271 Z-FINIT SECTION.                                                         
014280     CLOSE W23344                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014601     EJECT                                                                
014602 S11-WRITE-W23344 SECTION.                                                
014603                                                                          
014604     WRITE OUT-RECORD FROM OUT-AREA                                       
014605                                                                          
014607     MOVE 'W23344' TO POSTSUM-FDNAMN                                      
014608     MOVE 'W23344D1' TO POSTSUM-DDNAMN2                                   
014609     CALL POSTSUM USING POSTSUM-PARM                                      
014610     .                                                                    
014800     EJECT                                                                
014900 S99-ABEND SECTION.                                                       
015000                                                                          
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015701                                                                          
015702 IMS-GET-WDD9   SECTION.                                                  
015703                                                                          
015704     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-AREA                           
015705     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
015706     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
015707     PERFORM IMS-STATUSCHECK                                              
015710     .                                                                    
015800     EJECT                                                                
015801                                                                          
015810 IMS-GN-WDB601    SECTION.                                                
015820     STRING 'WDB601   '                                                   
015830          DELIMITED BY SIZE INTO SSA1                                     
015840     MOVE '  GB' TO GOOD-STATUSCODES                                      
015850     CALL CBLTDLI USING GN WDB6-PCB  WDB601-AREA SSA1                     
015860     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
015870     PERFORM IMS-STATUSCHECK                                              
015880     .                                                                    
015890     EJECT                                                                
015891                                                                          
015900 IMS-STATUSCHECK SECTION.                                                 
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GOOD-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO ERROR-TEXT                              
016600         DISPLAY ERROR-TEXT                                               
016700         CALL FELLOG                                                      
016800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
