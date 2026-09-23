000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W6115800.                                            
000400 AUTHOR.             STEFAN ÅSGÅRDEN.                                     
000500 DATE-WRITTEN.       JULI 2002.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001010*        PROGRAMMET ÄR ETT SUBPROGRAM FÖR BERÄKNING AV                    
001100*        BRUTTO-BEHOV FÖR CROSSDOCKING OMRÅDEN                            
001200*        FÖR EN SPECIFIK ARTIKEL                                          
002000*    SUBPROGRAM.                                                          
002100*            W009VADD    ADD AV VECKOR TILL DATUM                         
002200*            PERADD      ADD AV PERIODER TILL DATUM                       
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP1                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003310     SKIP2                                                                
003320 77  IDPGM                       PIC X(08)   VALUE 'W6115800'.            
003400     SKIP2                                                                
004300 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
004400     SKIP3                                                                
004500 01  KONSTANTER.                                                          
004600     03  JA                  PIC X       VALUE 'J'.                       
004700     03  NEJ                 PIC X       VALUE 'N'.                       
011500     SKIP3                                                                
011510 01  FILLER                      PIC X(16)   VALUE 'ARBETSAREOR'.         
011600 01  ARBETSAREOR.                                                         
011700     SKIP1                                                                
011750*                                                                         
011751     03 IX-K7                    PIC S9(9)   VALUE ZERO COMP-3.           
011752     03 IX-K7-MAX                PIC S9(9)   VALUE ZERO COMP-3.           
011760     03 WS-K7 OCCURS 15.                                                  
011761        05 WS-K7-IDDC            PIC  X(2)   VALUE SPACE.                 
011770        05 WS-K7-ADLAGOMR-CD     PIC S9(3)   VALUE ZERO COMP-3.           
011771        05 WS-K7-KVDAGAR-CDBEH   PIC S9(3)   VALUE ZERO COMP-3.           
011772        05 WS-K7-FLCDREL         PIC  X(1)   VALUE SPACE.                 
011773        05 WS-K7-KVBEHOV         PIC S9(7)V9(2)                           
011774                                             VALUE ZERO COMP-3.           
011775     03  WS-ANTAL-VECKOR         PIC  9(3)   VALUE ZERO COMP-3.           
011776     03  WS-TIAAVV-NUM           PIC  9(4)   VALUE ZERO.                  
011777     03  WS-TILLGANG             PIC S9(7)   VALUE ZERO COMP-3.           
011778     03  WS-BALANCE-ERSATT       PIC S9(7)   VALUE ZERO COMP-3.           
011779     03  WS-KVKUNDRETUR          PIC S9(7)   VALUE ZERO COMP-3.           
011780     03  WS-KVBEHOV              PIC S9(7)V9(2)                           
011781                                             VALUE ZERO COMP-3.           
011782                                                                          
011783     03  WS-CURRENT-DATE.                                                 
011784         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
011790         05  FILLER              PIC 9(4)   VALUE ZERO.                   
011791         05  FILLER              PIC 9(6)   VALUE ZERO.                   
011792                                                                          
011793     03  FILLER REDEFINES WS-CURRENT-DATE.                                
011794*-----   INKLUSIVE SEKEL                                                  
011795         05  WS-DAGENS-DATUM     PIC 9(8).                                
011796         05  WS-DAGENS-TID.                                               
011797             07 WS-DAGENS-TIMME  PIC 9(2).                                
011798             07 WS-DAGENS-MINUT  PIC 9(2).                                
011799             07 WS-DAGENS-SEKUND PIC 9(2).                                
011800                                                                          
011801     SKIP3                                                                
011810     03  IX-CD-OMR               PIC S9(9)   VALUE ZERO COMP-3.           
011900     03  IX-IDDC                 PIC S9(9)   VALUE ZERO COMP-3.           
012000     03  LINK-IX                 PIC S9(9)   VALUE ZERO COMP-3.           
027291                                                                          
027292 01  FILLER                      PIC X(16)   VALUE 'ARBETSFAELT'.         
027293 01  ARBETSFAELT.                                                         
027294*                                                                         
027295     03 DAGENS-DATUM.                                                     
027296         05 DAGENS-AAR           PIC 9(4).                                
027297         05 DAGENS-MAANAD        PIC 9(2).                                
027298         05 DAGENS-DAG           PIC 9(2).                                
027299                                                                          
027300 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
027301     88  SW-TRAEFF-JA                        VALUE 'J'.                   
027310     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
027400                                                                          
031300                                                                          
031400*      --- VALID IDDC CODES                                               
031500*                                                                         
031600*01    -COPY WWDCKONS                                                     
031900                                                                          
032000 01  W-IDDC-X.                                                            
032100     03  W-IDDC              PIC  X(2).                                   
032200 01  W-IDARTNR-X.                                                         
032300     03  W-IDARTNR           PIC S9(9)               COMP-3.              
032400 01  W-KDERS-0-X.                                                         
032500     03  W-KDERS-0           PIC S9(3)   COMP-3  VALUE ZERO.              
032520 01  W-IDARTNR-ERS-X.                                                     
032530     03  W-IDARTNR-ERS       PIC S9(9)   VALUE ZERO COMP-3.               
032531 01  W-KDSEGKEY-X.                                                        
032532     03  W-KDSEGKEY          PIC X(1)    VALUE '1'.                       
032540 01  W-IDDC-ERS-X.                                                        
032550     03  W-IDDC-ERS          PIC X(2)    VALUE SPACE.                     
032560 01  W-WDD7A1KY-MIN-X.                                                    
032570     03  W-IDARTNR-TILLK-MIN     PIC S9(9)   COMP-3  VALUE ZERO.          
032580     03  W-IDARTNR-ERS-MIN       PIC S9(9)   COMP-3  VALUE ZERO.          
032590     03  W-IDKORTNR-MIN          PIC S9(3)   COMP-3  VALUE ZERO.          
032592 01  W-WDD7A1KY-MAX-X.                                                    
032593     03  W-IDARTNR-TILLK-MAX     PIC S9(9)   COMP-3                       
032594                                             VALUE +999999999.            
032595     03  W-IDARTNR-ERS-MAX       PIC S9(9)   COMP-3                       
032596                                             VALUE +999999999.            
032597     03  W-IDKORTNR-MAX          PIC S9(3)   COMP-3  VALUE +999.          
032598                                                                          
032600                                                                          
032700     SKIP2                                                                
032800 01  FELTEXT.                                                             
032900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
033000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
033100 01  FELTEXT2.                                                            
033200     03  FILLER                  PIC X(9)    VALUE 'FELTEXT2'.            
033300     03  FELTEXT2-STR      PIC X(71)   VALUE SPACE.                       
033400                                                                          
033700     EJECT                                                                
033800 01  DYNAMISKA-SUBPROGRAM.                                                
033900     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
034000     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
034100     03  PERADD              PIC X(8)    VALUE 'PERADD'.                  
034500     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
034600     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
034610     03  WDAGKONV            PIC X(8)    VALUE 'WDAGKONV'.                
034620     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
034630     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
034640     03  WORKDAY             PIC X(8)    VALUE 'WORKDAY'.                 
034650     03  W61159              PIC X(8)    VALUE 'W61159'.                  
035000     EJECT                                                                
035100*    ---PARAMETRAR TILL DATKONV                                           
035200*01  -COPY WDATAREA                                                       
035210     EJECT                                                                
035860*    ---PARAMETRAR TILL WORKDAY                                           
035870*01  -COPY WORKAREA                                                       
035880     EJECT                                                                
035900 01  FILLER              PIC X(8)    VALUE 'W61159  '.                    
035910*01  AREA  -COPY W61159      -PRE W61159-.                                
035920*    --- PARAMETRAR TILL ABEND                                            
035930                                                                          
035940 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
035950 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
035996     SKIP3                                                                
036005     SKIP2                                                                
036010*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
036100*                                                                         
036200 01      IMS-WS.                                                          
036300   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
036400     SKIP3                                                                
036500*                            *** STATUSKOD FRÅN IMS                       
036600   03    STATUS-WS       PIC XX.                                          
036700     88  SEGMENT-FINNS               VALUE '  '.                          
036800     88  SEGMENT-SAKNAS              VALUE 'GE'                           
036900                                           'GB'.                          
037000     SKIP3                                                                
037100   03    SSA1            PIC X(128).                                      
037200   03    SSA2            PIC X(128).                                      
037300   03    SSA3            PIC X(128).                                      
037400     SKIP3                                                                
037420 01  NYCKLAR-TILL-DLI.                                                    
037430     03  W-WDGXKEY-X.                                                     
037440         05  W-IDHTYP           PIC X(4)    VALUE '2501'.                 
037450         05  W-IDDC-2501        PIC X(2)    VALUE ZERO.                   
037460         05  W-LOWVALUE         PIC X(24)   VALUE LOW-VALUE.              
037470*                                                                         
037480     03  W-IDREFTAB-X.                                                    
037490         05  W-IDREFTAB         PIC X(1)    VALUE SPACE.                  
037500   03    GODK-STATUSKODER.                                                
037600     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
037700     SKIP3                                                                
037800*01      -COPY W0003                                                      
037900     EJECT                                                                
038000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
038100     SKIP3                                                                
038200 01  DLI-IO-AREA-WDK601.                                                  
038300*        05  -COPY WDK601                                                 
038400     EJECT                                                                
038500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
038600     SKIP3                                                                
038700 01  DLI-IO-AREA-WDK611.                                                  
038800*        05  -COPY WDK611                                                 
040900     EJECT                                                                
041000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
041100     SKIP3                                                                
041200 01  DLI-IO-AREA-WDK701.                                                  
041300*        05  -COPY WDK701                                                 
041400     EJECT                                                                
041500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
041600     SKIP3                                                                
041700 01  DLI-IO-AREA-WDK711.                                                  
041800*        05  -COPY WDK711                                                 
041801     EJECT                                                                
041803                                                                          
041850                                                                          
041900     EJECT                                                                
042100 LINKAGE SECTION.                                                         
042200     SKIP3                                                                
042300*01  AREA  -COPY W61158      -PRE LINK-.                                  
042400     EJECT                                                                
042500*01  -COPY W0008  -PRE WDK6-.                                             
042600     05  FILLER              PIC X.                                       
042700     EJECT                                                                
042800*01  -COPY W0008  -PRE WDK7-.                                             
042900     05  FILLER              PIC X.                                       
043000     EJECT                                                                
043400*01  -COPY W0008  -PRE 2501-                                              
043500     05  FILLER                  PIC X.                                   
043501     EJECT                                                                
043502*01  -COPY W0008  -PRE WDK6-2-.                                           
043503     05  FILLER              PIC X.                                       
043504     EJECT                                                                
043505*01  -COPY W0008  -PRE WDK7-2-.                                           
043506     05  FILLER              PIC X.                                       
043507     EJECT                                                                
043508*01  -COPY W0008  -PRE WDL6-                                              
043509     05  FILLER                  PIC X.                                   
043510     EJECT                                                                
043511*01  -COPY W0008  -PRE WDD7A-                                             
043512     05  FILLER                  PIC X.                                   
043513     EJECT                                                                
043514*01  -COPY W0008      -PRE WDB6-                                          
043515     05  FILLER                  PIC X.                                   
043516                                                                          
043520     EJECT                                                                
043521 PROCEDURE DIVISION USING LINK-AREA                                       
043522                          WDK6-PCB WDK7-PCB 2501-PCB                      
043523                          WDK6-2-PCB WDK7-2-PCB                           
043524                          WDL6-PCB WDD7A-PCB                              
043525                          WDB6-PCB.                                       
044000     PERFORM A-INITIERA                                                   
044100                                                                          
044200     MOVE LINK-IDARTNR TO W-IDARTNR                                       
044210                          W61159-IDARTNR                                  
044300     PERFORM IMS-GU-K601                                                  
044400                                                                          
044500     IF  SEGMENT-FINNS                                                    
044700                                                                          
044800       PERFORM IMS-GNP-K611                                               
044900       IF SEGMENT-FINNS                                                   
045000                                                                          
045210         PERFORM B-LAS-WDK7                                               
045220         PERFORM C-BERAKNA-BEHOV                                          
045400                                                                          
071700       END-IF                                                             
072000     END-IF                                                               
072001*                                                                         
072002*   TVINGA FRAM EN DUMP                                                   
072003*                                                                         
072010*        MOVE 'FRAMTVINGAD DUMP I TEST              '                     
072020*                            TO FELTEXT-STR                               
072030*        DISPLAY FELTEXT                                                  
072040*        CALL FELLOG                                                      
072100     SKIP1                                                                
072700     SKIP1                                                                
072800     MOVE ZERO TO RETURN-CODE                                             
072900     GOBACK                                                               
073000     .                                                                    
073100     EJECT                                                                
073200 A-INITIERA SECTION.                                                      
073300******************************************************************        
073400*                                                                *        
073500*    BERÄKNING AV START- OCH SLUT-TIDPUNKTER (ÅR OCH VECKA)      *        
073600*    FÖR BERÄKNING                                               *        
073700*    NOLLSTÄLLNING AV TABELLER                                   *        
073800*                                                                *        
073900******************************************************************        
074000     SKIP1                                                                
074100     MOVE FUNCTION CURRENT-DATE                                           
074200                             TO WS-CURRENT-DATE                           
074300                                                                          
074400     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
074500     MOVE WS-DAGENS-DATUM (3:6)                                           
074600                             TO DAT-I-TIDATUM                             
074700                                                                          
074800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
074900                     DAT-O-TIDATUM DAT-KDSVAR                             
075000                                                                          
075100     IF DAT-KDSVAR-OK                                                     
075200                                                                          
075300        MOVE 1                   TO WS-ANTAL-VECKOR                       
075310                                                                          
075370        MOVE DAT-TIAAVV-GRP      TO WS-TIAAVV-NUM                         
075380        MOVE WS-TIAAVV-NUM       TO W61159-TIAAVV-AKTUELL                 
075390                                    W61159-TIBEHOV-START                  
075391        CALL W009VADD USING      W61159-TIBEHOV-START                     
075392                                 WS-ANTAL-VECKOR                          
075393        MOVE DAT-TID             TO W61159-TID-AKTUELL                    
075400                                                                          
075500     ELSE                                                                 
075810         MOVE 'FEL FRÅN WDATKONV I A-INIT I W6115800'                     
075820                             TO FELTEXT-STR                               
075830         DISPLAY FELTEXT                                                  
075840         CALL FELLOG                                                      
075900     END-IF                                                               
076000                                                                          
076700     SKIP1                                                                
076800     MOVE SPACE              TO LINK-RESULTATFLT                          
077300     MOVE ZERO               TO LINK-KVBEHOV-CD (1)                       
077400                                LINK-KVBEHOV-CD (2)                       
077500                                LINK-KVBEHOV-CD (3)                       
077510                                LINK-KVBEHOV-CD (4)                       
077511     MOVE 1                  TO IX-IDDC                                   
077512     PERFORM UNTIL IX-IDDC > 20                                           
077520       MOVE ZERO             TO LINK-KVBEHOV-DC (IX-IDDC)                 
077530       ADD 1                 TO IX-IDDC                                   
077540     END-PERFORM                                                          
077712     .                                                                    
077713     EJECT                                                                
134500 B-LAS-WDK7   SECTION.                                                    
134600******************************************************************        
134700*                                                                *        
134800*                                                                *        
134900*                                                                *        
135000******************************************************************        
135100                                                                          
135200                                                                          
135300     MOVE 1                  TO IX-K7                                     
135400                                                                          
135500     PERFORM IMS-GU-K701                                                  
135510     IF SEGMENT-FINNS                                                     
135600       PERFORM IMS-GNP-K711                                               
135610     END-IF                                                               
135700                                                                          
135800     PERFORM UNTIL SEGMENT-SAKNAS                                         
135900       IF      SLAG-KDREFSTA = 'A'                                        
136000       AND SLAG-ADLAGOMR-CD > ZERO                                        
136100         MOVE SLAG-IDDC      TO WS-K7-IDDC (IX-K7)                        
136200         MOVE SLAG-ADLAGOMR-CD                                            
136300                             TO WS-K7-ADLAGOMR-CD (IX-K7)                 
136400         MOVE SLAG-FLCDREL   TO WS-K7-FLCDREL (IX-K7)                     
136600         MOVE SLAG-KVDAGAR-CDBEH                                          
136700                             TO WS-K7-KVDAGAR-CDBEH (IX-K7)               
136800         MOVE IX-K7          TO IX-K7-MAX                                 
136900         ADD 1               TO IX-K7                                     
137000       END-IF                                                             
137100       PERFORM IMS-GNP-K711                                               
137200     END-PERFORM                                                          
137300                                                                          
138700     .                                                                    
149100     EJECT                                                                
149200 C-BERAKNA-BEHOV        SECTION.                                          
149210                                                                          
149220     MOVE 1                  TO IX-CD-OMR                                 
149221                                IX-IDDC                                   
149230                                                                          
149240     PERFORM UNTIL IX-CD-OMR > 4                                          
149250     OR CLAG-ADLAGOMR-CD (IX-CD-OMR) = ZERO                               
149260       MOVE 1                TO IX-K7                                     
149270                                                                          
149280       PERFORM UNTIL IX-K7 > IX-K7-MAX                                    
149290         IF CLAG-ADLAGOMR-CD (IX-CD-OMR) =                                
149291                               WS-K7-ADLAGOMR-CD (IX-K7)                  
149292           MOVE WS-K7-IDDC (IX-K7)                                        
149293                             TO W61159-IDDC                               
149294           MOVE 5            TO W61159-KVVECKOR-BEHOV                     
149302           CALL W61159 USING W61159-AREA WDK6-PCB WDK7-PCB                
149304                             2501-PCB WDK6-2-PCB WDK7-2-PCB               
149305                             WDL6-PCB WDD7A-PCB WDB6-PCB                  
149306           IF W61159-ANROP-OK                                             
149307             MOVE 002        TO WORK-KDCALL                               
149308             MOVE WC-CDC-SE                                               
149309                             TO WORK-IDDC                                 
149310             MOVE WS-DAGENS-DATUM (3:6)                                   
149311                             TO WORK-TIAAMMDD-FOM                         
149312             MOVE WS-K7-KVDAGAR-CDBEH (IX-K7)                             
149313                             TO WORK-KVWORKD                              
149314             CALL WORKDAY             USING WORK-KDCALL                   
149315                                            WORK-DATE-AREA                
149316                                            WORK-KDSVAR                   
149317             IF WORK-KDSVAR-OK                                            
149318                MOVE WORK-TIAAMMDD-TOM                                    
149319                             TO DAT-I-TIDATUM                             
149320             ELSE                                                         
149321                MOVE 'FEL FRÅN WORKDAY I W6115800'                        
149322                             TO FELTEXT-STR                               
149323                DISPLAY FELTEXT                                           
149324                CALL FELLOG                                               
149325             END-IF                                                       
149326                                                                          
149327             MOVE 'AAMMDD'                                                
149328                         TO DAT-KDDATFORM                                 
149329             MOVE WORK-TIAAMMDD-TOM                                       
149330                         TO DAT-I-TIDATUM                                 
149331                                                                          
149332             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
149333                             DAT-O-TIDATUM DAT-KDSVAR                     
149334                                                                          
149335             IF DAT-KDSVAR-OK                                             
149336*                                                                         
149337*    LINK-TIAAVVD FINNS MED FÖR ATT KUNNA BEGRÄNSA TIDSRAMEN              
149338*    FRÅN DET ANROPANDE PROGRAMMET                                        
149342*                                                                         
149343*                                                                         
149344               IF  LINK-TIAAVVD > ZERO                                    
149345               AND LINK-TIAAVVD < DAT-TIAAVVD                             
149346                 MOVE LINK-TIAAVVD                                        
149347                             TO DAT-TIAAVVD                               
149348               END-IF                                                     
149349             ELSE                                                         
149350               MOVE 'FEL FRÅN WDATKONV I C-BERÄKNA I W6115800'            
149351                             TO FELTEXT-STR                               
149352               DISPLAY FELTEXT                                            
149353               CALL FELLOG                                                
149354             END-IF                                                       
149355                                                                          
149356             MOVE 1          TO LINK-IX                                   
149357             MOVE NEJ        TO SW-TRAEFF                                 
149358             MOVE ZERO       TO WS-KVBEHOV                                
149365                                                                          
149366             PERFORM UNTIL LINK-IX > 70                                   
149367             OR W61159-TIAAVVD-BEHOV (LINK-IX) > DAT-TIAAVVD              
149368                                                                          
149369                IF W61159-TIAAVVD-BEHOV (LINK-IX) > ZERO                  
149370                   MOVE JA   TO SW-TRAEFF                                 
149371                END-IF                                                    
149372                                                                          
149373                ADD W61159-KVBEHOV-VECKA (LINK-IX)                        
149374                             TO WS-KVBEHOV                                
149375                                                                          
149376                ADD 1        TO LINK-IX                                   
149377             END-PERFORM                                                  
149378                                                                          
149383             IF WS-KVBEHOV < ZERO                                         
149384                MOVE ZERO TO WS-KVBEHOV                                   
149385             END-IF                                                       
149387             ADD WS-KVBEHOV  TO LINK-KVBEHOV-CD (IX-CD-OMR)               
149388             IF SW-TRAEFF-JA                                              
149389             AND WS-K7-FLCDREL (IX-K7) = JA                               
149390*                                                                         
149391*       LINK-IDDC (SAMT BEHOVET)                                          
149392*       FYLLS I OM REFILLORDER SKA SKAPAS                                 
149393*                                                                         
149395*                                                                         
149396               MOVE WS-K7-IDDC (IX-K7)                                    
149397                             TO LINK-IDDC (IX-IDDC)                       
149398               ADD WS-KVBEHOV                                             
149399                             TO LINK-KVBEHOV-DC (IX-IDDC)                 
149400               ADD 1         TO IX-IDDC                                   
149401             END-IF                                                       
149402                                                                          
149403           END-IF                                                         
149404         END-IF                                                           
149405         ADD 1               TO IX-K7                                     
149406                                                                          
149410       END-PERFORM                                                        
149415       ADD 1                 TO IX-CD-OMR                                 
149420                                                                          
149500     END-PERFORM                                                          
152100     .                                                                    
152300     EJECT                                                                
258477                                                                          
258478                                                                          
258479* --- IMS SEKTIONER ---                                                   
258480     SKIP3                                                                
258481                                                                          
258492     EJECT                                                                
258493                                                                          
258495 IMS-GU-K601 SECTION.                                                     
258496     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X                             
258497                    '&KDERS    =' W-KDERS-0-X ')'                         
258499            DELIMITED BY SIZE INTO SSA1                                   
258500     MOVE '  GE' TO GODK-STATUSKODER                                      
258501     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
258510     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
258600     PERFORM IMS-STATUSKONTROLL                                           
258700     .                                                                    
258800     SKIP3                                                                
258900 IMS-GNP-K611    SECTION.                                                 
259000     MOVE  'WDK611   '  TO SSA1                                           
259100     MOVE '  ' TO GODK-STATUSKODER                                        
259200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
259300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
259400     PERFORM IMS-STATUSKONTROLL                                           
259500     .                                                                    
265000     EJECT                                                                
265110 IMS-GU-K701      SECTION.                                                
265200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
265300            DELIMITED BY SIZE INTO SSA1                                   
265400     MOVE '  GE' TO GODK-STATUSKODER                                      
265500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
265600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
265700     PERFORM IMS-STATUSKONTROLL                                           
265800     .                                                                    
265900     SKIP3                                                                
266000 IMS-GNP-K711  SECTION.                                                   
266100     MOVE 'WDK711   '  TO SSA1                                            
266200     MOVE '  GE'        TO GODK-STATUSKODER                               
266300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
266400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
266500     PERFORM IMS-STATUSKONTROLL                                           
266600     .                                                                    
266816     EJECT                                                                
266820 IMS-STATUSKONTROLL SECTION.                                              
266900     SET STATUS-IX TO 1                                                   
267000     SEARCH GODK-STATUS  AT END CALL FELLOG                               
267100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
267200          CONTINUE                                                        
267300     END-SEARCH                                                           
267400     .                                                                    
