000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4639400.                                                
000300 AUTHOR.         SJÖBLOM ELAINE.                                          
000400 DATE-WRITTEN.   19/02/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        REDIGERA LISTA FÖR DDGS SCORE CARD                               
000810*        LISTA SKICKAS TILL D&P                                           
000900*                                                                         
001000*        PROGRAMMET LÄSER FIL MED DATA FRÅN WDF4                          
001100*                                                                         
001200* INFIL W4639S - UPPFÖLJNINGSFIL DIREKTLEVERANSRADER                      
001300*                                                                         
001400* URFIL W4639T - LISTPOSTER TILL D&P                                      
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900                                                                          
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*          --- DIREKTLEVERANS LISTA                                       
002800     SELECT W4639S                     ASSIGN TO W46394D1.                
002900*          --- DIREKTLEVERANSLISTA D&P                                    
003000     SELECT W4639T                     ASSIGN TO W46394D2.                
003100                                                                          
003200 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400                                                                          
003500 FD  W4639S                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800*01  POST      -COPY W4639S -PRE  LIST-  -L.                              
003900                                                                          
004000 FD  W4639T                                                               
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300 01  DOP-POST  PIC X(755).                                                
004400                                                                          
004500                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W4639400'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005200 77  CURRENT-IDLEVNR             PIC X(5)    VALUE SPACE.                 
005300 77  CURRENT-IDARTNR             PIC 9(8)    VALUE ZERO.                  
005310 77  INDX                        PIC S9(4) COMP VALUE ZERO.               
005320 01  WS-IDPTYP-ALFA              PIC X(3).                                
005330 01  WS-IDPTYP     REDEFINES WS-IDPTYP-ALFA PIC 9(3).                     
005400                                                                          
005500 77  W4639S-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  EOF-W4639S                          VALUE 'J'.                   
005700                                                                          
005701 77  FORSTA-SW                   PIC X       VALUE 'J'.                   
005702     88  FORSTA                              VALUE 'J'.                   
005703                                                                          
005800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES TODAYS-DATE.                                        
006000     03  TODAYS-DATE-AAR         PIC 9(2).                                
006100     03  TODAYS-DATE-MAANAD      PIC 9(2).                                
006200     03  TODAYS-DATE-DAG         PIC 9(2).                                
006300                                                                          
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500                                                                          
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000                                                                          
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007600                                                                          
007610 01  FILLER                     PIC X(10)   VALUE 'WDATAREA'.             
007620*01 -COPY WDATAREA                                                        
007630                                                                          
007700 01  FELTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000                                                                          
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400                                                                          
008500 01   LIST-AREA-START            PIC X(24) VALUE                          
008501                                 'LIST-AREA-START'.                       
008502                                                                          
008503*01  -COPY W4639S  -PRE  LIST-                                            
008504                                                                          
008505 01  SPAR-TABELL.                                                         
008506     05 TABELL OCCURS 9.                                                  
008507        07 RAD.                                                           
008510           09 RAD-KVTYP          PIC  9(9)   VALUE ZERO.                  
008600                                                                          
009000 01  BLANK-RAD.                                                           
009100     03  FILLER            PIC X(01) VALUE SPACE.                         
009200     03  FILLER  PIC X     VALUE X'5E'.                                   
009300                                                                          
009400 01  RUBRIK-DDGS.                                                         
009500     03  FILLER            PIC X(35) VALUE                                
009600        ' DDGS Scorecard                    '.                            
009700     03  FILLER  PIC X     VALUE X'5E'.                                   
009900                                                                          
010400 01  RUBRIK-RAD.                                                          
010500     03  FILLER            PIC X(11) VALUE  ' Partnumber'.                
010600     03  FILLER            PIC X     VALUE X'5E'.                         
010700     03  FILLER            PIC X(03) VALUE  'MFG'.                        
010800     03  FILLER            PIC X     VALUE X'5E'.                         
010900     03  FILLER            PIC X(64) VALUE  'Total number of open         
010910-        'orders with expired RFS date / code 96 date'.                   
010920     03  FILLER            PIC X     VALUE X'5E'.                         
010930     03  FILLER            PIC X(42) VALUE  'Total number of order        
010940-        's with RFS date Week '.                                         
011200     03  WEEK1             PIC 9(4).                                      
011201     03  FILLER            PIC X     VALUE X'5E'.                         
011210     03  FILLER            PIC X(55) VALUE  'Total number of open         
011220-        'orders with expired RFS date Week '.                            
011230     03  WEEK2             PIC 9(4).                                      
011400     03  FILLER            PIC X     VALUE X'5E'.                         
011410     03  FILLER            PIC X(66) VALUE  'Total number of order        
011420-        's with DESADV sent on correct RFS date Week '.                  
011440     03  WEEK3             PIC 9(4).                                      
011600     03  FILLER            PIC X     VALUE X'5E'.                         
011610     03  FILLER            PIC X(61) VALUE  'Total number of order        
011620-        's with DESADV sent after RFS date Week '.                       
011621     03  WEEK4             PIC 9(4).                                      
011630     03  FILLER            PIC X     VALUE X'5E'.                         
011710     03  FILLER            PIC X(61) VALUE  'Total number of order        
011720-       's with DESADV sent before RFS date Week '.                       
011730     03  WEEK5             PIC 9(4).                                      
011800     03  FILLER            PIC X     VALUE X'5E'.                         
011810     03  FILLER            PIC X(81) VALUE  'Total number of order        
011820-        's with DESADV sent after RFS date but at code 96 date We        
011830-        'ek '.                                                           
011840     03  WEEK6             PIC 9(4).                                      
012000     03  FILLER            PIC X     VALUE X'5E'.                         
012010     03  FILLER            PIC X(70) VALUE  'Total number of order        
012020-        's with DESADV sent later than code 96 date Week '.              
012030     03  WEEK7             PIC 9(4).                                      
012200     03  FILLER            PIC X     VALUE X'5E'.                         
012210     03  FILLER           PIC X(91)  VALUE  'Total number of order        
012220-        's with DESADV sent after RFS date but earlier then code         
012230-        '96 date Week '.                                                 
012240     03  WEEK8             PIC 9(4).                                      
012600     03  FILLER            PIC X     VALUE X'5E'.                         
012900                                                                          
013000                                                                          
021100 01  RAD.                                                                 
021200     03  FILLER            PIC X    VALUE SPACE.                          
021310     03  RAD-IDARTNR       PIC Z(8)9.                                     
021400     03  FILLER            PIC X    VALUE X'5E'.                          
021410     03  RAD-IDLEVNR       PIC X(5).                                      
021600     03  FILLER            PIC X    VALUE X'5E'.                          
021601     03 RAD-TABELL OCCURS 9.                                              
021602        05 RAD-FACK.                                                      
021603           07 RAD-KVKOL    PIC Z(6)9 VALUE ZERO.                          
021604           07 FILLER       PIC X     VALUE X'5E'.                         
023700                                                                          
030720 01  W001-DAP.                                                            
030730     03  FILLER                  PIC X(165)  VALUE SPACE.                 
030900                                                                          
031000                                                                          
031100 PROCEDURE DIVISION.                                                      
031200 MAIN SECTION.                                                            
031300                                                                          
031500     PERFORM A-INIT                                                       
031600                                                                          
031700     PERFORM S01-READ-W4639S                                              
031800     PERFORM UNTIL EOF-W4639S                                             
031900                                                                          
031901       IF LIST-IDLEVNR NOT = CURRENT-IDLEVNR OR                           
031902          LIST-IDARTNR NOT = CURRENT-IDARTNR                              
031903          PERFORM B-SKRIV-RAD                                             
031904       END-IF                                                             
031905                                                                          
031922       PERFORM C-SUMMERA-TAB                                              
032700                                                                          
032800       PERFORM S01-READ-W4639S                                            
032900     END-PERFORM                                                          
032910     PERFORM B-SKRIV-RAD                                                  
033000                                                                          
033100     PERFORM Z-FINIT                                                      
033200                                                                          
033300     MOVE ZERO TO RETURN-CODE                                             
033400     GOBACK                                                               
033500     .                                                                    
033600                                                                          
033700                                                                          
033800 A-INIT SECTION.                                                          
033900     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
034000                                                                          
034100     OPEN INPUT  W4639S                                                   
034200     OPEN OUTPUT W4639T                                                   
034300                                                                          
034400     ACCEPT TODAYS-DATE   FROM DATE                                       
034401                                                                          
034410     MOVE "AAMMDD" TO DAT-KDDATFORM                                       
034420     MOVE TODAYS-DATE TO DAT-I-TIDATUM                                    
034430     CALL WDATKONV USING                                                  
034440          DAT-KDDATFORM,                                                  
034450          DAT-I-TIDATUM,                                                  
034460          DAT-O-TIDATUM,                                                  
034470          DAT-KDSVAR                                                      
034471                                                                          
034480     MOVE DAT-TIAAVV-GRP TO WEEK1                                         
034490                            WEEK2                                         
034491                            WEEK3                                         
034492                            WEEK4                                         
034493                            WEEK5                                         
034494                            WEEK6                                         
034495                            WEEK7                                         
034496                            WEEK8                                         
034500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
034600     .                                                                    
034700                                                                          
034800                                                                          
034900 B-SKRIV-RAD        SECTION.                                              
035000     MOVE 'B-SKRIV-RAD     ' TO CURRENT-SECTION                           
035100                                                                          
035400     IF FORSTA                                                            
035410       PERFORM BA-SKRIV-RUBRIK                                            
035507     ELSE                                                                 
036101       PERFORM BB-REDIGERA-SKRIV-RAD                                      
036102       PERFORM BC-INITIERA-TABELL                                         
036103     END-IF                                                               
036104                                                                          
036107     .                                                                    
036108                                                                          
036109                                                                          
036110 BA-SKRIV-RUBRIK  SECTION.                                                
036111     MOVE 'BA-SKRIV-RUBRIK  ' TO CURRENT-SECTION                          
036112                                                                          
036113     MOVE NEJ              TO FORSTA-SW                                   
036114                                                                          
036115     MOVE '¤DAPW46394-001'  TO W001-DAP                                   
036116     WRITE DOP-POST FROM W001-DAP                                         
036117     MOVE 'W4639T'         TO POSTSUM-FDNAMN                              
036118     MOVE 'W46394D2'       TO POSTSUM-DDNAMN2                             
036119     MOVE 'ART'            TO POSTSUM-TRANSTYP                            
036120     CALL POSTSUM USING     POSTSUM-PARM                                  
036121                                                                          
036122     MOVE '¤DAPW46394    '  TO W001-DAP                                   
036123     WRITE DOP-POST FROM W001-DAP                                         
036124     MOVE 'W4639T'         TO POSTSUM-FDNAMN                              
036125     MOVE 'W46394D2'       TO POSTSUM-DDNAMN2                             
036126     MOVE 'ART'            TO POSTSUM-TRANSTYP                            
036127     CALL POSTSUM USING     POSTSUM-PARM                                  
036128                                                                          
036129     WRITE DOP-POST FROM RUBRIK-DDGS                                      
036130     MOVE 'W4639T'         TO POSTSUM-FDNAMN                              
036131     MOVE 'W46394D2'       TO POSTSUM-DDNAMN2                             
036132     MOVE 'ART'            TO POSTSUM-TRANSTYP                            
036133     CALL POSTSUM USING     POSTSUM-PARM                                  
036134                                                                          
036135     WRITE DOP-POST FROM RUBRIK-RAD                                       
036136     MOVE 'W4639T'         TO POSTSUM-FDNAMN                              
036137     MOVE 'W46394D2'       TO POSTSUM-DDNAMN2                             
036138     MOVE 'ART'            TO POSTSUM-TRANSTYP                            
036139     CALL POSTSUM USING     POSTSUM-PARM                                  
036140     .                                                                    
036141                                                                          
036142                                                                          
036143 BB-REDIGERA-SKRIV-RAD SECTION.                                           
036144     MOVE 'BB-REDIGERA-SKRIV-' TO CURRENT-SECTION                         
036145                                                                          
036146     MOVE CURRENT-IDLEVNR      TO RAD-IDLEVNR                             
036147     MOVE CURRENT-IDARTNR      TO RAD-IDARTNR                             
036148                                                                          
036149     MOVE 1                    TO INDX                                    
036150     PERFORM UNTIL INDX > 9                                               
036151       MOVE RAD-KVTYP (INDX)   TO RAD-KVKOL (INDX)                        
036152       ADD 1                   TO INDX                                    
036153     END-PERFORM                                                          
036154                                                                          
036155     WRITE DOP-POST FROM RAD                                              
036156     MOVE 'W4639T'             TO POSTSUM-FDNAMN                          
036157     MOVE 'W46394D2'           TO POSTSUM-DDNAMN2                         
036158     MOVE 'ART'                TO POSTSUM-TRANSTYP                        
036159     CALL POSTSUM USING        POSTSUM-PARM                               
036160     .                                                                    
036170                                                                          
036200                                                                          
036201 BC-INITIERA-TABELL SECTION.                                              
036202     MOVE 'BC-INITIERA-TABELL' TO CURRENT-SECTION                         
036211                                                                          
036212     MOVE 1                    TO INDX                                    
036213     PERFORM UNTIL INDX > 9                                               
036214       MOVE ZERO               TO RAD-KVTYP (INDX)                        
036215       ADD 1                   TO INDX                                    
036216     END-PERFORM                                                          
036217                                                                          
036220     .                                                                    
048800                                                                          
048810 C-SUMMERA-TAB      SECTION.                                              
048820     MOVE 'C-SUMMERA-TAB   ' TO CURRENT-SECTION                           
048821                                                                          
048825     MOVE LIST-IDPTYP        TO WS-IDPTYP-ALFA                            
048831     ADD 1                   TO RAD-KVTYP (WS-IDPTYP)                     
048833                                                                          
048834     MOVE LIST-IDLEVNR       TO CURRENT-IDLEVNR                           
048835     MOVE LIST-IDARTNR       TO CURRENT-IDARTNR                           
048837                                                                          
048840     .                                                                    
048900 Z-FINIT SECTION.                                                         
049000     CLOSE W4639S W4639T                                                  
049100                                                                          
049200     MOVE 'S'        TO POSTSUM-OPKOD                                     
049300     CALL POSTSUM USING POSTSUM-PARM                                      
049400     .                                                                    
049500                                                                          
049600                                                                          
049700 S01-READ-W4639S  SECTION.                                                
049800     MOVE 'S01-READ-W4639S ' TO CURRENT-SECTION                           
049900                                                                          
050000     READ W4639S INTO LIST-W4639S                                         
050100     AT END                                                               
050200        SET EOF-W4639S TO TRUE                                            
050300                                                                          
050400     NOT AT END                                                           
050500        MOVE 'W4639S'    TO POSTSUM-FDNAMN                                
050600        MOVE 'W46394D1'  TO POSTSUM-DDNAMN2                               
050700        MOVE LIST-IDPTYP TO POSTSUM-TRANSTYP                              
050800        CALL POSTSUM USING  POSTSUM-PARM                                  
050900     END-READ                                                             
051000     .                                                                    
