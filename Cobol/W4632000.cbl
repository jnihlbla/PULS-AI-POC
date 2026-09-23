000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4632000.                                                
000300 AUTHOR.         BO HAMMARIN, GDC GROUP.                                  
000400 DATE-WRITTEN.   MARS-99.                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER INFIL,                                                     
000900*        OMFORMAR DEN TILL EDI-POSTER AV TYPEN ORDERS OCH                 
001000*        SKRIVER UT EDI-FIL ENLIGT VERSION 1 - EDIFACT D/96B.             
001100*        SKRIVER ÄVEN UT EDI-LOGG FÖR VIDARE BEARBETNING.                 
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- INFIL MED DIREKTLEVERANSINFORMATION                        
002500     SELECT W46312                     ASSIGN TO W46320D1.                
002600                                                                          
002700*          --- UTFIL TILL EDI AV TYP ORDER                                
002800     SELECT W46340                     ASSIGN TO W46320D2.                
002900     EJECT                                                                
003000                                                                          
003100*          --- UTFIL TILL LOGG AV TYP ORDER                               
003200     SELECT W46341                     ASSIGN TO W46320D3.                
003300     EJECT                                                                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800                                                                          
003900 FD  W46312                                                               
004210     RECORDING V                                                          
004220     BLOCK CONTAINS 0.                                                    
004230                                                                          
004240 01  FILLER                  PIC X(998).                                  
004250 01  POST -COPY W46312 -L.                                                
004300     EJECT                                                                
004400                                                                          
004500 FD  W46340                                                               
004600     RECORDING       V                                                    
004700     BLOCK CONTAINS  0.                                                   
004800 01  EDI-POST                    PIC X(1005).                             
004900     EJECT                                                                
005000                                                                          
005100 FD  W46341                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400 01  UT-LOGG.                                                             
005500*    03 -COPY W46341  -L.                                                 
005600     EJECT                                                                
005700                                                                          
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000*    -- CHECKED BY WY2000                                                 
006100 77  IDPGM                       PIC X(8)    VALUE 'W4632000'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400 77  WS-HEX-BE                   PIC X       VALUE '´'.                   
006500 77  WS-HEX-61                   PIC X       VALUE '/'.                   
006600 77  WS-HEX-E0                   PIC X       VALUE 'É'.                   
006700 77  WS-HEX-4A                   PIC X       VALUE '§'.                   
006800 77  WS-HEX-79                   PIC X       VALUE 'é'.                   
006900 77  WS-HEX-49                   PIC X       VALUE 'ñ'.                   
007000 77  WS-HEX-69                   PIC X       VALUE 'Ñ'.                   
007100 77  WS-HEX-CE                   PIC X       VALUE 'ó'.                   
007200 77  WS-HEX-EE                   PIC X       VALUE 'Ó'.                   
007300 77  WS-HEX-9B                   PIC X       VALUE 'º'.                   
007400 77  WS-HEX-9A                   PIC X       VALUE 'ª'.                   
007500 77  WS-HEX-EC                   PIC X       VALUE '@'.                   
007600 77  WS-HEX-51                   PIC X       VALUE '`'.                   
007700 77  WS-HEX-FC                   PIC X       VALUE 'Ü'.                   
007800 77  WS-HEX-45                   PIC X       VALUE 'á'.                   
007900 77  WS-HEX-42                   PIC X       VALUE 'â'.                   
008000 77  WS-HEX-55                   PIC X       VALUE 'í'.                   
008100 77  WS-HEX-44                   PIC X       VALUE 'à'.                   
008200 77  WS-HEX-CD                   PIC X       VALUE 'ò'.                   
008300 77  WS-HEX-8D                   PIC X       VALUE 'ý'.                   
008400                                                                          
008500 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
008600 77  IA                          PIC S9(3)   VALUE ZERO COMP-3.           
008700 01  WTEXT                       PIC X(35)   VALUE SPACE.                 
008800 01  WTEXT3                      PIC X(40)   VALUE SPACE.                 
008900 01  FILLER REDEFINES  WTEXT3.                                            
009000 03  WTEXT2                      PIC X(35).                               
009100 77  WS-COUNT                    PIC S9(7)   COMP-3 VALUE ZERO.           
009200 77  WS-NO-OF-SEGM               PIC S9(3)   COMP-3 VALUE ZERO.           
009300 77  WS-NO-OF-PROD-UNITS         PIC S9(3)   COMP-3 VALUE ZERO.           
009400 77  WS-RECTYPE                  PIC X(3).                                
009500 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
009600     88  FIRST-TIME                          VALUE 'J'.                   
009700 77  W46312-EOF-SW               PIC X       VALUE 'N'.                   
009800     88  END-OF-W46312                       VALUE 'J'.                   
009900     EJECT                                                                
010000                                                                          
010110 01  WS-SPAR-DASKEPPN             PIC 9(8) VALUE ZERO.                    
010200 01  WS-SPAR-IDLEVNR              PIC X(5) VALUE SPACE.                   
010210 01  WS-TIREPDAT                  PIC 9(6) VALUE ZERO.                    
010300 01  WS-IDLEVNR-KOLL              PIC X(5) VALUE SPACE.                   
010400 01  WS-UNB-RECIPIENT-ID-X.                                               
010500     03  WS-UNB-RECIPIENT-ID      PIC 9(5).                               
010600 01  WS-DAGENS-DATUM-8            PIC 9(8).                               
010700 01  WS-DAGENS-DATUM-6            PIC 9(6).                               
010800 01  WS-DAGENS-KLOCKA.                                                    
010900     03  WS-DAGENS-KLOCKA-1-6.                                            
011000         05  WS-DAGENS-KLOCKA-1-4 PIC 9(4).                               
011100         05  WS-DAGENS-KLOCKA-5-6 PIC 9(2).                               
011200     03  WS-DAGENS-KLOCKA-7-9     PIC 9(3).                               
011300                                                                          
011400 01  WS-DAGENS-DATUM-KLOCKA-14.                                           
011500     03  WS-DAGENS-DATUM-ALFA-14  PIC X(8).                               
011600     03  WS-DAGENS-KLOCKA-ALFA-14 PIC X(6).                               
011700                                                                          
011800 01  WS-IDDISTR-IDKUNDNR.                                                 
011900     03  WS-IDDISTR               PIC 9(4).                               
012000     03  WS-IDKUNDNR              PIC 9(6).                               
012010                                                                          
012020 01  W-IDORDNR-X5                 PIC X(5).                               
012030 01  W-IDORDNR-N1.                                                        
012040     03  W-FILLER-1-4             PIC X(4) VALUE '0000'.                  
012092     03  W-IDORDNR-1              PIC X(1).                               
012093 01  W-IDORDNR-N2.                                                        
012094     03  W-FILLER-1-3             PIC X(3) VALUE '000'.                   
012095     03  W-IDORDNR-2              PIC X(2).                               
012096 01  W-IDORDNR-N3.                                                        
012097     03  W-FILLER-1-2             PIC X(2) VALUE '00'.                    
012098     03  W-IDORDNR-3              PIC X(3).                               
012099 01  W-IDORDNR-N4.                                                        
012100     03  W-FILLER-1               PIC X(1) VALUE '0'.                     
012101     03  W-IDORDNR-4              PIC X(4).                               
012110     EJECT                                                                
012200 01  TEST-IDDISTR                 PIC 9(5)  COMP-3.                       
012300*01  FILLER    -COPY WWDIST03   -RED TEST-IDDISTR.                        
012400     EJECT                                                                
012500                                                                          
012600 01  DYNAMISKA-SUBPROGRAM.                                                
012700*                                                                         
012800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013200     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
013300     SKIP2                                                                
013400*    --- PARAMETRAR TILL ABEND                                            
013500     EJECT                                                                
013600                                                                          
013700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014000                                                                          
014100 01  FELTEXT.                                                             
014200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014400     EJECT                                                                
014500                                                                          
014600*    --- PARAMETRAR TILL POSTSUM                                          
014700*                                                                         
014800*01  -COPY W0005   -PRE  POSTSUM-                                         
014900     EJECT                                                                
015000                                                                          
015100*    --- PARAMETRAR TILL W009CIA                                          
015200*01  -COPY W009CIA                                                        
015300     EJECT                                                                
015400                                                                          
015500 01  IN-AREA-START               PIC X(24)   VALUE                        
015600                                 'IN-AREA-START  '.                       
015700*01  AREA -COPY W46312     -PRE IN-                                       
015800     EJECT                                                                
015900                                                                          
016000 01  UT-AREA-LOGG-START          PIC X(24)   VALUE                        
016100                                 'UT-AREA-LOGG-START  '.                  
016200*01  AREA -COPY W46341     -PRE UT-                                       
016300     EJECT                                                                
016400                                                                          
016500 01  UT-AREOR-START              PIC X(24)   VALUE                        
016600                                'UT-AREOR-START  '.                       
016700*01  -COPY WEDIBGM1                                                       
016800                                                                          
016900*01  -COPY WEDIDTM1                                                       
017000                                                                          
017010*01  -COPY WEDIGIR0                                                       
017020                                                                          
017100*01  -COPY WEDIFTX1                                                       
017200                                                                          
017300*01  -COPY WEDIRFF1                                                       
017400                                                                          
017410*01  -COPY WEDILOCD                                                       
017420                                                                          
017500*01  -COPY WEDINAD1                                                       
017600                                                                          
017700*01  -COPY WEDIRCS1                                                       
017800                                                                          
017900*01  -COPY WEDIIMD1                                                       
018000                                                                          
018100*01  -COPY WEDIQTY1                                                       
018200                                                                          
018300*01  -COPY WEDILIN1                                                       
018400                                                                          
018500*01  -COPY WEDIALI0                                                       
018600                                                                          
018700*01  -COPY WEDIH001                                                       
018800                                                                          
018900*01  -COPY WEDIUNB0                                                       
019000                                                                          
019100*01  -COPY WEDIUNH0                                                       
019200                                                                          
019300*01  -COPY WEDIUNS1                                                       
019400                                                                          
019500*01  -COPY WEDIUNT0                                                       
019600                                                                          
019700*01  -COPY WEDIT003                                                       
019800     EJECT                                                                
019900                                                                          
020000 PROCEDURE DIVISION.                                                      
020100 MAIN SECTION.                                                            
020200     PERFORM A-INIT                                                       
020300                                                                          
020400     PERFORM S01-LAES-W46312                                              
020500     IF NOT END-OF-W46312                                                 
020600       PERFORM B-BYGG-EDI-HEADER                                          
020700     END-IF                                                               
020800                                                                          
020900     PERFORM UNTIL END-OF-W46312                                          
021000       PERFORM C-BYGG-EDI-MESSAGE                                         
021100       PERFORM S01-LAES-W46312                                            
021200     END-PERFORM                                                          
021300                                                                          
021400     IF WS-COUNT > ZERO                                                   
021500       PERFORM C-BYGG-EDI-MESSAGE                                         
021600       PERFORM D-BYGG-EDI-TRAILER                                         
021700     END-IF                                                               
021800                                                                          
021900     PERFORM Z-FINIT                                                      
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400                                                                          
022500 A-INIT SECTION.                                                          
022600     OPEN INPUT  W46312                                                   
022700     OPEN OUTPUT W46340                                                   
022800                 W46341                                                   
022900                                                                          
023000     ACCEPT WS-DAGENS-KLOCKA         FROM TIME                            
023100     MOVE WS-DAGENS-KLOCKA-1-6       TO WS-DAGENS-KLOCKA-ALFA-14          
023200     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM-8                 
023300     MOVE WS-DAGENS-DATUM-8          TO WS-DAGENS-DATUM-ALFA-14           
023400     MOVE FUNCTION CURRENT-DATE(3:6) TO WS-DAGENS-DATUM-6                 
023500                                                                          
023600     MOVE IDPGM                      TO POSTSUM-PROGNAMN                  
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000 B-BYGG-EDI-HEADER SECTION.                                               
024100     MOVE SPACE                   TO WEDIH001                             
024200                                                                          
024300     MOVE '001'                   TO WS-RECTYPE                           
024400                                     H001-IDPTYP                          
024500     MOVE 73                      TO H001-LENGTH                          
024600     MOVE 'WPAR'                  TO H001-SENDER-NODE                     
024700     MOVE 'VAMP'                  TO H001-RECEIVER-NODE                   
024800     MOVE 'ORDER96B'              TO H001-FILE-NAME                       
024900     MOVE WS-DAGENS-DATUM-6       TO H001-DATE                            
025000     MOVE WS-DAGENS-KLOCKA-1-6    TO H001-TIME                            
025100                                                                          
025200     PERFORM S02-SKRIV-W46340                                             
025300     .                                                                    
025400     EJECT                                                                
025500                                                                          
025600 C-BYGG-EDI-MESSAGE SECTION.                                              
025700     IF END-OF-W46312                                                     
025800       PERFORM CP-BYGG-UNS                                                
025900       PERFORM CQ-BYGG-UNT                                                
026000     ELSE                                                                 
026100       IF IN-PU-IDPTYP = 'HUV'                                            
026200         IF IN-PU-IDLEVNR NOT = WS-SPAR-IDLEVNR                           
026300           IF NOT FIRST-TIME                                              
026400             PERFORM CP-BYGG-UNS                                          
026500             PERFORM CQ-BYGG-UNT                                          
026600           ELSE                                                           
026700             MOVE NEJ         TO FIRST-TIME-SW                            
026800           END-IF                                                         
026900           PERFORM CR-BYGG-UNB                                            
027000           MOVE IN-PU-IDLEVNR TO WS-SPAR-IDLEVNR                          
027100         ELSE                                                             
027200           PERFORM CP-BYGG-UNS                                            
027300           PERFORM CQ-BYGG-UNT                                            
027400         END-IF                                                           
027500         PERFORM CA-BYGG-UNH                                              
027600         PERFORM CB-BYGG-BGM                                              
027700         PERFORM CC-BYGG-DTM                                              
027800         PERFORM CD-BYGG-FTX-XX                                           
027900         PERFORM CE-BYGG-RFF-CR                                           
028000         PERFORM CF-BYGG-NAD-BY                                           
028100         PERFORM CG-BYGG-RFF-VA                                           
028200         PERFORM CH-BYGG-NAD-XX                                           
028300         PERFORM CI-BYGG-RCS                                              
028400         MOVE IN-PU-DASKEPPN    TO WS-SPAR-DASKEPPN                       
028500       ELSE                                                               
028600         PERFORM CJ-BYGG-LIN                                              
028700         PERFORM CK-BYGG-IMD                                              
028800         PERFORM CL-BYGG-QTY                                              
028900         PERFORM CM-BYGG-ALI                                              
029000         PERFORM CN-BYGG-DTM                                              
029010         PERFORM CT-BYGG-DTM-TIREPDAT                                     
029020         PERFORM CU-BYGG-GIR-IDBILREG                                     
029100         PERFORM CO-BYGG-FTX-AAJ                                          
029110         PERFORM CV-BYGG-RFF-IDKUNDRF-WIP                                 
029120         PERFORM CX-BYGG-LOC-BEMEKAN                                      
029200         PERFORM CS-BYGG-LOGG                                             
029300       END-IF                                                             
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029800 CA-BYGG-UNH SECTION.                                                     
029900     MOVE SPACE                 TO WEDIUNH                                
030000                                                                          
030100     MOVE 'UNH'                 TO WS-RECTYPE                             
030200                                   UNH-IDPTYP                             
030300     MOVE 72                    TO UNH-LENGTH                             
030400     ADD +1                     TO WS-NO-OF-PROD-UNITS                    
030500     MOVE 'VO'                  TO CIA-IDARTPRE-IN                        
030600     MOVE WS-NO-OF-PROD-UNITS   TO CIA-IDARTBET-IN                        
030700     CALL W009CIA USING            CIA-W009CIA                            
030800     MOVE CIA-IDARTBET-UT       TO UNH-REFNO                              
030900     MOVE 'ORDERS'              TO UNH-TYPE                               
031000     MOVE 'D'                   TO UNH-VERSION                            
031100     MOVE '96B'                 TO UNH-RELEASE                            
031200     MOVE 'UN'                  TO UNH-CONTR-AGENCY                       
031300     MOVE 'A18030'              TO UNH-ASS-ASSIGN-CODE                    
031400                                                                          
031500     ADD +1                     TO WS-NO-OF-SEGM                          
031600     PERFORM S02-SKRIV-W46340                                             
031700     .                                                                    
031800     EJECT                                                                
031900                                                                          
032000 CB-BYGG-BGM SECTION.                                                     
032100     MOVE SPACE            TO WEDIBGM1                                    
032200                                                                          
032300     MOVE 'BGM'            TO WS-RECTYPE                                  
032400                              BGM1-IDPTYP                                 
032500     MOVE 44               TO BGM1-LENGTH                                 
032600     MOVE '220'            TO BGM1-DOC-NAME-CODE                          
032700     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
032800     MOVE IN-PU-IDPRODNR   TO UT-IDPRODNR                                 
032900                              CIA-IDARTBET-IN                             
033000     CALL W009CIA USING       CIA-W009CIA                                 
033100     MOVE CIA-IDARTBET-UT  TO BGM1-DOCNO                                  
033200     MOVE 'NA'             TO BGM1-RESPONSE-TYPE                          
033300                                                                          
033400     ADD +1                TO WS-NO-OF-SEGM                               
033500     PERFORM S02-SKRIV-W46340                                             
033600     .                                                                    
033700     EJECT                                                                
033800                                                                          
033900 CC-BYGG-DTM SECTION.                                                     
034000     MOVE SPACE            TO WEDIDTM1                                    
034100                                                                          
034200     MOVE 'DTM'            TO WS-RECTYPE                                  
034300                              DTM1-IDPTYP                                 
034400     MOVE 41               TO DTM1-LENGTH                                 
034500     MOVE '137'            TO DTM1-QUAL                                   
034600     MOVE WS-DAGENS-DATUM-KLOCKA-14                                       
034700                           TO DTM1-DATE-TIME                              
034800     MOVE '204'            TO DTM1-FORMAT-QUAL                            
034900                                                                          
035000     ADD +1                TO WS-NO-OF-SEGM                               
035100     PERFORM S02-SKRIV-W46340                                             
035200     .                                                                    
035300     EJECT                                                                
035400                                                                          
035500 CD-BYGG-FTX-XX SECTION.                                                  
035600     MOVE SPACE            TO WEDIFTX1                                    
035700                                                                          
035800     MOVE 'FTX'            TO WS-RECTYPE                                  
035900                              FTX1-IDPTYP                                 
036000     MOVE 233              TO FTX1-LENGTH                                 
036100     MOVE 'EUR'            TO FTX1-SUBJECT-QUAL                           
036200     MOVE '1'              TO FTX1-TEXT-ID                                
036300     MOVE '92'             TO FTX1-RESPONSIBLE                            
036400     MOVE IN-PU-IDZON      TO FTX1-FREE-TEXT-1                            
036500     MOVE IN-PU-IDDEPOT    TO FTX1-FREE-TEXT-2                            
036600     MOVE IN-PU-IDROUTE    TO FTX1-FREE-TEXT-3                            
036700                                                                          
036800     IF (FTX1-FREE-TEXT-2 NOT = SPACE  OR                                 
036810         FTX1-FREE-TEXT-3 NOT = SPACE  OR                                 
036820         FTX1-FREE-TEXT-4 NOT = SPACE  OR                                 
036830         FTX1-FREE-TEXT-5 NOT = SPACE) AND                                
036840         FTX1-FREE-TEXT-1 = SPACE                                         
036850       MOVE '00' TO FTX1-FREE-TEXT-1                                      
036860     END-IF                                                               
036870                                                                          
036900     ADD +1                TO WS-NO-OF-SEGM                               
037000     PERFORM S02-SKRIV-W46340                                             
037100                                                                          
037200     MOVE SPACE            TO WEDIFTX1                                    
037300                                                                          
037400     MOVE 'FTX'            TO WS-RECTYPE                                  
037500                              FTX1-IDPTYP                                 
037600     MOVE 233              TO FTX1-LENGTH                                 
037700     MOVE 'PKG'            TO FTX1-SUBJECT-QUAL                           
037800     MOVE '1'              TO FTX1-TEXT-ID                                
037900     MOVE '92'             TO FTX1-RESPONSIBLE                            
038000     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
038100     MOVE IN-PU-KDFRAKT    TO CIA-IDARTBET-IN                             
038200     CALL W009CIA USING       CIA-W009CIA                                 
038300     MOVE CIA-IDARTBET-UT  TO FTX1-FREE-TEXT-1                            
038400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Å' BY 'A'                    
038500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Ä' BY 'A'                    
038600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Ö' BY 'O'                    
038700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL '+' BY ' '                    
038800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL ':' BY ' '                    
038900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL '?' BY ' '                    
039000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL "'" BY ' '                    
039100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL "&" BY ' '                    
039200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'ü' BY 'U'                    
039300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-BE BY ' '              
039400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-61 BY ' '              
039500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-E0 BY 'E'              
039600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-4A BY ' '              
039700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-79 BY 'E'              
039800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-49 BY 'N'              
039900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-69 BY 'N'              
040000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-CE BY 'O'              
040100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-EE BY 'O'              
040200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-9B BY ' '              
040300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-9A BY ' '              
040400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-EC BY 'O'              
040500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-51 BY ' '              
040600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-FC BY 'U'              
040700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-45 BY 'A'              
040800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-42 BY 'A'              
040900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-55 BY 'I'              
041000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-44 BY 'A'              
041100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-CD BY 'O'              
041200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-8D BY 'Y'              
041300                                                                          
041400     ADD +1                TO WS-NO-OF-SEGM                               
041500     PERFORM S02-SKRIV-W46340                                             
041600                                                                          
041700     MOVE SPACE            TO WEDIFTX1                                    
041800                                                                          
041900     MOVE 'FTX'            TO WS-RECTYPE                                  
042000                              FTX1-IDPTYP                                 
042100     MOVE 233              TO FTX1-LENGTH                                 
042200     MOVE 'AAJ'            TO FTX1-SUBJECT-QUAL                           
042300     MOVE '1'              TO FTX1-TEXT-ID                                
042400     MOVE '92'             TO FTX1-RESPONSIBLE                            
042500     MOVE IN-PU-BEGDSMRK   TO FTX1-FREE-TEXT-1                            
042600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Å' BY 'A'                    
042700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Ä' BY 'A'                    
042800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Ö' BY 'O'                    
042900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL '+' BY ' '                    
043000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL ':' BY ' '                    
043100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL '?' BY ' '                    
043200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL "'" BY ' '                    
043300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL "&" BY ' '                    
043400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'ü' BY 'U'                    
043500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-BE BY ' '              
043600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-61 BY ' '              
043700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-E0 BY 'E'              
043800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-4A BY ' '              
043900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-79 BY 'E'              
044000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-49 BY 'N'              
044100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-69 BY 'N'              
044200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-CE BY 'O'              
044300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-EE BY 'O'              
044400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-9B BY ' '              
044500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-9A BY ' '              
044600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-EC BY 'O'              
044700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-51 BY ' '              
044800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-FC BY 'U'              
044900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-45 BY 'A'              
045000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-42 BY 'A'              
045100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-55 BY 'I'              
045200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-44 BY 'A'              
045300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-CD BY 'O'              
045400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-8D BY 'Y'              
045500                                                                          
045600     ADD +1                TO WS-NO-OF-SEGM                               
045700     PERFORM S02-SKRIV-W46340                                             
045800                                                                          
045900     MOVE SPACE            TO WEDIFTX1                                    
046000                                                                          
046100     MOVE 'FTX'            TO WS-RECTYPE                                  
046200                              FTX1-IDPTYP                                 
046300     MOVE 233              TO FTX1-LENGTH                                 
046400     MOVE 'WHI'            TO FTX1-SUBJECT-QUAL                           
046500     MOVE '1'              TO FTX1-TEXT-ID                                
046600     MOVE '92'             TO FTX1-RESPONSIBLE                            
046700     MOVE IN-PU-BELAGINS   TO FTX1-FREE-TEXT-1                            
046800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Å' BY 'A'                    
046900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Ä' BY 'A'                    
047000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'Ö' BY 'O'                    
047100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL '+' BY ' '                    
047200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL ':' BY ' '                    
047300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL '?' BY ' '                    
047400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL "'" BY ' '                    
047500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL "&" BY ' '                    
047600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL 'ü' BY 'U'                    
047700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-BE BY ' '              
047800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-61 BY ' '              
047900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-E0 BY 'E'              
048000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-4A BY ' '              
048100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-79 BY 'E'              
048200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-49 BY 'N'              
048300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-69 BY 'N'              
048400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-CE BY 'O'              
048500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-EE BY 'O'              
048600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-9B BY ' '              
048700     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-9A BY ' '              
048800     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-EC BY 'O'              
048900     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-51 BY ' '              
049000     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-FC BY 'U'              
049100     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-45 BY 'A'              
049200     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-42 BY 'A'              
049300     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-55 BY 'I'              
049400     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-44 BY 'A'              
049500     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-CD BY 'O'              
049600     INSPECT FTX1-FREE-TEXT-1 REPLACING ALL WS-HEX-8D BY 'Y'              
049700                                                                          
049800     ADD +1                TO WS-NO-OF-SEGM                               
049900     PERFORM S02-SKRIV-W46340                                             
050000     .                                                                    
050100     EJECT                                                                
050200                                                                          
050300 CE-BYGG-RFF-CR SECTION.                                                  
050400     MOVE SPACE            TO WEDIRFF1                                    
050500                                                                          
050600     MOVE 'RFF'            TO WS-RECTYPE                                  
050700                              RFF1-IDPTYP                                 
050800     MOVE 38               TO RFF1-LENGTH                                 
050900     MOVE 'CR'             TO RFF1-QUAL                                   
051000     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
051100     MOVE IN-PU-IDORDNR    TO UT-IDORDNR7                                 
051200                              CIA-IDARTBET-IN                             
051300     CALL W009CIA USING       CIA-W009CIA                                 
051395     MOVE CIA-IDARTBET-UT  TO W-IDORDNR-X5                                
051396     IF W-IDORDNR-X5(1:1) = SPACE                                         
051397       MOVE ZERO TO W-IDORDNR-X5                                          
051398     ELSE                                                                 
051399       IF W-IDORDNR-X5(2:1) = SPACE                                       
051400         MOVE W-IDORDNR-X5(1:1)  TO W-IDORDNR-1                           
051401         MOVE W-IDORDNR-N1       TO W-IDORDNR-X5                          
051402       ELSE                                                               
051403         IF W-IDORDNR-X5(3:1) = SPACE                                     
051404           MOVE W-IDORDNR-X5(1:2)  TO W-IDORDNR-2                         
051405           MOVE W-IDORDNR-N2       TO W-IDORDNR-X5                        
051406         ELSE                                                             
051407           IF W-IDORDNR-X5(4:1) = SPACE                                   
051408             MOVE W-IDORDNR-X5(1:3)  TO W-IDORDNR-3                       
051409             MOVE W-IDORDNR-N3       TO W-IDORDNR-X5                      
051410           ELSE                                                           
051411             IF W-IDORDNR-X5(5:1) = SPACE                                 
051412               MOVE W-IDORDNR-X5(1:4)  TO W-IDORDNR-4                     
051413               MOVE W-IDORDNR-N4       TO W-IDORDNR-X5                    
051414             END-IF                                                       
051415           END-IF                                                         
051416         END-IF                                                           
051417       END-IF                                                             
051418     END-IF                                                               
051419     MOVE W-IDORDNR-X5     TO CIA-IDARTBET-UT                             
051420     MOVE CIA-IDARTBET-UT  TO RFF1-REFNO                                  
051500                                                                          
051600     ADD +1                TO WS-NO-OF-SEGM                               
051700     PERFORM S02-SKRIV-W46340                                             
051800     .                                                                    
051900     EJECT                                                                
052000                                                                          
052100 CF-BYGG-NAD-BY SECTION.                                                  
052200     MOVE SPACE             TO WEDINAD1                                   
052300                                                                          
052400     MOVE 'NAD'             TO WS-RECTYPE                                 
052500                               NAD1-IDPTYP                                
052600     MOVE 225               TO NAD1-LENGTH                                
052700     MOVE 'BY'              TO NAD1-QUAL                                  
052800                                                                          
052900     MOVE IN-PU-IDLEVNR           TO WS-IDLEVNR-KOLL                      
053000     INSPECT WS-IDLEVNR-KOLL      REPLACING ALL SPACE BY ZERO             
053100     IF WS-IDLEVNR-KOLL NUMERIC                                           
053200       MOVE '02796'               TO NAD1-PARTY-ID                        
053300     ELSE                                                                 
053400       MOVE 'BP2T7'               TO NAD1-PARTY-ID                        
053500     END-IF                                                               
053600                                                                          
053700     MOVE '91'              TO NAD1-RESPONSIBLE                           
053800     MOVE 'Volvo Car Corporation - PS&L     '                             
053810                            TO NAD1-PARTY-NAME                            
053900     MOVE IN-PU-IDDC        TO NAD1-COUNTRY-CODE                          
054000                               UT-IDDC                                    
054100                                                                          
054200     ADD +1                 TO WS-NO-OF-SEGM                              
054300     PERFORM S02-SKRIV-W46340                                             
054400     .                                                                    
054500     EJECT                                                                
054600                                                                          
054700 CG-BYGG-RFF-VA SECTION.                                                  
054800     MOVE SPACE            TO WEDIRFF1                                    
054900                                                                          
055000     MOVE 'RFF'            TO WS-RECTYPE                                  
055100                              RFF1-IDPTYP                                 
055200     MOVE 38               TO RFF1-LENGTH                                 
055300     MOVE 'VA'             TO RFF1-QUAL                                   
055400     MOVE IN-PU-IDVAT      TO RFF1-REFNO                                  
055500                                                                          
055600     ADD +1                TO WS-NO-OF-SEGM                               
055700     PERFORM S02-SKRIV-W46340                                             
055800     .                                                                    
055900     EJECT                                                                
056000                                                                          
056100 CH-BYGG-NAD-XX SECTION.                                                  
056200     MOVE SPACE             TO WEDINAD1                                   
056300                                                                          
056400     MOVE IN-PU-IDDISTR     TO TEST-IDDISTR                               
056500                                                                          
056600     MOVE 'NAD'             TO WS-RECTYPE                                 
056700                               NAD1-IDPTYP                                
056800     MOVE 225               TO NAD1-LENGTH                                
056900     MOVE 'SE'              TO NAD1-QUAL                                  
057000     MOVE 'VO'              TO CIA-IDARTPRE-IN                            
057100     MOVE IN-PU-IDLEVNR     TO NAD1-PARTY-ID                              
057200                               UT-IDLEVNR                                 
057300     MOVE '92'              TO NAD1-RESPONSIBLE                           
057400                                                                          
057500     ADD +1                 TO WS-NO-OF-SEGM                              
057600     PERFORM S02-SKRIV-W46340                                             
057700                                                                          
057800     MOVE SPACE             TO WEDINAD1                                   
057900                                                                          
058000     MOVE 'NAD'             TO WS-RECTYPE                                 
058100                               NAD1-IDPTYP                                
058200     MOVE 225               TO NAD1-LENGTH                                
058300     MOVE 'DP'              TO NAD1-QUAL                                  
058400     MOVE IN-PU-KDVIA       TO NAD1-PARTY-ID                              
058500                               UT-KDVIA                                   
058600     MOVE '92'              TO NAD1-RESPONSIBLE                           
058700                                                                          
058800     ADD +1                 TO WS-NO-OF-SEGM                              
058900     PERFORM S02-SKRIV-W46340                                             
059000                                                                          
059100     MOVE SPACE             TO WEDINAD1                                   
059200                                                                          
059300     MOVE 'NAD'             TO WS-RECTYPE                                 
059400                               NAD1-IDPTYP                                
059500     MOVE 225               TO NAD1-LENGTH                                
059600     MOVE 'CN'              TO NAD1-QUAL                                  
059700     MOVE IN-PU-IDDISTR     TO WS-IDDISTR                                 
059800                               UT-IDDISTR                                 
059900     MOVE IN-PU-IDKUNDNR    TO WS-IDKUNDNR                                
060000                               UT-IDKUNDNR                                
060100     MOVE WS-IDDISTR-IDKUNDNR                                             
060200                            TO NAD1-PARTY-ID                              
060300     MOVE '92'              TO NAD1-RESPONSIBLE                           
060400     MOVE FUNCTION UPPER-CASE(IN-PU-BEGMT-RAD1)                           
060500                            TO NAD1-NAME-ADR-1                            
060600     INSPECT NAD1-NAME-ADR-1  REPLACING ALL 'Å' BY 'A'                    
060700     INSPECT NAD1-NAME-ADR-1  REPLACING ALL 'Ä' BY 'A'                    
060800     INSPECT NAD1-NAME-ADR-1  REPLACING ALL 'Ö' BY 'O'                    
060900     INSPECT NAD1-NAME-ADR-1  REPLACING ALL '+' BY ' '                    
061000     INSPECT NAD1-NAME-ADR-1  REPLACING ALL ':' BY ' '                    
061100     INSPECT NAD1-NAME-ADR-1  REPLACING ALL '?' BY ' '                    
061200     INSPECT NAD1-NAME-ADR-1  REPLACING ALL "'" BY ' '                    
061300     INSPECT NAD1-NAME-ADR-1  REPLACING ALL "&" BY ' '                    
061400     INSPECT NAD1-NAME-ADR-1  REPLACING ALL 'ü' BY 'U'                    
061500     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-BE BY ' '              
061600     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-61 BY ' '              
061700     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-E0 BY 'E'              
061800     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-4A BY ' '              
061900     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-79 BY 'E'              
062000     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-49 BY 'N'              
062100     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-69 BY 'N'              
062200     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-CE BY 'O'              
062300     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-EE BY 'O'              
062400     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-9B BY ' '              
062500     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-9A BY ' '              
062600     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-EC BY 'O'              
062700     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-51 BY ' '              
062800     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-FC BY 'U'              
062900     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-45 BY 'A'              
063000     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-42 BY 'A'              
063100     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-55 BY 'I'              
063200     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-44 BY 'A'              
063300     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-CD BY 'O'              
063400     INSPECT NAD1-NAME-ADR-1  REPLACING ALL WS-HEX-8D BY 'Y'              
063500     MOVE FUNCTION UPPER-CASE(IN-PU-BEGMT-RAD2)                           
063600                            TO NAD1-NAME-ADR-2                            
063700     INSPECT NAD1-NAME-ADR-2  REPLACING ALL 'Å' BY 'A'                    
063800     INSPECT NAD1-NAME-ADR-2  REPLACING ALL 'Ä' BY 'A'                    
063900     INSPECT NAD1-NAME-ADR-2  REPLACING ALL 'Ö' BY 'O'                    
064000     INSPECT NAD1-NAME-ADR-2  REPLACING ALL '+' BY ' '                    
064100     INSPECT NAD1-NAME-ADR-2  REPLACING ALL ':' BY ' '                    
064200     INSPECT NAD1-NAME-ADR-2  REPLACING ALL '?' BY ' '                    
064300     INSPECT NAD1-NAME-ADR-2  REPLACING ALL "'" BY ' '                    
064400     INSPECT NAD1-NAME-ADR-2  REPLACING ALL "&" BY ' '                    
064500     INSPECT NAD1-NAME-ADR-2  REPLACING ALL 'ü' BY 'U'                    
064600     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-BE BY ' '              
064700     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-61 BY ' '              
064800     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-E0 BY 'E'              
064900     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-4A BY ' '              
065000     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-79 BY 'E'              
065100     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-49 BY 'N'              
065200     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-69 BY 'N'              
065300     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-CE BY 'O'              
065400     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-EE BY 'O'              
065500     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-9B BY ' '              
065600     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-9A BY ' '              
065700     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-EC BY 'O'              
065800     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-51 BY ' '              
065900     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-FC BY 'U'              
066000     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-45 BY 'A'              
066100     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-42 BY 'A'              
066200     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-55 BY 'I'              
066300     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-44 BY 'A'              
066400     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-CD BY 'O'              
066500     INSPECT NAD1-NAME-ADR-2  REPLACING ALL WS-HEX-8D BY 'Y'              
066600     MOVE FUNCTION UPPER-CASE(IN-PU-ADGMT-GATA)                           
066700                            TO NAD1-NAME-ADR-3                            
066800     INSPECT NAD1-NAME-ADR-3  REPLACING ALL 'Å' BY 'A'                    
066900     INSPECT NAD1-NAME-ADR-3  REPLACING ALL 'Ä' BY 'A'                    
067000     INSPECT NAD1-NAME-ADR-3  REPLACING ALL 'Ö' BY 'O'                    
067100     INSPECT NAD1-NAME-ADR-3  REPLACING ALL '+' BY ' '                    
067200     INSPECT NAD1-NAME-ADR-3  REPLACING ALL ':' BY ' '                    
067300     INSPECT NAD1-NAME-ADR-3  REPLACING ALL '?' BY ' '                    
067400     INSPECT NAD1-NAME-ADR-3  REPLACING ALL "'" BY ' '                    
067500     INSPECT NAD1-NAME-ADR-3  REPLACING ALL "&" BY ' '                    
067600     INSPECT NAD1-NAME-ADR-3  REPLACING ALL 'ü' BY 'U'                    
067700     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-BE BY ' '              
067800     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-61 BY ' '              
067900     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-E0 BY 'E'              
068000     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-4A BY ' '              
068100     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-79 BY 'E'              
068200     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-49 BY 'N'              
068300     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-69 BY 'N'              
068400     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-CE BY 'O'              
068500     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-EE BY 'O'              
068600     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-9B BY ' '              
068700     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-9A BY ' '              
068800     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-EC BY 'O'              
068900     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-51 BY ' '              
069000     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-FC BY 'U'              
069100     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-45 BY 'A'              
069200     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-42 BY 'A'              
069300     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-55 BY 'I'              
069400     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-44 BY 'A'              
069500     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-CD BY 'O'              
069600     INSPECT NAD1-NAME-ADR-3  REPLACING ALL WS-HEX-8D BY 'Y'              
069700     MOVE FUNCTION UPPER-CASE(IN-PU-ADGMT-PADR)                           
069800                            TO NAD1-NAME-ADR-4                            
069900*    danskt ä till ae                                                     
070000     IF DIST03-DANMARK                                                    
070100       PERFORM S04-DANSKT-AE                                              
070200     END-IF                                                               
070300     INSPECT NAD1-NAME-ADR-4  REPLACING ALL 'Å' BY 'A'                    
070400     INSPECT NAD1-NAME-ADR-4  REPLACING ALL 'Ä' BY 'A'                    
070500     INSPECT NAD1-NAME-ADR-4  REPLACING ALL 'Ö' BY 'O'                    
070600     INSPECT NAD1-NAME-ADR-4  REPLACING ALL '+' BY ' '                    
070700     INSPECT NAD1-NAME-ADR-4  REPLACING ALL ':' BY ' '                    
070800     INSPECT NAD1-NAME-ADR-4  REPLACING ALL '?' BY ' '                    
070900     INSPECT NAD1-NAME-ADR-4  REPLACING ALL "'" BY ' '                    
071000     INSPECT NAD1-NAME-ADR-4  REPLACING ALL "&" BY ' '                    
071100     INSPECT NAD1-NAME-ADR-4  REPLACING ALL 'ü' BY 'U'                    
071200     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-BE BY ' '              
071300     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-61 BY ' '              
071400     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-E0 BY 'E'              
071500     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-4A BY ' '              
071600     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-79 BY 'E'              
071700     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-49 BY 'N'              
071800     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-69 BY 'N'              
071900     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-CE BY 'O'              
072000     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-EE BY 'O'              
072100     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-9B BY ' '              
072200     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-9A BY ' '              
072300     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-EC BY 'O'              
072400     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-51 BY ' '              
072500     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-FC BY 'U'              
072600     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-45 BY 'A'              
072700     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-42 BY 'A'              
072800     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-55 BY 'I'              
072900     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-44 BY 'A'              
073000     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-CD BY 'O'              
073100     INSPECT NAD1-NAME-ADR-4  REPLACING ALL WS-HEX-8D BY 'Y'              
073200     MOVE FUNCTION UPPER-CASE(IN-PU-ADGMT-LAND)                           
073300                            TO NAD1-NAME-ADR-5                            
073400     INSPECT NAD1-NAME-ADR-5  REPLACING ALL 'Å' BY 'A'                    
073500     INSPECT NAD1-NAME-ADR-5  REPLACING ALL 'Ä' BY 'A'                    
073600     INSPECT NAD1-NAME-ADR-5  REPLACING ALL 'Ö' BY 'O'                    
073700     INSPECT NAD1-NAME-ADR-5  REPLACING ALL '+' BY ' '                    
073800     INSPECT NAD1-NAME-ADR-5  REPLACING ALL ':' BY ' '                    
073900     INSPECT NAD1-NAME-ADR-5  REPLACING ALL '?' BY ' '                    
074000     INSPECT NAD1-NAME-ADR-5  REPLACING ALL "'" BY ' '                    
074100     INSPECT NAD1-NAME-ADR-5  REPLACING ALL "&" BY ' '                    
074200     INSPECT NAD1-NAME-ADR-5  REPLACING ALL 'ü' BY 'U'                    
074300     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-BE BY ' '              
074400     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-61 BY ' '              
074500     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-E0 BY 'E'              
074600     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-4A BY ' '              
074700     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-79 BY 'E'              
074800     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-49 BY 'N'              
074900     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-69 BY 'N'              
075000     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-CE BY 'O'              
075100     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-EE BY 'O'              
075200     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-9B BY ' '              
075300     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-9A BY ' '              
075400     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-EC BY 'O'              
075500     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-51 BY ' '              
075600     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-FC BY 'U'              
075700     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-45 BY 'A'              
075800     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-42 BY 'A'              
075900     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-55 BY 'I'              
076000     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-44 BY 'A'              
076100     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-CD BY 'O'              
076200     INSPECT NAD1-NAME-ADR-5  REPLACING ALL WS-HEX-8D BY 'Y'              
076300                                                                          
076400     ADD +1                 TO WS-NO-OF-SEGM                              
076500     PERFORM S02-SKRIV-W46340                                             
076600     .                                                                    
076700     EJECT                                                                
076800                                                                          
076900 CI-BYGG-RCS SECTION.                                                     
077000     MOVE SPACE             TO WEDIRCS1                                   
077100                                                                          
077200     MOVE 'RCS'             TO WS-RECTYPE                                 
077300                               RCS1-IDPTYP                                
077400     MOVE 58                TO RCS1-LENGTH                                
077500     MOVE '5'               TO RCS1-QUAL                                  
077600     MOVE 'VO'              TO CIA-IDARTPRE-IN                            
077700     MOVE IN-PU-KDORDKL     TO UT-KDORDKL                                 
077800                               CIA-IDARTBET-IN                            
077900     IF IN-PU-KDORDKL NOT = 0                                             
078000       CALL W009CIA USING      CIA-W009CIA                                
078100       MOVE CIA-IDARTBET-UT TO RCS1-REQ-COND-ID                           
078200     ELSE                                                                 
078300       MOVE '0'             TO RCS1-REQ-COND-ID                           
078400     END-IF                                                               
078500     MOVE '92'              TO RCS1-RESPONSIBLE                           
078600                                                                          
078700     ADD +1                 TO WS-NO-OF-SEGM                              
078800     PERFORM S02-SKRIV-W46340                                             
078900     .                                                                    
079000     EJECT                                                                
079100                                                                          
079200 CJ-BYGG-LIN SECTION.                                                     
079300     MOVE SPACE            TO WEDILIN1                                    
079400                                                                          
079500     MOVE 'LIN'            TO WS-RECTYPE                                  
079600                              LIN1-IDPTYP                                 
079700     MOVE 46               TO LIN1-LENGTH                                 
079800     MOVE IN-PU-IDRADNR    TO LIN1-LINENO                                 
079900                              UT-IDRADNR                                  
080000     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
080100     MOVE IN-PU-IDARTNR    TO UT-IDARTNR                                  
080200                              CIA-IDARTBET-IN                             
080300     CALL W009CIA USING       CIA-W009CIA                                 
080400     MOVE CIA-IDARTBET-UT  TO LIN1-ITEMNO                                 
080500     MOVE 'IN'             TO LIN1-ITEMNO-TYPE                            
080600     MOVE ZERO             TO LIN1-CONFIG-LEVEL                           
080700                                                                          
080800     ADD +1                TO WS-NO-OF-SEGM                               
080900     PERFORM S02-SKRIV-W46340                                             
081000     .                                                                    
081100     EJECT                                                                
081200                                                                          
081300 CK-BYGG-IMD SECTION.                                                     
081400     MOVE SPACE            TO WEDIIMD1                                    
081500                                                                          
081600     MOVE 'IMD'            TO WS-RECTYPE                                  
081700                              IMD1-IDPTYP                                 
081800     MOVE 73               TO IMD1-LENGTH                                 
081900     MOVE 'F'              TO IMD1-DESCR-TYPE                             
082000     MOVE IN-PU-BEART      TO IMD1-DESCRIPTION-1                          
082100     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL 'Å' BY 'A'                  
082200     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL 'Ä' BY 'A'                  
082300     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL 'Ö' BY 'O'                  
082400     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL '+' BY ' '                  
082500     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL ':' BY ' '                  
082600     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL '?' BY ' '                  
082700     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL "'" BY ' '                  
082800     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL "&" BY ' '                  
082900     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL 'ü' BY 'U'                  
083000     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-BE BY ' '            
083100     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-61 BY ' '            
083200     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-E0 BY 'E'            
083300     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-4A BY ' '            
083400     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-79 BY 'E'            
083500     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-49 BY 'N'            
083600     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-69 BY 'N'            
083700     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-CE BY 'O'            
083800     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-EE BY 'O'            
083900     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-9B BY ' '            
084000     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-9A BY ' '            
084100     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-EC BY 'O'            
084200     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-51 BY ' '            
084300     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-FC BY 'U'            
084400     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-45 BY 'A'            
084500     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-42 BY 'A'            
084600     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-55 BY 'I'            
084700     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-44 BY 'A'            
084800     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-CD BY 'O'            
084900     INSPECT IMD1-DESCRIPTION-1 REPLACING ALL WS-HEX-8D BY 'Y'            
085000                                                                          
085100     ADD +1                TO WS-NO-OF-SEGM                               
085200     PERFORM S02-SKRIV-W46340                                             
085300     .                                                                    
085400     EJECT                                                                
085500                                                                          
085600 CL-BYGG-QTY SECTION.                                                     
085700     MOVE SPACE            TO WEDIQTY1                                    
085800                                                                          
085900     MOVE 'QTY'            TO WS-RECTYPE                                  
086000                              QTY1-IDPTYP                                 
086100     MOVE 22               TO QTY1-LENGTH                                 
086200     MOVE '21'             TO QTY1-QUALIFIER                              
086300     MOVE IN-PU-KVBEART    TO QTY1-QUANTITY                               
086400                              UT-KVANTAL                                  
086500     MOVE 'PCE'            TO QTY1-UNIT-QUALIFIER                         
086600                                                                          
086700     ADD +1                TO WS-NO-OF-SEGM                               
086800     PERFORM S02-SKRIV-W46340                                             
086900     .                                                                    
087000     EJECT                                                                
087100                                                                          
087200 CM-BYGG-ALI SECTION.                                                     
087300     MOVE SPACE            TO WEDIALI                                     
087400                                                                          
087500     MOVE 'ALI'            TO WS-RECTYPE                                  
087600                              ALI-IDPTYP                                  
087700     MOVE 6                TO ALI-LENGTH                                  
087800     IF IN-PU-KDARTURS = SPACE                                            
087900       MOVE 'SE'           TO ALI-COUNTRY-OF-ORIG                         
088000     ELSE                                                                 
088100       MOVE IN-PU-KDARTURS TO ALI-COUNTRY-OF-ORIG                         
088200     END-IF                                                               
088300                                                                          
088400     ADD +1                TO WS-NO-OF-SEGM                               
088500     PERFORM S02-SKRIV-W46340                                             
088600     .                                                                    
088700     EJECT                                                                
088800                                                                          
088900 CN-BYGG-DTM SECTION.                                                     
089000     MOVE SPACE            TO WEDIDTM1                                    
089100                                                                          
089200     MOVE 'DTM'            TO WS-RECTYPE                                  
089300                              DTM1-IDPTYP                                 
089400     MOVE 41               TO DTM1-LENGTH                                 
089500     MOVE '2'              TO DTM1-QUAL                                   
089600     MOVE WS-SPAR-DASKEPPN TO DTM1-DATE-TIME                              
089700                              UT-DASKEPPN                                 
089800     MOVE '102'            TO DTM1-FORMAT-QUAL                            
089900                                                                          
090000     ADD +1                TO WS-NO-OF-SEGM                               
090100     PERFORM S02-SKRIV-W46340                                             
090200     .                                                                    
090300     EJECT                                                                
090400                                                                          
090500 CO-BYGG-FTX-AAJ SECTION.                                                 
090600     MOVE SPACE            TO WEDIFTX1                                    
090700                                                                          
090800     MOVE 'FTX'            TO WS-RECTYPE                                  
090900                              FTX1-IDPTYP                                 
091000     MOVE 373              TO FTX1-LENGTH                                 
091100     MOVE 'AAJ'            TO FTX1-SUBJECT-QUAL                           
091200     MOVE IN-PU-BERADREF   TO FTX1-TEXT-ID                                
091300                              UT-BERADREF                                 
091400     INSPECT FTX1-TEXT-ID REPLACING ALL 'Å' BY 'A'                        
091500     INSPECT FTX1-TEXT-ID REPLACING ALL 'Ä' BY 'A'                        
091600     INSPECT FTX1-TEXT-ID REPLACING ALL 'Ö' BY 'O'                        
091700     INSPECT FTX1-TEXT-ID REPLACING ALL '+' BY ' '                        
091800     INSPECT FTX1-TEXT-ID REPLACING ALL ':' BY ' '                        
091900     INSPECT FTX1-TEXT-ID REPLACING ALL '?' BY ' '                        
092000     INSPECT FTX1-TEXT-ID REPLACING ALL "'" BY ' '                        
092100     INSPECT FTX1-TEXT-ID REPLACING ALL "&" BY ' '                        
092200     INSPECT FTX1-TEXT-ID REPLACING ALL 'ü' BY 'U'                        
092300     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-BE BY ' '                  
092400     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-61 BY ' '                  
092500     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-E0 BY 'E'                  
092600     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-4A BY ' '                  
092700     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-79 BY 'E'                  
092800     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-49 BY 'N'                  
092900     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-69 BY 'N'                  
093000     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-CE BY 'O'                  
093100     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-EE BY 'O'                  
093200     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-9B BY ' '                  
093300     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-9A BY ' '                  
093400     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-EC BY 'O'                  
093500     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-51 BY ' '                  
093600     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-FC BY 'U'                  
093700     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-45 BY 'A'                  
093800     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-42 BY 'A'                  
093900     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-55 BY 'I'                  
094000     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-44 BY 'A'                  
094100     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-CD BY 'O'                  
094200     INSPECT FTX1-TEXT-ID REPLACING ALL WS-HEX-8D BY 'Y'                  
094300                                                                          
094400     MOVE '92'             TO FTX1-RESPONSIBLE                            
094500     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
094600     MOVE IN-PU-ADLAGOMR   TO CIA-IDARTBET-IN                             
094700     CALL W009CIA USING       CIA-W009CIA                                 
094800     MOVE CIA-IDARTBET-UT  TO FTX1-FREE-TEXT-1                            
094900     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
095000     MOVE IN-PU-ADGANG     TO CIA-IDARTBET-IN                             
095100     CALL W009CIA USING       CIA-W009CIA                                 
095200     MOVE CIA-IDARTBET-UT  TO FTX1-FREE-TEXT-2                            
095300     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
095400     MOVE IN-PU-ADPLATS    TO CIA-IDARTBET-IN                             
095500     CALL W009CIA USING       CIA-W009CIA                                 
095600     MOVE CIA-IDARTBET-UT  TO FTX1-FREE-TEXT-3                            
095700                                                                          
095710     IF (FTX1-FREE-TEXT-2 NOT = SPACE  OR                                 
095720         FTX1-FREE-TEXT-3 NOT = SPACE  OR                                 
095730         FTX1-FREE-TEXT-4 NOT = SPACE  OR                                 
095740         FTX1-FREE-TEXT-5 NOT = SPACE) AND                                
095750         FTX1-FREE-TEXT-1 = SPACE                                         
095760       MOVE '00' TO FTX1-FREE-TEXT-1                                      
095770     END-IF                                                               
095780                                                                          
095800     ADD +1                TO WS-NO-OF-SEGM                               
095900     PERFORM S02-SKRIV-W46340                                             
096000     .                                                                    
096100     EJECT                                                                
096200                                                                          
096300 CP-BYGG-UNS SECTION.                                                     
096400     MOVE SPACE            TO WEDIUNS1                                    
096500                                                                          
096600     MOVE 'UNS'            TO WS-RECTYPE                                  
096700                              UNS1-IDPTYP                                 
096800     MOVE 1                TO UNS1-LENGTH                                 
096900     MOVE 'S'              TO UNS1-SECTION-ID                             
097000     ADD +1                TO WS-NO-OF-SEGM                               
097100                                                                          
097200     PERFORM S02-SKRIV-W46340                                             
097300     .                                                                    
097400     EJECT                                                                
097500                                                                          
097600 CQ-BYGG-UNT SECTION.                                                     
097700     MOVE SPACE               TO WEDIUNT                                  
097800                                                                          
097900     MOVE 'UNT'               TO WS-RECTYPE                               
098000                                 UNT-IDPTYP                               
098100     MOVE 20                  TO UNT-LENGTH                               
098200     ADD +1                   TO WS-NO-OF-SEGM                            
098300     MOVE WS-NO-OF-SEGM       TO UNT-NUMBER-OF-SEGM                       
098400     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
098500     MOVE WS-NO-OF-PROD-UNITS TO CIA-IDARTBET-IN                          
098600     CALL W009CIA USING          CIA-W009CIA                              
098700     MOVE CIA-IDARTBET-UT     TO UNT-REFNO                                
098800                                                                          
098900     MOVE ZERO                TO WS-NO-OF-SEGM                            
099000*    PERFORM S02-SKRIV-W46340                                             
099100     .                                                                    
099200     EJECT                                                                
099300                                                                          
099400 CR-BYGG-UNB SECTION.                                                     
099500     MOVE SPACE                   TO WEDIUNB                              
099600                                                                          
099700     MOVE 'UNB'                   TO WS-RECTYPE                           
099800                                     UNB-IDPTYP                           
099900     MOVE 125                     TO UNB-LENGTH                           
100000     MOVE 'UNOA'                  TO UNB-SYNTAX-ID                        
100100     MOVE 1                       TO UNB-SYNTAX-VERS-NO                   
100200                                                                          
100300     MOVE IN-PU-IDLEVNR           TO WS-IDLEVNR-KOLL                      
100400     INSPECT WS-IDLEVNR-KOLL      REPLACING ALL SPACE BY ZERO             
100500     IF WS-IDLEVNR-KOLL NUMERIC                                           
100600       MOVE '02796'               TO UNB-SENDER-ID                        
100700**FIX FÖR ATT HÖGERSTÄLLT NOLLUTFYLLT                                     
100800       UNSTRING IN-PU-IDLEVNR DELIMITED BY SPACE                          
100900                                  INTO WS-UNB-RECIPIENT-ID                
101000       MOVE WS-UNB-RECIPIENT-ID-X TO UNB-RECIPIENT-ID                     
101100     ELSE                                                                 
101200       MOVE 'BP2T7'               TO UNB-SENDER-ID                        
101300       MOVE IN-PU-IDLEVNR         TO UNB-RECIPIENT-ID                     
101400     END-IF                                                               
101500                                                                          
101600     MOVE 'VO'                    TO CIA-IDARTPRE-IN                      
101700     MOVE WS-DAGENS-DATUM-6       TO UNB-DATE                             
101800     MOVE WS-DAGENS-KLOCKA-1-4    TO UNB-TIME                             
101900     MOVE WS-DAGENS-DATUM-KLOCKA-14                                       
102000                                  TO UNB-INTERCHANGE-REF                  
102100                                                                          
102200     PERFORM S02-SKRIV-W46340                                             
102300     .                                                                    
102400     EJECT                                                                
102500                                                                          
102600 CS-BYGG-LOGG SECTION.                                                    
102700     MOVE 'ORD'                   TO UT-IDPTYP                            
102800     MOVE WS-DAGENS-DATUM-8       TO UT-DAREGDAT                          
102900     MOVE WS-DAGENS-KLOCKA-1-6    TO UT-TIREGTID                          
103000     MOVE SPACE                   TO UT-IDSUPREF                          
103100     MOVE ZERO                    TO UT-IDKOLLI                           
103200                                     UT-DABEKDAT                          
103300                                     UT-DAFAKT                            
103400                                     UT-DALEVDAT                          
103500                                     UT-DAPACKN                           
103600                                     UT-DASNDDAT                          
103700                                     UT-DASUPREF                          
103800                                     UT-TIBEKR                            
103900                                     UT-TIPACTID                          
104000                                     UT-TISNDTID                          
104100                                     UT-TISUPTID                          
104200                                     UT-KDORDBEK                          
104300                                                                          
104400     PERFORM S03-SKRIV-W46341                                             
104500     .                                                                    
104600     EJECT                                                                
104700                                                                          
104710 CT-BYGG-DTM-TIREPDAT SECTION.                                            
104711                                                                          
104713     MOVE 'DTM'            TO WS-RECTYPE                                  
104714                              DTM1-IDPTYP                                 
104715     MOVE 041              TO DTM1-LENGTH                                 
104716     MOVE '199'            TO DTM1-QUAL                                   
104717     MOVE IN-PU-TIREPDAT   TO WS-TIREPDAT                                 
104718     MOVE WS-TIREPDAT      TO DTM1-DATE-TIME                              
104720     MOVE '101'            TO DTM1-FORMAT-QUAL                            
104721     ADD +1                TO WS-NO-OF-SEGM                               
104722     PERFORM S02-SKRIV-W46340                                             
104723     .                                                                    
104724     EJECT                                                                
104725                                                                          
104726 CU-BYGG-GIR-IDBILREG SECTION.                                            
104730                                                                          
104731     MOVE SPACE            TO WEDIGIR                                     
104732     MOVE 'GIR'            TO WS-RECTYPE                                  
104733                              GIR-IDPTYP                                  
104734     MOVE 193              TO GIR-LENGTH                                  
104735     MOVE '4  '            TO GIR-ID-QUAL                                 
104736     MOVE IN-PU-IDBILREG   TO GIR-IDNO-1                                  
104737     MOVE 'BL '            to GIR-IDNO-QUAL-1                             
104738     ADD +1                TO WS-NO-OF-SEGM                               
104739     PERFORM S02-SKRIV-W46340                                             
104740     .                                                                    
104741     EJECT                                                                
104742                                                                          
104743 CV-BYGG-RFF-IDKUNDRF-WIP SECTION.                                        
104744                                                                          
104746     MOVE 'RFF'              TO WS-RECTYPE                                
104747                                RFF1-IDPTYP                               
104748     MOVE 038                TO RFF1-LENGTH                               
104749     MOVE 'AIJ'              TO RFF1-QUAL                                 
104750     MOVE IN-PU-IDKUNDRF-WIP TO RFF1-REFNO                                
104751     ADD +1                  TO WS-NO-OF-SEGM                             
104753     PERFORM S02-SKRIV-W46340                                             
104754     .                                                                    
104755     EJECT                                                                
104756                                                                          
104757 CX-BYGG-LOC-BEMEKAN SECTION.                                             
104760                                                                          
104761     MOVE 'LOC'              TO WS-RECTYPE                                
104762                                LOC-IDPTYP                                
104763     MOVE 256                TO LOC-LENGTH                                
104764     MOVE '83 '              TO LOC-3227-PLACE-LOC-QUAL                   
104765     MOVE SPACE              TO LOC-C517-LOC-IDENTIFICATION               
104766     MOVE IN-PU-BEMEKAN      TO LOC-3225-LOC-ID                           
104767     ADD +1                  TO WS-NO-OF-SEGM                             
104770     PERFORM S02-SKRIV-W46340                                             
104780     .                                                                    
104790     EJECT                                                                
104791                                                                          
104800 D-BYGG-EDI-TRAILER SECTION.                                              
104900     MOVE SPACE                   TO WEDIT003                             
105000                                                                          
105100     MOVE '003'                   TO WS-RECTYPE                           
105200                                     T003-IDPTYP                          
105300     MOVE 73                      TO T003-LENGTH                          
105400                                                                          
105500     PERFORM S02-SKRIV-W46340                                             
105600     .                                                                    
105700     EJECT                                                                
105800                                                                          
105900 Z-FINIT SECTION.                                                         
106000     CLOSE W46312                                                         
106100           W46340                                                         
106200           W46341                                                         
106300                                                                          
106400     MOVE 'S' TO POSTSUM-OPKOD                                            
106500     CALL POSTSUM USING POSTSUM-PARM                                      
106600     .                                                                    
106700     EJECT                                                                
106800                                                                          
106900 S01-LAES-W46312  SECTION.                                                
107000     READ W46312 INTO IN-AREA                                             
107100     AT END                                                               
107200        MOVE HIGH-VALUE TO IN-AREA                                        
107300        SET END-OF-W46312 TO TRUE                                         
107400                                                                          
107500     NOT AT END                                                           
107600        ADD +1          TO WS-COUNT                                       
107700        MOVE 'W46312'   TO POSTSUM-FDNAMN                                 
107800        MOVE 'W46320D1' TO POSTSUM-DDNAMN2                                
107900        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
108000        CALL POSTSUM USING POSTSUM-PARM                                   
108100     END-READ                                                             
108200     .                                                                    
108300     EJECT                                                                
108400                                                                          
108500 S02-SKRIV-W46340 SECTION.                                                
108600     EVALUATE WS-RECTYPE                                                  
108700        WHEN '001'                                                        
108800              WRITE EDI-POST FROM WEDIH001                                
108900        WHEN 'UNB'                                                        
109000              WRITE EDI-POST FROM WEDIUNB                                 
109100        WHEN 'UNH'                                                        
109200              WRITE EDI-POST FROM WEDIUNH                                 
109300        WHEN 'BGM'                                                        
109400              WRITE EDI-POST FROM WEDIBGM1                                
109500        WHEN 'DTM'                                                        
109600              WRITE EDI-POST FROM WEDIDTM1                                
109610        WHEN 'GIR'                                                        
109620              WRITE EDI-POST FROM WEDIGIR                                 
109700        WHEN 'FTX'                                                        
109800              WRITE EDI-POST FROM WEDIFTX1                                
109900        WHEN 'RFF'                                                        
110000              WRITE EDI-POST FROM WEDIRFF1                                
110010        WHEN 'LOC'                                                        
110020              WRITE EDI-POST FROM WEDILOC                                 
110100        WHEN 'NAD'                                                        
110200              WRITE EDI-POST FROM WEDINAD1                                
110300        WHEN 'RCS'                                                        
110400              WRITE EDI-POST FROM WEDIRCS1                                
110500        WHEN 'LIN'                                                        
110600              WRITE EDI-POST FROM WEDILIN1                                
110700        WHEN 'IMD'                                                        
110800              WRITE EDI-POST FROM WEDIIMD1                                
110900        WHEN 'QTY'                                                        
111000              WRITE EDI-POST FROM WEDIQTY1                                
111100        WHEN 'ALI'                                                        
111200              WRITE EDI-POST FROM WEDIALI                                 
111300        WHEN 'UNS'                                                        
111400              WRITE EDI-POST FROM WEDIUNS1                                
111500        WHEN 'UNT'                                                        
111600              WRITE EDI-POST FROM WEDIUNT                                 
111700        WHEN '003'                                                        
111800              WRITE EDI-POST FROM WEDIT003                                
111900     END-EVALUATE                                                         
112000                                                                          
112100     MOVE 'EDI'      TO POSTSUM-TRANSTYP                                  
112200     MOVE 'W46340'   TO POSTSUM-FDNAMN                                    
112300     MOVE 'W46320D2' TO POSTSUM-DDNAMN2                                   
112400     CALL POSTSUM USING POSTSUM-PARM                                      
112500     .                                                                    
112600     EJECT                                                                
112700 S03-SKRIV-W46341 SECTION.                                                
112800     WRITE UT-LOGG FROM UT-W46341                                         
112900                                                                          
113000     MOVE 'LOGG'     TO POSTSUM-TRANSTYP                                  
113100     MOVE 'W46341'   TO POSTSUM-FDNAMN                                    
113200     MOVE 'W46320D3' TO POSTSUM-DDNAMN2                                   
113300     CALL POSTSUM USING POSTSUM-PARM                                      
113400     .                                                                    
113500 S04-DANSKT-AE  SECTION.                                                  
113600     MOVE 1           TO IX                                               
113700     MOVE 1           TO IA                                               
113800     MOVE NAD1-NAME-ADR-4   TO WTEXT                                      
113900     MOVE SPACE       TO WTEXT3                                           
114000     PERFORM UNTIL IX > 35                                                
114100       IF WTEXT(IX:1) = 'Ä'                                               
114200         MOVE 'AE'    TO WTEXT3(IA:2)                                     
114300         ADD 2        TO IA                                               
114400       ELSE                                                               
114500         MOVE WTEXT(IX:1)  TO WTEXT3(IA:1)                                
114600         ADD 1        TO IA                                               
114700       END-IF                                                             
114800       ADD 1          TO IX                                               
114900       IF IA > 35                                                         
115000         MOVE IA      TO IX                                               
115100       END-IF                                                             
115200     END-PERFORM                                                          
115300     MOVE WTEXT2     TO NAD1-NAME-ADR-4                                   
115400     .                                                                    
115500     EJECT                                                                
