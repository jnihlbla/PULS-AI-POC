000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W2615B00.                                                
000301 AUTHOR.         UMESH JAIN.                                              
000401 DATE-WRITTEN.   JULY 2011.                                               
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*                                                                         
000801*    FUNCTION:                                                            
000901*        LÄSER FIL W2615B ARTIKLAR MED TISKROT-AUTO > DAGENS-DAT          
001001*        OCH SKAPAR LISTA.                                                
001101*                                                                         
001201*    ABENDCODES:                                                          
001301*        U1000 - D&P ERROR                                                
001401*                                                                         
001501                                                                          
001601     SKIP3                                                                
001701 ENVIRONMENT DIVISION.                                                    
001801     SKIP2                                                                
001901 INPUT-OUTPUT SECTION.                                                    
002001                                                                          
002101 FILE-CONTROL.                                                            
002201     SKIP2                                                                
002301*          --- INFIL                                                      
002401     SELECT W2615B                     ASSIGN TO W2615BD1.                
002501     SKIP2                                                                
002601*          --- SORTERINGSFIL                                              
002701     SELECT SORTFIL                    ASSIGN TO W2615BDS.                
002801     SKIP2                                                                
002901 DATA DIVISION.                                                           
003001     SKIP3                                                                
003101 FILE SECTION.                                                            
003201     SKIP3                                                                
003301 FD  W2615B                                                               
003401     RECORDING       F                                                    
003501     BLOCK CONTAINS  0.                                                   
003601                                                                          
003701*01  -COPY W2615B    -L.                                                  
003801     SKIP3                                                                
003901 SD  SORTFIL.                                                             
004001                                                                          
004101*01  POST -COPY W2615B    -PRE SORT-                                      
004202     03  SORT-IDDC-grp      PIC X(2).                                     
004301     SKIP3                                                                
004401 WORKING-STORAGE SECTION.                                                 
004501                                                                          
004601 77  IDPGM                       PIC X(8)    VALUE 'W2615B00'.            
004701 77  JA                          PIC X       VALUE 'J'.                   
004801 77  NEJ                         PIC X       VALUE 'N'.                   
004902 77  SPAR-IDDC-grp               PIC X(2)    VALUE SPACE.                 
005001 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
005101 77  KDRC-DISPLAY                PIC Z(5).                                
005201                                                                          
005301 77  W2615B-EOF-SW               PIC X       VALUE 'N'.                   
005401     88  END-OF-W2615B                       VALUE 'Y'.                   
005501                                                                          
005601 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005701     88  END-OF-SORTFIL                      VALUE 'Y'.                   
005801     EJECT                                                                
005901*                                                                         
006001 01  HDR-AREA.                                                            
006101*    03  -COPY WZ01REQU                                                   
006201*    03  -COPY WZ04HDR                                                    
006301     EJECT                                                                
006401 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
006501 01  SEND-AREA.                                                           
006601*    03  -COPY WZ01SEND                                                   
006701     EJECT                                                                
006801 01  SEND-RAD-STYRTECKEN.                                                 
006901     03  STYRTECKEN-RAD          PIC X.                                   
007001     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
007101*    --- CONTROL CHARACTERS                                               
007201 01  WS-SKIP1                    PIC X       VALUE ' '.                   
007301 01  WS-SKIP2                    PIC X       VALUE '0'.                   
007401 01  WS-SKIP3                    PIC X       VALUE '-'.                   
007501 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
007601     EJECT                                                                
007701                                                                          
007801 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
007901                                                                          
008001     EJECT                                                                
008101*    --- LISTLAYOUT                                                       
008201 01  LISTA.                                                               
008301     03  RUBRIK-1.                                                        
008401         05  FILLER     PIC X       VALUE SPACE.                          
008501         05  FILLER     PIC X(15)   VALUE 'VCCS W2615B-001'.              
008601         05  FILLER     PIC X(3)    VALUE SPACE.                          
008701         05  FILLER     PIC X(23) VALUE 'Artiklar med stoppdatum'.        
008801         05  FILLER     PIC X(23) VALUE ' för skrot i framtiden '.        
008901         05  FILLER     PIC X(3)    VALUE SPACE.                          
009001         05  FILLER     PIC X(6)    VALUE 'DATUM'.                        
009101         05  FILLER     PIC X(1)    VALUE SPACE.                          
009201         05  RUB1-DAT   PIC X(6)    VALUE SPACE.                          
009301                                                                          
009401     03  RUBRIK-2.                                                        
009501         05  FILLER           PIC X(4)    VALUE SPACE.                    
009601         05  RUB2-IDDC        PIC X(08)   VALUE 'DC    '.                 
009701         05  RUB2-IDARTNR     PIC X(13)   VALUE 'Artikel      '.          
009801         05  RUB2-BEART       PIC X(30)   VALUE 'Benämning     '.         
009901         05  RUB2-TISKROT     PIC X(12)   VALUE 'Ej Aut tom  '.           
010201         05  FILLER           PIC X(3)    VALUE SPACE.                    
010301                                                                          
010401     03  RAD.                                                             
010501         05  FILLER           PIC X(4)    VALUE SPACE.                    
010601         05  RAD-IDDC         PIC X(2).                                   
010602         05  FILLER           PIC X(4)    VALUE SPACE.                    
010801         05  RAD-IDARTNR      PIC ZZZZZZZZ9.                              
010901         05  FILLER           PIC X(6)    VALUE SPACE.                    
011001         05  RAD-BEART        PIC X(25).                                  
011101         05  FILLER           PIC X(5)   VALUE SPACE.                     
011201         05  RAD-TISKROT      PIC 999999.                                 
011301         05  FILLER           PIC X(6)    VALUE SPACE.                    
011701                                                                          
011801                                                                          
011901 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012001 01  FILLER REDEFINES DAGENS-DATUM.                                       
012101     03  DAGENS-AA               PIC 9(2).                                
012201     03  DAGENS-MM               PIC 9(2).                                
012301     03  DAGENS-DD               PIC 9(2).                                
012401     EJECT                                                                
012501 01  GENERAL-SUBPROGRAMS.                                                 
012601*                                                                         
012701     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012801     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012901     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
013001     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
013101     SKIP2                                                                
013201*    --- PARAMETERS TO ABEND                                              
013301                                                                          
013401 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013501 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013601 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013701     SKIP2                                                                
013801 01  ERRTEXT.                                                             
013901     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
014001     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
014101     EJECT                                                                
014201*    --- PARAMETRAR TILL POSTSUM                                          
014301*                                                                         
014401*01  -COPY W0005   -PRE  POSTSUM-                                         
014501     EJECT                                                                
014601 01  IN-AREA-START               PIC X(24)   VALUE                        
014701                                 'IN-AREA-START  '.                       
014801     SKIP2                                                                
014901*01  AREA -COPY W2615B      -PRE IN-                                      
015001     SKIP2                                                                
015101                                                                          
015201     EJECT                                                                
015301 01  SORTWS-AREA-START               PIC X(24)   VALUE                    
015401                                 'SORTWS-AREA-START  '.                   
015501     SKIP2                                                                
015601*01  AREA -COPY W2615B      -PRE SORTWS-                                  
015702     03  SORTWS-IDDC-grp         PIC X(2).                                
015801                                                                          
015901 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
016001     EJECT                                                                
016101                                                                          
016201 LINKAGE SECTION.                                                         
016301                                                                          
016401*01  -COPY W0009            -PRE MSG-                                     
016501                                                                          
016601*01  -COPY W0009            -PRE DISTRDOC-                                
016701     EJECT                                                                
016801 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
016901 MAIN SECTION.                                                            
017001     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
017101                                                                          
017201     PERFORM A-INIT                                                       
017301                                                                          
017402     SORT SORTFIL ASCENDING  KEY SORT-IDDC-grp                            
017501                  DESCENDING     SORT-TISKROT-AUTO                        
017701                  ASCENDING      SORT-IDARTNR                             
017801                  INPUT PROCEDURE B-SORT-INPUT                            
017901                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
018001                                                                          
018101     IF SORT-RETURN NOT = 0                                               
018201       MOVE SORT-RETURN TO SORT-RETURN-X                                  
018301       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
018401       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
018501       DISPLAY ERRTEXT                                                    
018601       MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                              
018701       PERFORM S99-ABEND                                                  
018801     ELSE                                                                 
018901       PERFORM Z-FINIT                                                    
019001                                                                          
019101       MOVE ZERO TO RETURN-CODE                                           
019201       GOBACK                                                             
019301     END-IF                                                               
019401                                                                          
019501     .                                                                    
019601     EJECT                                                                
019701 A-INIT SECTION.                                                          
019801                                                                          
019901     OPEN INPUT  W2615B                                                   
020001     ACCEPT DAGENS-DATUM FROM DATE                                        
020101     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020201     .                                                                    
020301     EJECT                                                                
020401 B-SORT-INPUT  SECTION.                                                   
020501                                                                          
020601     PERFORM S01-LAS-W2615B                                               
020701     PERFORM UNTIL END-OF-W2615B                                          
020801       MOVE IN-W2615B        TO SORTWS-W2615B                             
020902       MOVE IN-IDDC          TO SORTWS-IDDC-grp                           
021101       PERFORM S31-SORT-RELEASE                                           
021201       PERFORM S01-LAS-W2615B                                             
021301     END-PERFORM                                                          
021401     .                                                                    
021501     EJECT                                                                
021601 C-SORT-OUTPUT SECTION.                                                   
021701                                                                          
021801     PERFORM S32-SORT-RETURN                                              
021901     PERFORM S90-SEND-OPEN                                                
022001     PERFORM S90-PUT-DAP-START                                            
022101     PERFORM CC-PRINT-HEAD                                                
022201                                                                          
022300     PERFORM UNTIL END-OF-SORTFIL                                         
022402       MOVE SORTWS-IDDC-grp   TO SPAR-IDDC-grp                            
022501       PERFORM UNTIL END-OF-SORTFIL OR                                    
022602         (SPAR-IDDC-grp NOT = SORTWS-IDDC-grp)                            
022701           PERFORM CD-RAD-DATA                                            
022801         PERFORM S32-SORT-RETURN                                          
022901       END-PERFORM                                                        
023001     END-PERFORM                                                          
023101                                                                          
023201     PERFORM S90-SEND-CLOSE                                               
023301     .                                                                    
023401     EJECT                                                                
023501 CC-PRINT-HEAD SECTION.                                                   
023601                                                                          
023701     MOVE SPACE                      TO SEND-RAD-STYRTECKEN               
023801     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
023901     MOVE SPACE                      TO SEND-RAD                          
024001     PERFORM S90-PUT-DOC-LINE                                             
024101     MOVE DAGENS-DATUM               TO RUB1-DAT                          
024201     MOVE RUBRIK-1                   TO SEND-RAD                          
025001     PERFORM S90-PUT-DOC-LINE                                             
025101                                                                          
025201     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
025301     MOVE SPACE                      TO SEND-RAD                          
025401     PERFORM S90-PUT-DOC-LINE                                             
025501     MOVE RUBRIK-2                   TO SEND-RAD                          
025601     PERFORM S90-PUT-DOC-LINE                                             
025701                                                                          
025801     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
025901     MOVE SPACE                      TO SEND-RAD                          
026001     PERFORM S90-PUT-DOC-LINE                                             
026101     .                                                                    
026201     EJECT                                                                
026301 CD-RAD-DATA SECTION.                                                     
026401                                                                          
026501     MOVE SORTWS-IDDC         TO RAD-IDDC                                 
026601     MOVE SORTWS-IDARTNR      TO RAD-IDARTNR                              
026701     MOVE SORTWS-BEART        TO RAD-BEART                                
026901     MOVE SORTWS-TISKROT-AUTO TO RAD-TISKROT                              
027001                                                                          
027101     MOVE RAD                 TO SEND-RAD                                 
027201     PERFORM S90-PUT-DOC-LINE                                             
027301     .                                                                    
027401     EJECT                                                                
027501 Z-FINIT SECTION.                                                         
027601                                                                          
027701     CLOSE W2615B                                                         
027801     MOVE 'S' TO POSTSUM-OPKOD                                            
027901     CALL POSTSUM USING POSTSUM-PARM                                      
028001     .                                                                    
028101     EJECT                                                                
028201 S01-LAS-W2615B SECTION.                                                  
028301     READ W2615B INTO IN-AREA                                             
028401     AT END                                                               
028501        MOVE HIGH-VALUE TO IN-AREA                                        
028601        SET END-OF-W2615B TO TRUE                                         
028701     NOT AT END                                                           
028801        MOVE 'W2615B'   TO POSTSUM-FDNAMN                                 
028901        MOVE 'W2615BD1' TO POSTSUM-DDNAMN2                                
029001        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029101        CALL POSTSUM USING POSTSUM-PARM                                   
029201     END-READ                                                             
029301     .                                                                    
029401     EJECT                                                                
029501 S31-SORT-RELEASE  SECTION.                                               
029601                                                                          
029701     RELEASE SORT-POST FROM SORTWS-AREA                                   
029801     .                                                                    
029901     EJECT                                                                
030001 S32-SORT-RETURN  SECTION.                                                
030101                                                                          
030201     RETURN SORTFIL INTO SORTWS-AREA                                      
030301     AT END                                                               
030401         SET END-OF-SORTFIL TO TRUE                                       
030501     .                                                                    
030601     EJECT                                                                
030701 S90-SEND-OPEN SECTION.                                                   
030801                                                                          
030901     MOVE 'OPEN'                        TO SEND-KDFUNC                    
031001     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
031101     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031201                         SEND-OPEN-AREA                                   
031301     IF SEND-KDRC > 0                                                     
031401       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
031501       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
031601       DELIMITED BY SIZE INTO ERRTEXT                                     
031701       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031801     END-IF                                                               
031901     .                                                                    
032001     EJECT                                                                
032101 S90-PUT-DAP-START SECTION.                                               
032201                                                                          
032301     MOVE 1                       TO REQU-IDMSGVER                        
032401     MOVE 'R'                     TO REQU-KDPGMACT                        
032501     MOVE IDPGM                   TO REQU-IDUSER                          
032601     MOVE 'W2615B-001'            TO HDR-IDOUTTYPE                        
032701     MOVE SPACE                   TO HDR-IDOUTREC                         
032801                                     HDR-IDLIST                           
032901     MOVE 'W2615B'                TO HDR-IDOUTREC                         
033001     MOVE DAGENS-DATUM            TO HDR-IDLIST                           
033101     MOVE 'PUT'                   TO SEND-KDFUNC                          
033201     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
033301     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033401                         SEND-KVDLEN                                      
033501                         HDR-AREA                                         
033601     IF SEND-KDRC > ZERO                                                  
033701       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
033801       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033901       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
034001       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034101     END-IF                                                               
034201     .                                                                    
034301     EJECT                                                                
034401 S90-PUT-DOC-LINE SECTION.                                                
034501                                                                          
034601     MOVE 'PUT'                           TO SEND-KDFUNC                  
034701*                     -- UTAN STYRTECKEN:                                 
034801     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
034901     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035001                         SEND-KVDLEN                                      
035101*                     -- UTAN STYRTECKEN:                                 
035201                         SEND-RAD                                         
035301     IF SEND-KDRC > ZERO                                                  
035401       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035501       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035601       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
035701       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035801     END-IF                                                               
035901     .                                                                    
036001     EJECT                                                                
036101 S90-SEND-CLOSE SECTION.                                                  
036201                                                                          
036301     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
036401     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036501                                                                          
036601     IF SEND-KDRC > 0                                                     
036701       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
036801       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
036901       DELIMITED BY SIZE INTO ERRTEXT                                     
037001       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037101     END-IF                                                               
037201     .                                                                    
037301     EJECT                                                                
037401 S99-ABEND SECTION.                                                       
037501                                                                          
037601     SKIP2                                                                
037701     MOVE 'S' TO POSTSUM-OPKOD                                            
037801     CALL POSTSUM USING POSTSUM-PARM                                      
037901     CALL ABEND USING RKOD-ABEND                                          
038001     .                                                                    
