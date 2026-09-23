000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W418ANSV.                                                
000500 AUTHOR.         LARS CALAIS.                                             
000600 DATE-WRITTEN.   95/06/29.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNCTION:                                                            
001000*        IMS SUB-PGM TO FIND RESPONSIBLE FOR DISCREPANCY-HANDLING.        
001100*                                                                         
001200*        THE PROGRAM READS     WL4113  ADM                                
001300*                              WL4115  RET                                
001400*                              WL4117  REM                                
001500*                                                                         
001600*    E-TRACKER 8687963 DATE  2010-03-18 REFERRALS PICKING AREA            
001700*                                                                         
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300     SKIP3                                                                
003400 77  IDPGM                       PIC X(8)    VALUE 'W418ANSV'.            
003500 77  YES                         PIC X       VALUE 'Y'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700 77  WRONG                       PIC X       VALUE 'F'.                   
003800 77  MISSING                     PIC X       VALUE 'S'.                   
003900 77  CORRECT                     PIC X       VALUE ' '.                   
004000                                                                          
005000 77 WS-IDDISTRF-PREV             PIC 9(5)    VALUE ZERO.                  
006000 77 WS-IDKUNDNRF-PREV            PIC 9(7)    VALUE ZERO.                  
007000 77 WS-IDDISTRT-PREV             PIC 9(5)    VALUE ZERO.                  
008000 77 WS-IDKUNDNRT-PREV            PIC 9(7)    VALUE ZERO.                  
009000                                                                          
010000     EJECT                                                                
020000 01  GENERAL-SUBPROGRAM.                                                  
030000*                                                                         
040000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
040100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
040200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
040300                                                                          
040400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
040500                                                                          
040600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
040700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
040800                                                                          
040900 01  ERRTEXT.                                                             
041000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
041100     03  ERRTEXT-STR             PIC X(40)   VALUE SPACE.                 
041200     EJECT                                                                
041300*    --- AREAS FOR IMS-SECTIONS                                           
041400*                                                                         
041500     EJECT                                                                
041600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
041700                                                                          
041800 01  KEYS-TILL-DLI.                                                       
041900     03  W-4113-X.                                                        
042000         05  FILLER                  PIC X(4)     VALUE '4113'.           
042100         05  W-IDFTG-4113            PIC 9(2)     VALUE ZERO.             
042200         05  FILLER                  PIC X(24)    VALUE LOW-VALUE.        
042300                                                                          
042400     03  W-4114-FOM.                                                      
042500         05  W-KDANMORS-4114-FOM     PIC X(2).                            
042600         05  FILLER                  PIC X(2)    VALUE LOW-VALUE.         
042700                                                                          
042800     03  W-4114-TOM.                                                      
042900         05  W-KDANMORS-4114-TOM     PIC X(2).                            
043000         05  FILLER                  PIC X(2)   VALUE HIGH-VALUE.         
043100                                                                          
043200     03  W-4115-X.                                                        
043300         05  FILLER                  PIC X(4)     VALUE '4115'.           
043400         05  W-IDFTG-4115            PIC 9(2)     VALUE ZERO.             
043500         05  FILLER                  PIC X(24)    VALUE LOW-VALUE.        
043600                                                                          
043700     03  W-4116-FOM.                                                      
043800         05  W-KDANMORS-4116-FOM     PIC X(2).                            
043900         05  FILLER                  PIC X(2)    VALUE LOW-VALUE.         
044000                                                                          
044100     03  W-4116-TOM.                                                      
044200         05  W-KDANMORS-4116-TOM     PIC X(2).                            
044300         05  FILLER                  PIC X(2)   VALUE HIGH-VALUE.         
044400                                                                          
044500     03  W-4117-X.                                                        
044600         05  FILLER                  PIC X(4)     VALUE '4117'.           
044700         05  W-IDFTG-4117            PIC 9(2)     VALUE ZERO.             
044800         05  FILLER                  PIC X(24)    VALUE LOW-VALUE.        
044900                                                                          
045000     03  W-4118-FOM.                                                      
045100         05  W-KDANMORS-4118-FOM     PIC X(2).                            
045200         05  W-IDDC-4118-FOM         PIC X(2).                            
045300         05  FILLER                  PIC X(2)    VALUE LOW-VALUE.         
045400                                                                          
045500     03  W-4118-TOM.                                                      
045600         05  W-KDANMORS-4118-TOM     PIC X(2).                            
045700         05  W-IDDC-4118-TOM         PIC X(2).                            
045800         05  FILLER                  PIC X(2)   VALUE HIGH-VALUE.         
045900                                                                          
046000     03  W-IDDISTRF-X.                                                    
046100         05  W-IDDISTRF              PIC S9(5)   COMP-3.                  
046200     03  W-IDDISTRT-X.                                                    
046300         05  W-IDDISTRT              PIC S9(5)   COMP-3.                  
046400     03  W-IDKUNDNF-X.                                                    
046500         05  W-IDKUNDNF              PIC S9(7)   COMP-3.                  
046600     03  W-IDKUNDNT-X.                                                    
046700         05  W-IDKUNDNT              PIC S9(7)   COMP-3.                  
046800     03  W-KDORDKL-X.                                                     
046900         05  W-KDORDKL               PIC S9      COMP-3.                  
047000     03  W-ADLAGOMR-X.                                                    
047100         05  W-ADLAGOMR              PIC S9(3)   COMP-3.                  
047200                                                                          
047300     03  W-IDDC-X.                                                        
047400         05  W-IDDC                  PIC  X(2)   VALUE SPACE.             
047500                                                                          
047600*    --- STATUS-KOD FRÅN IMS                                              
047700 01  STATUS-WS                   PIC XX.                                  
047800     88  SEGMENT-FOUND                       VALUE '  '.                  
047900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
048000                                                                          
048100 01  GOOD-STATUSCODES.                                                    
048200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
048300                                                                          
048400 01  SSA1                        PIC X(160).                              
048500     EJECT                                                                
048600*    --- IMS FUNCTION CODES                                               
048700*01  -COPY W0003                                                          
048800     EJECT                                                                
048900*    ---  DLI INPUT-OUTPUT AREA                                           
049000 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4113'.            
049100                                                                          
049200 01  DLI-IO-AREA-4113.                                                    
049300*        05  -COPY WDGX4113                                               
049400     EJECT                                                                
049500*                                                                         
049600 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4114'.            
049700                                                                          
049800 01  DLI-IO-AREA-4114.                                                    
049900*        05  -COPY WDGX4114                                               
050000     EJECT                                                                
050100*                                                                         
050200 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4115'.            
050300                                                                          
050400 01  DLI-IO-AREA-4115.                                                    
050500*        05  -COPY WDGX4115                                               
050600     EJECT                                                                
050700*                                                                         
050800 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4116'.            
050900                                                                          
051000 01  DLI-IO-AREA-4116.                                                    
051100*        05  -COPY WDGX4116                                               
051200     EJECT                                                                
051300*                                                                         
051400 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4117'.            
051500                                                                          
051600 01  DLI-IO-AREA-4117.                                                    
051700*        05  -COPY WDGX4117                                               
051800     EJECT                                                                
051900*                                                                         
052000 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA-4118'.            
052100                                                                          
052200 01  DLI-IO-AREA-4118.                                                    
052300*        05  -COPY WDGX4118                                               
052400     EJECT                                                                
052500*                                                                         
052600                                                                          
052700 LINKAGE SECTION.                                                         
052800                                                                          
052900     EJECT                                                                
053000*    -COPY W418ANSV                                                       
053100     EJECT                                                                
053200*01  -COPY W0008  -PRE 4113-                                              
053300     05  FILLER                  PIC X.                                   
053400     EJECT                                                                
053500*01  -COPY W0008  -PRE 4115-                                              
053600     05  FILLER                  PIC X.                                   
053700     EJECT                                                                
053800*01  -COPY W0008  -PRE 4117-                                              
053900     05  FILLER                  PIC X.                                   
054000     EJECT                                                                
054100 PROCEDURE DIVISION  USING ANSV-W418ANSV 4113-PCB                         
054200                                         4115-PCB 4117-PCB.               
054300     ENTRY 'DLITCBL' USING ANSV-W418ANSV 4113-PCB                         
054400                                         4115-PCB 4117-PCB.               
054500                                                                          
054600     PERFORM B-GET-RESPONSIBLE                                            
054700                                                                          
054800     MOVE ZERO TO RETURN-CODE                                             
054900     GOBACK                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 B-GET-RESPONSIBLE  SECTION.                                              
055300                                                                          
055400     IF ANSV-IDDISTR > ZERO                                               
055500       MOVE ANSV-IDFTG               TO W-IDFTG-4113                      
055600                                        W-IDFTG-4115                      
055700                                        W-IDFTG-4117                      
055800                                                                          
055900       MOVE ANSV-IDDISTR             TO W-IDDISTRF                        
056000                                        W-IDDISTRT                        
056100                                                                          
056200       MOVE ANSV-IDKUNDNR            TO W-IDKUNDNF                        
056300                                        W-IDKUNDNT                        
056400       EVALUATE ANSV-KDCALL                                               
056500         WHEN +1                                                          
056600            MOVE ANSV-KDANMORS       TO W-KDANMORS-4114-FOM               
056700                                        W-KDANMORS-4114-TOM               
056800            PERFORM IMS-GET-411301                                        
056900            PERFORM IMS-GET-411311                                        
057000            IF SEGMENT-FOUND                                              
057100              MOVE 4114-KDARBTYP-ADM TO ANSV-KDARBTYP                     
057200              MOVE 4114-IDPERSON-ADM TO ANSV-IDPERSON                     
057300              MOVE CORRECT           TO ANSV-KDSVAR                       
057400            ELSE                                                          
057500              MOVE LOW-VALUE         TO ANSV-UTDATA                       
057600              MOVE MISSING           TO ANSV-KDSVAR                       
057700            END-IF                                                        
057800         WHEN +2                                                          
057900            MOVE ANSV-KDANMORS       TO W-KDANMORS-4118-FOM               
058000                                        W-KDANMORS-4118-TOM               
059000            MOVE ANSV-IDDC           TO W-IDDC-4118-FOM                   
059100                                        W-IDDC-4118-TOM                   
059200            MOVE ANSV-KDORDKL        TO W-KDORDKL                         
059300            MOVE ANSV-ADLAGOMR       TO W-ADLAGOMR                        
059400                                                                          
059500            MOVE LOW-VALUE           TO ANSV-UTDATA                       
059600            MOVE MISSING             TO ANSV-KDSVAR                       
059700                                                                          
059800            PERFORM IMS-GU-411701                                         
059810*           HITTA EN UNIK REGEL - D+K+KL+LO                               
059900            PERFORM IMS-GNP-4118-KVAL                                     
059910                                                                          
060000            IF SEGMENT-MISSING                                            
060010*           HITTA EN REGEL SOM GÄLLER UNIKT DISTR.+ KUND-INT.             
060020*           + UNIK ORDERDEL + UNIKT LO                                    
060100            PERFORM IMS-GNP-4118-KVAL-D                                   
060200                                                                          
060300              IF SEGMENT-MISSING                                          
060310*             HITTA EN REGEL SOM GÄLLER UNIKT DISTR.+ KUND-INT.           
060320*             + UNIK ORDERKL + ALLA LO                                    
060400                MOVE ZERO        TO W-ADLAGOMR                            
060500                PERFORM IMS-GNP-4118-KVAL-D                               
060600                                                                          
060700                IF SEGMENT-MISSING                                        
060710*               HITTA EN REGEL SOM GÄLLER DISTR-INT.+ KUND-INT.           
060720*               + UNIK ORDERKL + ALLA LO                                  
060800                  PERFORM IMS-GNP-4118-OKVAL                              
060900                                                                          
061000                  IF SEGMENT-MISSING                                      
061010*                 HITTA EN REGEL SOM GÄLLER UNIKT DISTR.+ KUND-           
061020*                 INT + ALLA ORDERKL + UNIKT LO                           
061100                    MOVE ANSV-ADLAGOMR TO W-ADLAGOMR                      
061200                    MOVE +9            TO W-KDORDKL                       
061300                    PERFORM IMS-GNP-4118-KVAL-D                           
061400                                                                          
061500                    IF SEGMENT-MISSING                                    
061510*                   HITTA EN REGEL SOM GÄLLER DISTR INT.+ KUND-           
061520*                   INT + ALLA ORDERKL + UNIKT LO                         
061600                      PERFORM IMS-GNP-4118-OKVAL                          
061700                                                                          
061800                      IF SEGMENT-MISSING                                  
061810*                     HITTA EN REGEL SOM GÄLLER UNIKT DISTR.+             
061811*                     KUND-INT.+ ALLA ORDERKL + ALLA LO                   
061900                        MOVE ZERO       TO W-ADLAGOMR                     
062000                        PERFORM IMS-GNP-4118-KVAL-D                       
062010                                                                          
062100                        IF SEGMENT-MISSING                                
062110*                       HITTA EN REGEL SOM GÄLLER DISTR-INT.+             
062120*                       KUND-INT.+ ALLA ORDERKL + ALLA LO                 
062200                          PERFORM IMS-GNP-4118-OKVAL                      
062300                        END-IF                                            
062400                      END-IF                                              
062500                    END-IF                                                
062600                  END-IF                                                  
062700                END-IF                                                    
062800              END-IF                                                      
062900            END-IF                                                        
063000            IF SEGMENT-FOUND                                              
063100              MOVE 4118-KDARBTYP-REM TO ANSV-KDARBTYP                     
063200              MOVE 4118-IDPERSON-REM TO ANSV-IDPERSON                     
063300              MOVE CORRECT           TO ANSV-KDSVAR                       
063400            ELSE                                                          
063500              PERFORM BA-HITTA-BAESTA-REGELN                              
063600              IF ANSV-SAKNAS                                              
063700* SIST MÅSTE EN DEFAULT-REGEL PER DC FINNAS                               
063800                MOVE 'XX'           TO W-KDANMORS-4118-FOM                
063900                                       W-KDANMORS-4118-TOM                
064000                PERFORM IMS-GNP-4118-FIRST                                
065000                IF SEGMENT-FOUND                                          
065100                  MOVE 4118-KDARBTYP-REM TO ANSV-KDARBTYP                 
065200                  MOVE 4118-IDPERSON-REM TO ANSV-IDPERSON                 
065300                  MOVE CORRECT           TO ANSV-KDSVAR                   
065400                ELSE                                                      
065500                  MOVE 'DEFAULT ANMORS=XX SAKNAS FÖR DC:T'                
065600                                         TO ERRTEXT-STR                   
065700                  CALL ABEND USING RKOD-ABEND-NO-DUMP                     
065800                END-IF                                                    
065900              END-IF                                                      
066000            END-IF                                                        
066100         WHEN +3                                                          
066200            MOVE ANSV-KDANMORS       TO W-KDANMORS-4116-FOM               
066300            MOVE ANSV-KDANMORS       TO W-KDANMORS-4116-TOM               
066400            MOVE ANSV-IDDC           TO W-IDDC                            
066500            PERFORM IMS-GET-411501                                        
066600            PERFORM IMS-GET-411511                                        
066700            IF SEGMENT-FOUND                                              
066800              MOVE 4116-KDARBTYP-RET TO ANSV-KDARBTYP                     
066900              MOVE 4116-IDPERSON-RET TO ANSV-IDPERSON                     
067000              MOVE CORRECT           TO ANSV-KDSVAR                       
067100            ELSE                                                          
067200              MOVE HIGH-VALUE        TO W-KDANMORS-4116-FOM               
067300              MOVE HIGH-VALUE        TO W-KDANMORS-4116-TOM               
067400              PERFORM IMS-GET-411501                                      
067500              PERFORM IMS-GET-411511                                      
067600              IF SEGMENT-FOUND                                            
067700                MOVE 4116-KDARBTYP-RET TO ANSV-KDARBTYP                   
067800                MOVE 4116-IDPERSON-RET TO ANSV-IDPERSON                   
067900                MOVE CORRECT         TO ANSV-KDSVAR                       
068000              ELSE                                                        
068100                MOVE LOW-VALUE       TO ANSV-UTDATA                       
068200                MOVE MISSING         TO ANSV-KDSVAR                       
068300              END-IF                                                      
068400            END-IF                                                        
068500         WHEN OTHER                                                       
068600            MOVE LOW-VALUE           TO ANSV-UTDATA                       
068700            MOVE WRONG               TO ANSV-KDSVAR                       
068800       END-EVALUATE                                                       
068900     ELSE                                                                 
069000       MOVE LOW-VALUE                TO ANSV-UTDATA                       
069100       MOVE WRONG                    TO ANSV-KDSVAR                       
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 BA-HITTA-BAESTA-REGELN SECTION.                                          
069600                                                                          
069700     MOVE +1       TO WS-IDDISTRF-PREV                                    
069800     MOVE +99999   TO WS-IDDISTRT-PREV                                    
069900     MOVE +0       TO WS-IDKUNDNRF-PREV                                   
070000     MOVE +9999999 TO WS-IDKUNDNRT-PREV                                   
070100                                                                          
070200     PERFORM IMS-GNP-4118-FIRST                                           
070300     IF SEGMENT-FOUND                                                     
070400       MOVE 4118-KDARBTYP-REM TO ANSV-KDARBTYP                            
070500       MOVE 4118-IDPERSON-REM TO ANSV-IDPERSON                            
070600       MOVE CORRECT           TO ANSV-KDSVAR                              
070700     END-IF                                                               
070800     PERFORM UNTIL SEGMENT-MISSING                                        
070900* LOOPA FÖR ATT HITTA BÄSTA REGELN SOM HAR ETT                            
071000* DISTR/KUND INTERVALL SOM ÄR NÄRMAST ARGUMENT VÄRDEN                     
072000       IF 4118-IDDISTR-FOM > WS-IDDISTRF-PREV                             
073000         MOVE 4118-KDARBTYP-REM TO ANSV-KDARBTYP                          
074000         MOVE 4118-IDPERSON-REM TO ANSV-IDPERSON                          
075000         MOVE 4118-IDDISTR-FOM TO WS-IDDISTRF-PREV                        
076000       END-IF                                                             
077000       IF 4118-IDDISTR-TOM < WS-IDDISTRT-PREV                             
078000         MOVE 4118-KDARBTYP-REM TO ANSV-KDARBTYP                          
079000         MOVE 4118-IDPERSON-REM TO ANSV-IDPERSON                          
080000         MOVE 4118-IDDISTR-TOM TO WS-IDDISTRT-PREV                        
090000       END-IF                                                             
100000       IF 4118-IDKUNDNR-FOM > WS-IDKUNDNRF-PREV                           
110000         MOVE 4118-KDARBTYP-REM TO ANSV-KDARBTYP                          
120000         MOVE 4118-IDPERSON-REM TO ANSV-IDPERSON                          
130000         MOVE 4118-IDKUNDNR-FOM TO WS-IDKUNDNRF-PREV                      
140000       END-IF                                                             
150000       IF 4118-IDKUNDNR-TOM < WS-IDKUNDNRT-PREV                           
160000         MOVE 4118-KDARBTYP-REM TO ANSV-KDARBTYP                          
170000         MOVE 4118-IDPERSON-REM TO ANSV-IDPERSON                          
180000         MOVE 4118-IDKUNDNR-TOM TO WS-IDKUNDNRT-PREV                      
190000       END-IF                                                             
200000                                                                          
210000       PERFORM IMS-GNP-4118-OKVAL                                         
220000     END-PERFORM                                                          
230000     .                                                                    
240000     EJECT                                                                
250000* --- IMS SECTIONS  ---                                                   
260000                                                                          
270000     EJECT                                                                
280000 IMS-GET-411301    SECTION.                                               
290000                                                                          
300000     STRING 'WL411301(WDGXKEY  =' W-4113-X ')'                            
310000          DELIMITED BY SIZE INTO SSA1                                     
320000     MOVE '  ' TO GOOD-STATUSCODES                                        
321000     CALL CBLTDLI USING GU 4113-PCB DLI-IO-AREA-4113 SSA1                 
322000     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
323000     PERFORM IMS-STATUSCHECK                                              
324000     .                                                                    
325000                                                                          
326000 IMS-GET-411311       SECTION.                                            
327000                                                                          
328000     STRING 'WL411311(KEY4114 >=' W-4114-FOM                              
329000                    '&KEY4114 <=' W-4114-TOM                              
329100                    '&IDDISTRF<=' W-IDDISTRF-X                            
329200                    '&IDDISTRT>=' W-IDDISTRT-X                            
329300                    '&IDKUNDNF<=' W-IDKUNDNF-X                            
329400                    '&IDKUNDNT>=' W-IDKUNDNT-X ')'                        
329500          DELIMITED BY SIZE INTO SSA1                                     
329600     MOVE '  GE' TO GOOD-STATUSCODES                                      
329700     CALL CBLTDLI USING GNP 4113-PCB DLI-IO-AREA-4114 SSA1                
329800     MOVE 4113-STATUS-CODE TO STATUS-WS                                   
329900     PERFORM IMS-STATUSCHECK                                              
330000     .                                                                    
330100     EJECT                                                                
330200 IMS-GET-411501    SECTION.                                               
330300                                                                          
330400     STRING 'WL411501(WDGXKEY  =' W-4115-X ')'                            
330500          DELIMITED BY SIZE INTO SSA1                                     
330600     MOVE '  ' TO GOOD-STATUSCODES                                        
330700     CALL CBLTDLI USING GU 4115-PCB DLI-IO-AREA-4115 SSA1                 
330800     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
330900     PERFORM IMS-STATUSCHECK                                              
331000     .                                                                    
331100                                                                          
331200 IMS-GET-411511       SECTION.                                            
331300                                                                          
331400     STRING 'WL411511(KEY4116 >=' W-4116-FOM                              
331500                    '&KEY4116 <=' W-4116-TOM                              
331600                    '&IDDISTRF<=' W-IDDISTRF-X                            
331700                    '&IDDISTRT>=' W-IDDISTRT-X                            
331800                    '&IDKUNDNF<=' W-IDKUNDNF-X                            
331900                    '&IDKUNDNT>=' W-IDKUNDNT-X                            
332000                    '&IDDC     =' W-IDDC-X ')'                            
332100          DELIMITED BY SIZE INTO SSA1                                     
332200     MOVE '  GE' TO GOOD-STATUSCODES                                      
332300     CALL CBLTDLI USING GNP 4115-PCB DLI-IO-AREA-4116 SSA1                
332400     MOVE 4115-STATUS-CODE TO STATUS-WS                                   
332500     PERFORM IMS-STATUSCHECK                                              
332600     .                                                                    
332700     EJECT                                                                
332800 IMS-GU-411701    SECTION.                                                
332900                                                                          
333000     STRING 'WL411701(WDGXKEY  =' W-4117-X ')'                            
333100          DELIMITED BY SIZE INTO SSA1                                     
333200     MOVE '  ' TO GOOD-STATUSCODES                                        
333300     CALL CBLTDLI USING GU 4117-PCB DLI-IO-AREA-4117 SSA1                 
333400     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
333500     PERFORM IMS-STATUSCHECK                                              
333600     .                                                                    
333700                                                                          
333800 IMS-GNP-4118-KVAL   SECTION.                                             
333900                                                                          
334000     STRING 'WL411711(KEY4118 >=' W-4118-FOM                              
334100                      '&KEY4118 <=' W-4118-TOM                            
334200                      '&IDDISTRF =' W-IDDISTRF-X                          
334300                      '&IDDISTRT =' W-IDDISTRT-X                          
334400                      '&IDKUNDNF =' W-IDKUNDNF-X                          
334500                      '&IDKUNDNT =' W-IDKUNDNT-X                          
334600                      '&KDORDKL  =' W-KDORDKL-X                           
334700                      '&ADLAGOMR =' W-ADLAGOMR-X ')'                      
334800          DELIMITED BY SIZE INTO SSA1                                     
334900     MOVE '  GE' TO GOOD-STATUSCODES                                      
335000     CALL CBLTDLI USING GNP 4117-PCB DLI-IO-AREA-4118 SSA1                
335100     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
335200     PERFORM IMS-STATUSCHECK                                              
335300     .                                                                    
335400                                                                          
335500 IMS-GNP-4118-KVAL-D   SECTION.                                           
335600                                                                          
335700     STRING 'WL411711*F(KEY4118 >=' W-4118-FOM                            
335800                      '&KEY4118 <=' W-4118-TOM                            
335900                      '&IDDISTRF =' W-IDDISTRF-X                          
336000                      '&IDDISTRT =' W-IDDISTRT-X                          
336100                      '&IDKUNDNF<=' W-IDKUNDNF-X                          
336200                      '&IDKUNDNT>=' W-IDKUNDNT-X                          
336300                      '&KDORDKL  =' W-KDORDKL-X                           
336400                      '&ADLAGOMR =' W-ADLAGOMR-X ')'                      
336500          DELIMITED BY SIZE INTO SSA1                                     
336600     MOVE '  GE' TO GOOD-STATUSCODES                                      
336700     CALL CBLTDLI USING GNP 4117-PCB DLI-IO-AREA-4118 SSA1                
336800     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
336900     PERFORM IMS-STATUSCHECK                                              
337000     .                                                                    
337100                                                                          
337200 IMS-GNP-4118-OKVAL   SECTION.                                            
337300                                                                          
337400     STRING 'WL411711*F(KEY4118 >=' W-4118-FOM                            
337500                      '&KEY4118 <=' W-4118-TOM                            
337600                      '&IDDISTRF<=' W-IDDISTRF-X                          
337700                      '&IDDISTRT>=' W-IDDISTRT-X                          
337800                      '&IDKUNDNF<=' W-IDKUNDNF-X                          
337900                      '&IDKUNDNT>=' W-IDKUNDNT-X                          
338000                      '&KDORDKL  =' W-KDORDKL-X                           
338100                      '&ADLAGOMR =' W-ADLAGOMR-X ')'                      
338200          DELIMITED BY SIZE INTO SSA1                                     
338300     MOVE '  GE' TO GOOD-STATUSCODES                                      
338400     CALL CBLTDLI USING GNP 4117-PCB DLI-IO-AREA-4118 SSA1                
338500     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
338600     PERFORM IMS-STATUSCHECK                                              
338700     .                                                                    
338800                                                                          
338900 IMS-GNP-4118-FIRST   SECTION.                                            
339000                                                                          
339100     STRING 'WL411711*F(KEY4118 >=' W-4118-FOM                            
339200                      '&KEY4118 <=' W-4118-TOM                            
339300                      '&IDDISTRF<=' W-IDDISTRF-X                          
339400                      '&IDDISTRT>=' W-IDDISTRT-X                          
339500                      '&IDKUNDNF<=' W-IDKUNDNF-X                          
339600                      '&IDKUNDNT>=' W-IDKUNDNT-X                          
339700                      '&KDORDKL  =' W-KDORDKL-X                           
339800                      '&ADLAGOMR =' W-ADLAGOMR-X ')'                      
339900          DELIMITED BY SIZE INTO SSA1                                     
340000     MOVE '  GE' TO GOOD-STATUSCODES                                      
340100     CALL CBLTDLI USING GNP 4117-PCB DLI-IO-AREA-4118 SSA1                
340200     MOVE 4117-STATUS-CODE TO STATUS-WS                                   
340300     PERFORM IMS-STATUSCHECK                                              
340400     .                                                                    
340500                                                                          
340600 IMS-STATUSCHECK SECTION.                                                 
340700                                                                          
340800     SET STATUS-IX TO 1                                                   
340900     SEARCH GOOD-STATUS                                                   
341000       AT END                                                             
341100         MOVE 'NOT OK STATUSCODE FROM IMS' TO ERRTEXT-STR                 
341200         CALL FELLOG                                                      
341300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
341400         CONTINUE                                                         
341500     END-SEARCH                                                           
341600     .                                                                    
