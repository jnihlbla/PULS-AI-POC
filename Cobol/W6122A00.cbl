000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6122A00.                                                
000300 AUTHOR.         SRINADH NADIMPALLI.                                      
000400 DATE-WRITTEN.   23/01/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        READ INFILE AND UPDATE 6302-DABERANK-PROP                        
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDR5                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INPUT FILE                                                 
002500     SELECT W61229                     ASSIGN TO W6122AD1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W61229                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W61229      -L.                                                
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W6122A00'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004202 77  WS-IDFAKT                   PIC 9(7)    VALUE ZERO.                  
004303 77  WS-DABERANK                 PIC 9(8)    VALUE ZERO.                  
004403 77  ISRT-SW                     PIC X       VALUE 'N'.                   
004503     88  ISRT-OK                             VALUE 'J'.                   
004603     88  ISRT-NOK                            VALUE 'N'.                   
004703                                                                          
004803 01  CHKP-VAR.                                                            
004903     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32  COMP SYNC.        
005003     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005103     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32  COMP SYNC.        
005203     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005303     03 CHKP-ANT                 PIC S9(5)   VALUE +0    COMP-3.          
005403     03 CHKP-MAX                 PIC S9(5)   VALUE +1200 COMP-3.          
005503 77  YES                         PIC X       VALUE 'J'.                   
005603 77  NOO                         PIC X       VALUE 'N'.                   
005703                                                                          
005803 77  W61229-EOF-SW               PIC X       VALUE 'N'.                   
005903     88  END-OF-W61229                       VALUE 'J'.                   
006003     EJECT                                                                
006103 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006203 01  FILLER REDEFINES TODAYS-DATE.                                        
006303     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006403     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006503     03  TODAYS-DATE-DAY         PIC 9(2).                                
006603     EJECT                                                                
006703 01  GENERAL-SUBPROGRAMS.                                                 
006803*                                                                         
006903     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007003     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007103     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007203     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007303     SKIP2                                                                
007403*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007503                                                                          
007603 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007703 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007803 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007903     SKIP2                                                                
008003 01  ERROR-TEXT.                                                          
008103     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
008203     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
008303     EJECT                                                                
008403*    --- PARAMETRAR TILL POSTSUM                                          
008503*                                                                         
008603*01  -COPY W0005   -PRE  POSTSUM-                                         
008703     EJECT                                                                
008803 01  IN-AREA-START               PIC X(24)   VALUE                        
008903                                 'IN-AREA-START  '.                       
009003     SKIP2                                                                
009103                                                                          
009203*01  AREA -COPY W61229     -PRE IN-                                       
009303     EJECT                                                                
009403*    --- AREAS FOR IMS-SECTIONS                                           
009503*                                                                         
009603     EJECT                                                                
009703 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009803     SKIP3                                                                
009903 01  KEYS-FOR-DLI.                                                        
010003     03  W-6301KEY-X.                                                     
010103         05  W-6301-IDHTYP        PIC X(4)     VALUE '6301'.              
010203         05  W-6301-IDDC          PIC X(2).                               
010303         05  FILLER               PIC X(24)    VALUE LOW-VALUE.           
010403     03  W-6302-IDLBBET-X         PIC X(12)    VALUE SPACE.               
010503     03  W-IDARTNR-X.                                                     
010603         05  W-IDARTNR            PIC S9(9)    COMP-3.                    
010703     03  W-DAINLEV-X.                                                     
010803         05  W-DAINLEV            PIC 9(16).                              
010903     03  W-WDL6A1KY-MIN.                                                  
011003         05  W-SEQA-IDFAKT-MIN    PIC S9(7)    COMP-3.                    
011103         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
011203         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)    COMP-3.                    
011303         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)    COMP-3.                    
011403         05  W-SEQA-IDARTNR-MIN   PIC S9(9)    COMP-3.                    
011503         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
011603     03  W-WDL6A1KY-MAX.                                                  
011703         05  W-SEQA-IDFAKT-MAX    PIC S9(7)    COMP-3.                    
011803         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
011903         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)    COMP-3.                    
012003         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)    COMP-3.                    
012103         05  W-SEQA-IDARTNR-MAX   PIC S9(9)    COMP-3.                    
012203         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
012303     03  W-IDPTYP-X               PIC X(3)     VALUE SPACE.               
012403     SKIP2                                                                
012503*    --- STATUS-KOD FRÅN IMS                                              
012603 01  STATUS-WS                    PIC XX.                                 
012703     88  SEGMENT-FOUND                       VALUE '  '.                  
012803     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012903     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013003     88  SEGMENT-END                         VALUE 'GB'.                  
013103     88  IMS-NOT-OK                          VALUE 'XD'.                  
013203     SKIP2                                                                
013303 01  GOOD-STATUSCODES.                                                    
013403     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013503     SKIP3                                                                
013603 01  SSA1                        PIC X(160).                              
013703 01  SSA2                        PIC X(128).                              
013803     EJECT                                                                
013903*    --- IMS FUNCTION CODES                                               
014003*01  -COPY W0003                                                          
014103     EJECT                                                                
014203*    ---  DLI INPUT-OUTPUT AREA                                           
014303 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01  '.                    
015000 01  DLI-IO-WDGX01.                                                       
016000*    03  -COPY WDGX01DC                                                   
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6302'.                    
018000 01  DLI-IO-WDGX6302.                                                     
019000*    03  -COPY WDGX6302                                                   
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
030000 01  DLI-IO-WDL601.                                                       
031000*    03  -COPY WDL601                                                     
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
031200 01  DLI-IO-WDL611.                                                       
031300*    03  -COPY WDL611                                                     
031400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6A1'.                      
031500 01  DLI-IO-WDL6A1.                                                       
031600*    03  -COPY WDL6A1                                                     
031700     EJECT                                                                
031800 LINKAGE SECTION.                                                         
031900                                                                          
032000*01  -COPY W0009  -PRE MSG-                                               
032100*01  -COPY W0008  -PRE 6301-                                              
032110     05  FILLER                  PIC X.                                   
032200*01  -COPY W0008  -PRE WDL6-                                              
032210     05  FILLER                  PIC X.                                   
032300*01  -COPY W0008  -PRE WDL6A-                                             
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600 PROCEDURE DIVISION  USING MSG-PCB 6301-PCB WDL6-PCB WDL6A-PCB.           
032700 MAIN SECTION.                                                            
032800     ENTRY 'DLITCBL' USING MSG-PCB 6301-PCB WDL6-PCB WDL6A-PCB.           
032900                                                                          
033000                                                                          
033100     PERFORM A-INIT                                                       
033200                                                                          
033300     PERFORM S01-READ-W61229                                              
033400     PERFORM UNTIL END-OF-W61229                                          
033500       MOVE IN-IDLBBET  TO W-6302-IDLBBET-X                               
033600       MOVE IN-IDDC-REC TO W-6301-IDDC                                    
033700       PERFORM IMS-GU-WDR501                                              
033800       PERFORM IMS-GHNP-WDGX6302                                          
033900       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                       
034000        MOVE NEJ TO ISRT-SW                                               
034100        IF IN-DABERANK = 0                                                
034200         IF IN-DABERANK-PROP = 6302-DABERANK-PROP                         
034300           CONTINUE                                                       
034400         ELSE                                                             
034500           MOVE IN-DABERANK-PROP TO 6302-DABERANK-PROP                    
034600           PERFORM IMS-REPL-WDGX6302                                      
034700           ADD +1 TO CHKP-ANT                                             
034800         END-IF                                                           
034900        ELSE                                                              
035000         IF IN-DABERANK NOT = 6302-DABERANK                               
035100           MOVE JA TO ISRT-SW                                             
035200           PERFORM IMS-DLET-WDGX6302                                      
035300           IF IN-DABERANK-PROP > 0                                        
035400            MOVE IN-DABERANK-PROP TO 6302-DABERANK-PROP                   
035500           END-IF                                                         
035602           MOVE 6302-IDFAKT TO WS-IDFAKT                                  
035700           MOVE IN-DABERANK TO 6302-DABERANK                              
035803                               WS-DABERANK                                
035903           MOVE NEJ         TO 6302-FLMANETA                              
036003           MOVE SPACE       TO 6302-IDUSER-MANETA                         
036103           PERFORM IMS-ISRT-WDGX6302                                      
036203           ADD +1 TO CHKP-ANT                                             
036303           PERFORM B-UPDATE-PARTNO-ETA                                    
036403         END-IF                                                           
036503        END-IF                                                            
036603        IF ISRT-OK                                                        
036703         PERFORM IMS-GHNP-WDGX6302-FIRST                                  
036803        ELSE                                                              
036903         PERFORM IMS-GHNP-WDGX6302                                        
037003        END-IF                                                            
037103       END-PERFORM                                                        
037203       IF CHKP-ANT > CHKP-MAX                                             
037303         PERFORM X-TAKE-CHECKPOINT                                        
037403       END-IF                                                             
037503       PERFORM S01-READ-W61229                                            
037603     END-PERFORM                                                          
037703                                                                          
037803                                                                          
037903     PERFORM Z-FINIT                                                      
038003                                                                          
038103     MOVE ZERO TO RETURN-CODE                                             
038203     GOBACK                                                               
038303     .                                                                    
038403     EJECT                                                                
038503 A-INIT SECTION.                                                          
038603                                                                          
038703     PERFORM IMS-RESTART                                                  
038803                                                                          
038903     OPEN INPUT  W61229                                                   
039003                                                                          
039103     MOVE ZERO TO CHKP-ANT                                                
039203     ACCEPT TODAYS-DATE  FROM DATE                                        
039303     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039403     .                                                                    
039503     EJECT                                                                
039603 B-UPDATE-PARTNO-ETA SECTION.                                             
039703     MOVE WS-IDFAKT          TO W-SEQA-IDFAKT-MIN                         
039803                                W-SEQA-IDFAKT-MAX                         
039903     MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN                       
040003     MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN                       
040103                                W-SEQA-IDKOLLI-MIN                        
040203                                W-SEQA-IDARTNR-MIN                        
040303                                W-SEQA-DAINLEV-MIN                        
040403     MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                       
040503     MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                       
040603     MOVE 99999              TO W-SEQA-IDKOLLI-MAX                        
040703     MOVE 999999999          TO W-SEQA-IDARTNR-MAX                        
040803     MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX                        
040903     MOVE 'R30'              TO W-IDPTYP-X                                
041003                                                                          
041103     PERFORM IMS-GU-WDL6A1                                                
041203     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
041303        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
041403        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
041503        PERFORM IMS-GHU-WDL611                                            
041603                                                                          
041703        MOVE WS-DABERANK(3:6) TO INL-TIBERANK                             
041803        PERFORM IMS-REPL-WDL611                                           
041804        ADD +1 TO CHKP-ANT                                                
041903                                                                          
042003        PERFORM IMS-GN-WDL6A1                                             
042103     END-PERFORM                                                          
042202     .                                                                    
042302     EJECT                                                                
042402 Z-FINIT SECTION.                                                         
042502     CLOSE W61229                                                         
042602     SKIP2                                                                
042702     MOVE 'S' TO POSTSUM-OPKOD                                            
042802     CALL POSTSUM USING POSTSUM-PARM                                      
042902     .                                                                    
043002     EJECT                                                                
043102 S01-READ-W61229  SECTION.                                                
043202     READ W61229 INTO IN-AREA                                             
043302     AT END                                                               
043402        MOVE HIGH-VALUE TO IN-AREA                                        
043502        SET END-OF-W61229 TO TRUE                                         
043602                                                                          
043702     NOT AT END                                                           
043802        MOVE 'W61229' TO POSTSUM-FDNAMN                                   
043902        MOVE 'W6122AD1' TO POSTSUM-DDNAMN2                                
044002*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
044102        MOVE SPACES    TO POSTSUM-TRANSTYP                                
044202        CALL POSTSUM USING POSTSUM-PARM                                   
044302     END-READ                                                             
044402     .                                                                    
044502     EJECT                                                                
044602 S99-ABEND SECTION.                                                       
044702                                                                          
044802     SKIP2                                                                
044902     MOVE 'S' TO POSTSUM-OPKOD                                            
045002     CALL POSTSUM USING POSTSUM-PARM                                      
045102     CALL ABEND USING RKOD-ABEND                                          
045202     .                                                                    
045302     EJECT                                                                
045402* --- IMS SECTIONS  ---                                                   
045502                                                                          
045602     EJECT                                                                
045702 X-TAKE-CHECKPOINT   SECTION.                                             
045802                                                                          
045902* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
046002* --- SAVE DATABASE KEYS IF NECESSARY                                     
046102     PERFORM IMS-CHECKPOINT                                               
046202     MOVE ZERO TO CHKP-ANT                                                
046302* --- REREAD DATABASE IF NECESSARY                                        
046402     .                                                                    
046502     EJECT                                                                
046602* --- IMS SECTIONS  ---                                                   
046702                                                                          
046802     EJECT                                                                
046902 IMS-GU-WDR501 SECTION.                                                   
047002                                                                          
047102     STRING 'WDR501  (WDGXKEY = ' W-6301KEY-X ')'                         
047202          DELIMITED BY SIZE INTO SSA1                                     
047302     MOVE '  ' TO GOOD-STATUSCODES                                        
047402     CALL CBLTDLI USING GU 6301-PCB DLI-IO-WDGX01 SSA1                    
047502     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
047602     PERFORM IMS-STATUSCHECK                                              
047702     .                                                                    
047802     EJECT                                                                
047902 IMS-GHNP-WDGX6302 SECTION.                                               
048002                                                                          
048102     STRING 'WDGX6302(IDLBBET = ' W-6302-IDLBBET-X ')'                    
048202          DELIMITED BY SIZE INTO SSA1                                     
048302     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
048402     CALL CBLTDLI USING GHNP 6301-PCB DLI-IO-WDGX6302 SSA1                
048502     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
048602     PERFORM IMS-STATUSCHECK                                              
048702     .                                                                    
048802     SKIP3                                                                
048902 IMS-GHNP-WDGX6302-FIRST SECTION.                                         
049002                                                                          
049102     STRING 'WDGX6302*F(IDLBBET = ' W-6302-IDLBBET-X ')'                  
049202          DELIMITED BY SIZE INTO SSA1                                     
049302     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
049402     CALL CBLTDLI USING GHNP 6301-PCB DLI-IO-WDGX6302 SSA1                
049502     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
049602     PERFORM IMS-STATUSCHECK                                              
049702     .                                                                    
049703     SKIP3                                                                
049902 IMS-GU-WDL6A1  SECTION.                                                  
050002     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN                          
050102                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
050202                    '&IDPTYP  = ' W-IDPTYP-X ')'                          
050302          DELIMITED BY SIZE INTO SSA1                                     
050402     MOVE '  GE' TO GOOD-STATUSCODES                                      
050502     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-WDL6A1 SSA1                   
050602                                                                          
050702     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
050802     PERFORM IMS-STATUSCHECK                                              
050902     .                                                                    
051002     SKIP3                                                                
051102 IMS-GN-WDL6A1  SECTION.                                                  
051202     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN                          
051302                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
051402                    '&IDPTYP  = ' W-IDPTYP-X ')'                          
051502          DELIMITED BY SIZE INTO SSA1                                     
051602     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
051702     CALL CBLTDLI USING GN WDL6A-PCB DLI-IO-WDL6A1 SSA1                   
051802                                                                          
051902     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
052002     PERFORM IMS-STATUSCHECK                                              
052102     .                                                                    
052202 IMS-GHU-WDL611   SECTION.                                                
052302     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
052402          DELIMITED BY SIZE INTO SSA1                                     
052502     STRING 'WDL611  (DAINLEV = ' W-DAINLEV-X ')'                         
052602          DELIMITED BY SIZE INTO SSA2                                     
052702     MOVE SPACE  TO GOOD-STATUSCODES                                      
052802     CALL CBLTDLI USING GHU  WDL6-PCB DLI-IO-WDL611 SSA1 SSA2             
052902     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
053002     PERFORM IMS-STATUSCHECK                                              
053102     .                                                                    
053202     SKIP3                                                                
053302 IMS-ISRT-WDGX6302 SECTION.                                               
053402                                                                          
053502     STRING 'WDR501  (WDGXKEY = ' W-6301KEY-X ')'                         
053602          DELIMITED BY SIZE INTO SSA1                                     
053702     MOVE 'WDGX6302 ' TO SSA2                                             
053802     MOVE '  GE' TO GOOD-STATUSCODES                                      
053902     CALL CBLTDLI USING ISRT 6301-PCB DLI-IO-WDGX6302 SSA1 SSA2           
054002     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
054102     PERFORM IMS-STATUSCHECK                                              
054202     .                                                                    
054302     SKIP3                                                                
054402 IMS-REPL-WDGX6302 SECTION.                                               
054502                                                                          
054602     MOVE '  ' TO GOOD-STATUSCODES                                        
054702     CALL CBLTDLI USING REPL 6301-PCB DLI-IO-WDGX6302                     
054802     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
054902     PERFORM IMS-STATUSCHECK                                              
055002     .                                                                    
055102     EJECT                                                                
055202 IMS-DLET-WDGX6302 SECTION.                                               
055302                                                                          
055402     MOVE '  ' TO GOOD-STATUSCODES                                        
055502     CALL CBLTDLI USING DLET 6301-PCB DLI-IO-WDGX6302                     
055602     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
055702     PERFORM IMS-STATUSCHECK                                              
055802     .                                                                    
055902     SKIP3                                                                
056002 IMS-REPL-WDL611 SECTION.                                                 
056102     MOVE '  ' TO GOOD-STATUSCODES                                        
056202     CALL CBLTDLI USING REPL WDL6-PCB DLI-IO-WDL611                       
056302                                                                          
056402     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
056502     PERFORM IMS-STATUSCHECK                                              
056602     .                                                                    
056702     EJECT                                                                
056802 IMS-RESTART SECTION.                                                     
056902     SKIP2                                                                
057002     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
057102     MOVE '  ' TO GOOD-STATUSCODES                                        
057202     CALL CBLTDLI USING XRST MSG-PCB                                      
057302                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
057402                        CHKP-AREA-LENGTH CHKP-AREA                        
057502     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057602     PERFORM IMS-STATUSCHECK                                              
057702     .                                                                    
057802     SKIP3                                                                
057902 IMS-CHECKPOINT SECTION.                                                  
058002     SKIP2                                                                
058102     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
058202     MOVE '  XD' TO GOOD-STATUSCODES                                      
058302     CALL CBLTDLI USING CHKP MSG-PCB                                      
058402                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
058502                        CHKP-AREA-LENGTH CHKP-AREA                        
058602     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058702     PERFORM IMS-STATUSCHECK                                              
058802                                                                          
058902     IF IMS-NOT-OK                                                        
059002       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
059102             TO ERROR-TEXT-STR                                            
059202       DISPLAY ERROR-TEXT                                                 
059302       CALL FELLOG                                                        
059402     END-IF                                                               
059502     .                                                                    
059602     EJECT                                                                
059702 IMS-STATUSCHECK SECTION.                                                 
059802                                                                          
059902     SET STATUS-IX TO 1                                                   
060002     SEARCH GOOD-STATUS                                                   
060102       AT END                                                             
060202         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
060302           DELIMITED BY SIZE INTO ERROR-TEXT                              
060402         DISPLAY ERROR-TEXT                                               
060502         CALL FELLOG                                                      
060602       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
060702         CONTINUE                                                         
061002     END-SEARCH                                                           
070002     .                                                                    
