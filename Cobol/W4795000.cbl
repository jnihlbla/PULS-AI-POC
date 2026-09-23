000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4795000.                                                 
000300 AUTHOR.        SVANTE BJÖRKBERG.                                         
000400 DATE-WRITTEN.  JAN 1987.                                                 
000500                                                                          
000600     REMARKS.                                                             
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        PROGRAMMET LÄSER WDE4  MED DLI (SEQUENCE BUFFERING).             
001000*                         WDQ2  MED DLI.                                  
001100*                         WDE6  MED DLI.                                  
001200*                         WDQ3  MED DLI.                                  
001300*        SUGER UT INFORMATION OCH SKAPAR ETT ANTAL FILER.                 
001400*                                                                         
001500*                                                                         
001600*    E'TRACKER 8687963  DATUM 20100309 LÄGG TILL ADLAGOMR PÅ WDL5.        
001700*                                                                         
001800*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300*                                                                         
002400 FILE-CONTROL.                                                            
002500                                                                          
002600*                                                                         
002700     SELECT W47951    ASSIGN TO W47950D2.                                 
002800     SELECT W47952    ASSIGN TO W47950D3.                                 
002900     SELECT W47954    ASSIGN TO W47950D4.                                 
003000     SELECT W47956    ASSIGN TO W47950D5.                                 
003100     SELECT W47958    ASSIGN TO W47950D6.                                 
003200     SELECT W47959    ASSIGN TO W47950D7.                                 
003300     SELECT W4795C    ASSIGN TO W47950D8.                                 
003400*                                                                         
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900                                                                          
004000 FD  W47951                                                               
004100     LABEL RECORD    STANDARD                                             
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS 0.                                                    
004400                                                                          
004500*01  E51-AREA  -COPY W479051   -L                                         
004600     SKIP3                                                                
004700                                                                          
004800 FD  W47952                                                               
004900     LABEL RECORD    STANDARD                                             
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS 0.                                                    
005200                                                                          
005300*01  E52-AREA  -COPY W479052   -L                                         
005400     SKIP3                                                                
005500                                                                          
005600 FD  W47954                                                               
005700     LABEL RECORD    STANDARD                                             
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS 0.                                                    
006000                                                                          
006100*01  E54-AREA  -COPY W479054   -L                                         
006200     SKIP3                                                                
006300                                                                          
006400 FD  W47956                                                               
006500     LABEL RECORD    STANDARD                                             
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS 0.                                                    
006800                                                                          
006900*01  AREA-001  -COPY W462001   -L                                         
007000     EJECT                                                                
007100                                                                          
007200 FD  W47958                                                               
007300     LABEL RECORD    STANDARD                                             
007400     RECORDING       F                                                    
007500     BLOCK CONTAINS 0.                                                    
007600                                                                          
007700*01  A21-AREA  -COPY W479A21   -L                                         
007800     SKIP3                                                                
007900                                                                          
008000 FD  W47959                                                               
008100     LABEL RECORD    STANDARD                                             
008200     RECORDING       V                                                    
008300     BLOCK CONTAINS 0.                                                    
008400                                                                          
008500*01  A02-AREA  -COPY W479A01   -L                                         
008600     SKIP3                                                                
008700                                                                          
008800 FD  W4795C                                                               
008900     LABEL RECORD    STANDARD                                             
009000     RECORDING       F                                                    
009100     BLOCK CONTAINS 0.                                                    
009200                                                                          
009300*01  E5C-AREA  -COPY W47905C   -L                                         
009400     SKIP3                                                                
009500                                                                          
009600 WORKING-STORAGE SECTION.                                                 
009700*    -- CHECKED BY WY2000                                                 
009800     SKIP3                                                                
009900*                                                                         
010000 77   PROGRAM-NAMN               PIC X(6)    VALUE 'W47950'.              
010100                                                                          
010200 77  JA                          PIC X(1)    VALUE 'J'.                   
010300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010400 77  FL-SKRIV-A21                PIC X(1)    VALUE 'N'.                   
010500 77  SKRIVNA-POSTER              PIC S9(1)   COMP-3.                      
010600 77  SPARAT-IDPRODNR-001         PIC S9(7)   COMP-3 VALUE ZERO.           
010700 77  SPARAT-IDORDNR-001          PIC S9(7)   COMP-3 VALUE ZERO.           
010800 77  SPARAT-IDPRODNR-1-A02       PIC S9(7)   COMP-3 VALUE ZERO.           
010900 77  SPARAT-IDPRODNR-2-A02       PIC S9(7)   COMP-3 VALUE ZERO.           
011000 77  SPARAT-IDDISTR-E51          PIC S9(5)   COMP-3 VALUE ZERO.           
011100 77  SPARAT-IDKUNDNR-E51         PIC S9(7)   COMP-3 VALUE ZERO.           
011200 77  SPARAT-IDKUNDRF-E51         PIC X(10)          VALUE SPACE.          
011300*                                                                         
011400 01  WS-IDKUNDRF-X.                                                       
011500   03  WS-IDKUNDRF               PIC 9(5).                                
011600   03  FILLER                    PIC X(5).                                
011700*                                                                         
011800 01  DYNAMISKA-SUBPROGRAM.                                                
011900*                                                                         
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
012400                                                                          
012500     EJECT                                                                
012600*01  -COPY W460DIS1                                                       
012700     EJECT                                                                
012800*01  -COPY WWDIS130                                                       
012900     EJECT                                                                
012910*01  -COPY WWDCKONS                                                       
012920     EJECT                                                                
013000 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
013100     SKIP2                                                                
013200*01          -COPY W479051  -PRE E51-                                     
013300     EJECT                                                                
013400*01          -COPY W479052  -PRE E52-                                     
013500     EJECT                                                                
013600*01          -COPY W479054  -PRE E54-                                     
013700     EJECT                                                                
013800*01          -COPY W47905C  -PRE E5C-                                     
013900     EJECT                                                                
014000*01          -COPY W462001                                                
014100     EJECT                                                                
014200*01          -COPY W479A01  -PRE A02-                                     
014300     EJECT                                                                
014400*01          -COPY W479A21  -PRE A21-                                     
014500     EJECT                                                                
014600 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
014700     SKIP2                                                                
014800 01  NYCKLAR-TILL-DLI.                                                    
014900     03  W-IDORDER-X.                                                     
015000         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
015100*                                                                         
015200     03  W-IDPRODNR-X.                                                    
015300         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
015400*                                                                         
015500     03  W-WDQ301-KEY-X.                                                  
015600         05  W-WDQ301-IDORDER    PIC S9(7)   VALUE ZERO  COMP-3.          
015700         05  W-WDQ301-IDDC       PIC X(2).                                
015800         05  W-WDQ301-IDPRODNR   PIC S9(7)   VALUE ZERO  COMP-3.          
015900         05  W-WDQ301-IDPLKLST   PIC S9(3)   VALUE ZERO  COMP-3.          
016000*                                                                         
016100 01  IMS-WS.                                                              
016200                                                                          
016300     03  STATUS-WS               PIC X(2).                                
016400        88  SEGMENT-FINNS                    VALUE '  ' 'GA' 'GK'.        
016500        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
016600        88  SEGMENT-SLUT                     VALUE 'GB'.                  
016700                                                                          
016800     03  GODK-STATUSKODER.                                                
016900         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
017000                                                                          
017100     03  SSA1                    PIC X(64).                               
017200     EJECT                                                                
017300*01  -COPY W0003                                                          
017400     SKIP2                                                                
017500*01  -COPY W0005    -PRE POSTSUM-                                         
017600     SKIP2                                                                
017700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017800 01  DLI-IO-AREA.                                                         
017900     03  IO-AREA             PIC X(400).                                  
018000     SKIP3                                                                
018100*    03  WDE401    -COPY WDE401    -RED IO-AREA                           
018200     SKIP2                                                                
018300*    03  WDE411    -COPY WDE411    -RED IO-AREA                           
018400     SKIP2                                                                
018500*    03  WDE421    -COPY WDE421    -RED IO-AREA                           
018600     SKIP2                                                                
018700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-Q2'.        
018800 01  DLI-IO-AREA-WDQ201.                                                  
018900*    03  -COPY WDQ201                                                     
019000     SKIP2                                                                
019100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-E6'.        
019200 01  DLI-IO-AREA-WDE601.                                                  
019300*    03  -COPY WDE601                                                     
019400                                                                          
019500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ301'.        
019600 01  DLI-IO-WDQ301.                                                       
019700*    03 -COPY WDQ301                                                      
019800     SKIP2                                                                
019900 LINKAGE SECTION.                                                         
020000     SKIP3                                                                
020100*01  -COPY W0008   -PRE WDE4-                                             
020200         05  FILLER          PIC X(1).                                    
020300*01  -COPY W0008   -PRE WDQ2-                                             
020400         05  FILLER          PIC X(1).                                    
020500*01  -COPY W0008   -PRE WDE6-                                             
020600         05  FILLER          PIC X(1).                                    
020700*01  -COPY W0008   -PRE WDQ3-                                             
020800         05  FILLER          PIC X(1).                                    
020900     EJECT                                                                
021000 PROCEDURE DIVISION  USING WDE4-PCB WDQ2-PCB WDE6-PCB WDQ3-PCB.           
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING WDE4-PCB WDQ2-PCB WDE6-PCB WDQ3-PCB.           
021300                                                                          
021400     PERFORM A-INIT                                                       
021500     PERFORM IMS-GN-WDE4                                                  
021600                                                                          
021700     PERFORM UNTIL SEGMENT-SLUT                                           
021800                                                                          
021900       EVALUATE WDE4-SEG-NAME-FB                                          
022000         WHEN 'WDE401'                                                    
022100           IF KORD-IDDISTR  NOT = SPARAT-IDDISTR-E51  OR                  
022200              KORD-IDKUNDNR NOT = SPARAT-IDKUNDNR-E51 OR                  
022300              KORD-IDKUNDRF NOT = SPARAT-IDKUNDRF-E51                     
022400              PERFORM WDE401-SKRIV-E51                                    
022500           END-IF                                                         
022600           PERFORM WDE401-SKAPA-E54                                       
022700           PERFORM WDE401-SKAPA-E52                                       
022800           PERFORM WDE401-SKAPA-E5C                                       
022900           PERFORM WDE401-SKAPA-001                                       
023000           PERFORM WDE401-SKAPA-A02                                       
023100           PERFORM WDE401-SKAPA-A21                                       
023200                                                                          
023300         WHEN 'WDE411'                                                    
023400           PERFORM WDE411-SKRIV-E52                                       
023500           PERFORM WDE411-SKRIV-E54                                       
023600           PERFORM WDE411-SKRIV-E5C                                       
023700           PERFORM WDE411-SKRIV-001                                       
023800           PERFORM WDE411-SKRIV-A02                                       
023900           PERFORM WDE411-SKAPA-A21                                       
024000                                                                          
024100         WHEN 'WDE421'                                                    
024200           PERFORM WDE421-SKRIV-A21                                       
024300       END-EVALUATE                                                       
024400                                                                          
024500       PERFORM IMS-GN-WDE4                                                
024600     END-PERFORM                                                          
024700                                                                          
024800     IF FL-SKRIV-A21 = JA                                                 
024900       PERFORM SKRIV-A21                                                  
025000     END-IF                                                               
025100                                                                          
025200     PERFORM Z-FINIT                                                      
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 A-INIT SECTION.                                                          
025800                                                                          
025900     OPEN OUTPUT W47951                                                   
026000                 W47952                                                   
026100                 W47954                                                   
026200                 W47956                                                   
026300                 W47958                                                   
026400                 W47959                                                   
026500                 W4795C                                                   
026600                                                                          
026700     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
026800                                                                          
026900     .                                                                    
027000     EJECT                                                                
027100 WDE401-SKRIV-E51       SECTION.                                          
027200                                                                          
027300     MOVE 'E51'            TO E51-IDPTYP                                  
027400     MOVE KORD-IDDISTR     TO E51-IDDISTR                                 
027500     MOVE KORD-IDKUNDNR    TO E51-IDKUNDNR                                
027600     MOVE KORD-IDPRODNR    TO W-IDPRODNR                                  
027700     MOVE KORD-IDKUNDRF    TO E51-IDKUNDRF                                
027800     MOVE KORD-TIORDREG    TO E51-TIORDREG                                
027900     MOVE KORD-IDDISTR     TO SPARAT-IDDISTR-E51                          
028000     MOVE KORD-IDKUNDNR    TO SPARAT-IDKUNDNR-E51                         
028100     MOVE KORD-IDKUNDRF    TO SPARAT-IDKUNDRF-E51                         
028200                                                                          
028300     WRITE E51-AREA  FROM E51-W479051                                     
028400                                                                          
028500     MOVE 'W47951'     TO POSTSUM-FDNAMN                                  
028600     MOVE 'W47950D2'   TO POSTSUM-DDNAMN2                                 
028700     MOVE 'E51'        TO POSTSUM-TRANSTYP                                
028800                                                                          
028900     CALL POSTSUM USING POSTSUM-PARM                                      
029000     EJECT                                                                
029100     .                                                                    
029200 WDE401-SKAPA-E52       SECTION.                                          
029300                                                                          
029400     MOVE KORD-IDDISTR       TO E52-IDDISTR                               
029500     MOVE KORD-IDKUNDNR      TO E52-IDKUNDNR                              
029600     MOVE KORD-IDKUNDRF      TO E52-IDKUNDRF                              
029700     MOVE KORD-FLOVRLEV      TO E52-FLOVRLEV                              
029800     MOVE KORD-IDDC          TO E52-IDDC                                  
029900     MOVE KORD-KDFAKTYP      TO E52-KDFAKTYP                              
030000     MOVE KORD-KDFRAKT       TO E52-KDFRAKT                               
030100     MOVE KORD-KDORDKL       TO E52-KDORDKL                               
030200     MOVE KORD-KVORDRAD-PACK TO E52-KVORDRAD-PACK                         
030300     MOVE KORD-TIUTSKR       TO E52-TIUTSKR                               
030400     MOVE ZERO               TO E52-TIREF1                                
030500     MOVE KORD-TIORDREG      TO E52-TIORDREG                              
030600     IF SEGMENT-FINNS                                                     
030700         MOVE ODEL-DARFS (3:6)   TO E52-TIBEGPAC                          
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 WDE401-SKAPA-E54       SECTION.                                          
031200                                                                          
031300     MOVE KORD-IDDISTR        TO E54-IDDISTR                              
031400     MOVE KORD-IDKUNDNR       TO E54-IDKUNDNR                             
031500     MOVE KORD-IDKUNDRF       TO E54-IDKUNDRF                             
031600     MOVE KORD-IDDC           TO E54-IDDC                                 
031700                                 W-WDQ301-IDDC                            
031800     MOVE KORD-KDFAKTYP       TO E54-KDFAKTYP                             
031900     MOVE KORD-KDFRAKT        TO E54-KDFRAKT                              
032000     MOVE KORD-KDORDKL        TO E54-KDORDKL                              
032100     MOVE KORD-TIORDREG       TO E54-TIORDREG                             
032200     MOVE KORD-IDUSER         TO E54-IDUSER-PACK                          
032300     MOVE KORD-KDPERSON       TO E54-KDPERSON                             
032400                                                                          
032500     MOVE KORD-IDORDER        TO W-WDQ301-IDORDER                         
032600     MOVE KORD-IDPRODNR       TO W-WDQ301-IDPRODNR                        
032700     MOVE KORD-IDPLKLST       TO W-WDQ301-IDPLKLST                        
032800     PERFORM IMS-GU-WDQ301                                                
032900                                                                          
033000     IF SEGMENT-FINNS                                                     
033100       MOVE ODEL-IDBORD       TO E54-IDBORD                               
033200       MOVE ODEL-IDPRC        TO E54-IDPRC                                
033300     ELSE                                                                 
033400       MOVE SPACE             TO E54-IDBORD                               
033500                                 E54-IDPRC                                
033600     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 WDE401-SKAPA-E5C       SECTION.                                          
034000                                                                          
034100     MOVE KORD-IDDISTR       TO E5C-IDDISTR                               
034200     MOVE KORD-IDKUNDNR      TO E5C-IDKUNDNR                              
034300     MOVE KORD-IDKUNDRF      TO E5C-IDKUNDRF                              
034400     MOVE KORD-IDDC          TO E5C-IDDC                                  
034500     MOVE KORD-KDFAKTYP      TO E5C-KDFAKTYP                              
034600     MOVE KORD-KDFRAKT       TO E5C-KDFRAKT                               
034700     MOVE KORD-KDORDKL       TO E5C-KDORDKL                               
034800     MOVE KORD-KVORDRAD-PACK TO E5C-KVORDRAD-PACK                         
034900     MOVE KORD-TIUTSKR       TO E5C-TIUTSKR                               
035000     MOVE KORD-TIORDREG      TO E5C-TIORDREG                              
035100     IF SEGMENT-FINNS                                                     
035200        MOVE ODEL-DARFS (3:6)   TO E5C-TIBEGPAC                           
035300     END-IF                                                               
035400     .                                                                    
035500     EJECT                                                                
035600 WDE401-SKAPA-001       SECTION.                                          
035700                                                                          
035800     MOVE '001'               TO ORDER-IDPTYP                             
035900     MOVE KORD-IDDISTR        TO ORDER-IDDISTR                            
036000                                 DIS130-IDDISTR                           
036100                                 DIS1-IDDISTR                             
036200                                                                          
036300     CALL W460DIS1 USING DIS1-W460DIS1                                    
036400                                                                          
036500     MOVE KORD-IDKUNDNR       TO ORDER-IDKUNDNR                           
036600     MOVE KORD-IDKUNDRF       TO WS-IDKUNDRF-X                            
036700                                                                          
036800     IF WS-IDKUNDRF NOT NUMERIC                                           
036900       MOVE ZERO              TO WS-IDKUNDRF                              
037000     END-IF                                                               
037100                                                                          
037200     MOVE WS-IDKUNDRF         TO SPARAT-IDORDNR-001                       
037300     MOVE KORD-TIORDREG       TO ORDER-TIORDREG                           
037400     MOVE ZERO                TO SKRIVNA-POSTER                           
037500     .                                                                    
037600     EJECT                                                                
037700 WDE401-SKAPA-A02       SECTION.                                          
037800                                                                          
037900     MOVE 'A02'               TO A02-IDPTYP                               
038000     MOVE KORD-IDDISTR        TO A02-IDDISTR                              
038100     MOVE KORD-IDDC           TO A02-IDDC                                 
038200     MOVE KORD-IDKUNDNR       TO A02-IDKUNDNR                             
038300     MOVE KORD-IDKUNDRF       TO A02-IDKUNDRF                             
038400     MOVE KORD-TIORDREG       TO A02-TIORDREG                             
038500                                                                          
038600     IF KORD-IDORDER > +0                                                 
038700       MOVE KORD-IDORDER        TO W-IDORDER                              
038800       PERFORM IMS-GU-WDQ201                                              
038900       IF SEGMENT-FINNS                                                   
039000         MOVE OHUV-IDUSER       TO A02-IDUSER                             
039100                                   E54-IDUSER-OREG                        
039200         MOVE OHUV-BEVARREF     TO A02-BEVARREF                           
039300         MOVE OHUV-BEGMT        TO A02-BEGMT                              
039400         MOVE OHUV-ADGMT        TO A02-ADGMT                              
039500         MOVE OHUV-BELAGINS-GRP TO A02-BELAGINS-GRP                       
039600       ELSE                                                               
039700         MOVE 'PC12345 '        TO A02-IDUSER                             
039800                                   E54-IDUSER-OREG                        
039900       END-IF                                                             
040000     END-IF                                                               
040100                                                                          
040200     MOVE ZERO                TO SPARAT-IDPRODNR-1-A02                    
040300                                 SPARAT-IDPRODNR-2-A02                    
040400                                                                          
040500     PERFORM IMS-GU-WDE601                                                
040600     IF SEGMENT-FINNS                                                     
040700       MOVE VORD-BEGMRK     TO A02-BEGMRK                                 
040800*--                                                                       
040900*      STUDS-FLÖDET:2-A FAKT.MÅSTE HA STUDS-DC'T INTE LEV.DC              
041100       IF VORD-IDDC-EXP > SPACE                                           
041110         IF VORD-IDDC-EXP = WC-CDC-SE                                     
041200           MOVE VORD-IDDC-EXP TO E54-IDDC                                 
041300         END-IF                                                           
041310       END-IF                                                             
041400     END-IF                                                               
041500     .                                                                    
041600     EJECT                                                                
041700 WDE401-SKAPA-A21       SECTION.                                          
041800                                                                          
041900     IF FL-SKRIV-A21 = JA                                                 
042000       MOVE ZERO                TO A21-IDKOLLI                            
042100       MOVE SPACE               TO A21-BEART-SVE                          
042200                                                                          
042300       WRITE A21-AREA  FROM A21-W479A21                                   
042400                                                                          
042500       MOVE 'W47958'     TO POSTSUM-FDNAMN                                
042600       MOVE 'W47950D6'   TO POSTSUM-DDNAMN2                               
042700       MOVE 'E58'        TO POSTSUM-TRANSTYP                              
042800                                                                          
042900       CALL POSTSUM USING POSTSUM-PARM                                    
043000     END-IF                                                               
043100                                                                          
043200     MOVE NEJ                 TO FL-SKRIV-A21                             
043300                                                                          
043400     MOVE 'A21'               TO A21-IDPTYP                               
043500     MOVE KORD-IDDISTR        TO A21-IDDISTR                              
043600     MOVE KORD-IDKUNDNR       TO A21-IDKUNDNR                             
043700     MOVE KORD-IDKUNDRF       TO A21-IDKUNDRF                             
043800     MOVE KORD-IDDC           TO A21-IDDC                                 
043900                                                                          
044000     IF E52-KDORDKL = 5                                                   
044100       MOVE KORD-IDARTNR-SATS TO E52-IDARTNR-ERS                          
044200     END-IF                                                               
044300                                                                          
044400     IF E54-KDORDKL = 5                                                   
044500       MOVE KORD-IDARTNR-SATS TO E54-IDARTNR-SATS                         
044600     ELSE                                                                 
044700       MOVE ZERO              TO E54-IDARTNR-SATS                         
044800     END-IF                                                               
044900     .                                                                    
045000     EJECT                                                                
045100 WDE411-SKRIV-E52       SECTION.                                          
045200                                                                          
045300     MOVE ORAD-IDPURAD      TO E52-IDPURAD                                
045400     MOVE ORAD-KDRADSTA     TO E52-KDRADSTA                               
045500     MOVE ORAD-IDARTNR      TO E52-IDARTNR                                
045600     MOVE ORAD-REKSIFFR     TO E52-REKSIFFR                               
045700                                                                          
045800     IF ORAD-FLTILLK = JA                                                 
045900       MOVE '1'             TO E52-IDARTNR-ERS                            
046000     ELSE                                                                 
046100       MOVE  ZERO           TO E52-IDARTNR-ERS                            
046200     END-IF                                                               
046300                                                                          
046400     MOVE ORAD-ADLEVPL          TO E52-ADLEVPL                            
046500     MOVE ORAD-BERADREF         TO E52-BERADREF                           
046600     MOVE SPACE                 TO E52-FLKRED                             
046700     MOVE ORAD-FLDIRLEV         TO E52-FLDIRLEV                           
046800     MOVE ORAD-FLRESTN          TO E52-FLRESTN                            
046900     MOVE ORAD-IDPRODNR         TO E52-IDPRODNR                           
047000     MOVE ORAD-IDKUNDRF-RO      TO E52-IDKUNDRF-RO                        
047100     MOVE ORAD-IDLOPNR-RO       TO E52-IDLOPNR                            
047200     MOVE ORAD-KDFRAKT          TO E52-KDFRAKT                            
047300     MOVE ORAD-KDORDTYP         TO E52-KDORDTYP                           
047400     MOVE ORAD-KDPRODSL         TO E52-KDPRODSL                           
047500     MOVE ORAD-IDKONTO          TO E52-IDKONTO                            
047600     MOVE ORAD-IDKST            TO E52-IDKST                              
047700     MOVE ORAD-KVAVBART         TO E52-KVAVBART                           
047800     MOVE ORAD-KVLEVART         TO E52-KVLEVART                           
047900     MOVE ORAD-KVBEART          TO E52-KVBEART                            
048000     MOVE ZERO                  TO E52-KVEFRS-PACK                        
048100                                   E52-KVEFRS-OPACK                       
048200                                   E52-KVEFRS-SKEPP                       
048300                                   E52-IDDIVORD                           
048400     MOVE ORAD-PRARTNTO         TO E52-PRARTNTO                           
048500     MOVE ORAD-PRARTNTO-LOC     TO E52-PRARTNTO-LOC                       
048600     MOVE ORAD-PRARTNTO-LOCPREL TO E52-PRARTNTO-LOCPREL                   
048700     MOVE ORAD-KDVALISO         TO E52-KDVALISO                           
048800     MOVE ORAD-TIRODAT          TO E52-TIRODAT                            
048900     MOVE ORAD-TIUTSKR          TO E52-TIUTSKR                            
049000     MOVE ORAD-VKARTNTO         TO E52-VKARTNTO                           
049100     MOVE ORAD-VLARTNTO         TO E52-VLARTNTO                           
049200     MOVE ORAD-KDKVBRYT         TO E52-KDKVBRYT                           
049300     MOVE ORAD-BEVOLREF         TO E52-BEVOLREF                           
049400     MOVE 'E52'                 TO E52-IDPTYP                             
049500                                                                          
049600     WRITE E52-AREA  FROM E52-W479052                                     
049700                                                                          
049800     MOVE 'W47952'     TO POSTSUM-FDNAMN                                  
049900     MOVE 'W47950D3'   TO POSTSUM-DDNAMN2                                 
050000     MOVE 'E52'        TO POSTSUM-TRANSTYP                                
050100                                                                          
050200     CALL POSTSUM USING POSTSUM-PARM                                      
050300     .                                                                    
050400     EJECT                                                                
050500 WDE411-SKRIV-E54       SECTION.                                          
050600                                                                          
050700     MOVE ORAD-IDPURAD          TO E54-IDRADNR-KO                         
050800     MOVE ORAD-IDPRODNR         TO E54-IDPRODNR                           
050900     MOVE ORAD-IDARTNR          TO E54-IDARTNR                            
051000     MOVE ZERO                  TO E54-FLKRED                             
051100                                   E54-IDDIVORD                           
051200     MOVE ORAD-FLDIRLEV         TO E54-FLDIRLEV                           
051300     MOVE ORAD-IDKUNDRF-RO      TO E54-IDKUNDRF-RO                        
051400     MOVE ORAD-KDORDTYP         TO E54-KDORDTYP                           
051500     MOVE ORAD-KDPRODSL         TO E54-KDPRODSL                           
051600     MOVE ORAD-IDKONTO          TO E54-IDKONTO                            
051700     MOVE ORAD-IDKST            TO E54-IDKST                              
051800     MOVE ORAD-IDANALYS         TO E54-IDANALYS                           
051900     MOVE ORAD-KVAVBART         TO E54-KVAVBART                           
052000     MOVE ORAD-KVBEART          TO E54-KVBEART                            
052100     MOVE ORAD-KVLEVART         TO E54-KVLEVART                           
052200     MOVE ORAD-PRARTNTO         TO E54-PRARTNTO                           
052300     MOVE ORAD-PRARTNTO-LOC     TO E54-PRARTNTO-LOC                       
052400     MOVE ORAD-PRARTNTO-LOCPREL TO E54-PRARTNTO-LOCPREL                   
052500     MOVE ORAD-KDVALISO         TO E54-KDVALISO                           
052600     MOVE ORAD-VKARTNTO         TO E54-VKARTNTO                           
052700     MOVE ORAD-VLARTNTO         TO E54-VLARTNTO                           
052800     MOVE ORAD-FLPRTILL         TO E54-FLPRTILL                           
052900     MOVE ORAD-KDRADSTA         TO E54-KDRADSTA                           
053000     MOVE ORAD-KDARTURS         TO E54-KDARTURS                           
053100     MOVE ORAD-IDLEVNR          TO E54-IDLEVNR                            
053200     MOVE ORAD-KDVAT            TO E54-KDVAT                              
053300     MOVE ORAD-BEART-VIPS       TO E54-BEART-VIPS                         
053400     MOVE ORAD-ADLAGOMR         TO E54-ADLAGOMR                           
053500     MOVE 'E54'                 TO E54-IDPTYP                             
053600                                                                          
053700     WRITE E54-AREA  FROM E54-W479054                                     
053800                                                                          
053900     MOVE 'W47954'     TO POSTSUM-FDNAMN                                  
054000     MOVE 'W47950D4'   TO POSTSUM-DDNAMN2                                 
054100     MOVE 'E54'        TO POSTSUM-TRANSTYP                                
054200                                                                          
054300     CALL POSTSUM USING POSTSUM-PARM                                      
054400     .                                                                    
054500     EJECT                                                                
054600 WDE411-SKRIV-E5C       SECTION.                                          
054700                                                                          
054800     MOVE ORAD-IDPURAD      TO E5C-IDPURAD                                
054900     MOVE ORAD-KDRADSTA     TO E5C-KDRADSTA                               
055000     MOVE ORAD-IDARTNR      TO E5C-IDARTNR                                
055100     MOVE ORAD-REKSIFFR     TO E5C-REKSIFFR                               
055200                                                                          
055300     IF ORAD-FLTILLK = JA                                                 
055400       MOVE '1'             TO E5C-IDARTNR-ERS                            
055500     ELSE                                                                 
055600       MOVE  ZERO           TO E5C-IDARTNR-ERS                            
055700     END-IF                                                               
055800                                                                          
055900     MOVE ORAD-ADLEVPL          TO E5C-ADLEVPL                            
056000     MOVE ORAD-FLFYSAVV         TO E5C-FLFYSAVV                           
056100     MOVE ORAD-FLDIRLEV         TO E5C-FLDIRLEV                           
056200     MOVE ORAD-IDPRODNR         TO E5C-IDPRODNR                           
056300     MOVE ORAD-IDKUNDRF-RO      TO E5C-IDKUNDRF-RO                        
056400     MOVE ORAD-IDLOPNR-RO       TO E5C-IDLOPNR                            
056500     MOVE ORAD-KDFRAKT          TO E5C-KDFRAKT                            
056600     MOVE ORAD-KDORDTYP         TO E5C-KDORDTYP                           
056700     MOVE ORAD-KVAVBART         TO E5C-KVAVBART                           
056800     MOVE ORAD-KVLEVART         TO E5C-KVLEVART                           
056900     MOVE ORAD-KVBEART          TO E5C-KVBEART                            
057000     MOVE ORAD-TIRODAT          TO E5C-TIRODAT                            
057100     MOVE ORAD-TIUTSKR          TO E5C-TIUTSKR                            
057200     MOVE ORAD-KDKVBRYT         TO E5C-KDKVBRYT                           
057300     MOVE ORAD-BEVOLREF         TO E5C-BEVOLREF                           
057400     MOVE 'E5C'                 TO E5C-IDPTYP                             
057500                                                                          
057600     WRITE E5C-AREA  FROM E5C-W47905C                                     
057700                                                                          
057800     MOVE 'W4795C'     TO POSTSUM-FDNAMN                                  
057900     MOVE 'W47950D8'   TO POSTSUM-DDNAMN2                                 
058000     MOVE 'E5C'        TO POSTSUM-TRANSTYP                                
058100                                                                          
058200     CALL POSTSUM USING POSTSUM-PARM                                      
058300     .                                                                    
058400     EJECT                                                                
058500 WDE411-SKRIV-001       SECTION.                                          
058600     IF (DIS1-KDSVAR = JA OR DIS130-NOAC)                                 
058700       MOVE ORAD-IDKUNDRF-RO    TO WS-IDKUNDRF-X                          
058800                                                                          
058900       IF WS-IDKUNDRF NOT NUMERIC                                         
059000         MOVE ZERO              TO WS-IDKUNDRF                            
059100       END-IF                                                             
059200                                                                          
059300       IF WS-IDKUNDRF = ZERO                                              
059400         IF SKRIVNA-POSTER NOT > 1 AND                                    
059500            ORAD-IDPRODNR NOT = SPARAT-IDPRODNR-001                       
059600                                                                          
059700           MOVE ORAD-IDPRODNR      TO ORDER-IDPRODNR                      
059800           MOVE SPARAT-IDORDNR-001 TO ORDER-IDORDNR                       
059900           PERFORM S01-SKRIV-001                                          
060000           ADD 1 TO SKRIVNA-POSTER                                        
060100                                                                          
060200           IF SKRIVNA-POSTER = 1                                          
060300             MOVE ORAD-IDPRODNR TO SPARAT-IDPRODNR-001                    
060400           END-IF                                                         
060500         END-IF                                                           
060600       ELSE                                                               
060700         MOVE ORAD-IDPRODNR     TO ORDER-IDPRODNR                         
060800         MOVE WS-IDKUNDRF       TO ORDER-IDORDNR                          
060900         PERFORM S01-SKRIV-001                                            
061000       END-IF                                                             
061100     END-IF                                                               
061200     .                                                                    
061300 S01-SKRIV-001            SECTION.                                        
061400                                                                          
061500         WRITE AREA-001  FROM ORDER-W462001                               
061600                                                                          
061700         MOVE 'W47956'          TO POSTSUM-FDNAMN                         
061800         MOVE 'W47950D5'        TO POSTSUM-DDNAMN2                        
061900         MOVE '001'             TO POSTSUM-TRANSTYP                       
062000                                                                          
062100         CALL POSTSUM USING POSTSUM-PARM                                  
062200     .                                                                    
062300     EJECT                                                                
062400 WDE411-SKRIV-A02       SECTION.                                          
062500                                                                          
062600     IF ORAD-IDPRODNR NOT = SPARAT-IDPRODNR-1-A02   AND                   
062700                            SPARAT-IDPRODNR-2-A02                         
062800       MOVE ORAD-IDPRODNR       TO A02-IDPRODNR                           
062900                                                                          
063000       MOVE ZERO                TO A02-IDKOLLI                            
063100                                   A02-IDARTNR                            
063200                                   A02-IDPURAD                            
063300                                   A02-KDFRAKT                            
063400                                   A02-KDORDKL                            
063500                                   A02-TIBEGPAC                           
063600                                   A02-KDPERSON                           
063700                                   A02-IDLOTNR                            
063800                                   A02-VLORDNTO                           
063900                                   A02-VKORDNTO                           
064000                                   A02-KVORDRAD                           
064100       MOVE SPACE               TO A02-KDORDLOT                           
064200                                                                          
064300       WRITE A02-AREA  FROM A02-W479A01                                   
064400                                                                          
064500       MOVE 'W47959'            TO POSTSUM-FDNAMN                         
064600       MOVE 'W47950D7'          TO POSTSUM-DDNAMN2                        
064700       MOVE 'E59'               TO POSTSUM-TRANSTYP                       
064800                                                                          
064900       CALL POSTSUM USING POSTSUM-PARM                                    
065000                                                                          
065100       IF SPARAT-IDPRODNR-1-A02 = ZERO                                    
065200         MOVE ORAD-IDPRODNR     TO SPARAT-IDPRODNR-1-A02                  
065300       ELSE                                                               
065400         MOVE ORAD-IDPRODNR     TO SPARAT-IDPRODNR-2-A02                  
065500       END-IF                                                             
065600     END-IF                                                               
065700     .                                                                    
065800     EJECT                                                                
065900 WDE411-SKAPA-A21       SECTION.                                          
066000                                                                          
066100     IF FL-SKRIV-A21 = JA                                                 
066200       MOVE ZERO                TO A21-IDKOLLI                            
066300       MOVE SPACE               TO A21-BEART-SVE                          
066400                                                                          
066500       WRITE A21-AREA  FROM A21-W479A21                                   
066600                                                                          
066700       MOVE 'W47958'     TO POSTSUM-FDNAMN                                
066800       MOVE 'W47950D6'   TO POSTSUM-DDNAMN2                               
066900       MOVE 'E58'        TO POSTSUM-TRANSTYP                              
067000                                                                          
067100       CALL POSTSUM USING POSTSUM-PARM                                    
067200     END-IF                                                               
067300                                                                          
067400     MOVE JA                  TO FL-SKRIV-A21                             
067500                                                                          
067600     MOVE ORAD-IDPRODNR       TO A21-IDPRODNR                             
067700     MOVE ORAD-IDPURAD        TO A21-IDPURAD                              
067800     MOVE ORAD-IDARTNR        TO A21-IDARTNR                              
067900     MOVE ORAD-REKSIFFR       TO A21-REKSIFFR                             
068000     MOVE ORAD-KDARTURS       TO A21-KDARTURS                             
068100     MOVE ORAD-KDRADSTA       TO A21-KDRADSTA                             
068200     MOVE ORAD-IDKUNDRF-RO    TO A21-IDKUNDRF-RO                          
068300     MOVE ORAD-KVBEART        TO A21-KVBEART                              
068400     MOVE ORAD-KVLEVART       TO A21-KVLEVART                             
068500     MOVE ORAD-KVANNANT       TO A21-KVANNANT                             
068600     MOVE ORAD-KVAVBART       TO A21-KVAVBART                             
068700     .                                                                    
068800     EJECT                                                                
068900 WDE421-SKRIV-A21       SECTION.                                          
069000                                                                          
069100     MOVE KKOLLI-IDKOLLI      TO A21-IDKOLLI                              
069200     MOVE KKOLLI-KVLEVART     TO A21-KVLEVART                             
069300                                                                          
069400     MOVE SPACE               TO A21-BEART-SVE                            
069500                                                                          
069600     WRITE A21-AREA  FROM A21-W479A21                                     
069700                                                                          
069800     MOVE 'W47958'     TO POSTSUM-FDNAMN                                  
069900     MOVE 'W47950D6'   TO POSTSUM-DDNAMN2                                 
070000     MOVE 'E58'        TO POSTSUM-TRANSTYP                                
070100                                                                          
070200     CALL POSTSUM USING POSTSUM-PARM                                      
070300                                                                          
070400     MOVE NEJ                 TO FL-SKRIV-A21                             
070500     .                                                                    
070600     EJECT                                                                
070700 SKRIV-A21       SECTION.                                                 
070800                                                                          
070900     MOVE ZERO                TO A21-IDKOLLI                              
071000     MOVE SPACE               TO A21-BEART-SVE                            
071100                                                                          
071200     WRITE A21-AREA  FROM A21-W479A21                                     
071300                                                                          
071400     MOVE 'W47958'     TO POSTSUM-FDNAMN                                  
071500     MOVE 'W47950D6'   TO POSTSUM-DDNAMN2                                 
071600     MOVE 'E58'        TO POSTSUM-TRANSTYP                                
071700                                                                          
071800     CALL POSTSUM USING POSTSUM-PARM                                      
071900     .                                                                    
072000     EJECT                                                                
072100 Z-FINIT  SECTION.                                                        
072200                                                                          
072300     CLOSE W47951                                                         
072400           W47952                                                         
072500           W47954                                                         
072600           W47956                                                         
072700           W47958                                                         
072800           W47959                                                         
072900           W4795C                                                         
073000                                                                          
073100     MOVE 'S'          TO POSTSUM-OPKOD                                   
073200                                                                          
073300     CALL POSTSUM USING POSTSUM-PARM                                      
073400     .                                                                    
073500     EJECT                                                                
073600                                                                          
073700*         * I M S  S E C T I O N                                          
073800                                                                          
073900                                                                          
074000 IMS-GN-WDE4         SECTION.                                             
074100                                                                          
074200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
074300     CALL CBLTDLI USING GN WDE4-PCB IO-AREA                               
074400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
074500     PERFORM IMS-STATUSKONTROLL.                                          
074600     SKIP3                                                                
074700 IMS-GU-WDE601            SECTION.                                        
074800                                                                          
074900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
075000          DELIMITED BY SIZE INTO SSA1                                     
075100     MOVE '  GE' TO GODK-STATUSKODER                                      
075200     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-WDE601 SSA1               
075300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
075400     PERFORM IMS-STATUSKONTROLL.                                          
075500     SKIP3                                                                
075600 IMS-GU-WDQ201            SECTION.                                        
075700                                                                          
075800     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
075900          DELIMITED BY SIZE INTO SSA1                                     
076000     MOVE '  GE'               TO GODK-STATUSKODER                        
076100     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-WDQ201 SSA1               
076200     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
076300     PERFORM IMS-STATUSKONTROLL.                                          
076400     SKIP3                                                                
076500 IMS-GU-WDQ301   SECTION.                                                 
076600     STRING 'WDQ301  (WDQ301KY =' W-WDQ301-KEY-X ')'                      
076700            DELIMITED BY SIZE INTO SSA1                                   
076800     MOVE '  GE' TO GODK-STATUSKODER                                      
076900     CALL CBLTDLI USING GU    WDQ3-PCB DLI-IO-WDQ301 SSA1                 
077000     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
077100     PERFORM IMS-STATUSKONTROLL                                           
077200     .                                                                    
077300     SKIP3                                                                
077400 IMS-STATUSKONTROLL       SECTION.                                        
077500                                                                          
077600     SET STATUS-IX TO 1                                                   
077700     SEARCH GODK-STATUS AT END CALL FELLOG                                
077800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
077900     END-SEARCH.                                                          
