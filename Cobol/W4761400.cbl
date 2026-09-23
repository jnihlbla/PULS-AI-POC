000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4761400.                                                 
000300*AUTHOR.        STINA MOGREN.                                             
000400*DATE-WRITTEN.  AUG 2002.                                                 
000500*                                                                         
000600*                                                                         
000700*    REMARKS.   VIPS INFO IMPORTER                                        
000800*    FUNKTION:  DELAR UPP INFILEN PER LAND, SKRIVER POSTER                
000900*               MOT VCOM TILL RESP LAND GENOM WZ01                        
001000*                                                                         
001100*    ABENDKODER:                                                          
001200*                                                                         
001300*        U0016    - OM RETURKOD FRÅN SORT                                 
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*- - - - - - - - - - - - INFIL:                                           
002200     SELECT INFIL                        ASSIGN TO UT-S-W47614D1.         
002300*- - - - - - - - - - - - OUT FILE                                         
002400     SELECT W4766C                       ASSIGN TO UT-S-W47614D2.         
002500     SKIP2                                                                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  INFIL                                                                
003200     RECORDING       V                                                    
003300     BLOCK CONTAINS 0.                                                    
003400     SKIP2                                                                
003500*01  FILLER -COPY W461RIO2   -L.                                          
003600     SKIP2                                                                
003700                                                                          
003800 FD  W4766C                                                               
003900     RECORDING       V                                                    
004000     BLOCK CONTAINS 0.                                                    
004100 01  W47660-001                  PIC X(80).                               
004200*01  RI0-POST -COPY W461RI0N      -PRE UT-    -L.                         
004300*01  RIK-POST -COPY W461RIK1      -PRE UT-    -L.                         
004400*01  RIL-POST -COPY W461RILN      -PRE UT-    -L.                         
004500*01  RIM-POST -COPY W461RIM2      -PRE UT-    -L.                         
004600*01  RIN-POST -COPY W461RINN      -PRE UT-    -L.                         
004700*01  RIO-POST -COPY W461RIO2      -PRE UT-    -L.                         
004800*01  RIP-POST -COPY W461RIPN      -PRE UT-    -L.                         
004900*01  RI9-POST -COPY W461RI9       -PRE UT-    -L.                         
005000                                                                          
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
005500 77  IDPGM                       PIC X(8)    VALUE 'W4761400'.            
005600     SKIP2                                                                
005700*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
005800                                                                          
005900 77  JA                          PIC X(1)    VALUE 'J'.                   
006000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006100 77  YES                         PIC X(1)    VALUE 'Y'.                   
006200                                                                          
006300*- - - - - - - - - - - - - -  TVÅ-STÄLLIGA ISO-KODER                      
006400*01  -COPY W460LISO                                                       
006500     EJECT                                                                
006600*- - - - - - - - - - - - - -  SWITCHAR                                    
006700 77  FL-GODKEND-KDFAKTYP         PIC X(1).                                
006800 77  LAGRAD-KDLIDEL              PIC S9(1)   VALUE +9.                    
006900 77  W-IDLOPNRE                  PIC S9(3).                               
007000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
007100 77  KDRC-DISPLAY                PIC Z(5).                                
007200 77  W-LENGD                     PIC S9(4)  COMP.                         
007300 77  WS-ANTALPOSTER              PIC 9(7).                                
007400 77  WS-IDDC                     PIC X(2) VALUE SPACE.                    
007500                                                                          
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008000     SKIP2                                                                
008100 77  NY-FAKTURA-SW               PIC X   VALUE 'N'.                       
008200     88  NY-FAKTURA                      VALUE 'J'.                       
008300                                                                          
008400 77  W-TIHHMMSS                  PIC 9(6)  VALUE ZERO.                    
008500 01  WY-TIHHMMSS                 PIC 9(6)  VALUE ZERO.                    
008600 01  FILLER                      REDEFINES WY-TIHHMMSS.                   
008700     03  WY-HH                   PIC 9(2).                                
008800     03  WY-MM                   PIC 9(2).                                
008900     03  WY-SS                   PIC 9(2).                                
009000                                                                          
009100 01  WX                          PIC S9(3) VALUE ZERO COMP-3.             
009200                                                                          
009300 01  W-TIDER.                                                             
009400    03  FILLER                   OCCURS 30.                               
009500      05  WX-IDDISTR             PIC 9(5)  VALUE ZERO.                    
009600      05  WX-TIHHMMSS            PIC 9(6)  VALUE ZERO.                    
009700                                                                          
009800 01  WORK-FIELDS.                                                         
009900    03  WS-IDDISTR                  PIC S9(5) VALUE ZERO  COMP-3.         
010000    03  W-VKORDBTO-ORDER-LB         PIC S9(8)V9(1) COMP-3                 
010100                                                   VALUE ZERO.            
010200    03  SPAR-IDPTYP                 PIC X(3)  VALUE SPACE.                
010300    03  SPAR-IDORDNR                PIC S9(7) COMP-3.                     
010400    03  SPAR-BEVOLREF               PIC X(10).                            
010500    03  SPAR-IDDC                   PIC X(2).                             
010600    03  SPAR-IDDISTR                PIC S9(5) VALUE ZERO  COMP-3.         
010700    03 WS-ADDISPABS.                                                      
010800       05 WS-ADDISPABS-START        PIC X(14)  VALUE                      
010900                                         'CARPARTS.VIPS.'.                
011000       05 WS-ADDISPABS-IDLAND       PIC X(02) VALUE SPACE.                
011100       05 WS-ADDISPABS-SLUT         PIC X(10)  VALUE                      
011200                                         'CODRIKINFO'.                    
011300       05 FILLER                    PIC X(24) VALUE SPACE.                
011400                                                                          
011500    03 WS-SPAR-IDDISTR              PIC 9(4)  VALUE ZERO.                 
011600                                                                          
011700 01 WS-MQ-HDR.                                                            
011800    03 WS-MQ-PROP                   PIC X(16) VALUE                       
011900                                          '¤MQMPROP Market='.             
012000    03 WS-MARKET                    PIC X(02).                            
012100                                                                          
012200 77 WS-SEND-MQ-SW                   PIC X(01) VALUE 'N'.                  
012300    88 SEND-MQ                                VALUE 'Y'.                  
012400                                                                          
012500 01  WORK-DISP.                                                           
012600     03  WORK-TEXT                  PIC X(19)  VALUE SPACES.              
012700     03  FILLER                     REDEFINES WORK-TEXT.                  
012800       05  WORK-IDDISTR             PIC 9(4).                             
012900       05  FILLER                   PIC X.                                
013000       05  WORK-IDKUNDNR            PIC 9(6).                             
013100       05  FILLER                   PIC X.                                
013200       05  WORK-IDFAKT              PIC 9(7).                             
013300*      --- VALID IDDC CODES                                               
013400*                                                                         
013500     EJECT                                                                
013600     EJECT                                                                
013700 01  WS-KDVALUTA                 PIC 9(3)  VALUE ZERO.                    
013800 01  SPAR-RIM-IDORDNR            PIC 9(7)  VALUE ZERO.                    
013900 01  SPAR-RIM-IDKUNDNR           PIC 9(6)  VALUE ZERO.                    
014000     SKIP2                                                                
014100 01  SPAR-RIL-IDPTYPA            PIC X(3)  VALUE SPACE.                   
014200 01  -COPY W461RILN -PRE SPAR-                                            
014300                                                                          
014400 01  SPAR-IDTRPTNR               PIC 9(3)       VALUE ZERO.               
014500 01  SPAR-IDLBBET                PIC X(12)      VALUE SPACE.              
014600                                                                          
014700 01  WS-TIAAMMDD                 PIC 9(6).                                
014800 01  WS-DELAD-TIAAMMDD  REDEFINES WS-TIAAMMDD.                            
014900     03  FILLER                  PIC 9(2).                                
015000     03  WS-TIMM                 PIC 9(2).                                
015100     03  WS-TIDD                 PIC 9(2).                                
015200     EJECT                                                                
015300 01  TEST-IDDISTR                PIC 9(5) COMP-3.                         
015400*01  FILLER -COPY WWDIS102 -RED TEST-IDDISTR.                             
015500     SKIP3                                                                
015600*01  FILLER -COPY WWDIS121 -RED TEST-IDDISTR.                             
015700     SKIP3                                                                
015800 01  DYNAMISKA-SUBPROGRAM.                                                
015900   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
016000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG'.              
016100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
016200   03  W460DIS1                  PIC X(8)    VALUE 'W460DIS1'.            
016300   03  W400ARTU                  PIC X(8)    VALUE 'W400ARTU'.            
016400   03  WZ01SEND                  PIC X(8)    VALUE 'WZ01SEND'.            
016500   03  WISOLAND                  PIC X(8)    VALUE 'WISOLAND'.            
016600     SKIP3                                                                
016700*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
016800                                                                          
016900 01  RETURKODER.                                                          
017000   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
017100   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
017200   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
017300   03  RKOD-ABEND-WITH-DUMP      PIC S9(4)  COMP SYNC VALUE +1000.        
017400     EJECT                                                                
017500*- - - - - - - - - - - - - -  INDEX ETC.                                  
017600     SKIP2                                                                
017700 01  IX-STATNR             PIC S9(3) COMP-3.                              
017800 01  IX-RAD                PIC S9(5) COMP-3.                              
017900 01  IX-KOLUMN             PIC S9(5) COMP-3.                              
018000 01  KDHBLKRV              PIC S9(5) COMP-3.                              
018100     EJECT                                                                
018200*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
018300                                                                          
018400*01  -COPY W0005       -PRE  POSTSUM-.                                    
018500     EJECT                                                                
018600*- - - - - - - - - - - - - -  PARAMETRAR TILL W460DIS1                    
018700                                                                          
018800*01  -COPY W460DIS1                                                       
018900     EJECT                                                                
019000*- - - - - - - - - - - - - -  PARAMETRAR TILL W440ARTU                    
019100                                                                          
019200*01  -COPY W400ARTU                                                       
019300     EJECT                                                                
019400*                                                                         
019500*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
019800     SKIP3                                                                
019900*01 -COPY WISOLAND                                                        
020000                                                                          
020100*01  -COPY WWDCLAND                                                       
020200                                                                          
020300*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
020400*01 -COPY WWOMVAND                                                        
020500     EJECT                                                                
020600*    --- PARAMETRAR TILL W930VAL                                          
020700*01 -COPY W930VAL                                                         
020800*    --- AREOR FÖR ANROP TILL WZ01  ------                                
020900 01  FILLER                      PIC X(16)   VALUE 'WZ01-AREA'.           
021000*01  -COPY WZ01SEND                                                       
021100                                                                          
021200 01  FILLER                      PIC X(16) VALUE 'UT-AREA-RIM'.           
021300 01  W-UTAREA-RIM.                                                        
021400*                                                                         
021500*03  FILLER  -COPY W461RIM2        -PRE UT-                               
021600*                                                                         
021700     EJECT                                                                
021800******************************************************************        
021900*         SORTERADE POSTER                                       *        
022000******************************************************************        
022100 01  WSORT-AREA.                                                          
022200   03  WSORT-AREA-X          PIC X(211).                                  
022300     SKIP3                                                                
022400*  03  FILLER -COPY W461RIK1   -PRE WSORT- -RED WSORT-AREA-X.             
022500                                                                          
022600*  03  FILLER -COPY W461RIO2   -PRE WSORT- -RED WSORT-AREA-X.             
022700                                                                          
022800*  03  FILLER  -COPY W461RIM2      -PRE T- -RED WSORT-AREA-X              
022900     EJECT                                                                
023000******************************************************************        
023100*    POSTER TILL IMPORTÖR, KORTFORMAT.                           *        
023200******************************************************************        
023300     SKIP2                                                                
023400 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
023500 01  SEND-AREA.                                                           
023600*    03  -COPY WZ01REQU                                                   
023700     03  UT-AREA.                                                         
023800       05 UT-IDPTYP              PIC X(3).                                
023900       05 FILLER                 PIC X(1000).                             
024000*    03 FILLER    -COPY W461RIK1     -RED UT-AREA                         
024100*    03 FILLER    -COPY W461RILN     -RED UT-AREA                         
024200*    03 FILLER    -COPY W461RIM2     -RED UT-AREA                         
024300*    03 FILLER    -COPY W461RINN     -RED UT-AREA                         
024400*    03 FILLER    -COPY W461RIO1     -RED UT-AREA                         
024500*    03 FILLER    -COPY W461RIO2  -PRE SOFT- -RED UT-AREA                 
024600*    03 FILLER    -COPY W461RIPN     -RED UT-AREA                         
024700*    03 FILLER    -COPY W461RIZN     -RED UT-AREA                         
024800*    03 FILLER    -COPY W461RI0N     -RED UT-AREA                         
024900*    03 FILLER    -COPY W461RI9      -RED UT-AREA                         
025000     EJECT                                                                
025100 LINKAGE SECTION.                                                         
025200 01  O2-PCB                      PIC X.                                   
025300                                                                          
025400     EJECT                                                                
025500 PROCEDURE DIVISION  USING O2-PCB.                                        
025600                                                                          
025700                                                                          
025800                                                                          
025900     PERFORM A-INIT                                                       
026000                                                                          
026100     PERFORM B-BEARBETNING                                                
026200     SKIP2                                                                
026300     PERFORM Z-FINIT                                                      
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600                                                                          
026700     .                                                                    
026800     EJECT                                                                
026900 A-INIT SECTION.                                                          
027000     OPEN INPUT INFIL                                                     
027100     OPEN OUTPUT W4766C                                                   
027200     .                                                                    
027300     EJECT                                                                
027400 B-BEARBETNING SECTION.                                                   
027500     SKIP2                                                                
027600     PERFORM S01-LAS-SORTERAD-INFIL                                       
027700     PERFORM UNTIL (SORTFIL-EOF = JA)                                     
027800       MOVE WSORT-RIK-IDDISTR TO WS-SPAR-IDDISTR                          
027900                                 WS-IDDISTR                               
028000       PERFORM S11-LETA-MOTTAGARE                                         
028100                                                                          
028200       IF SEND-MQ                                                         
028300          IF WS-MARKET  NOT = DIS1-IDLANDX2                               
028400             PERFORM S18-WRITE-MQ-HEADER                                  
028500          END-IF                                                          
028600       ELSE                                                               
028700          PERFORM S12-SEND-OPEN                                           
028800       END-IF                                                             
028900                                                                          
029000       PERFORM S15-STARTPOST                                              
029100       MOVE ZERO TO WS-ANTALPOSTER                                        
029200       PERFORM UNTIL    (SORTFIL-EOF = JA)                                
029300                     OR (WS-IDDISTR NOT = WSORT-RIK-IDDISTR               
029400                         AND WSORT-RIK-IDPTYP = 'RIK')                    
029500         MOVE NEJ TO NY-FAKTURA-SW                                        
029600         PERFORM UNTIL    (SORTFIL-EOF = JA)                              
029700                       OR (WS-IDDISTR NOT = WSORT-RIK-IDDISTR             
029800                           AND WSORT-RIK-IDPTYP = 'RIK')                  
029900                       OR (NY-FAKTURA)                                    
030000           COMPUTE WS-ANTALPOSTER = WS-ANTALPOSTER + 1                    
030100           EVALUATE TRUE                                                  
030200           WHEN WSORT-RIO-IDPTYP = 'RIK'                                  
030300             PERFORM BJ-FAKTURA-HUVUD-1                                   
030400           WHEN WSORT-RIO-IDPTYP = 'RIL'                                  
030500             PERFORM BK-FAKTURA-HUVUD-2                                   
030600           WHEN WSORT-RIO-IDPTYP = 'RIM'                                  
030700             PERFORM BL-FAKTURA-REFERENS                                  
030800           WHEN WSORT-RIO-IDPTYP = 'RIN'                                  
030900             PERFORM BM-FAKTURA-KOLLI                                     
031000           WHEN WSORT-RIO-IDPTYP = 'RIO'                                  
031100             PERFORM BN-FAKTURA-RAD-1                                     
031200           WHEN WSORT-RIO-IDPTYP = 'RIP'                                  
031300             PERFORM BP-FAKTURA-RAD-2                                     
031400           END-EVALUATE                                                   
031500           MOVE WSORT-RIO-IDPTYP TO SPAR-IDPTYP                           
031600           PERFORM S01-LAS-SORTERAD-INFIL                                 
031700         END-PERFORM                                                      
031800       END-PERFORM                                                        
031900       PERFORM S16-SLUTPOST                                               
032000                                                                          
032100       IF SEND-MQ                                                         
032200          CONTINUE                                                        
032300       ELSE                                                               
032400          PERFORM S14-SEND-CLOSE                                          
032500       END-IF                                                             
032600                                                                          
032700     END-PERFORM                                                          
032800     .                                                                    
032900 BJ-FAKTURA-HUVUD-1 SECTION.                                              
033000     SKIP2                                                                
033100     MOVE NEJ TO FL-GODKEND-KDFAKTYP                                      
033200     MOVE WSORT-AREA              TO RIK-W461RIK1                         
033300     IF RIK-KDFAKTYP = 'R' OR 'G' OR 'K'                                  
033400       MOVE JA TO FL-GODKEND-KDFAKTYP                                     
033500       MOVE RIK-IDDC              TO SPAR-IDDC                            
033600       MOVE RIK-IDDISTR           TO SPAR-IDDISTR                         
033700                                     WORK-IDDISTR                         
033800       MOVE RIK-IDFAKT            TO WORK-IDFAKT                          
033900                                                                          
034000       PERFORM S13-SEND-MESSAGE                                           
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400                                                                          
034500 BK-FAKTURA-HUVUD-2 SECTION.                                              
034600     SKIP2                                                                
034700     IF FL-GODKEND-KDFAKTYP = JA                                          
034800                                                                          
034900       MOVE SPAR-IDDISTR           TO TEST-IDDISTR                        
035000       IF DIS102-ITALIEN                                                  
035100         MOVE 'RIL'                TO SPAR-RIL-IDPTYPA                    
035200         MOVE WSORT-AREA           TO SPAR-RIL-W461RILN-CTX               
035300       ELSE                                                               
035400         MOVE WSORT-AREA           TO RIL-W461RILN-CTX                    
035500         PERFORM S13-SEND-MESSAGE                                         
035600                                                                          
035700       END-IF                                                             
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 BL-FAKTURA-REFERENS SECTION.                                             
036200** DET SKAPAS TVÅ POSTTYP '011' PÅ ORDER SOM HAR BLANDAT                  
036300** VANLIGA ARTIKLAR OCH DIREKLEVERANSARTIKLAR. DET SKA                    
036400** BARA SKAPAS EN 'RIM'-POST TILL VIPS                                    
036500     SKIP2                                                                
036600     IF SPAR-RIL-IDPTYPA = 'RIL'                                          
036700         MOVE 'RIL'                 TO RIL-IDPTYP                         
036800         MOVE SPAR-RIL-W461RILN-CTX TO RIL-W461RILN-CTX                   
036900         PERFORM S13-SEND-MESSAGE                                         
037000         MOVE SPACE TO SPAR-RIL-IDPTYPA                                   
037100     END-IF                                                               
037200                                                                          
037300     IF FL-GODKEND-KDFAKTYP = JA                                          
037400        MOVE WSORT-AREA               TO RIM-W461RIM2-CTX                 
037500        IF RIM-IDKUNDNR    = SPAR-RIM-IDKUNDNR                            
037600           AND RIM-IDORDNR = SPAR-RIM-IDORDNR                             
037700           AND SPAR-IDPTYP        = 'RIM'                                 
037800           MOVE RIM-IDKUNDNR          TO SPAR-RIM-IDKUNDNR                
037900           MOVE RIM-IDORDNR           TO SPAR-RIM-IDORDNR                 
038000           CONTINUE                                                       
038100        ELSE                                                              
038200           MOVE SPAR-IDDC             TO WS-IDDC                          
038300           PERFORM S13-SEND-MESSAGE                                       
038400           MOVE RIM-IDKUNDNR          TO WORK-IDKUNDNR                    
038500           DISPLAY 'FAKTURA ' WORK-TEXT                                   
038600           MOVE RIM-IDKUNDNR          TO SPAR-RIM-IDKUNDNR                
038700           MOVE RIM-IDORDNR           TO SPAR-RIM-IDORDNR                 
038800        END-IF                                                            
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 BM-FAKTURA-KOLLI SECTION.                                                
039300     SKIP2                                                                
039400     IF FL-GODKEND-KDFAKTYP = JA                                          
039500       MOVE WSORT-AREA              TO RIN-W461RINN-CTX                   
039600       PERFORM S13-SEND-MESSAGE                                           
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 BN-FAKTURA-RAD-1 SECTION.                                                
040100     SKIP2                                                                
040200     IF FL-GODKEND-KDFAKTYP = JA                                          
040300                                                                          
040400       MOVE SPAR-IDDISTR     TO DIS1-IDDISTR                              
040500                                TEST-IDDISTR                              
040600       CALL W460DIS1 USING DIS1-W460DIS1                                  
040700       IF DIS1-IDLANDX2 = ISO-SAUDI     OR                                
040800          DIS1-IDLANDX2 = ISO-PERU      OR                                
040900         (DIS1-IDLANDX2 = ISO-BRASILIEN AND DIS121-BRASIL)                
041000         MOVE WSORT-AREA          TO RIO-W461RIO1-CTX                     
041100         MOVE SPAR-IDDC           TO RIO-IDDC                             
041200         PERFORM S13-SEND-MESSAGE                                         
041300       ELSE                                                               
041400         MOVE WSORT-AREA          TO SOFT-RIO-W461RIO2                    
041500* SVERIGE VIPS KLARAR INTE ALPHA DC - TAS FRÅN HUVUD ISTÄLLET             
041600*        MOVE WSORT-FRAD-IDDC     TO SOFT-RIO-IDDC                        
041700         MOVE SPAR-IDDC           TO SOFT-RIO-IDDC                        
041800         IF DIS1-IDLANDX2 = 'US' OR 'CA'                                  
041900           PERFORM S30-JA                                                 
042000         END-IF                                                           
042100         PERFORM S13-SEND-MESSAGE                                         
042200       END-IF                                                             
042300     END-IF                                                               
042400                                                                          
042500*    IF DIS1-IDLANDX2 = ISO-DANMARK                                       
042600*      PERFORM BNA-FAKTURA-RAD-DANMARK                                    
042700*    END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000*BNA-FAKTURA-RAD-DANMARK SECTION.                                         
043100     SKIP2                                                                
043200*    MOVE 'RIZ'                 TO RIZ-IDPTYP                             
043300*    MOVE WSORT-FRAD-KDSRA      TO RIZ-KDSRA                              
043400*    MOVE WSORT-FRAD-KDSORT     TO RIZ-KDSORT                             
043500*    MOVE WSORT-FRAD-KVQPACK-1  TO RIZ-KVQPACK-1                          
043600*                                                                         
043700*    PERFORM BNAA-AENDRA-KDARTURS-TILL-NUM                                
043800*    MOVE ARTU-KDARTURS-NUM     TO RIZ-KDARTURS                           
043900*                                                                         
044000*    MOVE WSORT-FRAD-IDSTATNR   TO RIZ-IDSTATNR                           
044100*    MOVE WSORT-FRAD-BEART      TO RIZ-BEART                              
044200*    MOVE WSORT-FRAD-VKART      TO RIZ-VKART                              
044300*    PERFORM S13-SEND-MESSAGE                                             
044400*    .                                                                    
044500*    EJECT                                                                
044600                                                                          
044700*BNAA-AENDRA-KDARTURS-TILL-NUM SECTION.                                   
044800*    SKIP2                                                                
044900*    MOVE WSORT-FRAD-KDARTURS    TO ARTU-KDARTURS                         
045000*    MOVE SPACE                  TO ARTU-IDDC                             
045100*    MOVE ZERO                   TO ARTU-IDDISTR                          
045200*    CALL W400ARTU USING ARTU-W400ARTU                                    
045300*    .                                                                    
045400*    EJECT                                                                
045500                                                                          
045600 BP-FAKTURA-RAD-2 SECTION.                                                
045700     SKIP2                                                                
045800     IF FL-GODKEND-KDFAKTYP = JA                                          
045900       MOVE WSORT-AREA             TO RIP-W461RIPN-CTX                    
046000       PERFORM S13-SEND-MESSAGE                                           
046100     END-IF                                                               
046200     .                                                                    
046300     EJECT                                                                
046400 S01-LAS-SORTERAD-INFIL SECTION.                                          
046500     SKIP3                                                                
046600     READ   INFIL   INTO WSORT-AREA                                       
046700                      AT END MOVE JA TO SORTFIL-EOF                       
046800     END-READ                                                             
046900                                                                          
047000     IF SORTFIL-EOF = NEJ                                                 
047100                                                                          
047200       MOVE 'INFIL'             TO POSTSUM-FDNAMN                         
047300       MOVE 'W47614D1'          TO POSTSUM-DDNAMN2                        
047400       MOVE RIK-IDPTYP          TO POSTSUM-TRANSTYP                       
047500       CALL POSTSUM   USING POSTSUM-PARM                                  
047600                                                                          
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000 S11-LETA-MOTTAGARE SECTION.                                              
048100                                                                          
048200     MOVE 'N'                 TO WS-SEND-MQ-SW                            
048300                                                                          
048400     MOVE WSORT-RIK-IDDISTR TO TEST-IDDISTR DIS1-IDDISTR                  
048500                                                                          
048600     CALL W460DIS1 USING DIS1-W460DIS1                                    
048700     EVALUATE DIS1-IDLANDX2                                               
048800        WHEN 'AT'                                                         
048900           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
049000        WHEN 'AU'                                                         
049100           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
049200        WHEN 'BE'                                                         
049300           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
049400*       WHEN 'CA'                                                         
049500*          MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
049600        WHEN 'CH'                                                         
049700           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
049800        WHEN 'ES'                                                         
049900           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
050000        WHEN 'FR'                                                         
050100           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
050200        WHEN 'GB'                                                         
050300           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
050400        WHEN 'DE'                                                         
050500           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
050600        WHEN 'IE'                                                         
050700           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
050800        WHEN 'IT'                                                         
050900           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
051000        WHEN 'JP'                                                         
051100           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
051200        WHEN 'NL'                                                         
051300           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
051400        WHEN 'SE'                                                         
051500           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
051600*       WHEN 'US'                                                         
051700*          MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
051800        WHEN 'C1'                                                         
051900           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
052000        WHEN 'PL'                                                         
052100           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
052200        WHEN 'IN'                                                         
052300           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
052400        WHEN 'KR'                                                         
052500           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
052600        WHEN 'TR'                                                         
052700           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
052800        WHEN 'CZ'                                                         
052900           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
053000        WHEN 'HU'                                                         
053100           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
053200        WHEN 'MY'                                                         
053300           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
053400        WHEN 'TH'                                                         
053500           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
053600        WHEN 'TW'                                                         
053700           MOVE DIS1-IDLANDX2 TO WS-ADDISPABS-IDLAND                      
053800        WHEN OTHER                                                        
053900                                                                          
054000           PERFORM S17-VALIDATE-COUNTRY                                   
054100                                                                          
054200           IF LAND-KDSVAR = SPACE                                         
054300              SET SEND-MQ       TO TRUE                                   
054400           ELSE                                                           
054500              MOVE 'O2'         TO WS-ADDISPABS-IDLAND                    
054600           END-IF                                                         
054700                                                                          
054800     END-EVALUATE                                                         
054900     .                                                                    
055000     EJECT                                                                
055100                                                                          
055200 S12-SEND-OPEN SECTION.                                                   
055300                                                                          
055400     MOVE SPACE TO REQU-WZ01REQU                                          
055500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
055600                                                                          
055700     MOVE WS-ADDISPABS               TO SEND-ADDISPABS                    
055800                                                                          
055900     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
056000                                                                          
056100     IF SEND-KDRC > 0                                                     
056200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
056300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
056400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
056500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
056600     END-IF                                                               
056700     .                                                                    
056800     SKIP3                                                                
056900 S13-SEND-MESSAGE SECTION.                                                
057000                                                                          
057100     IF SEND-MQ                                                           
057200                                                                          
057300        EVALUATE UT-IDPTYP                                                
057400          WHEN 'RIK'                                                      
057500           WRITE UT-RIK-POST    FROM RIK-W461RIK1                         
057600          WHEN 'RIL'                                                      
057700           WRITE UT-RIL-POST    FROM RIL-W461RILN-CTX                     
057800          WHEN 'RIM'                                                      
057900           WRITE UT-RIM-POST    FROM RIM-W461RIM2-CTX                     
058000          WHEN 'RIN'                                                      
058100           WRITE UT-RIN-POST    FROM RIN-W461RINN-CTX                     
058200          WHEN 'RIO'                                                      
058300           WRITE UT-RIO-POST    FROM SOFT-RIO-W461RIO2                    
058400          WHEN 'RIP'                                                      
058500           WRITE UT-RIP-POST    FROM RIP-W461RIPN-CTX                     
058600        END-EVALUATE                                                      
058700                                                                          
058800     ELSE                                                                 
058900        MOVE 'PUT'                      TO SEND-KDFUNC                    
059000        EVALUATE UT-IDPTYP                                                
059100        WHEN 'RIK'   MOVE LENGTH OF RIK-W461RIK1      TO W-LENGD          
059200        WHEN 'RIL'   MOVE LENGTH OF RIL-W461RILN-CTX  TO W-LENGD          
059300        WHEN 'RIM'   MOVE LENGTH OF RIM-W461RIM2-CTX  TO W-LENGD          
059400        WHEN 'RIN'   MOVE LENGTH OF RIN-W461RINN-CTX  TO W-LENGD          
059500        WHEN 'RIO'   MOVE LENGTH OF SOFT-RIO-W461RIO2 TO W-LENGD          
059600        WHEN 'RIP'   MOVE LENGTH OF RIP-W461RIPN-CTX  TO W-LENGD          
059700        END-EVALUATE                                                      
059800        MOVE W-LENGD TO SEND-KVDLEN                                       
059900                                                                          
060000        CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN UT-AREA         
060100                                                                          
060200        IF SEND-KDRC > 0                                                  
060300          MOVE SEND-KDRC TO KDRC-DISPLAY                                  
060400          STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                    
060500          DELIMITED BY SIZE INTO ERROR-TEXT                               
060600          CALL ABEND USING RKOD-ABEND-MED-DUMP                            
060700        END-IF                                                            
060800                                                                          
060900     END-IF                                                               
061000                                                                          
061100     MOVE WS-ADDISPABS-IDLAND   TO POSTSUM-FDNAMN                         
061200     CALL POSTSUM      USING POSTSUM-PARM                                 
061300                                                                          
061400     .                                                                    
061500     SKIP3                                                                
061600 S14-SEND-CLOSE SECTION.                                                  
061700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
061800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
061900                                                                          
062000     IF SEND-KDRC > 0                                                     
062100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
062200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
062300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
062400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 S15-STARTPOST SECTION.                                                   
062900     MOVE 'PUT'                      TO SEND-KDFUNC                       
063000     MOVE 'RI0'                      TO START-IDPTYP                      
063100     MOVE WSORT-RIK-IDDISTR          TO START-IDDISTR                     
063200     MOVE WSORT-RIK-IDDC             TO START-IDDC                        
063300     IF DIS1-IDLANDX2 = 'US'                                              
063400       MOVE 7574                     TO START-IDDISTR                     
063500     END-IF                                                               
063600     IF DIS1-IDLANDX2 = 'CA'                                              
063700       MOVE 7674                     TO START-IDDISTR                     
063800     END-IF                                                               
063900     MOVE FUNCTION CURRENT-DATE(3:6) TO START-TIFILDAT                    
064000     MOVE FUNCTION CURRENT-DATE(9:6) TO START-TIHHMMSS                    
064100                                                                          
064200*      TIDEN FÅR EJ VARA SAMMA SOM FÖREGÅENDE START-POST                  
064300*            VIPS TAR EJ EMOT TVÅ LIKA                                    
064400     PERFORM S20-LETA-TABELL                                              
064500                                                                          
064600     MOVE START-TIHHMMSS             TO W-TIHHMMSS                        
064700     DISPLAY 'DISTR, TID ' WSORT-RIK-IDDISTR START-TIHHMMSS               
064800     PERFORM S21-SPARA-TABELL                                             
064900                                                                          
065000     IF SEND-MQ                                                           
065100                                                                          
065200        WRITE UT-RI0-POST    FROM UT-AREA                                 
065300                                                                          
065400     ELSE                                                                 
065500                                                                          
065600        MOVE LENGTH OF START-W461RI0N-CTX  TO SEND-KVDLEN                 
065700        CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN UT-AREA         
065800                                                                          
065900        IF SEND-KDRC > 0                                                  
066000           MOVE SEND-KDRC TO KDRC-DISPLAY                                 
066100           STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                   
066200           DELIMITED BY SIZE INTO ERROR-TEXT                              
066300           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
066400        END-IF                                                            
066500     END-IF                                                               
066600                                                                          
066700     MOVE 'RI0'                 TO POSTSUM-FDNAMN                         
066800     CALL POSTSUM      USING POSTSUM-PARM                                 
066900                                                                          
067000     .                                                                    
067100     EJECT                                                                
067200                                                                          
067300 S16-SLUTPOST SECTION.                                                    
067400     MOVE SPACE                  TO UT-AREA                               
067500     MOVE 'PUT'                  TO SEND-KDFUNC                           
067600     MOVE 'RI9'                  TO SLUT-IDPTYP                           
067700     MOVE WS-ANTALPOSTER         TO SLUT-KVTRANS                          
067800                                                                          
067900     IF SEND-MQ                                                           
068000        WRITE UT-RI9-POST FROM UT-AREA                                    
068100     ELSE                                                                 
068200        MOVE LENGTH OF SLUT-W461RI9 TO SEND-KVDLEN                        
068300        CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN UT-AREA         
068400                                                                          
068500        IF SEND-KDRC > 0                                                  
068600          MOVE SEND-KDRC TO KDRC-DISPLAY                                  
068700          STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                    
068800          DELIMITED BY SIZE INTO ERROR-TEXT                               
068900          CALL ABEND USING RKOD-ABEND-MED-DUMP                            
069000        END-IF                                                            
069100     END-IF                                                               
069200                                                                          
069300     MOVE 'RI9'                 TO POSTSUM-FDNAMN                         
069400     CALL POSTSUM      USING POSTSUM-PARM                                 
069500                                                                          
069600     .                                                                    
069700     EJECT                                                                
069800                                                                          
069900 S17-VALIDATE-COUNTRY SECTION.                                            
070000                                                                          
070100     MOVE DIS1-IDLANDX2   TO LAND-IDLANDX2                                
070200     MOVE SPACE           TO LAND-IDLANDX3                                
070300                                                                          
070400     CALL WISOLAND USING LAND-WISOLAND                                    
070500                                                                          
070600     .                                                                    
070700                                                                          
070800 S18-WRITE-MQ-HEADER SECTION.                                             
070900                                                                          
071000     MOVE DIS1-IDLANDX2              TO WS-MARKET                         
071100*    MOVE LENGTH  OF WS-MQ-HDR       TO W-LENGD                           
071200                                                                          
071300     WRITE W47660-001 FROM WS-MQ-HDR                                      
071400     .                                                                    
071500     SKIP3                                                                
071600 S20-LETA-TABELL   SECTION.                                               
071700     MOVE +1                    TO WX                                     
071800     PERFORM UNTIL WX > 30                                                
071900       IF WX-IDDISTR(WX) = START-IDDISTR                                  
072000         IF WX-TIHHMMSS(WX) = START-TIHHMMSS OR                           
072100            WX-TIHHMMSS(WX) > START-TIHHMMSS                              
072200*          SAMMA TIDER                                                    
072300                                                                          
072400           MOVE START-TIHHMMSS  TO WY-TIHHMMSS                            
072500           IF START-TIHHMMSS < WX-TIHHMMSS(WX)                            
072600             MOVE WX-TIHHMMSS(WX) TO WY-TIHHMMSS                          
072700           END-IF                                                         
072800           ADD +1               TO WY-SS                                  
072900           IF WY-SS > +60                                                 
073000             MOVE +01           TO WY-SS                                  
073100             ADD  +01           TO WY-MM                                  
073200           END-IF                                                         
073300           IF WY-MM > +60                                                 
073400             MOVE +01           TO WY-MM                                  
073500             ADD  +01           TO WY-HH                                  
073600           END-IF                                                         
073700           MOVE WY-TIHHMMSS     TO START-TIHHMMSS                         
073800           MOVE +30             TO WX                                     
073900         END-IF                                                           
074000       END-IF                                                             
074100       IF WX-IDDISTR(WX) = ZERO                                           
074200         MOVE +30               TO WX                                     
074300       END-IF                                                             
074400       ADD +1                   TO WX                                     
074500     END-PERFORM                                                          
074600     .                                                                    
074700     EJECT                                                                
074800 S21-SPARA-TABELL  SECTION.                                               
074900     MOVE +1                    TO WX                                     
075000     PERFORM UNTIL WX > 30                                                
075100       IF WX-IDDISTR(WX) = START-IDDISTR  OR                              
075200          WX-IDDISTR(WX) = ZERO                                           
075300          MOVE START-IDDISTR    TO WX-IDDISTR(WX)                         
075400          MOVE START-TIHHMMSS   TO WX-TIHHMMSS(WX)                        
075500          MOVE +30              TO WX                                     
075600       END-IF                                                             
075700       ADD +1                   TO WX                                     
075800     END-PERFORM                                                          
075900     .                                                                    
076000     EJECT                                                                
076100                                                                          
076200 S30-JA SECTION.                                                          
076300     IF SOFT-RIO-FLINVEST = 'J'                                           
076400       MOVE 'Y'                  TO SOFT-RIO-FLINVEST                     
076500     END-IF                                                               
076600     IF SOFT-RIO-FLPRTILL = 'J'                                           
076700       MOVE 'Y'                  TO SOFT-RIO-FLPRTILL                     
076800     END-IF                                                               
076900     IF SOFT-RIO-FLDIRLEV = 'J'                                           
077000       MOVE 'Y'                  TO SOFT-RIO-FLDIRLEV                     
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400                                                                          
077500 Z-FINIT SECTION.                                                         
077600     CLOSE INFIL                                                          
077610           W4766C                                                         
077700                                                                          
077800     MOVE 'S' TO POSTSUM-OPKOD                                            
077900     CALL POSTSUM USING POSTSUM-PARM                                      
078000     .                                                                    
