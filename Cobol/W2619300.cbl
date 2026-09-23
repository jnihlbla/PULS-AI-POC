000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2619300.                                                
000400 AUTHOR.         INGER STENING.                                           
000500 DATE-WRITTEN.   19/02/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        FRAMSTÄLLER UNDERLAG FÖR AUTO-SKROTNING                          
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK7                                       
001300*        PROGRAMMET LÄSER      WDK7                                       
001600*        PROGRAMMET UPPDATERAR WDR5                                       
001700*        PROGRAMMET LÄSER      WDR5                                       
001900*        PROGRAMMET LÄSER      WDN6                                       
002000*        PROGRAMMET LÄSER      WDD9                                       
002100*        PROGRAMMET LÄSER      WDD3                                       
002200*        PROGRAMMET LÄSER      WDQ4 (WDQ4B1)                              
002300*        PROGRAMMET LÄSER      WDL8                                       
002400*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*          --- ARTIKLAR ATT SKROTA AUTO                                   
003700     SELECT W26192                     ASSIGN TO W26193D1.                
003800*          --- ARTIKLAR FÖR AUTOM. GODK.                                  
003900     SELECT W26193                     ASSIGN TO W26193D2.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W26192                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W26190     -PRE  IN-  -L.                                      
005000     SKIP3                                                                
005100 FD  W26193                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST -COPY W26169 -PRE  UT-  -L.                                     
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)    VALUE 'W2619300'.            
006000 01  CHKP-VAR.                                                            
006100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006600     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900 77  IX                          PIC 9(2)    VALUE ZERO.                  
007000 77  WS-IX                       PIC 9(2)    VALUE ZERO.                  
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500 77  WS-SPAR-BEANST-GODK         PIC X(25)   VALUE SPACE.                 
007600 77  W26192-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W26192                       VALUE 'J'.                   
008101 77  SW-SKROT-OK                 PIC X       VALUE 'N'.                   
008110 77  SW-DEKAL                    PIC X       VALUE 'N'.                   
008200 77  SW-AUT-GODK                 PIC X       VALUE 'N'.                   
008210 77  SW-SATS-OK                  PIC X       VALUE 'N'.                   
008300     EJECT                                                                
008400 01  SWITCHAR.                                                            
008500     03  INGAR-SATS-SW           PIC X.                                   
008600         88  INGAR-I-SATS    VALUE 'J'.                                   
008700*      --- VALID IDDC CODES                                               
008800*                                                                         
008900*01    -COPY WWDCKONS                                                     
009000     EJECT                                                                
009010*01    -COPY WWPRODSL                                                     
009020     EJECT                                                                
009100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009200 01  FILLER REDEFINES DAGENS-DATUM.                                       
009300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009600                                                                          
009700 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010010     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010500     03  W009LTXT                PIC X(8)    VALUE 'W009LTXT'.            
010600     SKIP3                                                                
010700 01  ARB.                                                                 
011600     03  IDARTNR-WS              PIC S9(9)   VALUE ZERO COMP-3.           
011700     03  WS-FLERS                PIC X(1)    VALUE SPACE.                 
011800                                                                          
012300     03  WS-AUTO-USERID          PIC X(8)    VALUE 'W2619300'.            
012400     03  WS-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
012500     03  WKVSKRANT               PIC S9(7).                               
012600     03  IX-RAD                  PIC S9(3)   VALUE ZERO COMP-3.           
012700     03  WS-TEMEMO               PIC X(25)   VALUE SPACE.                 
012800     03  WS-KVBR                 PIC S9(7)   VALUE ZERO COMP-3.           
012900     03  ANT-SKROTU              PIC S9(3)   VALUE ZERO COMP-3.           
013000     03  MAX-ANT-SKROTU          PIC S9(3)   VALUE 500  COMP-3.           
013100     03  WS-KVBEART              PIC S9(7)   VALUE ZERO COMP-3.           
013200     03  WS-MAX-SUBEL            PIC S9(7)   VALUE ZERO COMP-3.           
013300     03  WS-SUARTSTD             PIC 9(7)V9(2) VALUE ZERO.                
013400     03  WS-TIAAAA               PIC 9(4)    VALUE ZERO.                  
013500     03  FILLER  REDEFINES  WS-TIAAAA.                                    
013600         05  WS-TISEKEL          PIC 9(2).                                
013700         05  WS-TIAA-VECKA       PIC 9(2).                                
013800     03  WS-TIAAAA-1             PIC 9(4)    VALUE ZERO.                  
013900     03  WS-TIVV                 PIC S9(3)   VALUE ZERO  COMP-3.          
014000     03  WS-ANTAL-OI             PIC S9(7)   VALUE ZERO COMP-3.           
014100     03  WS-TEXT.                                                         
014200         05  WS-TEXT-GB          PIC X(25).                               
014300         05  WS-TEXT-SV          PIC X(25).                               
014400                                                                          
014500 01  W-DATUM.                                                             
014600     05  W-DATUM-DATE    PIC X(6).                                        
014700     EJECT                                                                
014800*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
014900                                                                          
015000*01  -COPY WDATAREA                                                       
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL POSTSUM                                          
015300*                                                                         
015400*01  -COPY W0005   -PRE  POSTSUM-                                         
015500     EJECT                                                                
015600*    --- TEXTSÖKNING                                                      
015700*                                                                         
015800*01  -COPY W009W041                                                       
015900     EJECT                                                                
016000 01  IN-AREA-START               PIC X(24)   VALUE                        
016100                                             'IN-AREA-START'.             
016200     SKIP2                                                                
016300                                                                          
016400*01  AREA -COPY W26190     -PRE IN-                                       
016500*                                                                         
016600     SKIP2                                                                
016700                                                                          
016800*01  AREA -COPY W26169     -PRE UT-                                       
016900     EJECT                                                                
016906                                                                          
016907 77  RKOD                        PIC S9(4)   COMP VALUE +0.               
016908 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016909 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016910 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016911                                                                          
016912 01  ERROR-TEXT.                                                          
016920     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
016930     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
016940     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017100     SKIP3                                                                
017200 01  NYCKLAR-TILL-DLI.                                                    
017300     03  W-IDARTNR-X.                                                     
017400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017500     03  W-IDDC-X.                                                        
017600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017610     03  W-IDDC-B6-X.                                                     
017620         05  W-IDDC-B6            PIC X(2)    VALUE SPACE.                
017700     03  W-WDD901KY-X.                                                    
017800         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
017900         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
018000     03  W-KDSEGKEY-X.                                                    
018100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018400     03  W-IDSKYLT-X.                                                     
018500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
018600     03  W-WDJ1CSEQ-X.                                                    
018700         05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                 
018800         05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                 
018900         05  W-IDARTNR-S         PIC S9(9)   COMP-3 VALUE ZERO.           
019800     03  W-WDN611KY-X.                                                    
019900         05  W-WDN611KY          PIC X(6)    VALUE SPACE.                 
020200                                                                          
021500     03  W-WDQ4B1KY-MIN.                                                  
021600         05  W-IDARTNR-Q4B1-MIN  PIC S9(9)    COMP-3.                     
021700         05  FILLER              PIC X(32)    VALUE LOW-VALUE.            
021800                                                                          
021810     03  W-WDQ4B1KY-MAX.                                                  
021820         05  W-IDARTNR-Q4B1-MAX  PIC S9(9)    COMP-3.                     
021830         05  FILLER              PIC X(32)    VALUE HIGH-VALUE.           
021840                                                                          
021900     03  W-IDDISTR-Q4B1-X.                                                
022000         05  W-IDDISTR-Q4B1      PIC S9(5)  VALUE ZERO COMP-3.            
022100                                                                          
022200     03  W-IDKUNDNR-Q4B1-X.                                               
022300         05  W-IDKUNDNR-Q4B1     PIC S9(7)  VALUE ZERO COMP-3.            
022400                                                                          
022800     03  W-TIAAAA-X.                                                      
022900         05  W-TIAAAA            PIC 9(4).                                
023000                                                                          
023100 01  W-IDARTNR-2.                                                         
023200     03  W-IDARTNR-PCB2       PIC S9(9) COMP-3 VALUE ZERO.                
023300                                                                          
024100 01  NYCKLAR-6321.                                                        
024200     03  W-WDGX6321-ROT-X.                                                
024300         05  FILLER              PIC X(04)  VALUE '6321'.                 
024400         05  W-KDARBTYP          PIC X(08)  VALUE 'ANSK    '.             
024500         05  FILLER              PIC X(18)  VALUE LOW-VALUE.              
024600     03  W-WDGX6322-KEY-X.                                                
024700         05  W-DASKROT9-BEORD    PIC 9(08)  VALUE ZERO.                   
024800     03  W-KY6324-KVAL-X.                                                 
024900         05  W-IDARTNR-KVAL      PIC S9(9)  VALUE ZERO COMP-3.            
025000         05  W-IDDC-KVAL         PIC X(2)   VALUE SPACE.                  
025100         05  W-KDSTASKR-KVAL     PIC S9     VALUE ZERO COMP-3.            
025200                                                                          
025300     03  W-WDGXKEY-X.                                                     
025400         05  FILLER             PIC X(4)    VALUE '6327'.                 
025500         05  W-KDARBTYP-6327    PIC X(8)    VALUE SPACE.                  
025600         05  W-IDDC-6327        PIC X(2)    VALUE SPACE.                  
025700         05  FILLER             PIC X(16)   VALUE LOW-VALUE.              
025800     03  W-IDUSER-GODK-X.                                                 
025900         05  W-IDUSER-GODK      PIC X(8)    VALUE SPACE.                  
026000     EJECT                                                                
026100     SKIP2                                                                
026200*    --- STATUS-KOD FRÅN IMS                                              
026300 01  STATUS-WS                   PIC XX.                                  
026400     88  SEGMENT-FINNS                       VALUE '  '.                  
026500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
026800     88  IMS-EJ-OK                           VALUE 'XD'.                  
026900     SKIP2                                                                
027000 01  GODK-STATUSKODER.                                                    
027100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027200     SKIP3                                                                
027300 01  SSA1                        PIC X(400).                              
027400 01  SSA2                        PIC X(64).                               
027500 01  SSA3                        PIC X(64).                               
027600 01  SSA4                        PIC X(64).                               
027700     EJECT                                                                
027800*    --- IMS FUNKTIONSKODER                                               
027900*01  -COPY W0003                                                          
028000     EJECT                                                                
028100*    ---  DLI INPUT-OUTPUT AREA                                           
028200                                                                          
028210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611-2'.                    
028220 01  DLI-IO-WDK611-2.                                                     
028230*    03  -COPY WDK611  -PRE CLAG2-                                        
028240     EJECT                                                                
028300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ1'.                        
028400 01  DLI-IO-WDJ1.                                                         
028500*    03  -COPY WDJ111 -PRE SATS-                                          
028600*    03  -COPY WDJ101 -PRE SATS-                                          
028700     EJECT                                                                
031100 01  FILLER         PIC X(26) VALUE 'DLI-IO-WDR501-6321'.                 
031200 01  DLI-IO-WDR501-6321.                                                  
031300*    03  -COPY WDGX6321                                                   
031400     EJECT                                                                
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
031600 01  DLI-IO-WDGX6322.                                                     
031700*    03  -COPY WDGX6322                                                   
031800     EJECT                                                                
031900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
032000 01  DLI-IO-WDGX6324.                                                     
032100*    03  -COPY WDGX6324                                                   
032200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
032300 01  DLI-IO-WDGX6325.                                                     
032400*    03  -COPY WDGX6325                                                   
032500     EJECT                                                                
032600 01  FILLER         PIC X(26) VALUE 'DLI-IO-WDR501-6327'.                 
032700 01  DLI-IO-WDR501-6327.                                                  
032800*    03  -COPY WDGX6327                                                   
032900     EJECT                                                                
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
033100 01  DLI-IO-WDGX6328.                                                     
033200*    03  -COPY WDGX6328                                                   
033600     EJECT                                                                
033700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
033800 01  DLI-IO-WDK711.                                                       
033900*    03  -COPY WDK711                                                     
033910     EJECT                                                                
034000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
034100 01  DLI-IO-WDN601.                                                       
034200*    03  -COPY WDN601                                                     
034300     EJECT                                                                
034400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
034500 01  DLI-IO-WDN611.                                                       
034600*    03  -COPY WDN611                                                     
034700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
034800 01  DLI-IO-WDD901.                                                       
034900*    03  -COPY WDD901  -PRE   LEV-                                        
035000     EJECT                                                                
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
035200 01  DLI-IO-WDD902.                                                       
035300*    03  -COPY WDD902  -PRE   LEV-                                        
035400     EJECT                                                                
035500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD3'.                        
035600 01  DLI-IO-WDD3.                                                         
035700*03  WLBENA11  -COPY WDD311  -PRE  BENA11-                                
035800     EJECT                                                                
035900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4B1'.                      
036000 01  DLI-IO-WDQ4B1.                                                       
036100*    03  -COPY WDQ4B1                                                     
036200     EJECT                                                                
036700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
036800 01  DLI-IO-WDL811.                                                       
036900*    03  -COPY WDL811                                                     
037000                                                                          
037100     EJECT                                                                
037110 01  FILLER         PIC X(16)  VALUE 'DLI-IO-AREA-WDB6'.                  
037120 01   DLI-IO-AREA-B601.                                                   
037130*     03  -COPY WDB601                                                    
037140                                                                          
037150 01  FILLER         PIC X(16)   VALUE 'B601-TABELL'.                      
037160 01  B601-IX        PIC S9(4)   COMP SYNC VALUE ZERO.                     
037170 01  MAX-B601-IX    PIC S9(4)   COMP SYNC VALUE +80.                      
037180 01  B601-TABELL.                                                         
037190     03  FILLER OCCURS 80.                                                
037191*      05  -COPY WDB601  -PRE TAB-                                        
037192                                                                          
039100     EJECT                                                                
040400 LINKAGE SECTION.                                                         
040500                                                                          
040600*01  -COPY W0009   -PRE MSG-                                              
040700                                                                          
042000*01  -COPY W0008  -PRE 6321-                                              
042100     05  FILLER                  PIC X.                                   
042200                                                                          
042300*01  -COPY W0008  -PRE 6327-                                              
042400     05  FILLER                  PIC X.                                   
042500                                                                          
042600*01  -COPY W0008  -PRE WDK7-                                              
042700     05  FILLER                  PIC X.                                   
042800                                                                          
042900*01  -COPY W0008  -PRE WDN6-                                              
043000     05  FILLER                  PIC X.                                   
043100                                                                          
043200*01  -COPY W0008  -PRE WDD9-                                              
043300     05  FILLER                  PIC X.                                   
043400                                                                          
043500*01  -COPY W0008  -PRE WDD3-                                              
043600     05  FILLER                  PIC X.                                   
043700                                                                          
043800*01  -COPY W0008  -PRE WDQ4B-                                             
043900     05  FILLER                  PIC X.                                   
044000                                                                          
044100*01  -COPY W0008  -PRE WDL8-                                              
044200     05  FILLER                  PIC X.                                   
044300     EJECT                                                                
044310*01  -COPY W0008  -PRE WDB6-                                              
044320     05  FILLER                  PIC X.                                   
044330     EJECT                                                                
044331*01  -COPY W0008  -PRE WDK62-                                             
044332     05  FILLER                  PIC X.                                   
044333                                                                          
044340*01  -COPY W0008  -PRE WDJ1C-                                             
044350     05  FILLER                  PIC X.                                   
044360                                                                          
044400 PROCEDURE DIVISION  USING MSG-PCB                                        
044600           6321-PCB  6327-PCB  WDK7-PCB WDN6-PCB                          
044700           WDD9-PCB  WDD3-PCB WDQ4B-PCB WDL8-PCB WDB6-PCB                 
044800           WDK62-PCB WDJ1C-PCB.                                           
044900 MAIN SECTION.                                                            
045000     ENTRY 'DLITCBL' USING MSG-PCB                                        
045100           6321-PCB  6327-PCB  WDK7-PCB WDN6-PCB                          
045101           WDD9-PCB  WDD3-PCB WDQ4B-PCB WDL8-PCB WDB6-PCB                 
045102           WDK62-PCB WDJ1C-PCB.                                           
045500                                                                          
045700     PERFORM A-INIT                                                       
045900     PERFORM S01-LAES-W26192                                              
046000     PERFORM UNTIL END-OF-W26192                                          
046010       MOVE IN-KDPRODSL              TO TEST-KDPRODSL                     
046020**     IF KDPRODSL-LYNK                                                   
046030*****    NO SCRAP FOR LYNK PARTS                                          
046040**       CONTINUE                                                         
046050**     ELSE                                                               
046100         IF CHKP-ANT > CHKP-MAX                                           
046200           PERFORM X-TAG-CHECKPOINT                                       
046300         END-IF                                                           
046310         PERFORM C-KOLLA-IDUSER                                           
046400         MOVE IN-IDARTNR TO W-IDARTNR                                     
046500                            IDARTNR-WS                                    
046700         IF ANT-SKROTU < MAX-ANT-SKROTU                                   
046800            IF IN-FLIART = JA                                             
046900               MOVE IN-FLIART TO INGAR-SATS-SW                            
047000            ELSE                                                          
047100               MOVE NEJ      TO INGAR-SATS-SW                             
047200            END-IF                                                        
047300            MOVE IN-FLERS    TO WS-FLERS                                  
047400            PERFORM B-LAES-SKROTINFO                                      
047500                                                                          
047600            IF SW-SKROT-OK = JA                AND                        
047700                IN-TISKROT-AUTO < DAGENS-DATUM AND                        
047800                IN-FLSKROT-BEORD NOT = JA                                 
047900                PERFORM D-UPPDATERA                                       
048000            END-IF                                                        
048100         END-IF                                                           
048110**     END-IF                                                             
048200       PERFORM S01-LAES-W26192                                            
048300     END-PERFORM                                                          
048400                                                                          
048500                                                                          
048600     PERFORM Z-FINIT                                                      
048700                                                                          
048800     MOVE ZERO TO RETURN-CODE                                             
048900     GOBACK                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 A-INIT SECTION.                                                          
049300     SKIP2                                                                
049400                                                                          
049500     PERFORM IMS-RESTART                                                  
049600                                                                          
049700     OPEN INPUT W26192                                                    
049800         OUTPUT W26193                                                    
049900                                                                          
050000                                                                          
050100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
050200                                                                          
050300     ACCEPT W-DATUM-DATE FROM DATE                                        
050400     ACCEPT DAGENS-DATUM FROM DATE                                        
050500                                                                          
050600     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
050700                                                                          
051000     MOVE 'IDAG' TO DAT-KDDATFORM                                         
051100     CALL WDATKONV USING DAT-KDDATFORM,                                   
051200                         DAT-I-TIDATUM,                                   
051300                         DAT-O-TIDATUM,                                   
051400                         DAT-KDSVAR                                       
051500                                                                          
051600     IF DAT-KDSVAR-FEL                                                    
051700        DISPLAY '****  FEL I WDATKONV  *******'                           
051900        CALL FELLOG                                                       
052000     END-IF                                                               
052100                                                                          
052200     MOVE DAT-TIVV       TO WS-TIVV                                       
052300     MOVE DAT-TISEKEL    TO WS-TISEKEL                                    
052400     MOVE DAT-TIAA-VECKA TO WS-TIAA-VECKA                                 
052500     MOVE WS-TIAAAA      TO WS-TIAAAA-1                                   
052600     SUBTRACT +1         FROM WS-TIAAAA-1                                 
052700                                                                          
052800     MOVE LOW-VALUE      TO W-WDQ4B1KY-MIN                                
052900     MOVE HIGH-VALUE     TO W-WDQ4B1KY-MAX                                
053000                                                                          
053100     .                                                                    
053200     EJECT                                                                
053345                                                                          
053346     .                                                                    
053347     EJECT                                                                
053348 B-LAES-SKROTINFO SECTION.                                                
053400                                                                          
053800     IF IN-TISKROT-AUTO < DAGENS-DATUM AND                                
053900        IN-FLSKROT-BEORD NOT = JA                                         
054500        MOVE JA            TO SW-SKROT-OK                                 
055000                                                                          
056000        IF INGAR-SATS-SW = JA                                             
056100           PERFORM BA-LAES-SATS-OI                                        
056200        END-IF                                                            
056610     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 BA-LAES-SATS-OI SECTION.                                                 
057100                                                                          
057200*    EFTERSOM INGÅENDE SATSARTIKLAR INTE FINNS MED FRÅN S&T               
057300*    SÅ ANVÄNDER VI ORDERINGÅNG I STÄLLET FÖR FÖRSÄLJNING                 
057400*    FÖR DESSA (ANTAL SÅLDA SISTA RULLANDE ÅR < 150)                      
057500                                                                          
057600     MOVE ZERO            TO WS-ANTAL-OI                                  
057700     MOVE IN-IDARTNR      TO W-IDARTNR                                    
057800     MOVE WS-TIAAAA       TO W-TIAAAA                                     
057900     PERFORM IMS-GU-WDL811                                                
058000     IF SEGMENT-FINNS                                                     
058100        MOVE +1           TO WS-IX                                        
058200        PERFORM UNTIL WS-IX >= WS-TIVV                                    
058300           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
058400           ADD +1         TO WS-IX                                        
058500        END-PERFORM                                                       
058600     END-IF                                                               
058700     MOVE WS-TIAAAA-1     TO W-TIAAAA                                     
058800     PERFORM IMS-GU-WDL811                                                
058900     IF SEGMENT-FINNS                                                     
059000        MOVE WS-TIVV      TO WS-IX                                        
059100        PERFORM UNTIL WS-IX >  53                                         
059200           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
059300           ADD +1         TO WS-IX                                        
059400        END-PERFORM                                                       
059500     END-IF                                                               
059600     IF (IN-SULEVANT-RAAR + WS-ANTAL-OI) > 150                            
059700*       EJ AKTUELL FÖR SKROT                                              
059800        MOVE NEJ TO SW-SKROT-OK                                           
059900     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
060200 C-KOLLA-IDUSER SECTION.                                                  
060300                                                                          
060400     MOVE IN-IDDC                TO W-IDDC-6327                           
060500     MOVE 'ANSK'                 TO W-KDARBTYP-6327                       
060600     MOVE SPACE                  TO WS-SPAR-BEANST-GODK                   
060700     PERFORM IMS-GET-WDR501-6327                                          
060800     IF SEGMENT-FINNS                                                     
060900        MOVE WS-AUTO-USERID      TO W-IDUSER-GODK                         
061000        PERFORM IMS-GET-WDGX6328                                          
061100        IF SEGMENT-FINNS                                                  
061200           MOVE 6328-BEANST-GODK TO WS-SPAR-BEANST-GODK                   
061300           MOVE 6328-SUBEL       TO WS-MAX-SUBEL                          
061400        END-IF                                                            
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 D-UPPDATERA SECTION.                                                     
061900                                                                          
062000     MOVE IDARTNR-WS TO W-IDARTNR                                         
062100                                                                          
062102     PERFORM DA-READ-OR-TAB-B601                                          
062110                                                                          
062200     PERFORM DB-SKAPA-KVSKRANT                                            
062300     IF WKVSKRANT > ZERO                                                  
062400        PERFORM DC-UPPD-SKROTSPARR                                        
062500        PERFORM DD-SKAPA-HANDELSETR-6321                                  
062600        PERFORM DE-SKAPA-TEMEMO                                           
062700        PERFORM DF-TEST-AUT-GODK                                          
062800     END-IF                                                               
062900                                                                          
063000     .                                                                    
063100     EJECT                                                                
063200 DA-READ-OR-TAB-B601 SECTION.                                             
063210                                                                          
063211     IF TAB-DCS-IDDC(1) = LOW-VALUE                                       
063212*--TAB IS EMPTY (FIRST CALL)                                              
063213       MOVE IN-IDDC           TO W-IDDC-B6                                
063214       PERFORM IMS-GU-WDB601                                              
063215       MOVE DCS-WDB601        TO TAB-DCS-WDB601 (1)                       
063216     ELSE                                                                 
063217       MOVE +1                TO B601-IX                                  
063218       PERFORM UNTIL B601-IX > MAX-B601-IX                                
063219         IF TAB-DCS-IDDC(B601-IX) = IN-IDDC                               
063220*--ALREADY SAVED. MOVE TAB TO DLI-IO-WDB601                               
063221           MOVE TAB-DCS-WDB601 (B601-IX) TO DCS-WDB601                    
063222           MOVE MAX-B601-IX   TO B601-IX                                  
063223         ELSE                                                             
063224           IF TAB-DCS-IDDC(B601-IX) = LOW-VALUE                           
063225*--NO MATCH. SAVE A NEW IDDC IN TABEL                                     
063226             MOVE IN-IDDC     TO W-IDDC-B6                                
063227             PERFORM IMS-GU-WDB601                                        
063228             MOVE DCS-WDB601  TO TAB-DCS-WDB601 (B601-IX)                 
063229             MOVE MAX-B601-IX TO B601-IX                                  
063230           END-IF                                                         
063231         END-IF                                                           
063232         ADD +1 TO B601-IX                                                
063233       END-PERFORM                                                        
063234       IF DCS-IDDC NOT = IN-IDDC                                          
063235*--NO MATCH. INDICATES THAT THE TABEL TO SMALL.                           
063236*--THERE ARE MORE THEN 80 XDC:S IN WDB601!!                               
063237         MOVE 'NO MATCH = TOO SMALL TABLE(80)' TO ERROR-TEXT-STR          
063238         DISPLAY ERROR-TEXT                                               
063239         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
063240       END-IF                                                             
063241     END-IF                                                               
063242     .                                                                    
063243     EJECT                                                                
063244 DB-SKAPA-KVSKRANT SECTION.                                               
063300                                                                          
063500     COMPUTE WKVSKRANT = IN-KVLS                                          
063510                       - IN-KVRESS                                        
063600                       - IN-KVOKS-BULK                                    
063700                       - IN-KVOKS-DAG                                     
063800                                                                          
065000     MOVE ZERO               TO WS-KVBEART                                
065100     MOVE W-IDARTNR          TO W-IDARTNR-Q4B1-MAX                        
065200                                W-IDARTNR-Q4B1-MIN                        
065600                                                                          
065610     MOVE DCS-IDDISTR-SKROT  TO W-IDDISTR-Q4B1                            
065620     MOVE DCS-IDKUNDNR-SKROT TO W-IDKUNDNR-Q4B1                           
065700                                                                          
065800     PERFORM IMS-GU-WDQ4B1                                                
065900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
066000                                                                          
066100        ADD SEQB-KVBEART-Q   TO WS-KVBEART                                
066200                                                                          
066300        PERFORM IMS-GN-WDQ4B1                                             
066400     END-PERFORM                                                          
066410                                                                          
066500     IF WS-KVBEART > ZERO                                                 
066600        COMPUTE WKVSKRANT = WKVSKRANT - WS-KVBEART                        
066700     END-IF                                                               
066800                                                                          
066900     .                                                                    
067000     EJECT                                                                
067100 DC-UPPD-SKROTSPARR SECTION.                                              
067200                                                                          
067210     MOVE IN-IDDC                    TO W-IDDC                            
067300     PERFORM IMS-GHU-WDK711                                               
067310     IF SEGMENT-FINNS                                                     
067400        MOVE JA                      TO SLAG-FLSKROT-BEORD                
067500        MOVE NEJ                     TO SLAG-FLSKROT-AUTO                 
067700        MOVE W-DATUM-DATE            TO SLAG-TISKROT-BEORD                
068000                                                                          
068100        PERFORM IMS-REPL-WDK711                                           
068110     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 DD-SKAPA-HANDELSETR-6321 SECTION.                                        
070100                                                                          
070200     MOVE IN-IDDC           TO W-IDDC-KVAL                                
070300     MOVE 'ANSK'            TO W-KDARBTYP                                 
070400     PERFORM IMS-GU-WDR501-6321                                           
070500     IF SEGMENT-SAKNAS                                                    
070600        MOVE '6321'         TO 6321-IDHTYP                                
070700        MOVE 'ANSK'         TO 6321-KDARBTYP                              
070800        MOVE LOW-VALUE      TO 6321-LOW-VALUE                             
070900        PERFORM IMS-ISRT-WDR501-6321                                      
071000                                                                          
071100     END-IF                                                               
071200                                                                          
071300     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
071400     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
071500     PERFORM IMS-ISRT-WDGX6322                                            
071600                                                                          
071700     MOVE IDARTNR-WS        TO 6324-IDARTNR                               
071800     MOVE IN-IDDC           TO 6324-IDDC                                  
071900     MOVE 1                 TO 6324-KDSTASKR                              
072000     MOVE NEJ               TO 6324-FLSKROT-GODK                          
072100     MOVE IN-IDANSK         TO 6324-IDPERSON                              
072200     MOVE DCS-IDDISTR-SKROT TO 6324-IDDISTR                               
072300     MOVE DCS-IDKUNDNR-SKROT TO 6324-IDKUNDNR                             
072400                                 WS-IDKUNDNR                              
072500     MOVE WS-AUTO-USERID    TO 6324-IDUSER                                
072600     MOVE ZERO              TO 6324-KDFRAKT                               
072700     MOVE 1                 TO 6324-KDORDKL                               
072800     MOVE WKVSKRANT         TO 6324-KVSKROT-BEORD                         
072900     MOVE SPACE             TO 6324-IDANALYS                              
073000                               6324-FLJUSTBUFF                            
073100                               6324-IDKST                                 
073200     MOVE ZERO              TO 6324-IDKONTO                               
073300                               6324-KVSKROT-KVAR                          
073400     MOVE ZERO              TO 6324-KVSKROT-ONDEM                         
073500     MOVE WS-SPAR-BEANST-GODK  TO 6324-BEANST                             
073600     COMPUTE WS-SUARTSTD ROUNDED =                                        
073700             6324-KVSKROT-BEORD * IN-PRMATRL                              
073800                                                                          
073806     MOVE 'AUTOMATBEORDRAD' TO 6324-BELAGINS-DEL                          
073807                                                                          
075300     MOVE IN-KDERS          TO 6324-KDERS-UTG                             
075400                                                                          
075700     COMPUTE 6324-KVTILLG-SDC ROUNDED = IN-KVLS                           
075800                                      - IN-KVRESS                         
076010                                      - IN-KVOKS-BULK                     
076020                                      - IN-KVOKS-DAG                      
076100                                                                          
076310     MOVE +0 TO 6324-KVTILLG-CDC                                          
076400                                                                          
076500     COMPUTE 6324-KVAKS-SDC ROUNDED = IN-KVAKS-SDC                        
076600                                    + IN-KVAKS-PAV                        
076800                                                                          
077010     MOVE +0 TO 6324-KVAKS-CDC                                            
077100                                                                          
077700     MOVE ZERO              TO 6324-SUTPO-TOT                             
077900                                                                          
078000     PERFORM DDD-LAS-FLYTTA-WDN6                                          
078100********************************************                              
078200                                                                          
078300     PERFORM IMS-ISRT-WDGX6324                                            
078400     ADD +1 TO ANT-SKROTU                                                 
078500********************************************                              
078600     .                                                                    
078700     EJECT                                                                
080900 DDD-LAS-FLYTTA-WDN6 SECTION.                                             
081000     MOVE +1 TO IX                                                        
081100     PERFORM UNTIL IX > 20                                                
081200        MOVE SPACE        TO 6324-BEEMBLEM (IX)                           
081300        ADD +1        TO IX                                               
081400     END-PERFORM                                                          
081500     PERFORM IMS-GET-WDN601                                               
081600     IF SEGMENT-FINNS                                                     
081700        PERFORM IMS-GET-WDN611                                            
081800        MOVE +1 TO IX                                                     
081900        PERFORM UNTIL IX > 20 OR SEGMENT-SAKNAS                           
082000           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (IX)                        
082100           ADD +1        TO IX                                            
082200           PERFORM IMS-GET-WDN611                                         
082300        END-PERFORM                                                       
082400     END-IF                                                               
082500     .                                                                    
082600     EJECT                                                                
082700 DE-SKAPA-TEMEMO    SECTION.                                              
082800                                                                          
082900     MOVE JA                         TO SW-AUT-GODK                       
083000     MOVE SPACE                      TO WS-TEMEMO                         
083100     MOVE ZERO                       TO IX-RAD                            
083200     IF IN-KVSPANT > ZERO                                                 
083300        ADD +1                       TO IX-RAD                            
083400        MOVE 'BLOCKED QUANTITY'      TO WS-TEMEMO                         
083500        PERFORM DEA-ISRT-6325                                             
083600        MOVE NEJ TO SW-AUT-GODK                                           
083700     END-IF                                                               
083800                                                                          
083900     IF IN-FLIART = JA                                                    
084000        ADD +1                       TO IX-RAD                            
084100        MOVE 'INCLUDED IN KIT'       TO WS-TEMEMO                         
084200*  TESTA FÖRST OM SJÄLVA SATSEN ÄR 09-MÄRKT, ANNARS                       
084300        MOVE NEJ TO SW-SATS-OK                                            
084400        PERFORM DEB-KOLLA-SATS                                            
084500        IF SW-SATS-OK = JA                                                
084600           PERFORM DEA-ISRT-6325                                          
084700           MOVE NEJ TO SW-AUT-GODK                                        
084800        END-IF                                                            
084900     END-IF                                                               
085000                                                                          
085100     IF IN-KDPRODSL = 21                                                  
085200        ADD +1                       TO IX-RAD                            
085300        MOVE '300-/400-SERIES '      TO WS-TEMEMO                         
085400        PERFORM DEA-ISRT-6325                                             
085500        MOVE NEJ TO SW-AUT-GODK                                           
085600     END-IF                                                               
085700                                                                          
085800     IF IN-KDPRODSL = 14                                                  
085900        ADD +1                       TO IX-RAD                            
086000        MOVE 'EXCHANGE'              TO WS-TEMEMO                         
086100        PERFORM DEA-ISRT-6325                                             
086200        MOVE NEJ TO SW-AUT-GODK                                           
086300     END-IF                                                               
086400                                                                          
086500     IF IN-KDPRODSL = 15 OR 16 OR 17 OR 25 OR 26                          
086600        ADD +1                       TO IX-RAD                            
086700        MOVE 'ACCESSORIES     '      TO WS-TEMEMO                         
086800        PERFORM DEA-ISRT-6325                                             
086900        MOVE NEJ TO SW-AUT-GODK                                           
087000     END-IF                                                               
087100                                                                          
087200     MOVE ZERO      TO WS-KVBR                                            
087300     MOVE W-IDARTNR TO W-IDARTNR-D9                                       
087400     MOVE IN-IDDC   TO W-IDDC-D9                                          
087500     PERFORM IMS-GET-WDD901                                               
087600     IF SEGMENT-FINNS                                                     
087700        PERFORM IMS-GET-WDD902                                            
087800        PERFORM UNTIL SEGMENT-SAKNAS                                      
087900           ADD LEV-KVBR    TO WS-KVBR                                     
088000           PERFORM IMS-GET-WDD902                                         
088100        END-PERFORM                                                       
088200     END-IF                                                               
088300     IF WS-KVBR > ZERO                                                    
088400        ADD +1                       TO IX-RAD                            
088500        MOVE 'ORDER QUANTITY   '     TO WS-TEMEMO                         
088600        PERFORM DEA-ISRT-6325                                             
088700        MOVE NEJ TO SW-AUT-GODK                                           
088800     END-IF                                                               
088900                                                                          
089900     IF IN-IDFKNGRP > 1000 AND                                            
090000        IN-IDFKNGRP < 2000                                                
090100        ADD +1                       TO IX-RAD                            
090200        MOVE 'STANDARD'              TO WS-TEMEMO                         
090300        PERFORM DEA-ISRT-6325                                             
090400        MOVE NEJ TO SW-AUT-GODK                                           
090500     END-IF                                                               
090600                                                                          
090700     IF IN-IDFKNGRP > 8840 AND                                            
090800        IN-IDFKNGRP < 8849                                                
090900        ADD +1                       TO IX-RAD                            
091000        MOVE 'SAFETY PRODUCT    '    TO WS-TEMEMO                         
091100        PERFORM DEA-ISRT-6325                                             
091200        MOVE NEJ TO SW-AUT-GODK                                           
091300     END-IF                                                               
091400                                                                          
091500     PERFORM DEC-KOLLA-DEKAL                                              
091600     .                                                                    
091700     EJECT                                                                
091800 DEA-ISRT-6325      SECTION.                                              
091900                                                                          
092000     MOVE IX-RAD                  TO 6325-IDRADNR                         
092100     MOVE WS-TEMEMO               TO 6325-TEMEMO                          
092200     MOVE 6321-KDARBTYP           TO W-KDARBTYP                           
092300     MOVE 6322-DASKROT9-BEORD     TO W-DASKROT9-BEORD                     
092400     MOVE 6324-IDARTNR            TO W-IDARTNR-KVAL                       
092600     MOVE 6324-IDDC               TO W-IDDC-KVAL                          
092610     MOVE 6324-KDSTASKR           TO W-KDSTASKR-KVAL                      
092700     PERFORM IMS-ISRT-WDGX6325                                            
092800     .                                                                    
092900     EJECT                                                                
094200 DEB-KOLLA-SATS     SECTION.                                              
094300                                                                          
094400*LÄS WDJ1 FÖR ATT FÅ FRAM VILKA SATSER ARTIKELN INGÅR I                   
094500*LÄS DÄREFTER WDK6 FÖR SATSNUMRET FÖR ATT FÅ FRAM OM 09-MÄRKT             
094600     MOVE W-IDARTNR TO W-IDARTNR-S                                        
094700     PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                       
094800     PERFORM UNTIL SEGMENT-SAKNAS OR SW-SATS-OK = JA                      
094900       IF SEGMENT-FINNS                                                   
095000         MOVE SATS-STR-IDARTNR TO W-IDARTNR-PCB2                          
095100         PERFORM IMS-GET-WDK611-2                                         
095200         IF SEGMENT-FINNS AND CLAG2-CLAG-KDERS NOT = 09                   
095300           MOVE JA TO SW-SATS-OK                                          
095400         END-IF                                                           
095500       END-IF                                                             
095600       PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                     
095700     END-PERFORM                                                          
095800     .                                                                    
095900     EJECT                                                                
096600 DEC-KOLLA-DEKAL       SECTION.                                           
096700                                                                          
096800     MOVE NEJ                 TO SW-DEKAL                                 
096900     MOVE SPACE               TO WS-TEXT                                  
097000     MOVE 'GB '               TO W-IDSKYLT                                
097100     MOVE SPACE               TO BENA11-TEXT-BEART                        
097200     PERFORM IMS-GET-BENA11-BSEQ                                          
097300     MOVE BENA11-TEXT-BEART   TO WS-TEXT-GB                               
097400     MOVE 'S  '               TO W-IDSKYLT                                
097500     MOVE SPACE               TO BENA11-TEXT-BEART                        
097600     PERFORM IMS-GET-BENA11-BSEQ                                          
097700     MOVE BENA11-TEXT-BEART   TO WS-TEXT-SV                               
097800     MOVE WS-TEXT             TO W041-BESTEXT                             
097900     MOVE 50                  TO W041-DIFAELT                             
098000                                                                          
098100     MOVE 'DEKAL'             TO W041-BESORD                              
098200     CALL W009LTXT USING W041-W009W041                                    
098300     IF W041-OK                                                           
098400        MOVE JA               TO SW-DEKAL                                 
098500     END-IF                                                               
098600                                                                          
098700     MOVE 'DECAL'             TO W041-BESORD                              
098800     CALL W009LTXT USING W041-W009W041                                    
098900     IF W041-OK                                                           
099000        MOVE JA               TO SW-DEKAL                                 
099100     END-IF                                                               
099200                                                                          
099300     IF SW-DEKAL = JA                                                     
099400        ADD +1                TO IX-RAD                                   
099500        MOVE 'DECAL   '       TO WS-TEMEMO                                
099600        PERFORM DEA-ISRT-6325                                             
099700        MOVE NEJ              TO SW-AUT-GODK                              
099800     END-IF                                                               
099900     .                                                                    
100000     EJECT                                                                
100100 DF-TEST-AUT-GODK  SECTION.                                               
100200                                                                          
100300     IF WS-SUARTSTD < WS-MAX-SUBEL AND                                    
100400        SW-AUT-GODK = JA                                                  
100500        MOVE IN-IDARTNR       TO UT-IDARTNR                               
100600        MOVE IN-IDDC          TO UT-IDDC                                  
100700        MOVE DAGENS-DATUM-Y2K TO UT-DADATUM                               
100800        MOVE WS-SUARTSTD      TO UT-SUARTSTD                              
100900        MOVE WS-IDKUNDNR      TO UT-IDKUNDNR                              
100910        MOVE IN-KDERS         TO UT-KDERS                                 
101000                                                                          
101100        PERFORM S11-SKRIV-W26193                                          
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 Z-FINIT SECTION.                                                         
101600                                                                          
101700                                                                          
101800     CLOSE W26192                                                         
101900           W26193                                                         
102000     SKIP2                                                                
102100     MOVE 'S' TO POSTSUM-OPKOD                                            
102200     CALL POSTSUM USING POSTSUM-PARM                                      
102300     .                                                                    
102400     EJECT                                                                
102500 S01-LAES-W26192  SECTION.                                                
102600                                                                          
102700     READ W26192 INTO IN-AREA                                             
102800     AT END                                                               
102900        SET END-OF-W26192 TO TRUE                                         
103000                                                                          
103100     NOT AT END                                                           
103200        MOVE 'W26192'   TO POSTSUM-FDNAMN                                 
103300        MOVE 'W26193D1' TO POSTSUM-DDNAMN2                                
103400        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
103500        CALL POSTSUM USING POSTSUM-PARM                                   
103600                                                                          
103700     END-READ                                                             
103800     .                                                                    
103900     EJECT                                                                
104000 S11-SKRIV-W26193 SECTION.                                                
104100                                                                          
104200     WRITE UT-POST FROM UT-AREA                                           
104300                                                                          
104400     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
104500     MOVE 'W26193'   TO POSTSUM-FDNAMN                                    
104600     MOVE 'W26193D2' TO POSTSUM-DDNAMN2                                   
104700     CALL POSTSUM USING POSTSUM-PARM                                      
104800     .                                                                    
104900     EJECT                                                                
105000 X-TAG-CHECKPOINT   SECTION.                                              
105100                                                                          
105200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
105300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
105400     PERFORM IMS-CHECKPOINT                                               
105500     MOVE ZERO TO CHKP-ANT                                                
105600* --- LÄS OM DATABAS OM DET BEHÖVS                                        
105700     .                                                                    
105800     EJECT                                                                
105900* --- IMS SEKTIONER ---                                                   
106000                                                                          
106100 IMS-GU-WDB601    SECTION.                                                
106300                                                                          
106400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
106500          DELIMITED BY SIZE INTO SSA1                                     
106600     MOVE '  '                TO GODK-STATUSKODER                         
106700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
106800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
106900     PERFORM IMS-STATUSKONTROLL                                           
107000     .                                                                    
107100                                                                          
114800     EJECT                                                                
114810 IMS-GET-WDK611-2 SECTION.                                                
114820                                                                          
114830     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-2 ')'                         
114840          DELIMITED BY SIZE INTO SSA1                                     
114850     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
114860          DELIMITED BY SIZE INTO SSA2                                     
114870     MOVE '  GE' TO GODK-STATUSKODER                                      
114880     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-WDK611-2 SSA1 SSA2            
114890     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
114891     PERFORM IMS-STATUSKONTROLL                                           
114892     .                                                                    
114893     EJECT                                                                
114900 IMS-GET-WDJ1-CSEQ-NEXT SECTION.                                          
115000     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
115100             DELIMITED BY SIZE INTO SSA1                                  
115200     MOVE 'WDJ101   ' TO SSA2                                             
115300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
115400     CALL CBLTDLI USING GN WDJ1C-PCB DLI-IO-WDJ1                          
115500                SSA1 SSA2                                                 
115600     MOVE WDJ1C-STATUS-CODE TO STATUS-WS                                  
115700     PERFORM IMS-STATUSKONTROLL                                           
115800     .                                                                    
115900     EJECT                                                                
116000 IMS-GU-WDR501-6321 SECTION.                                              
116100                                                                          
116200     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
116300            DELIMITED BY SIZE INTO SSA1                                   
116400     MOVE 'GE  '                TO GODK-STATUSKODER                       
116500     CALL CBLTDLI USING GU   6321-PCB DLI-IO-WDR501-6321 SSA1             
116600     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
116700     PERFORM IMS-STATUSKONTROLL                                           
116800     .                                                                    
116900     SKIP3                                                                
117000 IMS-ISRT-WDR501-6321 SECTION.                                            
117100                                                                          
117200     STRING 'WDR501     '                                                 
117300            DELIMITED BY SIZE INTO SSA1                                   
117400     MOVE '  '                  TO GODK-STATUSKODER                       
117500     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
117600     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
117700     PERFORM IMS-STATUSKONTROLL                                           
117800     ADD +1  TO CHKP-ANT                                                  
117900     .                                                                    
118000     EJECT                                                                
118100 IMS-ISRT-WDGX6322 SECTION.                                               
118200                                                                          
118300     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
118400            DELIMITED BY SIZE INTO SSA1                                   
118500     MOVE 'WDGX6322'            TO SSA2                                   
118600     MOVE '  II'                TO GODK-STATUSKODER                       
118700     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
118800     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
118900     PERFORM IMS-STATUSKONTROLL                                           
119000     ADD +1  TO CHKP-ANT                                                  
119100     SKIP3                                                                
119200     .                                                                    
119300     SKIP3                                                                
119400 IMS-ISRT-WDGX6324 SECTION.                                               
119500                                                                          
119600     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
119700            DELIMITED BY SIZE INTO SSA1                                   
119800     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
119900            DELIMITED BY SIZE INTO SSA2                                   
120000     MOVE 'WDGX6324'            TO SSA3                                   
120100     MOVE '  '                  TO GODK-STATUSKODER                       
120200     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
120300                                      SSA1 SSA2 SSA3                      
120400     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
120500     PERFORM IMS-STATUSKONTROLL                                           
120600     ADD +1  TO CHKP-ANT                                                  
120700     SKIP3                                                                
120800     .                                                                    
120900     EJECT                                                                
121000 IMS-ISRT-WDGX6325 SECTION.                                               
121100                                                                          
121200     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
121300            DELIMITED BY SIZE INTO SSA1                                   
121400     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
121500            DELIMITED BY SIZE INTO SSA2                                   
121600     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
121700          DELIMITED BY SIZE INTO SSA3                                     
121800     MOVE 'WDGX6325 '           TO SSA4                                   
121900     MOVE '  II'                TO GODK-STATUSKODER                       
122000     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6325                     
122100                                      SSA1 SSA2 SSA3 SSA4                 
122200     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
122300     PERFORM IMS-STATUSKONTROLL                                           
122400     ADD +1  TO CHKP-ANT                                                  
122500     .                                                                    
122600     EJECT                                                                
122700 IMS-GET-WDR501-6327 SECTION.                                             
122800     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
122900          DELIMITED BY SIZE INTO SSA1                                     
123000     MOVE '  GE' TO GODK-STATUSKODER                                      
123100     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDR501-6327 SSA1               
123200     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
123300     PERFORM IMS-STATUSKONTROLL                                           
123400     .                                                                    
123500     SKIP3                                                                
123600 IMS-GET-WDGX6328 SECTION.                                                
123700     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
123800          DELIMITED BY SIZE INTO SSA1                                     
123900     MOVE '  GE' TO GODK-STATUSKODER                                      
124000     CALL CBLTDLI USING GHNP 6327-PCB DLI-IO-WDGX6328 SSA1                
124100     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     EJECT                                                                
126601 IMS-GHU-WDK711 SECTION.                                                  
126603                                                                          
126604     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
126605          DELIMITED BY SIZE INTO SSA1                                     
126606     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
126607          DELIMITED BY SIZE INTO SSA2                                     
126608     MOVE '  GE' TO GODK-STATUSKODER                                      
126609     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
126610     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
126611     PERFORM IMS-STATUSKONTROLL                                           
126612     .                                                                    
126613     EJECT                                                                
126614 IMS-REPL-WDK711 SECTION.                                                 
126616                                                                          
126617     MOVE '  ' TO GODK-STATUSKODER                                        
126618     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
126619     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
126620     PERFORM IMS-STATUSKONTROLL                                           
126621     ADD +1  TO CHKP-ANT                                                  
126622     .                                                                    
126630 IMS-GET-WDN601 SECTION.                                                  
126700                                                                          
126800     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
126900          DELIMITED BY SIZE INTO SSA1                                     
127000     MOVE '  GE' TO GODK-STATUSKODER                                      
127100     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
127200     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
127300     PERFORM IMS-STATUSKONTROLL                                           
127400     .                                                                    
127500     EJECT                                                                
127600 IMS-GET-WDN611 SECTION.                                                  
127700                                                                          
127800     STRING 'WDN611  (WDN611KY =' W-WDN611KY-X ')'                        
127900          DELIMITED BY SIZE INTO SSA1                                     
128000     MOVE '  GE' TO GODK-STATUSKODER                                      
128100     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
128200     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
128300     PERFORM IMS-STATUSKONTROLL                                           
128400     .                                                                    
128500     EJECT                                                                
128600 IMS-GET-WDD901 SECTION.                                                  
128700                                                                          
128800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE '  GE' TO GODK-STATUSKODER                                      
129100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
129200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
129300     PERFORM IMS-STATUSKONTROLL                                           
129400     .                                                                    
129500     EJECT                                                                
129600 IMS-GET-WDD902 SECTION.                                                  
129700                                                                          
129800     STRING 'WDD902     '                                                 
129900          DELIMITED BY SIZE INTO SSA1                                     
130000     MOVE '  GE' TO GODK-STATUSKODER                                      
130100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
130200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
130300     PERFORM IMS-STATUSKONTROLL                                           
130400     .                                                                    
130500     EJECT                                                                
130600 IMS-GET-BENA11-BSEQ SECTION.                                             
130700                                                                          
130800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
130900             DELIMITED BY SIZE INTO SSA1                                  
131000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
131100             DELIMITED BY SIZE INTO SSA2                                  
131200     MOVE '  GE' TO GODK-STATUSKODER                                      
131300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD3 SSA1 SSA2                 
131400     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     .                                                                    
131700     EJECT                                                                
131800 IMS-GU-WDQ4B1 SECTION.                                                   
131900                                                                          
132000     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN                          
132100                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                          
132200                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
132300                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X ')'                   
132800                                                                          
132900          DELIMITED BY SIZE INTO SSA1                                     
133000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
133100     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
133200     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     SKIP3                                                                
133600 IMS-GN-WDQ4B1 SECTION.                                                   
133700                                                                          
133800     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN                          
133900                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                          
134000                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
134100                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X ')'                   
134600          DELIMITED BY SIZE INTO SSA1                                     
134700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
134800     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
134900     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
135000     PERFORM IMS-STATUSKONTROLL                                           
135100     .                                                                    
135200     SKIP3                                                                
135300 IMS-GU-WDL811 SECTION.                                                   
135400                                                                          
135500     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
135600          DELIMITED BY SIZE INTO SSA1                                     
135700     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
135800          DELIMITED BY SIZE INTO SSA2                                     
135900     MOVE '  GE' TO GODK-STATUSKODER                                      
136000     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
136100     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
136200     PERFORM IMS-STATUSKONTROLL                                           
136300     .                                                                    
136400     EJECT                                                                
136500 IMS-RESTART SECTION.                                                     
136600     SKIP2                                                                
136700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
136800     MOVE '  ' TO GODK-STATUSKODER                                        
136900     CALL CBLTDLI USING XRST MSG-PCB                                      
137000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
137100                        CHKP-AREA-LENGTH CHKP-AREA                        
137200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137300     PERFORM IMS-STATUSKONTROLL                                           
137400     .                                                                    
137500     SKIP3                                                                
137600 IMS-CHECKPOINT SECTION.                                                  
137700     SKIP2                                                                
137800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
137900     MOVE '  XD' TO GODK-STATUSKODER                                      
138000     CALL CBLTDLI USING CHKP MSG-PCB                                      
138100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
138200                        CHKP-AREA-LENGTH CHKP-AREA                        
138300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138400     PERFORM IMS-STATUSKONTROLL                                           
138500                                                                          
138600     IF IMS-EJ-OK                                                         
138700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
138800       DISPLAY FELTEXT                                                    
138900       CALL FELLOG                                                        
139000     END-IF                                                               
139100     .                                                                    
139200     EJECT                                                                
139300 IMS-STATUSKONTROLL SECTION.                                              
139400     SKIP2                                                                
139500     SET STATUS-IX TO 1                                                   
139600     SEARCH GODK-STATUS                                                   
139700       AT END                                                             
139800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
139900           DELIMITED BY SIZE INTO FELTEXT                                 
140000         DISPLAY FELTEXT                                                  
140100         CALL FELLOG                                                      
140200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
140300         CONTINUE                                                         
140400     END-SEARCH                                                           
140500     .                                                                    
140600     EJECT                                                                
