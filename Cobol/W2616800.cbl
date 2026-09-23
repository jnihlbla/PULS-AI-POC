000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2616800.                                                
000400 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000500 DATE-WRITTEN.   07/03/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        FRAMSTÄLLER UNDERLAG FÖR AUTO-SKROTNING                          
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK6                                       
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*        PROGRAMMET LÄSER      WDK9                                       
001500*        PROGRAMMET LÄSER      WDJ1C                                      
001600*        PROGRAMMET UPPDATERAR WDR5                                       
001700*        PROGRAMMET LÄSER      WDR5                                       
001800*        PROGRAMMET LÄSER      WDK7                                       
001900*        PROGRAMMET LÄSER      WDN6                                       
002000*        PROGRAMMET LÄSER      WDD9                                       
002100*        PROGRAMMET LÄSER      WDD3                                       
002200*        PROGRAMMET LÄSER      WDQ4 (WDQ4B1)                              
002300*        PROGRAMMET LÄSER      WDL8                                       
002400*                                                                         
002500*        THE PROGRAM READS   TABLE TP1KAMP                                
002600*        THE PROGRAM READS   TABLE TP1ARTK                                
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*          --- ARTIKLAR ATT SKROTA AUTO                                   
003700     SELECT W26168                     ASSIGN TO W26168D1.                
003800*          --- ARTIKLAR FÖR AUTOM. GODK.                                  
003900     SELECT W26169                     ASSIGN TO W26168D2.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W26168                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W26165   -L.                                                   
005000     SKIP3                                                                
005100 FD  W26169                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST -COPY W26169 -PRE  UT-  -L.                                     
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)    VALUE 'W2616800'.            
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
007600 77  W26168-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W26168                       VALUE 'J'.                   
007800 77  SW-K611-OK                  PIC X       VALUE 'N'.                   
007900 77  SW-KAMPANJ                  PIC X       VALUE 'N'.                   
008000 77  SW-DEKAL                    PIC X       VALUE 'N'.                   
008100 77  SW-SATS-OK                  PIC X       VALUE 'N'.                   
008200 77  SW-AUT-GODK                 PIC X       VALUE 'N'.                   
008300     EJECT                                                                
008400 01  SWITCHAR.                                                            
008500     03  INGAR-SATS-SW           PIC X.                                   
008600         88  INGAR-I-SATS    VALUE 'J'.                                   
008700*      --- VALID IDDC CODES                                               
008800*                                                                         
008900*01    -COPY WWDCKONS                                                     
009000     EJECT                                                                
009100*01    -COPY WWPRODSL                                                     
009200     EJECT                                                                
009300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-DATUM.                                       
009500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009800                                                                          
009900 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
010000     EJECT                                                                
010100 01  DYNAMISKA-SUBPROGRAM.                                                
010200*                                                                         
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     03  W009LTXT                PIC X(8)    VALUE 'W009LTXT'.            
010800     SKIP3                                                                
010900 01  ARB.                                                                 
011000     03  WARTC-KVUTRS-C1         PIC S9(7)   COMP-3.                      
011100     03  WARTC-KVAKS-C1          PIC S9(7)   COMP-3.                      
011200     03  WARTC-KDERS-C1          PIC S9(3)   COMP-3.                      
011300     03  WARTC-KVLS-C1           PIC S9(7)   COMP-3.                      
011400     03  WARTC-KVRESS-C1         PIC S9(7)   COMP-3.                      
011500     03  WARTC-IDANSK            PIC S9(3)   COMP-3.                      
011600     03  WARTC-KVSPANT           PIC S9(7)   COMP-3.                      
011700     03  WS-KVOKS-C1             PIC S9(6)   VALUE ZERO.                  
011800     03  IDARTNR-WS              PIC S9(9)   VALUE ZERO COMP-3.           
011900     03  WS-FLERS                PIC X(1)    VALUE SPACE.                 
012000                                                                          
012100     03  W-SDC-KVLS              PIC S9(7)   VALUE ZERO COMP-3.           
012200     03  W-SDC-KVAKS             PIC S9(7)   VALUE ZERO COMP-3.           
012300     03  W-SDC-KVOKS             PIC S9(7)   VALUE ZERO COMP-3.           
012400                                                                          
012500     03  WS-AUTO-USERID          PIC X(8)    VALUE 'W2616800'.            
012600     03  WS-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
012700     03  WKVSKRANT               PIC S9(7).                               
012800     03  IX-RAD                  PIC S9(3)   VALUE ZERO COMP-3.           
012900     03  WS-TEMEMO               PIC X(25)   VALUE SPACE.                 
013000     03  WS-KVBR                 PIC S9(7)   VALUE ZERO COMP-3.           
013100     03  ANT-SKROTU              PIC S9(3)   VALUE ZERO COMP-3.           
013200     03  MAX-ANT-SKROTU          PIC S9(3)   VALUE 500  COMP-3.           
013300     03  WS-KVBEART              PIC S9(7)   VALUE ZERO COMP-3.           
013400     03  WS-MAX-SUBEL            PIC S9(7)   VALUE ZERO COMP-3.           
013500     03  WS-SUARTSTD             PIC 9(7)V9(2) VALUE ZERO.                
013600     03  WS-TIAAAA               PIC 9(4)    VALUE ZERO.                  
013700     03  FILLER  REDEFINES  WS-TIAAAA.                                    
013800         05  WS-TISEKEL          PIC 9(2).                                
013900         05  WS-TIAA-VECKA       PIC 9(2).                                
014000     03  WS-TIAAAA-1             PIC 9(4)    VALUE ZERO.                  
014100     03  WS-TIVV                 PIC S9(3)   VALUE ZERO  COMP-3.          
014200     03  WS-ANTAL-OI             PIC S9(7)   VALUE ZERO COMP-3.           
014300     03  WS-TEXT.                                                         
014400         05  WS-TEXT-GB          PIC X(25).                               
014500         05  WS-TEXT-SV          PIC X(25).                               
014600                                                                          
014700 01  W-DATUM.                                                             
014800     05  W-DATUM-DATE    PIC X(6).                                        
014900     EJECT                                                                
015000*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
015100                                                                          
015200*01  -COPY WDATAREA                                                       
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL POSTSUM                                          
015500*                                                                         
015600*01  -COPY W0005   -PRE  POSTSUM-                                         
015700     EJECT                                                                
015800*    --- TEXTSÖKNING                                                      
015900*                                                                         
016000*01  -COPY W009W041                                                       
016100     EJECT                                                                
016200 01  IN-AREA-START               PIC X(24)   VALUE                        
016300                                             'IN-AREA-START'.             
016400     SKIP2                                                                
016500                                                                          
016600*01  AREA -COPY W26165     -PRE IN-                                       
016700*                                                                         
016800     SKIP2                                                                
016900                                                                          
017000*01  AREA -COPY W26169     -PRE UT-                                       
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-IDARTNR-X.                                                     
017600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017700     03  W-WDD901KY-X.                                                    
017800         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
017900         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
018000     03  W-KDSEGKEY-X.                                                    
018100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018200     03  W-DASKROT-X.                                                     
018300         05  W-DASKROT           PIC X(8)    VALUE SPACE.                 
018400     03  W-IDSKYLT-X.                                                     
018500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
018600     03  W-WDJ1CSEQ-X.                                                    
018700         05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                 
018800         05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                 
018900         05  W-IDARTNR-S         PIC S9(9)   COMP-3 VALUE ZERO.           
019000     03  W-DASKROT9-X.                                                    
019100         05  W-DASKROT9          PIC S9(8)   VALUE ZERO COMP-3.           
019200     03  W-KY6324-X.                                                      
019300         05  W-KY6324            PIC X(8)    VALUE SPACE.                 
019400     03  W-KY6328-X.                                                      
019500         05  W-KY6328            PIC X(15)    VALUE SPACE.                
019600     03  W-IDDC-X.                                                        
019700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019800     03  W-WDN611KY-X.                                                    
019900         05  W-WDN611KY          PIC X(6)    VALUE SPACE.                 
020000     03  W-IDLEVNR-X.                                                     
020100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
020200                                                                          
020300     03  W-IDHTYP-X.                                                      
020400         05  W-4533-IDHTYP        PIC X(04)    VALUE '4533'.              
020500                                                                          
020600     03  W-WDGXKEY-4534-X.                                                
020700         05  W-4534-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
020800         05  W-4534-IDDC         PIC X(2)     VALUE '11'.                 
020900         05  W-4534-LOW-VALUE    PIC X(03)    VALUE LOW-VALUE.            
021000                                                                          
021100     03  W-WDQ4B1KY-MAX.                                                  
021200         05  W-IDARTNR-Q4B1-MAX  PIC S9(9)    COMP-3.                     
021300         05  FILLER              PIC X(32)    VALUE HIGH-VALUE.           
021400                                                                          
021500     03  W-WDQ4B1KY-MIN.                                                  
021600         05  W-IDARTNR-Q4B1-MIN  PIC S9(9)    COMP-3.                     
021700         05  FILLER              PIC X(32)    VALUE LOW-VALUE.            
021800                                                                          
021900     03  W-IDDISTR-Q4B1-X.                                                
022000         05  W-IDDISTR-Q4B1      PIC S9(5)  VALUE ZERO COMP-3.            
022100                                                                          
022200     03  W-IDKUNDNR-Q4B1-X.                                               
022300         05  W-IDKUNDNR-Q4B1     PIC S9(7)  VALUE ZERO COMP-3.            
022400                                                                          
022500     03  W-IDKUNDNR-Q4B1-C-X.                                             
022600         05  W-IDKUNDNR-Q4B1-CL  PIC S9(7)  VALUE ZERO COMP-3.            
022700                                                                          
022800     03  W-TIAAAA-X.                                                      
022900         05  W-TIAAAA            PIC 9(4).                                
023000                                                                          
023100 01  W-IDARTNR-STR-X.                                                     
023200     03  W-IDARTNR-STR        PIC S9(9) COMP-3 VALUE ZERO.                
023300                                                                          
023400 01  W-IDARTNR-2.                                                         
023500     03  W-IDARTNR-PCB2       PIC S9(9) COMP-3 VALUE ZERO.                
023600                                                                          
023700 01  W-IDSKYLT-KEY-X.                                                     
023800   03  W-IDSKYLT-KEY     PIC X(3)    VALUE SPACE.                         
023900                                                                          
024000                                                                          
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
028300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
028400 01  DLI-IO-WDK601.                                                       
028500*    03  -COPY WDK601                                                     
028600     EJECT                                                                
028700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
028800 01  DLI-IO-WDK611.                                                       
028900*    03  -COPY WDK611                                                     
029000     EJECT                                                                
029100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK627'.                      
029200 01  DLI-IO-WDK627.                                                       
029300*    03  -COPY WDK627                                                     
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601-2'.                    
029500 01  DLI-IO-WDK601-2.                                                     
029600*    03  -COPY WDK601  -PRE ART2-                                         
029700     EJECT                                                                
029800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611-2'.                    
029900 01  DLI-IO-WDK611-2.                                                     
030000*    03  -COPY WDK611  -PRE CLAG2-                                        
030100     EJECT                                                                
030200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
030300 01  DLI-IO-WDK901.                                                       
030400*    03  -COPY WDK901  -PRE ARTM-                                         
030500     EJECT                                                                
030600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ1'.                        
030700 01  DLI-IO-WDJ1.                                                         
030800*    03  -COPY WDJ111 -PRE SATS-                                          
030900*    03  -COPY WDJ101 -PRE SATS-                                          
031000     EJECT                                                                
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
033300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
033400 01  DLI-IO-WDK701.                                                       
033500*    03  -COPY WDK701                                                     
033600     EJECT                                                                
033700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
033800 01  DLI-IO-WDK711.                                                       
033900*    03  -COPY WDK711                                                     
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
036300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL801'.                      
036400 01  DLI-IO-WDL801.                                                       
036500*    03  -COPY WDL801                                                     
036600     EJECT                                                                
036700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
036800 01  DLI-IO-WDL811.                                                       
036900*    03  -COPY WDL811                                                     
037000                                                                          
037100     EJECT                                                                
037200 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
037300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
037400                                                                          
037500 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
037600 01  DB2-WS.                                                              
037700     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
037800         88  CURSOR-OK                      VALUE 000.                    
037900         88  LINES-FOUND                    VALUE 000.                    
038000         88  LINES-MISSING                  VALUE 100.                    
038100         88  RESOURCE-WRONG                 VALUE 904.                    
038200     03  GOOD-SQLCODECODES.                                               
038300         05  GOOD-SQLCODE OCCURS 5                                        
038400             INDEXED BY SQLCODE-IX PIC 9(3).                              
038500                                                                          
038600 01  WS.                                                                  
038700     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
038800     03 FILLER                   PIC X(16)   VALUE                        
038900                                             'WS-DB2-SEKTION'.            
039000     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
039100     EJECT                                                                
039200 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
039300                                                                          
039400*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
039500     EJECT                                                                
039600 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
039700                                                                          
039800*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
039900     EJECT                                                                
040000     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
040100     EJECT                                                                
040200     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
040300     EJECT                                                                
040400 LINKAGE SECTION.                                                         
040500                                                                          
040600*01  -COPY W0009   -PRE MSG-                                              
040700                                                                          
040800*01  -COPY W0008  -PRE WDK6-                                              
040900     05  FILLER                  PIC X.                                   
041000                                                                          
041100*01  -COPY W0008  -PRE WDK62-                                             
041200     05  FILLER                  PIC X.                                   
041300                                                                          
041400*01  -COPY W0008  -PRE WDK9-                                              
041500     05  FILLER                  PIC X.                                   
041600                                                                          
041700*01  -COPY W0008  -PRE WDJ1C-                                             
041800     05  FILLER                  PIC X.                                   
041900                                                                          
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
044400 PROCEDURE DIVISION  USING MSG-PCB                                        
044500           WDK6-PCB WDK62-PCB  WDK9-PCB WDJ1C-PCB                         
044600                     6321-PCB  6327-PCB WDK7-PCB                          
044700           WDN6-PCB  WDD9-PCB  WDD3-PCB WDQ4B-PCB                         
044800           WDL8-PCB.                                                      
044900 MAIN SECTION.                                                            
045000     ENTRY 'DLITCBL' USING MSG-PCB                                        
045100           WDK6-PCB WDK62-PCB  WDK9-PCB WDJ1C-PCB                         
045200                     6321-PCB  6327-PCB WDK7-PCB                          
045300           WDN6-PCB  WDD9-PCB  WDD3-PCB WDQ4B-PCB                         
045400           WDL8-PCB.                                                      
045500                                                                          
045600     SKIP2                                                                
045700     PERFORM A-INIT                                                       
045800     PERFORM C-KOLLA-IDUSER                                               
045900     PERFORM S01-LAES-W26168                                              
046000     PERFORM UNTIL END-OF-W26168                                          
046100       IF CHKP-ANT > CHKP-MAX                                             
046200         PERFORM X-TAG-CHECKPOINT                                         
046300       END-IF                                                             
046400       MOVE IN-IDARTNR TO W-IDARTNR                                       
046500                          IDARTNR-WS                                      
046600       PERFORM IMS-GET-WDK601                                             
046700       IF SEGMENT-FINNS AND ANT-SKROTU < MAX-ANT-SKROTU                   
046710          MOVE ART-KDPRODSL           TO TEST-KDPRODSL                    
046720**        IF KDPRODSL-LYNK                                                
046721*****       NO SCRAP FOR LYNK PARTS                                       
046730**          CONTINUE                                                      
046740**        ELSE                                                            
046800          IF ART-FLIART = JA                                              
046900             MOVE ART-FLIART TO INGAR-SATS-SW                             
047000          ELSE                                                            
047100             MOVE NEJ        TO INGAR-SATS-SW                             
047200          END-IF                                                          
047300          MOVE ART-FLERS     TO WS-FLERS                                  
047400          PERFORM B-LAES-SKROTINFO                                        
047500                                                                          
047600          IF SW-K611-OK = JA                   AND                        
047700              CLAG-TISKROT-AUTO < DAGENS-DATUM AND                        
047800              CLAG-FLSKROT-BEORD NOT = JA                                 
047900              PERFORM D-UPPDATERA                                         
048000          END-IF                                                          
048010**        END-IF                                                          
048100       END-IF                                                             
048200       PERFORM S01-LAES-W26168                                            
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
049700     OPEN INPUT W26168                                                    
049800         OUTPUT W26169                                                    
049900                                                                          
050000                                                                          
050100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
050200                                                                          
050300     ACCEPT W-DATUM-DATE FROM DATE                                        
050400     ACCEPT DAGENS-DATUM FROM DATE                                        
050500                                                                          
050600     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
050700                                                                          
050800     INITIALIZE GOOD-SQLCODECODES                                         
050900                                                                          
051000     MOVE 'IDAG' TO DAT-KDDATFORM                                         
051100     CALL WDATKONV USING DAT-KDDATFORM,                                   
051200                         DAT-I-TIDATUM,                                   
051300                         DAT-O-TIDATUM,                                   
051400                         DAT-KDSVAR                                       
051500                                                                          
051600     IF DAT-KDSVAR-FEL                                                    
051700        DISPLAY '****  FEL I WDATKONV  *******'                           
051800********PERFORM S99-ABEND                                                 
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
053300 B-LAES-SKROTINFO SECTION.                                                
053400                                                                          
053500     PERFORM IMS-GET-WDK611                                               
053600     IF  SEGMENT-FINNS                    AND                             
053700         CLAG-TISKROT-AUTO < DAGENS-DATUM AND                             
053800         CLAG-FLSKROT-BEORD NOT = JA                                      
053900        MOVE JA               TO SW-K611-OK                               
054000        MOVE CLAG-KDERS       TO WARTC-KDERS-C1                           
054100        MOVE CLAG-KVLS        TO WARTC-KVLS-C1                            
054200        MOVE CLAG-KVRESS      TO WARTC-KVRESS-C1                          
054300        MOVE CLAG-IDANSK      TO WARTC-IDANSK                             
054400        MOVE CLAG-KVSPANT     TO WARTC-KVSPANT                            
054500                                                                          
054600        PERFORM IMS-GET-WDK901                                            
054700        IF SEGMENT-FINNS                                                  
054800                                                                          
054900           COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK   +                  
055000                                 ARTM-ART-KVOKS-DAG    +                  
055100                                 ARTM-ART-KVOKS-VOR                       
055200        ELSE                                                              
055300           MOVE ZERO          TO WS-KVOKS-C1                              
055400        END-IF                                                            
055500        IF INGAR-SATS-SW = JA                                             
055600           PERFORM BA-LAES-SATS-OI                                        
055700        END-IF                                                            
055800     ELSE                                                                 
055900        MOVE NEJ              TO SW-K611-OK                               
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 BA-LAES-SATS-OI SECTION.                                                 
056400                                                                          
056500*    EFTERSOM INGÅENDE SATSARTIKLAR INTE FINNS MED FRÅN S&T               
056600*    SÅ ANVÄNDER VI ORDERINGÅNG I STÄLLET FÖR FÖRSÄLJNING                 
056700*    FÖR DESSA (ANTAL SÅLDA SISTA RULLANDE ÅR < 150)                      
056800                                                                          
056900     MOVE ZERO            TO WS-ANTAL-OI                                  
057000     MOVE IN-IDARTNR      TO W-IDARTNR                                    
057100     MOVE WS-TIAAAA       TO W-TIAAAA                                     
057200     PERFORM IMS-GU-WDL811                                                
057300     IF SEGMENT-FINNS                                                     
057400        MOVE +1           TO WS-IX                                        
057500        PERFORM UNTIL WS-IX >= WS-TIVV                                    
057600           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
057700***********ADD AAR-KVOI-DIV  (WS-IX) TO WS-ANTAL-OI                       
057800           ADD +1         TO WS-IX                                        
057900        END-PERFORM                                                       
058000     END-IF                                                               
058100     MOVE WS-TIAAAA-1     TO W-TIAAAA                                     
058200     PERFORM IMS-GU-WDL811                                                
058300     IF SEGMENT-FINNS                                                     
058400        MOVE WS-TIVV      TO WS-IX                                        
058500        PERFORM UNTIL WS-IX >  53                                         
058600           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
058700***********ADD AAR-KVOI-DIV  (WS-IX) TO WS-ANTAL-OI                       
058800           ADD +1         TO WS-IX                                        
058900        END-PERFORM                                                       
059000     END-IF                                                               
059100     IF (IN-SULEVANT-RAAR + WS-ANTAL-OI) > 150                            
059200*       EJ AKTUELL FÖR SKROT                                              
059300        MOVE NEJ TO SW-K611-OK                                            
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 C-KOLLA-IDUSER SECTION.                                                  
059800                                                                          
059900     MOVE WC-CDC-SE              TO W-IDDC-6327                           
060000     MOVE 'ANSK'                 TO W-KDARBTYP-6327                       
060100     MOVE SPACE                  TO WS-SPAR-BEANST-GODK                   
060200     PERFORM IMS-GET-WDR501-6327                                          
060300     IF SEGMENT-FINNS                                                     
060400        MOVE WS-AUTO-USERID      TO W-IDUSER-GODK                         
060500        PERFORM IMS-GET-WDGX6328                                          
060600        IF SEGMENT-FINNS                                                  
060700           MOVE 6328-BEANST-GODK TO WS-SPAR-BEANST-GODK                   
060800           MOVE 6328-SUBEL       TO WS-MAX-SUBEL                          
060900        END-IF                                                            
061000     END-IF                                                               
061100     .                                                                    
061200     EJECT                                                                
061300 D-UPPDATERA SECTION.                                                     
061400                                                                          
061500     MOVE IDARTNR-WS TO W-IDARTNR                                         
061600                                                                          
061700     PERFORM DA-SKAPA-KVSKRANT                                            
061800     IF WKVSKRANT > ZERO                                                  
061900        PERFORM DB-UPPD-SKROTSPARR                                        
062000        PERFORM DC-SKAPA-HANDELSETR-6321                                  
062100        PERFORM DE-SKAPA-TEMEMO                                           
062200        PERFORM DF-TEST-AUT-GODK                                          
062300     END-IF                                                               
062400                                                                          
062500     .                                                                    
062600     EJECT                                                                
062700 DA-SKAPA-KVSKRANT SECTION.                                               
062800                                                                          
062900     COMPUTE WKVSKRANT =                                                  
063000             WARTC-KVLS-C1 - WARTC-KVRESS-C1                              
063100                           - WS-KVOKS-C1                                  
063200***************************- WARTC-KVSPANT BORT 22/5-07                   
063300                                                                          
063400     MOVE ART-KDPRODSL           TO TEST-KDPRODSL                         
063500     IF KDPRODSL-VCBV OR                                                  
063600        KDPRODSL-BIMA OR                                                  
063700        KDPRODSL-LOCAL OR                                                 
063800        KDPRODSL-ACC OR                                                   
063900        KDPRODSL-WHEELS OR                                                
064000       (IN-IDFKNGRP > 8845 AND IN-IDFKNGRP < 8849) OR                     
064100       (CLAG-REDIRLEV = 1)                                                
064200        MOVE 11             TO 6324-IDKUNDNR                              
064300                                 WS-IDKUNDNR                              
064400     ELSE                                                                 
064500********TILL CLASSIC  KDPRODSL = 11  14,17                                
064600        MOVE 111            TO 6324-IDKUNDNR                              
064700                                 WS-IDKUNDNR                              
064800     END-IF                                                               
064900     MOVE ZERO        TO WS-KVBEART                                       
065000     MOVE W-IDARTNR   TO W-IDARTNR-Q4B1-MAX                               
065100                         W-IDARTNR-Q4B1-MIN                               
065200     MOVE 81          TO W-IDDISTR-Q4B1                                   
065300     MOVE 11          TO W-IDKUNDNR-Q4B1                                  
065400     MOVE 111         TO W-IDKUNDNR-Q4B1-CL                               
065500                                                                          
065600                                                                          
065700     PERFORM IMS-GU-WDQ4B1                                                
065800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
065900                                                                          
066000          ADD SEQB-KVBEART-Q TO WS-KVBEART                                
066100                                                                          
066200        PERFORM IMS-GN-WDQ4B1                                             
066300     END-PERFORM                                                          
066400     IF WS-KVBEART > ZERO                                                 
066500        COMPUTE WKVSKRANT = WKVSKRANT - WS-KVBEART                        
066600     END-IF                                                               
066700                                                                          
066800     .                                                                    
066900     EJECT                                                                
067000 DB-UPPD-SKROTSPARR SECTION.                                              
067100                                                                          
067200     PERFORM IMS-GET-WDK601                                               
067300     PERFORM IMS-GET-WDK611                                               
067400     MOVE JA  TO CLAG-FLSKROT-BEORD                                       
067500     MOVE NEJ TO CLAG-FLSKROT-AUTO                                        
067600                                                                          
067700     PERFORM IMS-REPL-WDK611                                              
067800                                                                          
067900     PERFORM IMS-GET-WDK627                                               
068000                                                                          
068100     IF SEGMENT-FINNS                                                     
068200         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
068300                                                                          
068400         PERFORM IMS-REPL-WDK627                                          
068500     ELSE                                                                 
068600         MOVE ZERO                    TO SKROT-KVSKROT                    
068700         MOVE ZERO                    TO SKROT-DASKROT                    
068800         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
068900                                                                          
069000                                                                          
069100         PERFORM IMS-ISRT-WDK627                                          
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 DC-SKAPA-HANDELSETR-6321 SECTION.                                        
069600                                                                          
069700     MOVE WC-CDC-SE         TO W-IDDC-KVAL                                
069800     MOVE 'ANSK'            TO W-KDARBTYP                                 
069900     PERFORM IMS-GU-WDR501-6321                                           
070000     IF SEGMENT-SAKNAS                                                    
070100        MOVE '6321'         TO 6321-IDHTYP                                
070200        MOVE 'ANSK'         TO 6321-KDARBTYP                              
070300        MOVE LOW-VALUE      TO 6321-LOW-VALUE                             
070400        PERFORM IMS-ISRT-WDR501-6321                                      
070500                                                                          
070600     END-IF                                                               
070700                                                                          
070800     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
070900     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
071000     PERFORM IMS-ISRT-WDGX6322                                            
071100                                                                          
071200     MOVE IDARTNR-WS        TO 6324-IDARTNR                               
071300     MOVE WC-CDC-SE         TO 6324-IDDC                                  
071400     MOVE 1                 TO 6324-KDSTASKR                              
071500     MOVE NEJ               TO 6324-FLSKROT-GODK                          
071600     MOVE WARTC-IDANSK      TO 6324-IDPERSON                              
071700     MOVE 81                TO 6324-IDDISTR                               
071800     MOVE 11                TO 6324-IDKUNDNR                              
071900                                 WS-IDKUNDNR                              
072000     MOVE WS-AUTO-USERID    TO 6324-IDUSER                                
072100     MOVE ZERO              TO 6324-KDFRAKT                               
072200     MOVE 1                 TO 6324-KDORDKL                               
072300     MOVE WKVSKRANT         TO 6324-KVSKROT-BEORD                         
072400     MOVE SPACE             TO 6324-IDANALYS                              
072500                               6324-FLJUSTBUFF                            
072600                               6324-IDKST                                 
072700     MOVE ZERO              TO 6324-IDKONTO                               
072800                               6324-KVSKROT-KVAR                          
072900     MOVE ZERO              TO 6324-KVSKROT-ONDEM                         
073000     MOVE WS-SPAR-BEANST-GODK  TO 6324-BEANST                             
073100     COMPUTE WS-SUARTSTD ROUNDED =                                        
073200             6324-KVSKROT-BEORD * CLAG-PRARTSTD                           
073300                                                                          
073400     MOVE 'AUTOMATBEORDRAD' TO 6324-BELAGINS-DEL                          
073500     MOVE ART-KDPRODSL           TO TEST-KDPRODSL                         
073600     IF KDPRODSL-VCBV OR                                                  
073700        KDPRODSL-BIMA OR                                                  
073800        KDPRODSL-LOCAL OR                                                 
073900        KDPRODSL-ACC OR                                                   
074000        KDPRODSL-WHEELS OR                                                
074100       (IN-IDFKNGRP > 8845 AND IN-IDFKNGRP < 8849) OR                     
074200       (CLAG-REDIRLEV = 1)                                                
074300        CONTINUE                                                          
074400     ELSE                                                                 
074500********TILL CLASSIC  KDPRODSL = 11  14,17                                
074600        MOVE 482319         TO 6324-IDKONTO                               
074700        MOVE '158600000635' TO 6324-IDANALYS                              
074800        MOVE 111            TO 6324-IDKUNDNR                              
074900                                 WS-IDKUNDNR                              
075000     END-IF                                                               
075100********************************************                              
075200     MOVE CLAG-KDERS        TO 6324-KDERS-UTG                             
075300                                                                          
075400**** PERFORM DCA-LAS-FLYTTA-WDK7                                          
075500                                                                          
075600     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
075700            CLAG-KVLS     - CLAG-KVRESS                                   
075800                          - CLAG-KVROS                                    
075900                          - WS-KVOKS-C1                                   
076000                                                                          
076100     COMPUTE 6324-KVTILLG-SDC ROUNDED =                                   
076200            W-SDC-KVLS - W-SDC-KVOKS                                      
076300                                                                          
076400     COMPUTE 6324-KVAKS-CDC ROUNDED =                                     
076500          CLAG-KVAKS-CDC  + CLAG-KVAKS-PAV                                
076600                          + CLAG-KVAKS-T                                  
076700                                                                          
076800     COMPUTE 6324-KVAKS-SDC ROUNDED =                                     
076900          W-SDC-KVAKS                                                     
077000                                                                          
077100     PERFORM IMS-GET-WDK901                                               
077200     IF SEGMENT-FINNS                                                     
077300       COMPUTE 6324-SUTPO-TOT =                                           
077400                ARTM-ART-SUTPO-TOT                                        
077500     ELSE                                                                 
077600       MOVE ZERO              TO 6324-SUTPO-TOT                           
077700     END-IF                                                               
077800                                                                          
077900     PERFORM DCB-LAS-FLYTTA-WDN6                                          
078000********************************************                              
078100                                                                          
078200     PERFORM IMS-ISRT-WDGX6324                                            
078300     ADD +1 TO ANT-SKROTU                                                 
078400********************************************                              
078500     .                                                                    
078600     EJECT                                                                
078700 DCA-LAS-FLYTTA-WDK7 SECTION.                                             
078800                                                                          
078900     PERFORM IMS-GET-WDK701                                               
079000     IF SEGMENT-FINNS                                                     
079100        MOVE ZERO                TO W-SDC-KVLS                            
079200                                    W-SDC-KVAKS                           
079300                                    W-SDC-KVOKS                           
079400        PERFORM IMS-GET-WDK711                                            
079500        PERFORM UNTIL SEGMENT-SAKNAS                                      
079600          IF WC-CDC-SE = SLAG-IDDC                                        
079700             ADD SLAG-KVLS       TO W-SDC-KVLS                            
079800             ADD SLAG-KVAKS-SDC  TO W-SDC-KVAKS                           
079900             ADD SLAG-KVAKS-PAV  TO W-SDC-KVAKS                           
080000             ADD SLAG-KVOKS-DAG  TO W-SDC-KVOKS                           
080100             ADD SLAG-KVOKS-BULK TO W-SDC-KVOKS                           
080200          END-IF                                                          
080300          PERFORM IMS-GET-WDK711                                          
080400        END-PERFORM                                                       
080500     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
080800 DCB-LAS-FLYTTA-WDN6 SECTION.                                             
080900     MOVE +1 TO IX                                                        
081000     PERFORM UNTIL IX > 20                                                
081100        MOVE SPACE        TO 6324-BEEMBLEM (IX)                           
081200        ADD +1        TO IX                                               
081300     END-PERFORM                                                          
081400     PERFORM IMS-GET-WDN601                                               
081500     IF SEGMENT-FINNS                                                     
081600        PERFORM IMS-GET-WDN611                                            
081700        MOVE +1 TO IX                                                     
081800        PERFORM UNTIL IX > 20 OR SEGMENT-SAKNAS                           
081900           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (IX)                        
082000           ADD +1        TO IX                                            
082100           PERFORM IMS-GET-WDN611                                         
082200        END-PERFORM                                                       
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600 DE-SKAPA-TEMEMO    SECTION.                                              
082700                                                                          
082800     MOVE JA                         TO SW-AUT-GODK                       
082900     MOVE SPACE                      TO WS-TEMEMO                         
083000     MOVE ZERO                       TO IX-RAD                            
083100     IF CLAG-KVSPANT > ZERO                                               
083200        ADD +1                       TO IX-RAD                            
083300        MOVE 'SPÄRRAD KVANT '        TO WS-TEMEMO                         
083400        PERFORM DEA-ISRT-6325                                             
083500        MOVE NEJ TO SW-AUT-GODK                                           
083600     END-IF                                                               
083700                                                                          
083800     IF ART-FLIART = JA                                                   
083900        ADD +1                       TO IX-RAD                            
084000        MOVE 'INGÅR I SATS '         TO WS-TEMEMO                         
084100*  TESTA FÖRST OM SJÄLVA SATSEN ÄR 09-MÄRKT, ANNARS                       
084200        MOVE NEJ TO SW-SATS-OK                                            
084300        PERFORM DEB-KOLLA-SATS                                            
084400        IF SW-SATS-OK = JA                                                
084500           PERFORM DEA-ISRT-6325                                          
084600           MOVE NEJ TO SW-AUT-GODK                                        
084700        END-IF                                                            
084800     END-IF                                                               
084900                                                                          
085000     MOVE ART-KDPRODSL           TO TEST-KDPRODSL                         
085100     IF KDPRODSL-VCBV-PARTS                                               
085200        ADD +1                       TO IX-RAD                            
085300        MOVE '300-/400-SERIEN '      TO WS-TEMEMO                         
085400        PERFORM DEA-ISRT-6325                                             
085500        MOVE NEJ TO SW-AUT-GODK                                           
085600     END-IF                                                               
085700                                                                          
085800     IF KDPRODSL-BYTES                                                    
085900        ADD +1                       TO IX-RAD                            
086000        MOVE 'BYTES'                 TO WS-TEMEMO                         
086100        PERFORM DEA-ISRT-6325                                             
086200        MOVE NEJ TO SW-AUT-GODK                                           
086300     END-IF                                                               
086400                                                                          
086500     IF KDPRODSL-ACC OR                                                   
086600        KDPRODSL-WHEELS OR                                                
086700        KDPRODSL-BRANDON OR                                               
086800        KDPRODSL-VCBV-WHEELS                                              
086900        ADD +1                       TO IX-RAD                            
087000        MOVE 'TILLBEHÖR       '      TO WS-TEMEMO                         
087100        PERFORM DEA-ISRT-6325                                             
087200        MOVE NEJ TO SW-AUT-GODK                                           
087300     END-IF                                                               
087400                                                                          
087500     MOVE ZERO      TO WS-KVBR                                            
087600     MOVE W-IDARTNR TO W-IDARTNR-D9                                       
087700     MOVE WC-CDC-SE TO W-IDDC-D9                                          
087800     PERFORM IMS-GET-WDD901                                               
087900     IF SEGMENT-FINNS                                                     
088000        PERFORM IMS-GET-WDD902                                            
088100        PERFORM UNTIL SEGMENT-SAKNAS                                      
088200           ADD LEV-KVBR    TO WS-KVBR                                     
088300           PERFORM IMS-GET-WDD902                                         
088400        END-PERFORM                                                       
088500     END-IF                                                               
088600     IF WS-KVBR > ZERO                                                    
088700        ADD +1                       TO IX-RAD                            
088800        MOVE 'BESTÄLLNINGSREST '     TO WS-TEMEMO                         
088900        PERFORM DEA-ISRT-6325                                             
089000        MOVE NEJ TO SW-AUT-GODK                                           
089100     END-IF                                                               
089200                                                                          
089300     MOVE NEJ                        TO SW-KAMPANJ                        
089400     PERFORM DEC-KOLLA-KAMPANJ-DB2                                        
089500     IF SW-KAMPANJ = JA                                                   
089600        ADD +1                       TO IX-RAD                            
089700        MOVE 'KAMPANJ '              TO WS-TEMEMO                         
089800        PERFORM DEA-ISRT-6325                                             
089900        MOVE NEJ TO SW-AUT-GODK                                           
090000     END-IF                                                               
090100                                                                          
090200     IF IN-IDFKNGRP > 1000 AND                                            
090300        IN-IDFKNGRP < 2000                                                
090400        ADD +1                       TO IX-RAD                            
090500        MOVE 'STANDARD'              TO WS-TEMEMO                         
090600        PERFORM DEA-ISRT-6325                                             
090700        MOVE NEJ TO SW-AUT-GODK                                           
090800     END-IF                                                               
090900                                                                          
091000     IF IN-IDFKNGRP > 8840 AND                                            
091100        IN-IDFKNGRP < 8849                                                
091200        ADD +1                       TO IX-RAD                            
091300        MOVE 'SÄKERHETSPRODUKT  '    TO WS-TEMEMO                         
091400        PERFORM DEA-ISRT-6325                                             
091500        MOVE NEJ TO SW-AUT-GODK                                           
091600     END-IF                                                               
091700                                                                          
091800     PERFORM DED-KOLLA-DEKAL                                              
091900     .                                                                    
092000     EJECT                                                                
092100 DEA-ISRT-6325      SECTION.                                              
092200                                                                          
092300     MOVE IX-RAD                  TO 6325-IDRADNR                         
092400     MOVE WS-TEMEMO               TO 6325-TEMEMO                          
092500     MOVE 6321-KDARBTYP           TO W-KDARBTYP                           
092600     MOVE 6322-DASKROT9-BEORD     TO W-DASKROT9-BEORD                     
092700     MOVE 6324-IDARTNR            TO W-IDARTNR-KVAL                       
092800     MOVE 6324-KDSTASKR           TO W-KDSTASKR-KVAL                      
092900     MOVE 6324-IDDC               TO W-IDDC-KVAL                          
093000     PERFORM IMS-ISRT-WDGX6325                                            
093100     .                                                                    
093200     EJECT                                                                
093300 DEB-KOLLA-SATS     SECTION.                                              
093400                                                                          
093500*LÄS WDJ1 FÖR ATT FÅ FRAM VILKA SATSER ARTIKELN INGÅR I                   
093600*LÄS DÄREFTER WDK6 FÖR SATSNUMRET FÖR ATT FÅ FRAM OM 09-MÄRKT             
093700     MOVE W-IDARTNR TO W-IDARTNR-S                                        
093800     PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                       
093900     PERFORM UNTIL SEGMENT-SAKNAS OR SW-SATS-OK = JA                      
094000       IF SEGMENT-FINNS                                                   
094100         MOVE SATS-STR-IDARTNR TO W-IDARTNR-PCB2                          
094200         PERFORM IMS-GET-WDK611-2                                         
094300         IF SEGMENT-FINNS AND CLAG2-CLAG-KDERS NOT = 09                   
094400           MOVE JA TO SW-SATS-OK                                          
094500         END-IF                                                           
094600       END-IF                                                             
094700       PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                     
094800     END-PERFORM                                                          
094900     .                                                                    
095000     EJECT                                                                
095100 DEC-KOLLA-KAMPANJ-DB2 SECTION.                                           
095200                                                                          
095300     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
095400                                                                          
095500     MOVE SQLCODE TO SQLCODE-WS                                           
095600     IF SQLCODE-WS = ZERO                                                 
095700***     READ TP1ARTK AND TP1KAMP                                          
095800        PERFORM DB2-FETCH-TP1ARTK-CRS                                     
095900        IF LINES-FOUND                                                    
096000           MOVE JA   TO SW-KAMPANJ                                        
096100        ELSE                                                              
096200           CONTINUE                                                       
096300        END-IF                                                            
096400     END-IF                                                               
096500                                                                          
096600     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
096700     .                                                                    
096800     EJECT                                                                
096900 DED-KOLLA-DEKAL       SECTION.                                           
097000                                                                          
097100     MOVE NEJ                 TO SW-DEKAL                                 
097200     MOVE SPACE               TO WS-TEXT                                  
097300     MOVE 'GB '               TO W-IDSKYLT                                
097400     MOVE SPACE               TO BENA11-TEXT-BEART                        
097500     PERFORM IMS-GET-BENA11-BSEQ                                          
097600     MOVE BENA11-TEXT-BEART   TO WS-TEXT-GB                               
097700     MOVE 'S  '               TO W-IDSKYLT                                
097800     MOVE SPACE               TO BENA11-TEXT-BEART                        
097900     PERFORM IMS-GET-BENA11-BSEQ                                          
098000     MOVE BENA11-TEXT-BEART   TO WS-TEXT-SV                               
098100     MOVE WS-TEXT             TO W041-BESTEXT                             
098200     MOVE 50                  TO W041-DIFAELT                             
098300                                                                          
098400     MOVE 'DEKAL'             TO W041-BESORD                              
098500     CALL W009LTXT USING W041-W009W041                                    
098600     IF W041-OK                                                           
098700        MOVE JA               TO SW-DEKAL                                 
098800     END-IF                                                               
098900                                                                          
099000     MOVE 'DECAL'             TO W041-BESORD                              
099100     CALL W009LTXT USING W041-W009W041                                    
099200     IF W041-OK                                                           
099300        MOVE JA               TO SW-DEKAL                                 
099400     END-IF                                                               
099500                                                                          
099600     IF SW-DEKAL = JA                                                     
099700        ADD +1                TO IX-RAD                                   
099800        MOVE 'DEKAL   '       TO WS-TEMEMO                                
099900        PERFORM DEA-ISRT-6325                                             
100000        MOVE NEJ              TO SW-AUT-GODK                              
100100     END-IF                                                               
100200     .                                                                    
100300     EJECT                                                                
100400 DF-TEST-AUT-GODK  SECTION.                                               
100500                                                                          
100600     IF WS-SUARTSTD < WS-MAX-SUBEL AND                                    
100700        SW-AUT-GODK = JA                                                  
100800        MOVE IN-IDARTNR       TO UT-IDARTNR                               
100900        MOVE WC-CDC-SE        TO UT-IDDC                                  
101000        MOVE DAGENS-DATUM-Y2K TO UT-DADATUM                               
101100        MOVE WS-SUARTSTD      TO UT-SUARTSTD                              
101200        MOVE WS-IDKUNDNR      TO UT-IDKUNDNR                              
101300        MOVE IN-KDERS         TO UT-KDERS                                 
101400                                                                          
101500        PERFORM S11-SKRIV-W26169                                          
101600     END-IF                                                               
101700     .                                                                    
101800     EJECT                                                                
101900 Z-FINIT SECTION.                                                         
102000                                                                          
102100                                                                          
102200     CLOSE W26168                                                         
102300           W26169                                                         
102400     SKIP2                                                                
102500     MOVE 'S' TO POSTSUM-OPKOD                                            
102600     CALL POSTSUM USING POSTSUM-PARM                                      
102700     .                                                                    
102800     EJECT                                                                
102900 S01-LAES-W26168  SECTION.                                                
103000     SKIP2                                                                
103100     READ W26168 INTO IN-AREA                                             
103200     AT END                                                               
103300        SET END-OF-W26168 TO TRUE                                         
103400                                                                          
103500     NOT AT END                                                           
103600        MOVE 'W26168'   TO POSTSUM-FDNAMN                                 
103700        MOVE 'W26168D1' TO POSTSUM-DDNAMN2                                
103800        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
103900        CALL POSTSUM USING POSTSUM-PARM                                   
104000                                                                          
104100     END-READ                                                             
104200     .                                                                    
104300     EJECT                                                                
104400 S11-SKRIV-W26169 SECTION.                                                
104500                                                                          
104600     WRITE UT-POST FROM UT-AREA                                           
104700                                                                          
104800     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
104900     MOVE 'W26169'   TO POSTSUM-FDNAMN                                    
105000     MOVE 'W26168D2' TO POSTSUM-DDNAMN2                                   
105100     CALL POSTSUM USING POSTSUM-PARM                                      
105200     .                                                                    
105300     EJECT                                                                
105400 X-TAG-CHECKPOINT   SECTION.                                              
105500                                                                          
105600* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
105700* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
105800     PERFORM IMS-CHECKPOINT                                               
105900     MOVE ZERO TO CHKP-ANT                                                
106000* --- LÄS OM DATABAS OM DET BEHÖVS                                        
106100     .                                                                    
106200     EJECT                                                                
106300* --- IMS SEKTIONER ---                                                   
106400                                                                          
106500 IMS-GET-WDK601 SECTION.                                                  
106600                                                                          
106700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
106800          DELIMITED BY SIZE INTO SSA1                                     
106900     MOVE '  GE' TO GODK-STATUSKODER                                      
107000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
107100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
107200     PERFORM IMS-STATUSKONTROLL                                           
107300     .                                                                    
107400     EJECT                                                                
107500 IMS-GET-WDK611 SECTION.                                                  
107600                                                                          
107700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
107800          DELIMITED BY SIZE INTO SSA1                                     
107900     MOVE '  GE' TO GODK-STATUSKODER                                      
108000     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
108100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
108200     PERFORM IMS-STATUSKONTROLL                                           
108300     .                                                                    
108400     SKIP3                                                                
108500 IMS-REPL-WDK611 SECTION.                                                 
108600                                                                          
108700     MOVE '  ' TO GODK-STATUSKODER                                        
108800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
108900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     ADD +1  TO CHKP-ANT                                                  
109200     .                                                                    
109300     EJECT                                                                
109400 IMS-REPL-WDK627 SECTION.                                                 
109500                                                                          
109600     MOVE '  ' TO GODK-STATUSKODER                                        
109700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK627                       
109800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
109900     PERFORM IMS-STATUSKONTROLL                                           
110000     ADD +1  TO CHKP-ANT                                                  
110100     .                                                                    
110200     SKIP3                                                                
110300 IMS-GET-WDK627 SECTION.                                                  
110400*                      LÄSNING AV SKROTNINGSSEGMENT WDK627                
110500     MOVE   'WDK611  *F'         TO SSA1                                  
110600     MOVE   'WDK627   '          TO SSA2                                  
110700     MOVE   '  GE'               TO GODK-STATUSKODER                      
110800     SKIP2                                                                
110900     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK627 SSA1                  
111000                                               SSA2                       
111100     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
111200     PERFORM IMS-STATUSKONTROLL                                           
111300     EJECT                                                                
111400     SKIP3                                                                
111500     .                                                                    
111600 IMS-ISRT-WDK627 SECTION.                                                 
111700*                        INSERT AV SKROTNINGSSEGMENT WDK627               
111800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
111900             DELIMITED BY SIZE INTO SSA1                                  
112000     MOVE   'WDK611   '          TO SSA2                                  
112100     MOVE   'WDK627   '          TO SSA3                                  
112200     MOVE   '  '  TO GODK-STATUSKODER                                     
112300                                                                          
112400     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK627 SSA1                  
112500                                          SSA2 SSA3                       
112600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
112700     PERFORM IMS-STATUSKONTROLL                                           
112800     ADD +1  TO CHKP-ANT                                                  
112900     .                                                                    
113000     EJECT                                                                
113100 IMS-GET-WDK611-2 SECTION.                                                
113200                                                                          
113300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-2 ')'                         
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
113600          DELIMITED BY SIZE INTO SSA2                                     
113700     MOVE '  GE' TO GODK-STATUSKODER                                      
113800     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-WDK611-2 SSA1 SSA2            
113900     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
114000     PERFORM IMS-STATUSKONTROLL                                           
114100     .                                                                    
114200     EJECT                                                                
114300 IMS-GET-WDK901 SECTION.                                                  
114400                                                                          
114500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
114600          DELIMITED BY SIZE INTO SSA1                                     
114700     MOVE '  GE' TO GODK-STATUSKODER                                      
114800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
114900     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
115000     PERFORM IMS-STATUSKONTROLL                                           
115100     .                                                                    
115200     EJECT                                                                
115300 IMS-GET-WDJ1-CSEQ-NEXT SECTION.                                          
115400     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
115500             DELIMITED BY SIZE INTO SSA1                                  
115600     MOVE 'WDJ101   ' TO SSA2                                             
115700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
115800     CALL CBLTDLI USING GN WDJ1C-PCB DLI-IO-WDJ1                          
115900                SSA1 SSA2                                                 
116000     MOVE WDJ1C-STATUS-CODE TO STATUS-WS                                  
116100     PERFORM IMS-STATUSKONTROLL                                           
116200     .                                                                    
116300     EJECT                                                                
116400 IMS-GU-WDR501-6321 SECTION.                                              
116500                                                                          
116600     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
116700            DELIMITED BY SIZE INTO SSA1                                   
116800     MOVE 'GE  '                TO GODK-STATUSKODER                       
116900     CALL CBLTDLI USING GU   6321-PCB DLI-IO-WDR501-6321 SSA1             
117000     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
117100     PERFORM IMS-STATUSKONTROLL                                           
117200     .                                                                    
117300     SKIP3                                                                
117400 IMS-ISRT-WDR501-6321 SECTION.                                            
117500                                                                          
117600     STRING 'WDR501     '                                                 
117700            DELIMITED BY SIZE INTO SSA1                                   
117800     MOVE '  '                  TO GODK-STATUSKODER                       
117900     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
118000     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     ADD +1  TO CHKP-ANT                                                  
118300     .                                                                    
118400     EJECT                                                                
118500 IMS-ISRT-WDGX6322 SECTION.                                               
118600                                                                          
118700     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
118800            DELIMITED BY SIZE INTO SSA1                                   
118900     MOVE 'WDGX6322'            TO SSA2                                   
119000     MOVE '  II'                TO GODK-STATUSKODER                       
119100     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
119200     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
119300     PERFORM IMS-STATUSKONTROLL                                           
119400     ADD +1  TO CHKP-ANT                                                  
119500     SKIP3                                                                
119600     .                                                                    
119700     SKIP3                                                                
119800 IMS-ISRT-WDGX6324 SECTION.                                               
119900                                                                          
120000     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
120100            DELIMITED BY SIZE INTO SSA1                                   
120200     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
120300            DELIMITED BY SIZE INTO SSA2                                   
120400     MOVE 'WDGX6324'            TO SSA3                                   
120500     MOVE '  '                  TO GODK-STATUSKODER                       
120600     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
120700                                      SSA1 SSA2 SSA3                      
120800     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
120900     PERFORM IMS-STATUSKONTROLL                                           
121000     ADD +1  TO CHKP-ANT                                                  
121100     SKIP3                                                                
121200     .                                                                    
121300     EJECT                                                                
121400 IMS-ISRT-WDGX6325 SECTION.                                               
121500                                                                          
121600     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
121700            DELIMITED BY SIZE INTO SSA1                                   
121800     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
121900            DELIMITED BY SIZE INTO SSA2                                   
122000     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
122100          DELIMITED BY SIZE INTO SSA3                                     
122200     MOVE 'WDGX6325 '           TO SSA4                                   
122300     MOVE '  II'                TO GODK-STATUSKODER                       
122400     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6325                     
122500                                      SSA1 SSA2 SSA3 SSA4                 
122600     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     ADD +1  TO CHKP-ANT                                                  
122900     .                                                                    
123000     EJECT                                                                
123100 IMS-GET-WDR501-6327 SECTION.                                             
123200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
123300          DELIMITED BY SIZE INTO SSA1                                     
123400     MOVE '  GE' TO GODK-STATUSKODER                                      
123500     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDR501-6327 SSA1               
123600     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     .                                                                    
123900     SKIP3                                                                
124000 IMS-GET-WDGX6328 SECTION.                                                
124100     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
124200          DELIMITED BY SIZE INTO SSA1                                     
124300     MOVE '  GE' TO GODK-STATUSKODER                                      
124400     CALL CBLTDLI USING GHNP 6327-PCB DLI-IO-WDGX6328 SSA1                
124500     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
124600     PERFORM IMS-STATUSKONTROLL                                           
124700     .                                                                    
124800     EJECT                                                                
124900 IMS-GET-WDK701 SECTION.                                                  
125000                                                                          
125100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
125200          DELIMITED BY SIZE INTO SSA1                                     
125300     MOVE '  GE' TO GODK-STATUSKODER                                      
125400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
125500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
125600     PERFORM IMS-STATUSKONTROLL                                           
125700     .                                                                    
125800     EJECT                                                                
125900 IMS-GET-WDK711 SECTION.                                                  
126000                                                                          
126100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
126200          DELIMITED BY SIZE INTO SSA1                                     
126300     MOVE '  GE' TO GODK-STATUSKODER                                      
126400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
126500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126900 IMS-GET-WDN601 SECTION.                                                  
127000                                                                          
127100     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
127200          DELIMITED BY SIZE INTO SSA1                                     
127300     MOVE '  GE' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
127500     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     EJECT                                                                
127900 IMS-GET-WDN611 SECTION.                                                  
128000                                                                          
128100     STRING 'WDN611  (WDN611KY =' W-WDN611KY-X ')'                        
128200          DELIMITED BY SIZE INTO SSA1                                     
128300     MOVE '  GE' TO GODK-STATUSKODER                                      
128400     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
128500     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
128600     PERFORM IMS-STATUSKONTROLL                                           
128700     .                                                                    
128800     EJECT                                                                
128900 IMS-GET-WDD901 SECTION.                                                  
129000                                                                          
129100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
129200          DELIMITED BY SIZE INTO SSA1                                     
129300     MOVE '  GE' TO GODK-STATUSKODER                                      
129400     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
129500     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
129600     PERFORM IMS-STATUSKONTROLL                                           
129700     .                                                                    
129800     EJECT                                                                
129900 IMS-GET-WDD902 SECTION.                                                  
130000                                                                          
130100     STRING 'WDD902     '                                                 
130200          DELIMITED BY SIZE INTO SSA1                                     
130300     MOVE '  GE' TO GODK-STATUSKODER                                      
130400     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
130500     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
130600     PERFORM IMS-STATUSKONTROLL                                           
130700     .                                                                    
130800     EJECT                                                                
130900 IMS-GET-BENA11-BSEQ SECTION.                                             
131000                                                                          
131100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
131200             DELIMITED BY SIZE INTO SSA1                                  
131300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
131400             DELIMITED BY SIZE INTO SSA2                                  
131500     MOVE '  GE' TO GODK-STATUSKODER                                      
131600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD3 SSA1 SSA2                 
131700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
131800     PERFORM IMS-STATUSKONTROLL                                           
131900     .                                                                    
132000     EJECT                                                                
132100 IMS-GU-WDQ4B1 SECTION.                                                   
132200                                                                          
132300     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN                          
132400                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                          
132500                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
132600                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X                       
132700                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN                          
132800                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                          
132900                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
133000                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-C-X ')'                 
133100                                                                          
133200          DELIMITED BY SIZE INTO SSA1                                     
133300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
133400     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
133500     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
133600     PERFORM IMS-STATUSKONTROLL                                           
133700     .                                                                    
133800     SKIP3                                                                
133900 IMS-GN-WDQ4B1 SECTION.                                                   
134000                                                                          
134100     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN                          
134200                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                          
134300                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
134400                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X                       
134500                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN                          
134600                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                          
134700                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
134800                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-C-X ')'                 
134900          DELIMITED BY SIZE INTO SSA1                                     
135000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
135100     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
135200     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
135300     PERFORM IMS-STATUSKONTROLL                                           
135400     .                                                                    
135500     SKIP3                                                                
135600 IMS-GU-WDL811 SECTION.                                                   
135700                                                                          
135800     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
135900          DELIMITED BY SIZE INTO SSA1                                     
136000     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
136100          DELIMITED BY SIZE INTO SSA2                                     
136200     MOVE '  GE' TO GODK-STATUSKODER                                      
136300     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
136400     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
136500     PERFORM IMS-STATUSKONTROLL                                           
136600     .                                                                    
136700     EJECT                                                                
136800 IMS-RESTART SECTION.                                                     
136900     SKIP2                                                                
137000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
137100     MOVE '  ' TO GODK-STATUSKODER                                        
137200     CALL CBLTDLI USING XRST MSG-PCB                                      
137300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
137400                        CHKP-AREA-LENGTH CHKP-AREA                        
137500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137600     PERFORM IMS-STATUSKONTROLL                                           
137700     .                                                                    
137800     SKIP3                                                                
137900 IMS-CHECKPOINT SECTION.                                                  
138000     SKIP2                                                                
138100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
138200     MOVE '  XD' TO GODK-STATUSKODER                                      
138300     CALL CBLTDLI USING CHKP MSG-PCB                                      
138400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
138500                        CHKP-AREA-LENGTH CHKP-AREA                        
138600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138700     PERFORM IMS-STATUSKONTROLL                                           
138800                                                                          
138900     IF IMS-EJ-OK                                                         
139000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
139100       DISPLAY FELTEXT                                                    
139200       CALL FELLOG                                                        
139300     END-IF                                                               
139400     .                                                                    
139500     EJECT                                                                
139600 IMS-STATUSKONTROLL SECTION.                                              
139700     SKIP2                                                                
139800     SET STATUS-IX TO 1                                                   
139900     SEARCH GODK-STATUS                                                   
140000       AT END                                                             
140100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
140200           DELIMITED BY SIZE INTO FELTEXT                                 
140300         DISPLAY FELTEXT                                                  
140400         CALL FELLOG                                                      
140500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
140600         CONTINUE                                                         
140700     END-SEARCH                                                           
140800     .                                                                    
140900     EJECT                                                                
141000 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
141100     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
141200                                                                          
141300     MOVE 000100  TO GOOD-SQLCODECODES                                    
141400                                                                          
141500     EXEC SQL                                                             
141600         DECLARE TP1ARTK-CRS CURSOR FOR                                   
141700           SELECT  A.IDKAMP                                               
141800                  ,A.IDARTNR                                              
141900                  ,B.TISTADAT_KAMP                                        
142000                  ,B.TISTODAT_KAMP                                        
142100                  ,B.KDKAMP                                               
142200                  ,B.IDKAMP_GRP                                           
142300                                                                          
142400           FROM    TP1ARTK A                                              
142500                  ,TP1KAMP B                                              
142600                                                                          
142700           WHERE   A.IDARTNR = :W-IDARTNR                                 
142800               AND A.IDKAMP  =  B.IDKAMP                                  
142900                                                                          
143000           ORDER BY B.IDKAMP                                              
143100     END-EXEC                                                             
143200                                                                          
143300     MOVE 000100  TO GOOD-SQLCODECODES                                    
143400     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
143500     .                                                                    
143600     SKIP3                                                                
143700 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
143800     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
143900     SKIP2                                                                
144000     MOVE 000100  TO GOOD-SQLCODECODES                                    
144100     EXEC SQL                                                             
144200         FETCH TP1ARTK-CRS INTO                                           
144300                    :TP1KAMP-IDKAMP                                       
144400                   ,:TP1ARTK-IDARTNR                                      
144500                   ,:TP1KAMP-TISTADAT-KAMP                                
144600                   ,:TP1KAMP-TISTODAT-KAMP                                
144700                   ,:TP1KAMP-KDKAMP                                       
144800                   ,:TP1KAMP-IDKAMP-GRP                                   
144900     END-EXEC                                                             
145000                                                                          
145100     MOVE SQLCODE TO SQLCODE-WS                                           
145200     PERFORM DB2-STATUS-CHECK                                             
145300     .                                                                    
145400     SKIP3                                                                
145500 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
145600     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
145700                                                                          
145800     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
145900     .                                                                    
146000     EJECT                                                                
146100 DB2-STATUS-CHECK  SECTION.                                               
146200                                                                          
146300     SET SQLCODE-IX TO 1                                                  
146400     SEARCH GOOD-SQLCODE                                                  
146500       AT END                                                             
146600*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
146700*         DELIMITED BY SIZE INTO ERROR-TEXT                               
146800          CALL FELLOG                                                     
146900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
147000     END-SEARCH                                                           
147100     .                                                                    
