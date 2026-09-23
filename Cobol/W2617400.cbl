000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2617400.                                                
000400 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000500 DATE-WRITTEN.   09 12 10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
000910*        (PROGRAMMET KOPIA AV W26168-KONCEPTET)                           
001000*        FRAMSTÄLLER UNDERLAG FÖR AUTO-SKROTNING                          
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK6                                       
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*        PROGRAMMET LÄSER      WDK9                                       
001600*        PROGRAMMET UPPDATERAR WDR5                                       
001700*        PROGRAMMET LÄSER      WDR5                                       
001900*        PROGRAMMET LÄSER      WDN6                                       
002110*        PROGRAMMET LÄSER      WDQ4                                       
002111*        PROGRAMMET LÄSER      WDJ1C                                      
002112*        PROGRAMMET LÄSER      WDD9                                       
002113*        PROGRAMMET LÄSER      WDD3                                       
002200*                                                                         
002300*        THE PROGRAM READS   TABLE TP1KAMP                                
002400*        THE PROGRAM READS   TABLE TP1ARTK                                
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- ARTIKLAR LAGERBAND                                         
003500     SELECT W01160                     ASSIGN TO W26174D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W01160                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W01160   -L.                                                   
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W2617400'.            
005800 01  CHKP-VAR.                                                            
005900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006400     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  IX                          PIC 9(2)    VALUE ZERO.                  
006710 77  WS-IX                       PIC 9(2)    VALUE ZERO.                  
006720 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200 77  WS-SPAR-BEANST-GODK         PIC X(25)   VALUE SPACE.                 
007300 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W01160                       VALUE 'J'.                   
007500 77  SW-K611-OK                  PIC X       VALUE 'N'.                   
007600 77  SW-KAMPANJ                  PIC X       VALUE 'N'.                   
007610 77  SW-DEKAL                    PIC X       VALUE 'N'.                   
007700 77  SW-SATS-OK                  PIC X       VALUE 'N'.                   
007900     EJECT                                                                
008000 01  SWITCHAR.                                                            
008100     03  INGAR-SATS-SW           PIC X.                                   
008200         88  INGAR-I-SATS    VALUE 'J'.                                   
008300*      --- VALID IDDC CODES                                               
008400*                                                                         
008500*01    -COPY WWDCKONS                                                     
008600     EJECT                                                                
008700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008800 01  FILLER REDEFINES DAGENS-DATUM.                                       
008900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009200                                                                          
009300 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
009400     EJECT                                                                
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600*                                                                         
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009920     03  W009LTXT                PIC X(8)    VALUE 'W009LTXT'.            
010000     SKIP3                                                                
010100 01  ARB.                                                                 
010200     03  WARTC-KVUTRS-C1         PIC S9(7)   COMP-3.                      
010300     03  WARTC-KVAKS-C1          PIC S9(7)   COMP-3.                      
010400     03  WARTC-KDERS-C1          PIC S9(3)   COMP-3.                      
010500     03  WARTC-KVLS-C1           PIC S9(7)   COMP-3.                      
010600     03  WARTC-KVRESS-C1         PIC S9(7)   COMP-3.                      
010700     03  WARTC-IDANSK            PIC S9(3)   COMP-3.                      
010800     03  WARTC-KVSPANT           PIC S9(7)   COMP-3.                      
010900     03  WS-KVOKS-C1             PIC S9(6)   VALUE ZERO.                  
011000     03  IDARTNR-WS              PIC S9(9)   VALUE ZERO COMP-3.           
011100     03  WS-FLERS                PIC X(1)    VALUE SPACE.                 
011200                                                                          
011300     03  W-SDC-KVLS              PIC S9(7)   VALUE ZERO COMP-3.           
011400     03  W-SDC-KVAKS             PIC S9(7)   VALUE ZERO COMP-3.           
011500     03  W-SDC-KVOKS             PIC S9(7)   VALUE ZERO COMP-3.           
011600                                                                          
011700     03  WS-AUTO-USERID          PIC X(8)    VALUE 'W2617400'.            
011710     03  WS-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
011800     03  WKVSKRANT               PIC S9(7).                               
011900     03  IX-RAD                  PIC S9(3)   VALUE ZERO COMP-3.           
012000     03  WS-TEMEMO               PIC X(25)   VALUE SPACE.                 
012100     03  FILLER REDEFINES WS-TEMEMO.                                      
012200         05  WS-TEMEMO-KDERS     PIC 9(2).                                
012201         05  WS-TEMEMO-TXT       PIC X(23).                               
012202     03  WS-KVBR                 PIC S9(7)   VALUE ZERO COMP-3.           
012203     03  ANT-SKROTU              PIC S9(3)   VALUE ZERO COMP-3.           
012300     03  MAX-ANT-SKROTU          PIC S9(3)   VALUE 999  COMP-3.           
012400     03  WS-KVBEART              PIC S9(7)   VALUE ZERO COMP-3.           
012500     03  WS-MAX-SUBEL            PIC S9(7)   VALUE ZERO COMP-3.           
012700     03  WS-SUARTSTD             PIC 9(7)V9(2) VALUE ZERO.                
012710     03  WS-TIAAAA               PIC 9(4)    VALUE ZERO.                  
012720     03  FILLER  REDEFINES  WS-TIAAAA.                                    
012730         05  WS-TISEKEL          PIC 9(2).                                
012740         05  WS-TIAA-VECKA       PIC 9(2).                                
012741     03  WS-TIAAAA-1             PIC 9(4)    VALUE ZERO.                  
012750     03  WS-TIVV                 PIC S9(3)   VALUE ZERO  COMP-3.          
012760     03  WS-ANTAL-OI             PIC S9(7)   VALUE ZERO COMP-3.           
012770     03  WS-TEXT.                                                         
012780         05  WS-TEXT-GB          PIC X(25).                               
012790         05  WS-TEXT-SV          PIC X(25).                               
012800                                                                          
012900 01  W-DATUM.                                                             
013000     05  W-DATUM-DATE    PIC X(6).                                        
013010     EJECT                                                                
013020*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
013030                                                                          
013040*01  -COPY WDATAREA                                                       
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL POSTSUM                                          
013300*                                                                         
013400*01  -COPY W0005   -PRE  POSTSUM-                                         
013500     EJECT                                                                
013510*    --- TEXTSÖKNING                                                      
013520*                                                                         
013530*01  -COPY W009W041                                                       
013540     EJECT                                                                
013600 01  IN-AREA-START               PIC X(24)   VALUE                        
013700                                             'IN-AREA-START'.             
013800     SKIP2                                                                
013900                                                                          
014000*01  AREA -COPY W01160     -PRE IN-                                       
014100*                                                                         
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  NYCKLAR-TILL-DLI.                                                    
014900     03  W-IDARTNR-X.                                                     
015000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015010     03  W-WDD901KY-X.                                                    
015020         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
015030         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
015100     03  W-KDSEGKEY-X.                                                    
015200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
015300     03  W-DASKROT-X.                                                     
015400         05  W-DASKROT           PIC X(8)    VALUE SPACE.                 
015500     03  W-IDSKYLT-X.                                                     
015600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
016100     03  W-DASKROT9-X.                                                    
016200         05  W-DASKROT9          PIC S9(8)   VALUE ZERO COMP-3.           
016300     03  W-KY6324-X.                                                      
016400         05  W-KY6324            PIC X(8)    VALUE SPACE.                 
016500     03  W-KY6328-X.                                                      
016600         05  W-KY6328            PIC X(15)    VALUE SPACE.                
016700     03  W-IDDC-X.                                                        
016800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016900     03  W-WDN611KY-X.                                                    
017000         05  W-WDN611KY          PIC X(6)    VALUE SPACE.                 
017100     03  W-IDLEVNR-X.                                                     
017200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017201     03  W-WDJ1CSEQ-X.                                                    
017202         05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                 
017203         05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                 
017204         05  W-IDARTNR-S         PIC S9(9)   COMP-3 VALUE ZERO.           
017300                                                                          
017400     03  W-IDHTYP-X.                                                      
017500         05  W-4533-IDHTYP        PIC X(04)    VALUE '4533'.              
017600                                                                          
017700     03  W-WDGXKEY-4534-X.                                                
017800         05  W-4534-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
017900         05  W-4534-IDDC         PIC X(2)     VALUE '11'.                 
018000         05  W-4534-LOW-VALUE    PIC X(03)    VALUE LOW-VALUE.            
018100                                                                          
018200     03  W-WDQ4B1KY-MAX.                                                  
018300         05  W-IDARTNR-Q4B1-MAX  PIC S9(9)    COMP-3.                     
018400         05  FILLER              PIC X(32)    VALUE HIGH-VALUE.           
018500                                                                          
018600     03  W-WDQ4B1KY-MIN.                                                  
018700         05  W-IDARTNR-Q4B1-MIN  PIC S9(9)    COMP-3.                     
018800         05  FILLER              PIC X(32)    VALUE LOW-VALUE.            
018900                                                                          
019000     03  W-WDQ401KY-X.                                                    
019100         05  W-IDWDQ401          PIC X(20).                               
019200     03  FILLER   REDEFINES W-WDQ401KY-X.                                 
019300         05  FILLER              PIC X(18).                               
019400         05  W-IDLOPNR-Q4B1      PIC S9(3)    COMP-3.                     
019500                                                                          
019510     03  W-TIAAAA-X.                                                      
019520         05  W-TIAAAA            PIC 9(4).                                
019530                                                                          
019600 01  W-IDARTNR-STR-X.                                                     
019700     03  W-IDARTNR-STR        PIC S9(9) COMP-3 VALUE ZERO.                
019800                                                                          
019900 01  W-IDARTNR-2.                                                         
020000     03  W-IDARTNR-PCB2       PIC S9(9) COMP-3 VALUE ZERO.                
020100                                                                          
020200 01  W-IDSKYLT-KEY-X.                                                     
020300   03  W-IDSKYLT-KEY     PIC X(3)    VALUE SPACE.                         
020400                                                                          
020500                                                                          
020600 01  NYCKLAR-6321.                                                        
020700     03  W-WDGX6321-ROT-X.                                                
020800         05  FILLER              PIC X(04)  VALUE '6321'.                 
020900         05  W-KDARBTYP          PIC X(08)  VALUE 'ANSK    '.             
021000         05  FILLER              PIC X(18)  VALUE LOW-VALUE.              
021100     03  W-WDGX6322-KEY-X.                                                
021200         05  W-DASKROT9-BEORD    PIC 9(08)  VALUE ZERO.                   
021300     03  W-KY6324-KVAL-X.                                                 
021400         05  W-IDARTNR-KVAL      PIC S9(9)  VALUE ZERO COMP-3.            
021500         05  W-IDDC-KVAL         PIC X(2)   VALUE SPACE.                  
021600         05  W-KDSTASKR-KVAL     PIC S9     VALUE ZERO COMP-3.            
021700                                                                          
021800     03  W-WDGXKEY-X.                                                     
021900         05  FILLER             PIC X(4)    VALUE '6327'.                 
022000         05  W-KDARBTYP-6327    PIC X(8)    VALUE SPACE.                  
022100         05  W-IDDC-6327        PIC X(2)    VALUE SPACE.                  
022200         05  FILLER             PIC X(16)   VALUE LOW-VALUE.              
022300     03  W-IDUSER-GODK-X.                                                 
022400         05  W-IDUSER-GODK      PIC X(8)    VALUE SPACE.                  
022500     EJECT                                                                
022600     SKIP2                                                                
022700*    --- STATUS-KOD FRÅN IMS                                              
022800 01  STATUS-WS                   PIC XX.                                  
022900     88  SEGMENT-FINNS                       VALUE '  '.                  
023000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
023300     88  IMS-EJ-OK                           VALUE 'XD'.                  
023400     SKIP2                                                                
023500 01  GODK-STATUSKODER.                                                    
023600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023700     SKIP3                                                                
023800 01  SSA1                        PIC X(128).                              
023900 01  SSA2                        PIC X(64).                               
024000 01  SSA3                        PIC X(64).                               
024100 01  SSA4                        PIC X(64).                               
024200     EJECT                                                                
024300*    --- IMS FUNKTIONSKODER                                               
024400*01  -COPY W0003                                                          
024500     EJECT                                                                
024600*    ---  DLI INPUT-OUTPUT AREA                                           
024700                                                                          
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
024900 01  DLI-IO-WDK601.                                                       
025000*    03  -COPY WDK601                                                     
025100     EJECT                                                                
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
025300 01  DLI-IO-WDK611.                                                       
025400*    03  -COPY WDK611                                                     
025500     EJECT                                                                
025600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK627'.                      
025700 01  DLI-IO-WDK627.                                                       
025800*    03  -COPY WDK627                                                     
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601-2'.                    
026000 01  DLI-IO-WDK601-2.                                                     
026100*    03  -COPY WDK601  -PRE ART2-                                         
026200     EJECT                                                                
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
026800 01  DLI-IO-WDK901.                                                       
026900*    03  -COPY WDK901  -PRE ARTM-                                         
027000     EJECT                                                                
027600 01  FILLER         PIC X(26) VALUE 'DLI-IO-WDR501-6321'.                 
027700 01  DLI-IO-WDR501-6321.                                                  
027800*    03  -COPY WDGX6321                                                   
027900     EJECT                                                                
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
028100 01  DLI-IO-WDGX6322.                                                     
028200*    03  -COPY WDGX6322                                                   
028300     EJECT                                                                
028400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
028500 01  DLI-IO-WDGX6324.                                                     
028600*    03  -COPY WDGX6324                                                   
028700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
028800 01  DLI-IO-WDGX6325.                                                     
028900*    03  -COPY WDGX6325                                                   
029000     EJECT                                                                
029100 01  FILLER         PIC X(26) VALUE 'DLI-IO-WDR501-6327'.                 
029200 01  DLI-IO-WDR501-6327.                                                  
029300*    03  -COPY WDGX6327                                                   
029400     EJECT                                                                
029500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
029600 01  DLI-IO-WDGX6328.                                                     
029700*    03  -COPY WDGX6328                                                   
030500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
030600 01  DLI-IO-WDN601.                                                       
030700*    03  -COPY WDN601                                                     
030800     EJECT                                                                
030900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
031000 01  DLI-IO-WDN611.                                                       
031100*    03  -COPY WDN611                                                     
031900     EJECT                                                                
032400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4B1'.                      
032500 01  DLI-IO-WDQ4B1.                                                       
032600*    03  -COPY WDQ4B1                                                     
032700     EJECT                                                                
032800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4'.                        
032900 01  DLI-IO-WDQ401.                                                       
033000*    03  -COPY WDQ401                                                     
033001     EJECT                                                                
033002 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ1'.                        
033003 01  DLI-IO-WDJ1.                                                         
033004*    03  -COPY WDJ111 -PRE SATS-                                          
033005*    03  -COPY WDJ101 -PRE SATS-                                          
033006     EJECT                                                                
033007 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
033008 01  DLI-IO-WDD901.                                                       
033009*    03  -COPY WDD901  -PRE   LEV-                                        
033010     EJECT                                                                
033011 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
033012 01  DLI-IO-WDD902.                                                       
033013*    03  -COPY WDD902  -PRE   LEV-                                        
033014     EJECT                                                                
033015 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD3'.                        
033016 01  DLI-IO-WDD3.                                                         
033017*03  WLBENA11  -COPY WDD311  -PRE  BENA11-                                
033018     EJECT                                                                
033019 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601-2'.                    
033020 01  DLI-IO-WDK601-2.                                                     
033021*    03  -COPY WDK601  -PRE ART2-                                         
033022     EJECT                                                                
033023 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611-2'.                    
033024 01  DLI-IO-WDK611-2.                                                     
033025*    03  -COPY WDK611  -PRE CLAG2-                                        
033026     EJECT                                                                
033300 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
033400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
033500                                                                          
033600 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
033700 01  DB2-WS.                                                              
033800     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
033900         88  CURSOR-OK                      VALUE 000.                    
034000         88  LINES-FOUND                    VALUE 000.                    
034100         88  LINES-MISSING                  VALUE 100.                    
034200         88  RESOURCE-WRONG                 VALUE 904.                    
034300     03  GOOD-SQLCODECODES.                                               
034400         05  GOOD-SQLCODE OCCURS 5                                        
034500             INDEXED BY SQLCODE-IX PIC 9(3).                              
034600                                                                          
034700 01  WS.                                                                  
034800     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
034900     03 FILLER                   PIC X(16)   VALUE                        
035000                                             'WS-DB2-SEKTION'.            
035100     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
035200     EJECT                                                                
035300 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
035400                                                                          
035500*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
035600     EJECT                                                                
035700 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
035800                                                                          
035900*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
036000     EJECT                                                                
036100     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
036200     EJECT                                                                
036300     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
036400     EJECT                                                                
036410                                                                          
036500 LINKAGE SECTION.                                                         
036600                                                                          
036700*01  -COPY W0009   -PRE MSG-                                              
036800                                                                          
036900*01  -COPY W0008  -PRE WDK6-                                              
037000     05  FILLER                  PIC X.                                   
037100                                                                          
037500*01  -COPY W0008  -PRE WDK9-                                              
037600     05  FILLER                  PIC X.                                   
037700                                                                          
038100*01  -COPY W0008  -PRE 6321-                                              
038200     05  FILLER                  PIC X.                                   
038300                                                                          
038400*01  -COPY W0008  -PRE 6327-                                              
038500     05  FILLER                  PIC X.                                   
038600                                                                          
039000*01  -COPY W0008  -PRE WDN6-                                              
039100     05  FILLER                  PIC X.                                   
039200                                                                          
039900*01  -COPY W0008  -PRE WDQ4B-                                             
040000     05  FILLER                  PIC X.                                   
040100                                                                          
040200*01  -COPY W0008  -PRE WDQ4-                                              
040300     05  FILLER                  PIC X.                                   
040310                                                                          
040311*01  -COPY W0008  -PRE WDJ1C-                                             
040312     05  FILLER                  PIC X.                                   
040313                                                                          
040314*01  -COPY W0008  -PRE WDD9-                                              
040315     05  FILLER                  PIC X.                                   
040316                                                                          
040317*01  -COPY W0008  -PRE WDD3-                                              
040318     05  FILLER                  PIC X.                                   
040319                                                                          
040320*01  -COPY W0008  -PRE WDK62-                                             
040330     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500 PROCEDURE DIVISION  USING MSG-PCB                                        
040600           WDK6-PCB  WDK9-PCB                                             
040700           6321-PCB  6327-PCB                                             
040800           WDN6-PCB  WDQ4B-PCB                                            
040900           WDQ4-PCB  WDJ1C-PCB                                            
041000           WDD9-PCB  WDD3-PCB                                             
041001           WDK62-PCB.                                                     
041002 MAIN SECTION.                                                            
041003     ENTRY 'DLITCBL' USING MSG-PCB                                        
041004           WDK6-PCB  WDK9-PCB                                             
041005           6321-PCB  6327-PCB                                             
041006           WDN6-PCB  WDQ4B-PCB                                            
041007           WDQ4-PCB  WDJ1C-PCB                                            
041008           WDD9-PCB  WDD3-PCB                                             
041009           WDK62-PCB.                                                     
041400                                                                          
041500     SKIP2                                                                
041600     PERFORM A-INIT                                                       
041700     PERFORM C-KOLLA-IDUSER                                               
041800     PERFORM S01-LAES-W01160                                              
041900     PERFORM UNTIL END-OF-W01160                                          
042000       IF CHKP-ANT > CHKP-MAX                                             
042100         PERFORM X-TAG-CHECKPOINT                                         
042200       END-IF                                                             
042300       MOVE IN-CLAG-IDARTNR TO W-IDARTNR                                  
042400                               IDARTNR-WS                                 
042500       IF (ANT-SKROTU < MAX-ANT-SKROTU)                       AND         
042600          (IN-CLAG-KVLS > ZERO)                               AND         
042700          (IN-CLAG-KDPRODSL = 11 OR 13 OR 14 OR 15 OR                     
042800           16 OR 17 OR 18 OR 19 OR                                        
042900           (IN-CLAG-KDPRODSL > 20 AND IN-CLAG-KDPRODSL < 30)) AND         
043000          (IN-CLAG-KDERS-UTG = ZERO)                          AND         
043100          (IN-CLAG-KDERS = 21 OR 22 OR 23 OR 24 OR 25 OR                  
043200           26 OR 29 OR 52)                                                
043300                                                                          
043400          PERFORM IMS-GET-WDK601                                          
043500          IF SEGMENT-FINNS                                                
043600             PERFORM B-LAES-SKROTINFO                                     
043700                                                                          
043800             IF  SW-K611-OK = JA                  AND                     
043900                 CLAG-TISKROT-AUTO < DAGENS-DATUM AND                     
044000                 CLAG-FLSKROT-BEORD NOT = JA                              
044100                 PERFORM D-UPPDATERA                                      
044200             END-IF                                                       
044300          END-IF                                                          
044301                                                                          
044302       END-IF                                                             
044303       PERFORM S01-LAES-W01160                                            
044400     END-PERFORM                                                          
044500                                                                          
044600                                                                          
044700     PERFORM Z-FINIT                                                      
044800                                                                          
044900     MOVE ZERO TO RETURN-CODE                                             
045000     GOBACK                                                               
045100     .                                                                    
045200     EJECT                                                                
045300 A-INIT SECTION.                                                          
045400     SKIP2                                                                
045500                                                                          
045600     PERFORM IMS-RESTART                                                  
045700                                                                          
045800     OPEN INPUT W01160                                                    
046000                                                                          
046100                                                                          
046200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
046300                                                                          
046400     ACCEPT W-DATUM-DATE FROM DATE                                        
046500     ACCEPT DAGENS-DATUM FROM DATE                                        
046600                                                                          
046700     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
046800                                                                          
047010     MOVE 'IDAG' TO DAT-KDDATFORM                                         
047020     CALL WDATKONV USING DAT-KDDATFORM,                                   
047030                         DAT-I-TIDATUM,                                   
047040                         DAT-O-TIDATUM,                                   
047050                         DAT-KDSVAR                                       
047060                                                                          
047070     IF DAT-KDSVAR-FEL                                                    
047080        DISPLAY '****  FEL I WDATKONV  *******'                           
047091        CALL FELLOG                                                       
047092     END-IF                                                               
047093                                                                          
047094     MOVE DAT-TIVV       TO WS-TIVV                                       
047095     MOVE DAT-TISEKEL    TO WS-TISEKEL                                    
047096     MOVE DAT-TIAA-VECKA TO WS-TIAA-VECKA                                 
047097     MOVE WS-TIAAAA      TO WS-TIAAAA-1                                   
047100     SUBTRACT +1         FROM WS-TIAAAA-1                                 
047200     .                                                                    
047300     EJECT                                                                
047400 B-LAES-SKROTINFO SECTION.                                                
047500                                                                          
047600     PERFORM IMS-GET-WDK611                                               
047700     IF  SEGMENT-FINNS                    AND                             
047800         CLAG-TISKROT-AUTO < DAGENS-DATUM AND                             
047900         CLAG-FLSKROT-BEORD NOT = JA                                      
048400        MOVE JA               TO SW-K611-OK                               
048500        MOVE CLAG-KDERS       TO WARTC-KDERS-C1                           
048600        MOVE CLAG-KVLS        TO WARTC-KVLS-C1                            
048700        MOVE CLAG-KVRESS      TO WARTC-KVRESS-C1                          
048800        MOVE CLAG-IDANSK      TO WARTC-IDANSK                             
048900        MOVE CLAG-KVSPANT     TO WARTC-KVSPANT                            
049000                                                                          
049100        PERFORM IMS-GET-WDK901                                            
049200        IF SEGMENT-FINNS                                                  
049300                                                                          
049400           COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK   +                  
049500                                 ARTM-ART-KVOKS-DAG    +                  
049600                                 ARTM-ART-KVOKS-VOR                       
049700        ELSE                                                              
049800           MOVE ZERO          TO WS-KVOKS-C1                              
049900        END-IF                                                            
050000     ELSE                                                                 
050500        MOVE NEJ              TO SW-K611-OK                               
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
051039 C-KOLLA-IDUSER SECTION.                                                  
051040                                                                          
051100     MOVE WC-CDC-SE              TO W-IDDC-6327                           
051200     MOVE 'ANSK'                 TO W-KDARBTYP-6327                       
051300     MOVE SPACE                  TO WS-SPAR-BEANST-GODK                   
051400     PERFORM IMS-GET-WDR501-6327                                          
051500     IF SEGMENT-FINNS                                                     
051600        MOVE WS-AUTO-USERID      TO W-IDUSER-GODK                         
051700        PERFORM IMS-GET-WDGX6328                                          
051800        IF SEGMENT-FINNS                                                  
051900           MOVE 6328-BEANST-GODK TO WS-SPAR-BEANST-GODK                   
052000           MOVE 6328-SUBEL       TO WS-MAX-SUBEL                          
052100        END-IF                                                            
052200     END-IF                                                               
052600     .                                                                    
052700     EJECT                                                                
052800 D-UPPDATERA SECTION.                                                     
052900                                                                          
053000     MOVE IDARTNR-WS TO W-IDARTNR                                         
053100                                                                          
053200     PERFORM DA-SKAPA-KVSKRANT                                            
053300     IF WKVSKRANT > ZERO                                                  
053400        PERFORM DB-UPPD-SKROTSPARR                                        
053500        PERFORM DC-SKAPA-HANDELSETR-6321                                  
053600        PERFORM DE-SKAPA-TEMEMO                                           
053800     END-IF                                                               
053900                                                                          
054000     .                                                                    
054100     EJECT                                                                
054200 DA-SKAPA-KVSKRANT SECTION.                                               
054300                                                                          
054400     COMPUTE WKVSKRANT =                                                  
054500             WARTC-KVLS-C1 - WARTC-KVRESS-C1                              
054600                           - WS-KVOKS-C1                                  
054800                                                                          
054900     MOVE 11        TO 6324-IDKUNDNR                                      
055000                       WS-IDKUNDNR                                        
055600     MOVE ZERO      TO WS-KVBEART                                         
055700     MOVE W-IDARTNR TO W-IDARTNR-Q4B1-MAX                                 
055800                       W-IDARTNR-Q4B1-MIN                                 
055900     PERFORM IMS-GU-WDQ4B1                                                
056000     PERFORM UNTIL SEGMENT-SAKNAS                                         
056100        IF SEQB-IDDISTR = 81 AND                                          
056200          (SEQB-IDKUNDNR = 11 OR 111)                                     
056300           MOVE SEQB-IDWDQ401 TO W-IDWDQ401                               
056400           MOVE SEQB-IDLOPNR TO W-IDLOPNR-Q4B1                            
056500           PERFORM IMS-GU-WDQ401                                          
056600           IF SEGMENT-FINNS                                               
056700              ADD ORAD-KVBEART-Q TO WS-KVBEART                            
056800           END-IF                                                         
056900        END-IF                                                            
057000        PERFORM IMS-GN-WDQ4B1                                             
057100     END-PERFORM                                                          
057200     IF WS-KVBEART > ZERO                                                 
057300        COMPUTE WKVSKRANT = WKVSKRANT - WS-KVBEART                        
057400     END-IF                                                               
057500                                                                          
057600     .                                                                    
057700     EJECT                                                                
057800 DB-UPPD-SKROTSPARR SECTION.                                              
057900                                                                          
058000     PERFORM IMS-GET-WDK601                                               
058100     PERFORM IMS-GET-WDK611                                               
058200     MOVE JA  TO CLAG-FLSKROT-BEORD                                       
058300     MOVE NEJ TO CLAG-FLSKROT-AUTO                                        
058400                                                                          
058500     PERFORM IMS-REPL-WDK611                                              
058600                                                                          
058700     PERFORM IMS-GET-WDK627                                               
058800                                                                          
058900     IF SEGMENT-FINNS                                                     
059000         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
059100                                                                          
059200         PERFORM IMS-REPL-WDK627                                          
059300     ELSE                                                                 
059400         MOVE ZERO                    TO SKROT-KVSKROT                    
059500         MOVE ZERO                    TO SKROT-DASKROT                    
059600         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
059700                                                                          
059900         PERFORM IMS-ISRT-WDK627                                          
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300 DC-SKAPA-HANDELSETR-6321 SECTION.                                        
060400                                                                          
060500     MOVE WC-CDC-SE         TO W-IDDC-KVAL                                
060600     MOVE 'ANSK'            TO W-KDARBTYP                                 
060700     PERFORM IMS-GU-WDR501-6321                                           
060800     IF SEGMENT-SAKNAS                                                    
060900        MOVE '6321'         TO 6321-IDHTYP                                
061000        MOVE 'ANSK'         TO 6321-KDARBTYP                              
061100        MOVE LOW-VALUE      TO 6321-LOW-VALUE                             
061200        PERFORM IMS-ISRT-WDR501-6321                                      
061500                                                                          
061600     END-IF                                                               
061700                                                                          
061800     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
061900     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
062000     PERFORM IMS-ISRT-WDGX6322                                            
062300                                                                          
062400     MOVE IDARTNR-WS        TO 6324-IDARTNR                               
062500     MOVE WC-CDC-SE         TO 6324-IDDC                                  
062600     MOVE 1                 TO 6324-KDSTASKR                              
062700     MOVE NEJ               TO 6324-FLSKROT-GODK                          
062800     MOVE WARTC-IDANSK      TO 6324-IDPERSON                              
062900     MOVE 81                TO 6324-IDDISTR                               
063000     MOVE 11                TO 6324-IDKUNDNR                              
063010                                 WS-IDKUNDNR                              
063100     MOVE WS-AUTO-USERID    TO 6324-IDUSER                                
063200     MOVE ZERO              TO 6324-KDFRAKT                               
063300     MOVE 1                 TO 6324-KDORDKL                               
063400     MOVE WKVSKRANT         TO 6324-KVSKROT-BEORD                         
063600     MOVE SPACE             TO 6324-IDANALYS                              
063700                               6324-FLJUSTBUFF                            
063710                               6324-IDKST                                 
063800     MOVE ZERO              TO 6324-IDKONTO                               
063900                               6324-KVSKROT-KVAR                          
064000     MOVE ZERO              TO 6324-KVSKROT-ONDEM                         
064100     MOVE WS-SPAR-BEANST-GODK  TO 6324-BEANST                             
064200     COMPUTE WS-SUARTSTD ROUNDED =                                        
064300             6324-KVSKROT-BEORD * CLAG-PRARTSTD                           
064400                                                                          
068300     PERFORM DCA-KONTO-ANALYS                                             
068301                                                                          
068302     MOVE 'AUTOMATBEORDRAD' TO 6324-BELAGINS-DEL                          
068303     MOVE CLAG-KDERS        TO 6324-KDERS-UTG                             
068304                                                                          
068305     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
068306            CLAG-KVLS     - CLAG-KVRESS                                   
068307                          - CLAG-KVROS                                    
068308                          - WS-KVOKS-C1                                   
068309                                                                          
068310     COMPUTE 6324-KVTILLG-SDC ROUNDED =                                   
068311            W-SDC-KVLS - W-SDC-KVOKS                                      
068312                                                                          
068313     COMPUTE 6324-KVAKS-CDC ROUNDED =                                     
068314          CLAG-KVAKS-CDC  + CLAG-KVAKS-PAV                                
068315                          + CLAG-KVAKS-T                                  
068316                                                                          
068317     COMPUTE 6324-KVAKS-SDC ROUNDED =                                     
068318          W-SDC-KVAKS                                                     
068319                                                                          
068320     PERFORM IMS-GET-WDK901                                               
068321     IF SEGMENT-FINNS                                                     
068322       COMPUTE 6324-SUTPO-TOT =                                           
068323                ARTM-ART-SUTPO-TOT                                        
068324     ELSE                                                                 
068325       MOVE ZERO              TO 6324-SUTPO-TOT                           
068326     END-IF                                                               
068327                                                                          
068328     PERFORM DCB-LAS-FLYTTA-WDN6                                          
068329*****                                                                     
068600     PERFORM IMS-ISRT-WDGX6324                                            
068700     ADD +1 TO ANT-SKROTU                                                 
068800*****                                                                     
069100     .                                                                    
069200     EJECT                                                                
071400 DCA-KONTO-ANALYS    SECTION.                                             
071500                                                                          
071600     IF CLAG-KDERS = (22 OR 23 OR 25 OR 26)                               
071700        IF ART-KDPRODSL = 11                                              
071800           MOVE '158600002002'    TO 6324-IDANALYS                        
071900           MOVE 481180            TO 6324-IDKONTO                         
072000        END-IF                                                            
072100        IF ART-KDPRODSL = 13                                              
072200           MOVE '158600002003'    TO 6324-IDANALYS                        
072300           MOVE 481180            TO 6324-IDKONTO                         
072400        END-IF                                                            
072500        IF ART-KDPRODSL = 14                                              
072600           MOVE '158600002004'    TO 6324-IDANALYS                        
072700           MOVE 481180            TO 6324-IDKONTO                         
072800        END-IF                                                            
072900        IF ART-KDPRODSL = 15                                              
073000           MOVE '158600002005'    TO 6324-IDANALYS                        
073100           MOVE 481180            TO 6324-IDKONTO                         
073200        END-IF                                                            
073300        IF ART-KDPRODSL = 16                                              
073400           MOVE '158600002006'    TO 6324-IDANALYS                        
073500           MOVE 481180            TO 6324-IDKONTO                         
073600        END-IF                                                            
073700        IF ART-KDPRODSL = 17                                              
073800           MOVE '158600002007'    TO 6324-IDANALYS                        
073900           MOVE 481180            TO 6324-IDKONTO                         
074000        END-IF                                                            
074100        IF ART-KDPRODSL = 18                                              
074200           MOVE '158600002008'    TO 6324-IDANALYS                        
074300           MOVE 481180            TO 6324-IDKONTO                         
074400        END-IF                                                            
074500        IF ART-KDPRODSL = 19                                              
074600           MOVE '158600002009'    TO 6324-IDANALYS                        
074700           MOVE 481180            TO 6324-IDKONTO                         
074800        END-IF                                                            
074900        IF ART-KDPRODSL > 20 AND ART-KDPRODSL < 30                        
075000           MOVE '158600002010'    TO 6324-IDANALYS                        
075100           MOVE 481180            TO 6324-IDKONTO                         
075200        END-IF                                                            
075201     END-IF                                                               
075202     .                                                                    
075203     EJECT                                                                
075204 DCB-LAS-FLYTTA-WDN6 SECTION.                                             
075205     MOVE +1 TO IX                                                        
075206     PERFORM UNTIL IX > 20                                                
075207        MOVE SPACE        TO 6324-BEEMBLEM (IX)                           
075208        ADD +1        TO IX                                               
075209     END-PERFORM                                                          
075210     PERFORM IMS-GET-WDN601                                               
075211     IF SEGMENT-FINNS                                                     
075212        PERFORM IMS-GET-WDN611                                            
075213        MOVE +1 TO IX                                                     
075214        PERFORM UNTIL IX > 20 OR SEGMENT-SAKNAS                           
075215           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (IX)                        
075216           ADD +1        TO IX                                            
075217           PERFORM IMS-GET-WDN611                                         
075218        END-PERFORM                                                       
075219     END-IF                                                               
075220     .                                                                    
075221     EJECT                                                                
075222 DE-SKAPA-TEMEMO    SECTION.                                              
075223                                                                          
075224     MOVE '-MÄRKT '                  TO WS-TEMEMO-TXT                     
075225     MOVE ZERO                       TO IX-RAD                            
075226     ADD +1                          TO IX-RAD                            
075227     MOVE CLAG-KDERS                 TO WS-TEMEMO-KDERS                   
075228     PERFORM DEA-ISRT-6325                                                
075229                                                                          
075230     MOVE SPACE                      TO WS-TEMEMO                         
075231     IF CLAG-KVSPANT > ZERO                                               
075232        ADD +1                       TO IX-RAD                            
075233        MOVE 'SPÄRRAD KVANT '        TO WS-TEMEMO                         
075234        PERFORM DEA-ISRT-6325                                             
075235     END-IF                                                               
075236                                                                          
075237     IF ART-FLIART = JA                                                   
075238        ADD +1                       TO IX-RAD                            
075239        MOVE 'INGÅR I SATS '         TO WS-TEMEMO                         
075240***TESTA FÖRST OM SJÄLVA SATSEN ÄR 09-MÄRKT, ANNARS                       
075241        MOVE NEJ TO SW-SATS-OK                                            
075242        PERFORM DEB-KOLLA-SATS                                            
075243        IF SW-SATS-OK = JA                                                
075244           PERFORM DEA-ISRT-6325                                          
075300        END-IF                                                            
075400     END-IF                                                               
075500                                                                          
075600     IF ART-KDPRODSL = 21                                                 
075700        ADD +1                       TO IX-RAD                            
075800        MOVE '300-/400-SERIEN '      TO WS-TEMEMO                         
075900        PERFORM DEA-ISRT-6325                                             
076100     END-IF                                                               
076110                                                                          
076120     IF ART-KDPRODSL = 14                                                 
076130        ADD +1                       TO IX-RAD                            
076140        MOVE 'BYTES'                 TO WS-TEMEMO                         
076150        PERFORM DEA-ISRT-6325                                             
076180     END-IF                                                               
076200                                                                          
076300     IF ART-KDPRODSL = 15 OR 16 OR 17 OR 25 OR 26                         
076400        ADD +1                       TO IX-RAD                            
076500        MOVE 'TILLBEHÖR       '      TO WS-TEMEMO                         
076600        PERFORM DEA-ISRT-6325                                             
076900     END-IF                                                               
077000                                                                          
077100     MOVE ZERO      TO WS-KVBR                                            
077110     MOVE W-IDARTNR TO W-IDARTNR-D9                                       
077120     MOVE WC-CDC-SE TO W-IDDC-D9                                          
077200     PERFORM IMS-GET-WDD901                                               
077300     IF SEGMENT-FINNS                                                     
077400        PERFORM IMS-GET-WDD902                                            
077500        PERFORM UNTIL SEGMENT-SAKNAS                                      
077600           ADD LEV-KVBR    TO WS-KVBR                                     
077700           PERFORM IMS-GET-WDD902                                         
077800        END-PERFORM                                                       
077900     END-IF                                                               
078000     IF WS-KVBR > ZERO                                                    
078100        ADD +1                       TO IX-RAD                            
078200        MOVE 'BESTÄLLNINGSREST '     TO WS-TEMEMO                         
078300        PERFORM DEA-ISRT-6325                                             
078600     END-IF                                                               
078700                                                                          
078800     MOVE NEJ                        TO SW-KAMPANJ                        
078810     PERFORM DEC-KOLLA-KAMPANJ-DB2                                        
080200     IF SW-KAMPANJ = JA                                                   
080300        ADD +1                       TO IX-RAD                            
080400        MOVE 'KAMPANJ '              TO WS-TEMEMO                         
080500        PERFORM DEA-ISRT-6325                                             
080700     END-IF                                                               
080710                                                                          
080720     IF ART-IDFKNGRP > 1000 AND                                           
080721        ART-IDFKNGRP < 2000                                               
080730        ADD +1                       TO IX-RAD                            
080740        MOVE 'STANDARD'              TO WS-TEMEMO                         
080750        PERFORM DEA-ISRT-6325                                             
080780     END-IF                                                               
080790                                                                          
080791     IF ART-IDFKNGRP > 8840 AND                                           
080792        ART-IDFKNGRP < 8849                                               
080793        ADD +1                       TO IX-RAD                            
080794        MOVE 'SÄKERHETSPRODUKT  '    TO WS-TEMEMO                         
080795        PERFORM DEA-ISRT-6325                                             
080798     END-IF                                                               
080799                                                                          
080800     PERFORM DED-KOLLA-DEKAL                                              
080810                                                                          
080820     .                                                                    
080900     EJECT                                                                
081000 DEA-ISRT-6325      SECTION.                                              
081100                                                                          
081200     MOVE IX-RAD                  TO 6325-IDRADNR                         
081300     MOVE WS-TEMEMO               TO 6325-TEMEMO                          
081400     MOVE 6321-KDARBTYP           TO W-KDARBTYP                           
081500     MOVE 6322-DASKROT9-BEORD     TO W-DASKROT9-BEORD                     
081600     MOVE 6324-IDARTNR            TO W-IDARTNR-KVAL                       
081700     MOVE 6324-KDSTASKR           TO W-KDSTASKR-KVAL                      
081800     MOVE 6324-IDDC               TO W-IDDC-KVAL                          
081900     PERFORM IMS-ISRT-WDGX6325                                            
082200     .                                                                    
082300     EJECT                                                                
082400 DEB-KOLLA-SATS     SECTION.                                              
082500                                                                          
082600*LÄS WDJ1 FÖR ATT FÅ FRAM VILKA SATSER ARTIKELN INGÅR I                   
082700*LÄS DÄREFTER WDK6 FÖR SATSNUMRET FÖR ATT FÅ FRAM OM 09-MÄRKT             
082800     MOVE W-IDARTNR TO W-IDARTNR-S                                        
082900     PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                       
083000     PERFORM UNTIL SEGMENT-SAKNAS OR SW-SATS-OK = JA                      
083100       IF SEGMENT-FINNS                                                   
083200         MOVE SATS-STR-IDARTNR TO W-IDARTNR-PCB2                          
083300         PERFORM IMS-GET-WDK611-2                                         
083400         IF SEGMENT-FINNS AND CLAG2-CLAG-KDERS NOT = 09                   
083500           MOVE JA TO SW-SATS-OK                                          
083600         END-IF                                                           
083700       END-IF                                                             
083800       PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                     
083900     END-PERFORM                                                          
084000     .                                                                    
084100     EJECT                                                                
084200 DEC-KOLLA-KAMPANJ-DB2 SECTION.                                           
084300                                                                          
084400     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
084500                                                                          
084600     IF SQLCODE-WS = ZERO                                                 
084700********READ TP1ARTK AND TP1KAMP                                          
084800        PERFORM DB2-FETCH-TP1ARTK-CRS                                     
084900        IF LINES-FOUND                                                    
085000           MOVE JA   TO SW-KAMPANJ                                        
085100        ELSE                                                              
085200           CONTINUE                                                       
085300        END-IF                                                            
085400     END-IF                                                               
085500                                                                          
085600     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
085610     .                                                                    
085620     EJECT                                                                
085630 DED-KOLLA-DEKAL       SECTION.                                           
085640                                                                          
085641     MOVE NEJ                 TO SW-DEKAL                                 
085642     MOVE SPACE               TO WS-TEXT                                  
085650     MOVE 'GB '               TO W-IDSKYLT                                
085651     MOVE SPACE               TO BENA11-TEXT-BEART                        
085660     PERFORM IMS-GET-BENA11-BSEQ                                          
085670     MOVE BENA11-TEXT-BEART   TO WS-TEXT-GB                               
085680     MOVE 'S  '               TO W-IDSKYLT                                
085681     MOVE SPACE               TO BENA11-TEXT-BEART                        
085690     PERFORM IMS-GET-BENA11-BSEQ                                          
085691     MOVE BENA11-TEXT-BEART   TO WS-TEXT-SV                               
085692     MOVE WS-TEXT             TO W041-BESTEXT                             
085693     MOVE 50                  TO W041-DIFAELT                             
085694                                                                          
085695     MOVE 'DEKAL'             TO W041-BESORD                              
085696     CALL W009LTXT USING W041-W009W041                                    
085697     IF W041-OK                                                           
085698        MOVE JA               TO SW-DEKAL                                 
085699     END-IF                                                               
085700                                                                          
085701     MOVE 'DECAL'             TO W041-BESORD                              
085702     CALL W009LTXT USING W041-W009W041                                    
085703     IF W041-OK                                                           
085704        MOVE JA               TO SW-DEKAL                                 
085705     END-IF                                                               
085706                                                                          
085707     IF SW-DEKAL = JA                                                     
085708        ADD +1                TO IX-RAD                                   
085709        MOVE 'DEKAL   '       TO WS-TEMEMO                                
085710        PERFORM DEA-ISRT-6325                                             
085713     END-IF                                                               
085720     .                                                                    
085800     EJECT                                                                
087200 Z-FINIT SECTION.                                                         
087300                                                                          
087400                                                                          
087500     CLOSE W01160                                                         
087700     SKIP2                                                                
087800     MOVE 'S' TO POSTSUM-OPKOD                                            
087900     CALL POSTSUM USING POSTSUM-PARM                                      
088000     .                                                                    
088100     EJECT                                                                
088200 S01-LAES-W01160  SECTION.                                                
088300     SKIP2                                                                
088400     READ W01160 INTO IN-AREA                                             
088500     AT END                                                               
088600        SET END-OF-W01160 TO TRUE                                         
088700                                                                          
088800     NOT AT END                                                           
088900        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
089000        MOVE 'W26174D1' TO POSTSUM-DDNAMN2                                
089100        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
089200        CALL POSTSUM USING POSTSUM-PARM                                   
089300                                                                          
089400     END-READ                                                             
089500     .                                                                    
089600     EJECT                                                                
090700 X-TAG-CHECKPOINT   SECTION.                                              
090800                                                                          
090900* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
091000* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
091100     PERFORM IMS-CHECKPOINT                                               
091200     MOVE ZERO TO CHKP-ANT                                                
091300* --- LÄS OM DATABAS OM DET BEHÖVS                                        
091400     .                                                                    
091500     EJECT                                                                
091600* --- IMS SEKTIONER ---                                                   
091700                                                                          
091800 IMS-GET-WDK601 SECTION.                                                  
091900                                                                          
092000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
092100          DELIMITED BY SIZE INTO SSA1                                     
092200     MOVE '  GE' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
092400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     EJECT                                                                
092800 IMS-GET-WDK611 SECTION.                                                  
092900                                                                          
093000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
093100          DELIMITED BY SIZE INTO SSA1                                     
093200     MOVE '  GE' TO GODK-STATUSKODER                                      
093300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
093400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
093500     PERFORM IMS-STATUSKONTROLL                                           
093600     .                                                                    
093700     SKIP3                                                                
093800 IMS-REPL-WDK611 SECTION.                                                 
093900                                                                          
094000     MOVE '  ' TO GODK-STATUSKODER                                        
094400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
094500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
094600     PERFORM IMS-STATUSKONTROLL                                           
094700     ADD +1  TO CHKP-ANT                                                  
094800     .                                                                    
094900     EJECT                                                                
095000 IMS-REPL-WDK627 SECTION.                                                 
095100                                                                          
095200     MOVE '  ' TO GODK-STATUSKODER                                        
095600     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK627                       
095700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     ADD +1  TO CHKP-ANT                                                  
096000     .                                                                    
096100     SKIP3                                                                
096200 IMS-GET-WDK627 SECTION.                                                  
096300*                      LÄSNING AV SKROTNINGSSEGMENT WDK627                
096400     MOVE   'WDK611  *F'         TO SSA1                                  
096500     MOVE   'WDK627   '          TO SSA2                                  
096600     MOVE   '  GE'               TO GODK-STATUSKODER                      
096700     SKIP2                                                                
096800     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK627 SSA1                  
096900                                               SSA2                       
097000     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
097100     PERFORM IMS-STATUSKONTROLL                                           
097200     EJECT                                                                
097300     SKIP3                                                                
097400     .                                                                    
097500 IMS-ISRT-WDK627 SECTION.                                                 
097600*                        INSERT AV SKROTNINGSSEGMENT WDK627               
097700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
097800             DELIMITED BY SIZE INTO SSA1                                  
097900     MOVE   'WDK611   '          TO SSA2                                  
098000     MOVE   'WDK627   '          TO SSA3                                  
098100     MOVE   '  '  TO GODK-STATUSKODER                                     
098200                                                                          
098600     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK627 SSA1                  
098700                                          SSA2 SSA3                       
098800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
098900     PERFORM IMS-STATUSKONTROLL                                           
099000     ADD +1  TO CHKP-ANT                                                  
099100     .                                                                    
099200     EJECT                                                                
100500 IMS-GET-WDK901 SECTION.                                                  
100600                                                                          
100700     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
100800          DELIMITED BY SIZE INTO SSA1                                     
100900     MOVE '  GE' TO GODK-STATUSKODER                                      
101000     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
101100     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     EJECT                                                                
102600 IMS-GU-WDR501-6321 SECTION.                                              
102700                                                                          
102800     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
102900            DELIMITED BY SIZE INTO SSA1                                   
103000     MOVE 'GE  '                TO GODK-STATUSKODER                       
103100     CALL CBLTDLI USING GU   6321-PCB DLI-IO-WDR501-6321 SSA1             
103200     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
103300     PERFORM IMS-STATUSKONTROLL                                           
103400     .                                                                    
103500     SKIP3                                                                
103600 IMS-ISRT-WDR501-6321 SECTION.                                            
103700                                                                          
103800     STRING 'WDR501     '                                                 
103900            DELIMITED BY SIZE INTO SSA1                                   
104000     MOVE '  '                  TO GODK-STATUSKODER                       
104400     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
104500     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
104600     PERFORM IMS-STATUSKONTROLL                                           
104700     ADD +1  TO CHKP-ANT                                                  
104800     .                                                                    
104900     EJECT                                                                
105000 IMS-ISRT-WDGX6322 SECTION.                                               
105100                                                                          
105200     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
105300            DELIMITED BY SIZE INTO SSA1                                   
105400     MOVE 'WDGX6322'            TO SSA2                                   
105500     MOVE '  II'                TO GODK-STATUSKODER                       
105900     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
106000     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
106100     PERFORM IMS-STATUSKONTROLL                                           
106200     ADD +1  TO CHKP-ANT                                                  
106300     SKIP3                                                                
106400     .                                                                    
106500     SKIP3                                                                
106600 IMS-ISRT-WDGX6324 SECTION.                                               
106700                                                                          
106800     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
106900            DELIMITED BY SIZE INTO SSA1                                   
107000     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
107100            DELIMITED BY SIZE INTO SSA2                                   
107200     MOVE 'WDGX6324'            TO SSA3                                   
107300     MOVE '  '                  TO GODK-STATUSKODER                       
107700     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
107800                                      SSA1 SSA2 SSA3                      
107900     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     ADD +1  TO CHKP-ANT                                                  
108200     SKIP3                                                                
108300     .                                                                    
108400     EJECT                                                                
108500 IMS-ISRT-WDGX6325 SECTION.                                               
108600                                                                          
108700     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
108800            DELIMITED BY SIZE INTO SSA1                                   
108900     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
109000            DELIMITED BY SIZE INTO SSA2                                   
109100     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
109200          DELIMITED BY SIZE INTO SSA3                                     
109300     MOVE 'WDGX6325 '           TO SSA4                                   
109400     MOVE '  II'                TO GODK-STATUSKODER                       
109800     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6325                     
109900                                      SSA1 SSA2 SSA3 SSA4                 
110000     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
110100     PERFORM IMS-STATUSKONTROLL                                           
110200     ADD +1  TO CHKP-ANT                                                  
110300     .                                                                    
110400     EJECT                                                                
110500 IMS-GET-WDR501-6327 SECTION.                                             
110600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
110700          DELIMITED BY SIZE INTO SSA1                                     
110800     MOVE '  GE' TO GODK-STATUSKODER                                      
110900     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDR501-6327 SSA1               
111000     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
111100     PERFORM IMS-STATUSKONTROLL                                           
111200     .                                                                    
111300     SKIP3                                                                
111400 IMS-GET-WDGX6328 SECTION.                                                
111500     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
111600          DELIMITED BY SIZE INTO SSA1                                     
111700     MOVE '  GE' TO GODK-STATUSKODER                                      
111800     CALL CBLTDLI USING GHNP 6327-PCB DLI-IO-WDGX6328 SSA1                
111900     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
112000     PERFORM IMS-STATUSKONTROLL                                           
112100     .                                                                    
112200     EJECT                                                                
114300 IMS-GET-WDN601 SECTION.                                                  
114400                                                                          
114500     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
114600          DELIMITED BY SIZE INTO SSA1                                     
114700     MOVE '  GE' TO GODK-STATUSKODER                                      
114800     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
114900     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
115000     PERFORM IMS-STATUSKONTROLL                                           
115100     .                                                                    
115200     EJECT                                                                
115300 IMS-GET-WDN611 SECTION.                                                  
115400                                                                          
115500     STRING 'WDN611  (WDN611KY =' W-WDN611KY-X ')'                        
115600          DELIMITED BY SIZE INTO SSA1                                     
115700     MOVE '  GE' TO GODK-STATUSKODER                                      
115800     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
115900     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116200     EJECT                                                                
120300 IMS-GU-WDQ4B1 SECTION.                                                   
120400                                                                          
120500     STRING 'WDQ4B1  (WDQ4B1KY >' W-WDQ4B1KY-MIN                          
120600                    '&WDQ4B1KY <' W-WDQ4B1KY-MAX ')'                      
120700          DELIMITED BY SIZE INTO SSA1                                     
120800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
120900     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
121000     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
121100     PERFORM IMS-STATUSKONTROLL                                           
121200     .                                                                    
121300     SKIP3                                                                
121400 IMS-GN-WDQ4B1 SECTION.                                                   
121500                                                                          
121600     STRING 'WDQ4B1  (WDQ4B1KY >' W-WDQ4B1KY-MIN                          
121700                    '&WDQ4B1KY <' W-WDQ4B1KY-MAX ')'                      
121800          DELIMITED BY SIZE INTO SSA1                                     
121900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
122000     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
122100     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
122200     PERFORM IMS-STATUSKONTROLL                                           
122300     .                                                                    
122400     SKIP3                                                                
122500 IMS-GU-WDQ401 SECTION.                                                   
122600                                                                          
122700     STRING 'WDQ401  (WDQ401KY =' W-WDQ401KY-X ')'                        
122800          DELIMITED BY SIZE INTO SSA1                                     
122900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
123000     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
123100     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
123200     PERFORM IMS-STATUSKONTROLL                                           
123300     .                                                                    
123400     EJECT                                                                
123401 IMS-GET-WDJ1-CSEQ-NEXT SECTION.                                          
123402     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
123403             DELIMITED BY SIZE INTO SSA1                                  
123404     MOVE 'WDJ101   ' TO SSA2                                             
123405     MOVE '  GEGB' TO GODK-STATUSKODER                                    
123406     CALL CBLTDLI USING GN WDJ1C-PCB DLI-IO-WDJ1                          
123407                SSA1 SSA2                                                 
123408     MOVE WDJ1C-STATUS-CODE TO STATUS-WS                                  
123409     PERFORM IMS-STATUSKONTROLL                                           
123410     .                                                                    
123411     EJECT                                                                
123412 IMS-GET-WDD901 SECTION.                                                  
123413                                                                          
123414     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
123415          DELIMITED BY SIZE INTO SSA1                                     
123416     MOVE '  GE' TO GODK-STATUSKODER                                      
123417     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
123418     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
123419     PERFORM IMS-STATUSKONTROLL                                           
123420     .                                                                    
123421     EJECT                                                                
123422 IMS-GET-WDD902 SECTION.                                                  
123423                                                                          
123424     STRING 'WDD902     '                                                 
123425          DELIMITED BY SIZE INTO SSA1                                     
123426     MOVE '  GE' TO GODK-STATUSKODER                                      
123427     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
123428     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
123429     PERFORM IMS-STATUSKONTROLL                                           
123430     .                                                                    
123431     EJECT                                                                
123432 IMS-GET-BENA11-BSEQ SECTION.                                             
123433                                                                          
123434     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
123435             DELIMITED BY SIZE INTO SSA1                                  
123436     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
123437             DELIMITED BY SIZE INTO SSA2                                  
123438     MOVE '  GE' TO GODK-STATUSKODER                                      
123439     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD3 SSA1 SSA2                 
123440     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
123441     PERFORM IMS-STATUSKONTROLL                                           
123442     .                                                                    
123443     EJECT                                                                
123444 IMS-GET-WDK611-2 SECTION.                                                
123445                                                                          
123446     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-2 ')'                         
123447          DELIMITED BY SIZE INTO SSA1                                     
123448     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
123449          DELIMITED BY SIZE INTO SSA2                                     
123450     MOVE '  GE' TO GODK-STATUSKODER                                      
123451     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-WDK611-2 SSA1 SSA2            
123452     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
123453     PERFORM IMS-STATUSKONTROLL                                           
123454     .                                                                    
123455     EJECT                                                                
123500 IMS-RESTART SECTION.                                                     
123600     SKIP2                                                                
123700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
123800     MOVE '  ' TO GODK-STATUSKODER                                        
123900     CALL CBLTDLI USING XRST MSG-PCB                                      
124000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
124100                        CHKP-AREA-LENGTH CHKP-AREA                        
124200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124300     PERFORM IMS-STATUSKONTROLL                                           
124400     .                                                                    
124500     SKIP3                                                                
124600 IMS-CHECKPOINT SECTION.                                                  
124700     SKIP2                                                                
124800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
124900     MOVE '  XD' TO GODK-STATUSKODER                                      
125000     CALL CBLTDLI USING CHKP MSG-PCB                                      
125100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
125200                        CHKP-AREA-LENGTH CHKP-AREA                        
125300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
125400     PERFORM IMS-STATUSKONTROLL                                           
125500                                                                          
125600     IF IMS-EJ-OK                                                         
125700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
125800       DISPLAY FELTEXT                                                    
125900       CALL FELLOG                                                        
126000     END-IF                                                               
126100     .                                                                    
126200     EJECT                                                                
126300 IMS-STATUSKONTROLL SECTION.                                              
126400     SKIP2                                                                
126500     SET STATUS-IX TO 1                                                   
126600     SEARCH GODK-STATUS                                                   
126700       AT END                                                             
126800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
126900           DELIMITED BY SIZE INTO FELTEXT                                 
127000         DISPLAY FELTEXT                                                  
127100         CALL FELLOG                                                      
127200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
127300         CONTINUE                                                         
127400     END-SEARCH                                                           
127500     .                                                                    
127600     EJECT                                                                
127700 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
127800     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
127900                                                                          
128000     MOVE 000100  TO GOOD-SQLCODECODES                                    
128100                                                                          
128200     EXEC SQL                                                             
128300         DECLARE TP1ARTK-CRS CURSOR FOR                                   
128400           SELECT  A.IDKAMP                                               
128500                  ,A.IDARTNR                                              
128600                  ,B.TISTADAT_KAMP                                        
128700                  ,B.TISTODAT_KAMP                                        
128800                  ,B.KDKAMP                                               
128900                  ,B.IDKAMP_GRP                                           
129000                                                                          
129100           FROM    TP1ARTK A                                              
129200                  ,TP1KAMP B                                              
129300                                                                          
129400           WHERE   A.IDARTNR = :W-IDARTNR                                 
129500               AND A.IDKAMP  =  B.IDKAMP                                  
129600                                                                          
129700           ORDER BY B.IDKAMP                                              
129800     END-EXEC                                                             
129900                                                                          
130000     MOVE 000100  TO GOOD-SQLCODECODES                                    
131800     MOVE SQLCODE TO SQLCODE-WS                                           
131801     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
131802     .                                                                    
131803     SKIP3                                                                
131804 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
131805     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
131806     SKIP2                                                                
131807     MOVE 000100  TO GOOD-SQLCODECODES                                    
131808     EXEC SQL                                                             
131809         FETCH TP1ARTK-CRS INTO                                           
131810                    :TP1KAMP-IDKAMP                                       
131811                   ,:TP1ARTK-IDARTNR                                      
131812                   ,:TP1KAMP-TISTADAT-KAMP                                
131813                   ,:TP1KAMP-TISTODAT-KAMP                                
131814                   ,:TP1KAMP-KDKAMP                                       
131815                   ,:TP1KAMP-IDKAMP-GRP                                   
131816     END-EXEC                                                             
131817                                                                          
131818     MOVE SQLCODE TO SQLCODE-WS                                           
131900     PERFORM DB2-STATUS-CHECK                                             
132000     .                                                                    
132100     SKIP3                                                                
132200 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
132300     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
132400                                                                          
132500     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
132600     .                                                                    
132700     EJECT                                                                
132800 DB2-STATUS-CHECK  SECTION.                                               
132900                                                                          
133000     SET SQLCODE-IX TO 1                                                  
133100     SEARCH GOOD-SQLCODE                                                  
133200       AT END                                                             
133300*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
133400*         DELIMITED BY SIZE INTO ERROR-TEXT                               
133500          CALL FELLOG                                                     
133600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
133700     END-SEARCH                                                           
133800     .                                                                    
