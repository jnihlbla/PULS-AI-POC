000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017100.                                                
000300 AUTHOR.         KJELLSON GÖRAN  GUIDE.                                   
000400 DATE-WRITTEN.   2004  OCTOBER                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.INVADJUSTMENT                               
000800*    WEB-LDC: WL017100 PROGRAM IS A REPLICA OF W5010600 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNCTION: UPDATES WDK6 AND WDK7 WITH INVENTORY ADJUSTMENTS           
001200*                                                                         
001300*              UPDATES WDR9-PEDAL                                         
001400*              UPDATES WDR8                                               
001500*                                                                         
001600*              UPDATES WDK7 (FLREFNYO = NOO) WHEN NOT DC11.               
001700*                                                                         
001800*    CHANGES:                                                             
001900*           ETRACKER: 10286188  DO NOT EXECUTE SECTION S08 IF             
002000*                               NO INVENTORY SEGMENTS EXIST               
002100*           ETRACKER: 5277658                                             
002200*           070711:A TYPE 7 WILL NOT BE ALLOWED TO BE CERATED             
002300*           ETRACKER: 4637061   NYTT DATA ELEMENT PÅ WDH711               
002400*                               KVANTAL - NOLLAS                          
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: WL0171T                                             
002800*        REQUEST:     WL0171I1                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        RESPONSE:    WL0171O1                                            
003200                                                                          
003300                                                                          
003400 ENVIRONMENT DIVISION.                                                    
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800 DATA DIVISION.                                                           
003900 FILE SECTION.                                                            
004000                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(08)   VALUE 'WL017100'.            
004400 77  CURR-DATE                   PIC 9(8).                                
004500 77  W-CURR-DATE                 PIC 9(8).                                
004600 77  TRANS-TIME                  PIC 9(9).                                
004700 77  WS-TID                      PIC 9(9).                                
004800                                                                          
004900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200 77  CURR-DISPLAY                PIC X(80) VALUE 'MAIN'.                  
005300 77  CURR-SECTION                PIC X(80) VALUE 'MAIN'.                  
005400 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
005500                                                                          
005600 77  YES                         PIC X      VALUE 'J'.                    
005700 77  NOO                         PIC X      VALUE 'N'.                    
005800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005900 77  INDX                        PIC S9(9)  COMP SYNC.                    
006000 77  INDY                        PIC S9(9)  COMP SYNC.                    
006100 77  WS-KVRADER                  PIC 9(9).                                
006200 77  IX                      PIC 9(3)   VALUE ZERO.                       
006300 77  IX2                     PIC 9(3)   VALUE ZERO.                       
006400 77  W-IDARTNR-EDIT-X            PIC Z(9).                                
006500                                                                          
006600 77  WS-KVANTAL              PIC S9(7)   COMP-3.                          
006700 77  WS-ANTAL                 PIC S9(9) VALUE ZERO.                       
006800 77  WS-FLTRACK                  PIC X(1)    VALUE 'N'.                   
006900 77  WS-FLLOCAL                  PIC X(1)    VALUE 'N'.                   
007000 77  WS-USE-TEMP                 PIC X(1)    VALUE 'N'.                   
007100 77  WS-VALD-KVAVIS              PIC 9(6)    VALUE ZERO.                  
007200 77  WS-TRCK-KVANTMOT            PIC 9(6)    VALUE ZERO.                  
007300 77  WS-TEMP-KVAVIS              PIC 9(6)    VALUE ZERO.                  
007400 77  WS-TEMP-KVAVIS-SHOW         PIC Z(4)9.                               
007500 77  WS-SLAG-KVLS                PIC S9(7)   COMP-3.                      
007600 77  WS-CLAG-KVLS                PIC S9(7)   COMP-3.                      
007700 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
007800 77  IDTRACK-QTY-SW              PIC X       VALUE 'N'.                   
007900     88  IDTRACK-QTY-DONE                    VALUE 'J'.                   
008000     88  IDTRACK-QTY-NOT-DONE                VALUE 'N'.                   
008100                                                                          
008200 77  W-PART-KVROS-SDC-NDC        PIC S9(7)  VALUE ZERO COMP-3.            
008300 77  DELIVERY-DATE               PIC S9(7)  COMP-3.                       
008400 77  DELIVERY-NUMB               PIC S9(7)  COMP-3.                       
008500 77  W-KDSPRAK                   PIC S9     COMP-3.                       
008600 77  WWW                         PIC X(4) VALUE 'L001'.                   
008700 77  W-SUM                       PIC S9(7)  COMP-3.                       
008800 77  W-SUM-STD                   PIC S9(11)V9(2)  COMP-3.                 
008900 77  KDINVKAT-WS                 PIC S9(3)  COMP-3 VALUE ZERO.            
009000 77  WS-KDSORT                   PIC X(2)   VALUE SPACE.                  
009100 77  W-PRAVCOST                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009200 77  KVJUSTKV-WS                 PIC X(6).                                
009300 77  WS-LOGT-SIGN                PIC X(1).                                
009400 77  WS-TRCK-IDTRACK             PIC X(25).                               
009500 77  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
009600 77  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
009700 77  WS-CP-UTF8                  PIC X(4)   VALUE 'UTF8'.                 
009800 77  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
009900 77  WS-ADDRESS-MQASYNC          PIC X(50)                                
010000       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
010100 77  WS-ADDRESS-WHSTOCKA         PIC X(50)                                
010200       VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                  
010300                                                                          
010400 77  KEYS-SW                     PIC X      VALUE 'J'.                    
010500     88  KEYS-OK                            VALUE 'J'.                    
010600     88  KEYS-WRONG                         VALUE 'N'.                    
010700                                                                          
010800 77  INDATA-SW                   PIC X      VALUE 'J'.                    
010900     88  INDATA-OK                          VALUE 'J'.                    
011000     88  INDATA-WRONG                       VALUE 'N'.                    
011100                                                                          
011200 77  KAT9-SW                     PIC X      VALUE 'N'.                    
011300     88  KAT9-YES                           VALUE 'J'.                    
011400     88  KAT9-NOO                           VALUE 'N'.                    
011500                                                                          
011600 01  DIVERSE.                                                             
011700     03  LOGG-DATUM          PIC 9(6).                                    
011800     03  DAGENS-DATUM        PIC 9(6).                                    
011900     03  DAGENS-TID          PIC 9(8).                                    
012000     03  DAGENS-DAT-WDH111   PIC 9(6).                                    
012100     03  WS-EKH-KDEKSHT.                                                  
012200         05 WS-KDEKSHT-1     PIC X(2).                                    
012300         05 WS-KDEKSHT-2     PIC X.                                       
012400     03  W-EKH-IDARTNR       PIC X(9)  VALUE SPACE.                       
012500     03  INVKOE-ART-FINNS    PIC X.                                       
012600     03  W-DATUM-Y.                                                       
012700         05  W-DATUM-LOCAL   PIC 9(6).                                    
012800     03  AKTUELL-TID-X.                                                   
012900         05  AKTUELL-TTMM-LOC    PIC 9(4).                                
013000         05  FILLER              PIC 9(4).                                
013100                                                                          
013200 01  WS-KVJUSTKV-X.                                                       
013300     03  WS-KVJUSTKV-N       PIC 9(7).                                    
013400                                                                          
013500 01  WS-KVJUSTKV-COMP        PIC S9(7)    COMP-3 VALUE ZERO.              
013600                                                                          
013700 01  WS-SPAR-HIST.                                                        
013800     03 WS-DAREGDAT-CRE          PIC 9(8) VALUE ZERO.                     
013900     03 WS-DAREGDAT-CLO          PIC 9(8) VALUE ZERO.                     
014000     03 WS-DAREGDAT-PR1          PIC 9(8) VALUE ZERO.                     
014100     03 WS-DAREGDAT-PR2          PIC 9(8) VALUE ZERO.                     
014200     03 WS-DAREGDAT-PR3          PIC 9(8) VALUE ZERO.                     
014300     03 WS-IDUSER-PR1            PIC X(8) VALUE SPACE.                    
014400     03 WS-IDUSER-PR2            PIC X(8) VALUE SPACE.                    
014500     03 WS-IDUSER-PR3            PIC X(8) VALUE SPACE.                    
014600     03 WS-IDUSER-CRE            PIC X(8) VALUE SPACE.                    
014700     03 WS-IDUSER-CLO            PIC X(8) VALUE SPACE.                    
014800                                                                          
014900 01  CONSTANSTS.                                                          
015000     03  PART-DC-EXISTS      PIC X.                                       
015100     03  INV-EXISTS          PIC X.                                       
015200     03  PART-INV-EXISTS     PIC X.                                       
015300     03  INVKOE-PART-EXISTS  PIC X.                                       
015400     03  INV-CAT-EXISTS      PIC X    VALUE 'N'.                          
015500                                                                          
015600 01  WS-FIX-DATUM.                                                        
015700     03  WS-FIX-TISEGKEY     PIC 9(9).                                    
015800     03  WS-FILLER1 REDEFINES WS-FIX-TISEGKEY.                            
015900         05 WS-FILLER1-1-2   PIC 9(2).                                    
016000         05 WS-TISEGKEY-3-8  PIC 9(6).                                    
016100         05 WS-FILLER1-9     PIC 9(1).                                    
016200     03  WS-FILLER2 REDEFINES WS-FIX-TISEGKEY.                            
016300         05 WS-TISEGKEY-1-8  PIC 9(8).                                    
016400         05 WS-FILLER2-9     PIC 9(1).                                    
016500                                                                          
016600 01  WS-INV-DAREGDAT-AREA.                                                
016700     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
016800     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
016900       05  WS-INV-NOLL         PIC 9(1).                                  
017000       05  WS-INV-SEKEL        PIC 9(2).                                  
017100       05  WS-INV-AAMMDD       PIC 9(6).                                  
017200                                                                          
017300 01  WS-TISEGKEYAREA.                                                     
017400     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
017500     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
017600         05  WS-SEKEL        PIC 9(2).                                    
017700         05  WS-TIAAMMDD     PIC 9(6).                                    
017800         05  WS-LOPNR        PIC 9(1).                                    
017900     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
018000                                                                          
018100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
018200 01  GENERAL-SUBPROGRAMS.                                                 
018300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
018700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018900     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
019000     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
019100     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
019200                                                                          
019300*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
019400*01 -COPY W009CIA                                                         
019500                                                                          
019600 01  FILLER              PIC X(16)  VALUE 'WDATAREA'.                     
019700*01  -COPY WDATAREA                                                       
019800 01  FILLER              PIC X(16) VALUE 'WL01TIDZ-AREA'.                 
019900*01  -COPY WL01TIDZ                                                       
020000*    --- PARAMETERS TO ABEND                                              
020100                                                                          
020200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
020400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
020500                                                                          
020600 01  MESSAGE-CODES.                                                       
020700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
020800                                                                          
020900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
021000*01  -COPY WZ01SEND                                                       
021100                                                                          
021200 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
021300*01  -COPY WZ04PROP                                                       
021400     EJECT                                                                
021500                                                                          
021600*                                                                         
021700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
021800                                                                          
021900*01  -COPY WZ01SUB                                                        
022000                                                                          
022100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
022200                                                                          
022300 01  REQU-AREA.                                                           
022400*    03  -COPY WZ01REQU                                                   
022500*    03  -COPY WL0171I1                                                   
022600                                                                          
022700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
022800                                                                          
022900 01  RESP-AREA.                                                           
023000*    03  -COPY WZ01RESP                                                   
023100*    03  -COPY WL0171O1                                                   
023200                                                                          
023300 01  FILLER                      PIC X(16)   VALUE 'DC CODES'.            
023400                                                                          
023500*01  -COPY WWDC99                                                         
023600*                                                                         
023700*01  -COPY WWDCKONS                                                       
023800*                                                                         
023900*01  -COPY WTRAUTF8                                                       
024000                                                                          
024100*********** NYCKLAR TILL LÄSNINGAR                                        
024200                                                                          
024300 01      W-IDARTNR-X.                                                     
024400   03    W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
024500                                                                          
024600 01  W-WDGXKEY-ROT-X.                                                     
024700     03  FILLER              PIC X(4)  VALUE '5115'.                      
024800     03  FILLER              PIC X(26) VALUE LOW-VALUE.                   
024900                                                                          
025000 01      W-KDSEGKEY-X.                                                    
025100   03    W-KDSEGKEY      PIC X       VALUE '1'.                           
025200                                                                          
025300 01      W-WDD811KY-X.                                                    
025400   03    W-IDDC-WDD8     PIC X(2)           VALUE  SPACE.                 
025500   03    W-ADBUFFOMR     PIC S9(3)  COMP-3  VALUE  +1.                    
025600   03    W-DABUFPAF      PIC  9(8)          VALUE  ZERO.                  
025700   03    W-ADBUFFGANG    PIC S9(3)  COMP-3  VALUE  ZERO.                  
025800   03    W-ADBUFFPL      PIC S9(5)  COMP-3  VALUE  ZERO.                  
025900                                                                          
026000 01  W-DAINLEV-X.                                                         
026100     03  W-DAINLEV      PIC  9(16).                                       
026200                                                                          
026300 01  W-IDLEVNYCK-X.                                                       
026400     03  W-IDPTYP       PIC X(3)   VALUE 'R34'.                           
026500                                                                          
026600 01  W-IDSKYLT-X.                                                         
026700     03  W-IDSKYLT      PIC X(3)   VALUE 'S  '.                           
026800                                                                          
026900 01  W-IDDC-X.                                                            
027000     03  W-IDDC              PIC X(2)   VALUE SPACE.                      
027100                                                                          
027200 01  W-TISEGKEY-X.                                                        
027300     03  W-TISEGKEY          PIC S9(9)  VALUE +999999999 COMP-3.          
027400                                                                          
027500 01  FILLER              PIC X(4)   VALUE 'L222'.                         
027600 01  W-KVJUSTKV-IN-X.                                                     
027700     03  W-KVJUSTKV-IN-N     PIC 9(7) VALUE ZERO.                         
027800 01  W-KVJUSTKV-COMP         PIC 9(7)     COMP-3 VALUE ZERO.              
027900                                                                          
028000 01  IDARTNR-WS              PIC X(9).                                    
028100 01  FILLER REDEFINES IDARTNR-WS.                                         
028200     03  NOLL-ARTIKEL        PIC X(1).                                    
028300     03  IDARTNR-WS-X        PIC X(8).                                    
028400                                                                          
028500 01  W-IDARTNR-UTR-X.                                                     
028600     03  W-IDDC-UTR          PIC X(2)   VALUE SPACE.                      
028700     03  W-IDARTNR-UTR       PIC S9(9)  VALUE ZERO COMP-3.                
028800     03  FILLER              PIC  X(8)  VALUE LOW-VALUE.                  
028900                                                                          
029000 01  FILLER              PIC X(8)   VALUE 'W1-WDH1='.                     
029100 01  W1-WDH1KEY-X.                                                        
029200     03  W1-IDDC-WDH1    PIC X(2)   VALUE SPACE.                          
029300     03  W1-KDINVKAT     PIC S9(3)  COMP-3 VALUE ZERO.                    
029400     03  W1-TISEGKEY     PIC S9(9)  COMP-3 VALUE ZERO.                    
029500     03  W1-DAREGDAT-SORT    PIC 9(8) VALUE ZERO.                         
029600                                                                          
029700 01  W-WDH1KEY-UNIK-X.                                                    
029800     03  W-IDDC-UNIK             PIC X(2)   VALUE SPACE.                  
029900     03  W-KDINVKAT-UNIK         PIC S9(3)  COMP-3 VALUE ZERO.            
030000     03  W-TISEGKEY-UNIK         PIC S9(9)  COMP-3 VALUE ZERO.            
030100     03  W-DAREGDAT-SORT-UNIK    PIC 9(8) VALUE ZERO.                     
030200                                                                          
030300 01  FILLER              PIC X(8)   VALUE 'W2-WDH1='.                     
030400 01  W2-WDH1KEY-X.                                                        
030500     03  W2-IDDC-WDH1    PIC X(2)   VALUE SPACE.                          
030600     03  W2-KDINVKAT     PIC S9(3)  COMP-3 VALUE +999.                    
030700     03  W2-TISEGKEY     PIC S9(9)  COMP-3 VALUE +999999999.              
030800     03  W2-DAREGDAT-SORT    PIC 9(8) VALUE 99999999.                     
030900 01  W-WDH1KEY-MIN-X.                                                     
031000     03  W-IDDC-WDH1-MIN PIC X(2).                                        
031100     03  W-KDINVKAT-MIN  PIC S9(3)  COMP-3.                               
031200     03  W-TISEGKEY-MIN  PIC S9(9)  COMP-3 VALUE ZERO.                    
031300     03  W-DAREGDAT-SORT-MIN    PIC 9(8) VALUE ZERO.                      
031400                                                                          
031500 01  FILLER              PIC X(8)   VALUE 'WDH1MAX='.                     
031600 01  W-WDH1KEY-MAX-X.                                                     
031700     03  W-IDDC-WDH1-MAX PIC X(2).                                        
031800     03  W-KDINVKAT-MAX  PIC S9(3)  COMP-3 VALUE +999.                    
031900     03  W-TISEGKEY-MAX  PIC S9(9)  COMP-3 VALUE +999999999.              
032000     03  W-DAREGDAT-SORT-MAX PIC 9(8)    VALUE 99999999.                  
032100                                                                          
032200 01  W-WDGX-4505-KEY-X.                                                   
032300     03  W-IDHTYP-4505       PIC X(4)    VALUE '4505'.                    
032400     03  W-IDDC-4505         PIC X(2)    VALUE '11'.                      
032500     03  FILLER              PIC X(24) VALUE LOW-VALUE.                   
032600                                                                          
032700 01  W-KDERS-0-X.                                                         
032800     03  W-KDERS-0           PIC S9(3)  COMP-3  VALUE +0.                 
032900*    NOTAFISCAL                                                           
033000 01  NOTF-AREA.                                                           
033100*    03  -COPY W611NOTF                                                   
033200     EJECT                                                                
033300                                                                          
033400 01  IMS-WS.                                                              
033500   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
033600*****                    **** STATUS-KOD FRÅN IMS                         
033700   03    STATUS-WS       PIC XX.                                          
033800         88  SEGMENT-FOUND       VALUE '  '.                              
033900         88  SEGMENT-MISSING     VALUE 'GE'.                              
034000         88  SEGMENT-EXISTS      VALUE 'II'.                              
034100         88  INDEX-EXISTS        VALUE 'NI'.                              
034200                                                                          
034300   03    GOOD-STATUSCODES.                                                
034400     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034500                                                                          
034600 01      SSA1            PIC X(128) VALUE SPACE.                          
034700 01      SSA2            PIC X(128) VALUE SPACE.                          
034800 01      SSA3            PIC X(128) VALUE SPACE.                          
034900                                                                          
035000*                            IMS FUNCTION CODES                           
035100*01      -COPY W0003                                                      
035200*-------- WDH1-INVENTERINGSREG                                            
035300                                                                          
035400 01  FILLER              PIC X(16)   VALUE 'WDH101'.                      
035500*                                                                         
035600*01  WDH101       -COPY WDH101 -PRE INVB-.                                
035700     EJECT                                                                
035800                                                                          
035900 01  FILLER              PIC X(16)   VALUE 'WDH111'.                      
036000*                                                                         
036100*01  WDH111       -COPY WDH111 -PRE INVB-.                                
036200     EJECT                                                                
036300 01  FILLER              PIC X(16)   VALUE 'WDH121'.                      
036400*                                                                         
036500*01  WDH121       -COPY WDH121 -PRE INVB-.                                
036600     EJECT                                                                
036700*-------- WDK6-ARTIKELREG                                                 
036800                                                                          
036900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
037000 01  DLI-IO-WDK601.                                                       
037100*  03  -COPY WDK601.                                                      
037200                                                                          
037300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
037400 01  DLI-IO-WDK611.                                                       
037500*  03  -COPY WDK611.                                                      
037600                                                                          
037700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
037800 01  DLI-IO-WDK629.                                                       
037900*    03  -COPY WDK629                                                     
038000                                                                          
038100 01  FILLER              PIC X(16)   VALUE 'WDGX4506'.                    
038200*                                                                         
038300*01  WL450511     -COPY WDGX4506.                                         
038400     EJECT                                                                
038500                                                                          
038600 01  FILLER              PIC X(16)   VALUE 'WDGX5116'.                    
038700*                                                                         
038800*01  WLXXEF11     -COPY WDGX5116.                                         
038900     EJECT                                                                
039000                                                                          
039100 01  FILLER              PIC X(16)   VALUE 'WDL201'.                      
039200*                                                                         
039300*01  WLINLE01 -COPY WDL201 -PRE INL-                                      
039400     EJECT                                                                
039500                                                                          
039600 01  FILLER              PIC X(16)   VALUE 'WDL211'.                      
039700*                                                                         
039800*01  WLINLE11 -COPY WDL211 -PRE INL-                                      
039900     EJECT                                                                
040000                                                                          
040100 01  FILLER              PIC X(16)   VALUE 'WDL221'.                      
040200*                                                                         
040300*01  WLINLE21 -COPY WDL221 -PRE INL-                                      
040400     EJECT                                                                
040500                                                                          
040600 01  FILLER              PIC X(16)   VALUE 'WDL231'.                      
040700*                                                                         
040800*01  WLINLE31 -COPY WDL231 -PRE INL-                                      
040900     EJECT                                                                
041000                                                                          
041100 01  FILLER              PIC X(16)   VALUE 'WDL222'.                      
041200*                                                                         
041300*01  WLINLE22 -COPY WDL222 -PRE INL-                                      
041400     EJECT                                                                
041500                                                                          
041600 01  FILLER              PIC X(16)   VALUE 'WDD801'.                      
041700*                                                                         
041800*01  WLARTD01 -COPY WDD801 -PRE ART-                                      
041900     EJECT                                                                
042000                                                                          
042100 01  FILLER              PIC X(16)   VALUE 'WDD811'.                      
042200*                                                                         
042300*01  WLARTD11 -COPY WDD811 -PRE ART-                                      
042400     EJECT                                                                
042500                                                                          
042600 01  FILLER              PIC X(16)   VALUE 'WDD311'.                      
042700*                                                                         
042800*01  WLBENA11 -COPY WDD311 -PRE BEN-                                      
042900     EJECT                                                                
043000                                                                          
043100 01  FILLER              PIC X(16)   VALUE 'WDL601'.                      
043200*                                                                         
043300*01  WLINLC01 -COPY WDL601 -PRE INLC-                                     
043400     EJECT                                                                
043500                                                                          
043600 01  FILLER              PIC X(16)   VALUE 'WDL611'.                      
043700*                                                                         
043800*01  WLINLC11 -COPY WDL611 -PRE INLC-                                     
043900     EJECT                                                                
044000                                                                          
044100 01  FILLER              PIC X(16)   VALUE 'WDL623'.                      
044200*01  WDL623  -COPY WDL623                                                 
044300     EJECT                                                                
044400                                                                          
044500                                                                          
044600*-------- WDK7-ARTIKELREG                                                 
044700                                                                          
044800 01  FILLER              PIC X(16)   VALUE 'WDK701'.                      
044900*                                                                         
045000*01  WLARTS01     -COPY WDK701                                            
045100     EJECT                                                                
045200                                                                          
045300 01  FILLER              PIC X(16)   VALUE 'WDK711'.                      
045400*                                                                         
045500*01  WLARTS11     -COPY WDK711                                            
045600     EJECT                                                                
045700                                                                          
045800 01  FILLER              PIC X(16)   VALUE 'WDK728'.                      
045900*                                                                         
046000*01  WDK728       -COPY WDK728                                            
046100     EJECT                                                                
046200                                                                          
046300*-------- WDH7-INVENTERINGSHISTORIK                                       
046400                                                                          
046500 01  FILLER              PIC X(16)   VALUE 'WDH701'.                      
046600*                                                                         
046700*01  WLINVC01     -COPY WDH701                                            
046800     EJECT                                                                
046900                                                                          
047000 01  FILLER              PIC X(16)   VALUE 'WDH711'.                      
047100*                                                                         
047200*01  WLINVC11     -COPY WDH711                                            
047300                                                                          
047400 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
047500*01  WLLOGA01     -COPY WDL901                                            
047600                                                                          
047700 01  FILLER              PIC X(16)   VALUE 'WDL301'.                      
047800*01  WDL301  -COPY WDL301                                                 
047900     EJECT                                                                
048000                                                                          
048100 01  FILLER                      PIC X(16) VALUE 'WLSAPA01'.              
048200*01  WLSAPA01    -COPY WDR901                                             
048300*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
048400 01  FILLER                      PIC X(16) VALUE 'WDR801'.                
048500*01  WDR801      -COPY WDR801 -PRE WDR8-                                  
048600*    05 -COPY W510EKHA -RED WDR8-FIL-WDR801-DATA -PRE WDR8-               
048700*    05 -COPY W510A08  -RED WDR8-FIL-WDR801-DATA -PRE A08-                
048800 01  FILLER              PIC X(16)   VALUE 'WDB601'.                      
048900*                                                                         
049000*01  WDB601       -COPY WDB601                                            
049100     EJECT                                                                
049200 LINKAGE SECTION.                                                         
049300*01  -COPY W0009     -PRE MSG-                                            
049400     EJECT                                                                
049500 01  MQASYNC-PCB              PIC X.                                      
049600 EJECT                                                                    
049700*01  -COPY W0008     -PRE INVREG-.                                        
049800         05  FILLER           PIC X.                                      
049900     EJECT                                                                
050000*01  -COPY W0008     -PRE WDK6-.                                          
050100         05  FILLER           PIC X.                                      
050200*01  -COPY W0008     -PRE 4505-                                           
050300         05  FILLER           PIC X.                                      
050400     EJECT                                                                
050500*01  -COPY W0008     -PRE XXEF-                                           
050600         05  FILLER           PIC X.                                      
050700                                                                          
050800*01  -COPY W0008     -PRE BEN-                                            
050900         05  FILLER           PIC X.                                      
051000     EJECT                                                                
051100*01  -COPY W0008     -PRE ARTS-                                           
051200         05  FILLER           PIC X.                                      
051300                                                                          
051400*01  -COPY W0008     -PRE WDK7-                                           
051500     05  FILLER               PIC X.                                      
051600     EJECT                                                                
051700*01  -COPY W0008     -PRE INLC-                                           
051800         05  FILLER           PIC X.                                      
051900     EJECT                                                                
052000*01  -COPY W0008     -PRE INVC-                                           
052100         05  FILLER           PIC X.                                      
052200     EJECT                                                                
052300*01  -COPY W0008     -PRE LOGA-                                           
052400         05  FILLER           PIC X.                                      
052500     EJECT                                                                
052600*01  -COPY W0008     -PRE SAPA-                                           
052700         05  FILLER           PIC X.                                      
052800*01  -COPY W0008     -PRE WDR8-                                           
052900         05  FILLER           PIC X.                                      
053000*01  -COPY W0008     -PRE WDB6-                                           
053100         05  FILLER           PIC X.                                      
053200                                                                          
053300*01  -COPY W0008     -PRE WDL6-                                           
053400         05  FILLER           PIC X.                                      
053500*01  -COPY W0008     -PRE WDL3-                                           
053600         05  FILLER           PIC X.                                      
053700                                                                          
053800                                                                          
053900 PROCEDURE DIVISION  USING MSG-PCB MQASYNC-PCB                            
054000                           INVREG-PCB WDK6-PCB                            
054100                           4505-PCB   XXEF-PCB                            
054200                           BEN-PCB    ARTS-PCB   WDK7-PCB                 
054300                           INLC-PCB   INVC-PCB   LOGA-PCB                 
054400                           SAPA-PCB   WDR8-PCB   WDB6-PCB                 
054500                           WDL6-PCB   WDL3-PCB.                           
054600 MAIN SECTION.                                                            
054700     ENTRY 'DLITCBL' USING MSG-PCB MQASYNC-PCB                            
054800                           INVREG-PCB WDK6-PCB                            
054900                           4505-PCB   XXEF-PCB                            
055000                           BEN-PCB    ARTS-PCB   WDK7-PCB                 
055100                           INLC-PCB   INVC-PCB   LOGA-PCB                 
055200                           SAPA-PCB   WDR8-PCB   WDB6-PCB                 
055300                           WDL6-PCB   WDL3-PCB.                           
055400                                                                          
055500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
055600     IF SUB-KDRC = 0                                                      
055700        PERFORM A-INIT                                                    
055800        PERFORM B-CHECK-KEYS                                              
055900        IF KEYS-OK AND INDATA-OK                                          
056000           PERFORM I-CHECK-INVENTORY                                      
056100           MOVE REQU-IDDC-KEY  TO WS-IDDC                                 
056200           IF XDC-NON-VCC-OWNED OR LDC-CN                                 
056300             IF KEYS-OK AND INDATA-OK                                     
056400              PERFORM J-CHECK-AVGCOST                                     
056500             END-IF                                                       
056600           END-IF                                                         
056700        END-IF                                                            
056800                                                                          
056900        MOVE REQU-IDDC-KEY    TO W-IDDC                                   
057000        PERFORM IMS-GU-WDB601                                             
057100        IF DCS-FLTRACK = 'J'                                              
057200           MOVE 'J'  TO WS-FLTRACK                                        
057300        ELSE                                                              
057400           MOVE 'N'  TO WS-FLTRACK                                        
057500        END-IF                                                            
057600        IF WS-FLTRACK = 'J'                                               
057700          MOVE 'J' TO WS-FLLOCAL                                          
057800          IF NDC-MX                                                       
057900            PERFORM IMS-GU-WDK711                                         
058000            IF SLAG-IDDC-REF = SPACE                                      
058100              MOVE 'N' TO WS-FLLOCAL                                      
058200            END-IF                                                        
058300          END-IF                                                          
058400        END-IF                                                            
058500        IF WS-FLTRACK = 'J' AND WS-FLLOCAL = 'J'                          
058600           IF KEYS-OK AND INDATA-OK                                       
058700              IF REQU-KDPGMACT = 'E' OR 'C'                               
058800                IF (REQU-FLSLACK = YES AND KAT9-YES)                      
058900                  CONTINUE                                                
059000                ELSE                                                      
059100****              IDTRACK                                                 
059200                  IF REQU-KDAVVTYP = '+' OR ' '                           
059300                     PERFORM KA-VALIDATE-WDK728                           
059400                     IF IDTRACK-QTY-NOT-DONE                              
059500                        MOVE SPACES TO RESP-IDELMT-ERROR                  
059600                        MOVE WS-TEMP-KVAVIS   TO                          
059700                                     WS-TEMP-KVAVIS-SHOW                  
059800                        STRING ' :' WS-TEMP-KVAVIS-SHOW                   
059900                        DELIMITED BY SIZE INTO RESP-IDELMT-ERROR          
060000                        MOVE '423'           TO RESP-IDMSG-ERROR          
060100                        MOVE NOO             TO INDATA-SW                 
060200                     END-IF                                               
060300                  END-IF                                                  
060400                  IF REQU-KDAVVTYP = '-'                                  
060500                     IF REQU-KVJUSTKV-IN <= SLAG-KVLS                     
060600                        PERFORM KB-VALIDATE-WDK728                        
060700** NOT LESS THAN ZERO FOR IDTRACK!                                        
060800                        IF IDTRACK-QTY-NOT-DONE                           
060900** THEN USE ONLY WS-TEMP-KVAVIS                                           
061000                           MOVE 'J' TO WS-USE-TEMP                        
061100                        END-IF                                            
061200                     ELSE                                                 
061300                        MOVE SPACES TO RESP-IDELMT-ERROR                  
061400                        MOVE SLAG-KVLS TO WS-TEMP-KVAVIS-SHOW             
061500                        STRING ' :' WS-TEMP-KVAVIS-SHOW                   
061600                        DELIMITED BY SIZE INTO RESP-IDELMT-ERROR          
061700                        MOVE '424'           TO RESP-IDMSG-ERROR          
061800                        MOVE NOO             TO INDATA-SW                 
061900                     END-IF                                               
062000                  END-IF                                                  
062100                END-IF                                                    
062200              END-IF                                                      
062300           END-IF                                                         
062400        END-IF                                                            
062500                                                                          
062600        IF KEYS-OK AND INDATA-OK                                          
062700           IF REQU-KDPGMACT = 'E'                                         
062800             IF (REQU-FLSLACK = YES AND KAT9-YES)                         
062900               CONTINUE                                                   
063000             ELSE                                                         
063100               PERFORM C-UPDATE-INVENTORY                                 
063200               IF INDATA-OK                                               
063300                 IF REQU-FLFLYTTN = NOO                                   
063400                    PERFORM D-RENSA-WDH1                                  
063500                    IF REQU-FLSLACK = NOO                                 
063600                       PERFORM E-SKAPA-INV-SEGM                           
063700                    END-IF                                                
063800                 END-IF                                                   
063900                 PERFORM F-SKAPA-UPPDATERA-WDK7-POST                      
064000                 MOVE '001' TO RESP-IDMSG-INFO                            
064100                 MOVE SPACES TO RESP-IDMSG-ERROR                          
064200               END-IF                                                     
064300             END-IF                                                       
064400           END-IF                                                         
064500        END-IF                                                            
064600        IF KEYS-OK AND INDATA-OK                                          
064700          PERFORM G-READ-AND-SHOW-INVENTORY                               
064800        END-IF                                                            
064900        IF KEYS-OK AND INDATA-OK                                          
065000          IF REQU-KDPGMACT = 'C'                                          
065100            IF (REQU-FLSLACK = YES AND KAT9-YES)                          
065200              CONTINUE                                                    
065300            ELSE                                                          
065400               PERFORM H-CALCULATE-INVENTORY                              
065500            END-IF                                                        
065600          END-IF                                                          
065700        END-IF                                                            
065800        IF KEYS-WRONG                                                     
065900          IF REQU-KDPGMACT = 'S'                                          
066000            MOVE ZERO               TO RESP-KVRADER                       
066100          END-IF                                                          
066200        ELSE                                                              
066300          IF INDATA-WRONG                                                 
066400            MOVE 5                  TO RESP-KVRADER                       
066500          END-IF                                                          
066600        END-IF                                                            
066700        PERFORM S02-RETURN-RESPONSE                                       
066800     END-IF                                                               
066900                                                                          
067000     MOVE ZERO TO RETURN-CODE                                             
067100     GOBACK                                                               
067200     .                                                                    
067300                                                                          
067400 A-INIT SECTION.                                                          
067500     MOVE 'A-INIT'  TO CURR-SECTION                                       
067600                                                                          
067700     MOVE ALL '+'   TO RESP-AREA                                          
067800     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
067900                       RESP-IDMSG-INFO                                    
068000                       RESP-IDELMT-ERROR                                  
068100     MOVE 001       TO RESP-IDMSGVER                                      
068200*    MOVE SPACE     TO RESP-WL0171O1                                      
068300     MOVE ZERO      TO RESP-KVRADER                                       
068400                                                                          
068500     ACCEPT DAGENS-DAT-WDH111 FROM DATE                                   
068600     ACCEPT DAGENS-DATUM      FROM DATE                                   
068700     ACCEPT DAGENS-TID        FROM TIME                                   
068800     MOVE DAGENS-TID   TO AKTUELL-TID-X                                   
068900     MOVE FUNCTION CURRENT-DATE (1:8) TO W-CURR-DATE                      
069000     .                                                                    
069100     EJECT                                                                
069200 B-CHECK-KEYS SECTION.                                                    
069300     MOVE 'B-CHECK-KEYS'    TO CURR-SECTION                               
069400                                                                          
069500     MOVE YES TO KEYS-SW                                                  
069600     MOVE YES TO INDATA-SW                                                
069700     MOVE NOO TO KAT9-SW                                                  
069800     MOVE REQU-IDDC-KEY  TO WS-IDDC                                       
069900                                                                          
070000     IF REQU-KDPGMACT = 'C' OR 'S' OR 'E'                                 
070100        CONTINUE                                                          
070200     ELSE                                                                 
070300       MOVE '023'              TO RESP-IDMSG-ERROR                        
070400*      WRONG ACTION KEY ***                                               
070500       MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                       
070600       MOVE NOO                TO KEYS-SW                                 
070700     END-IF                                                               
070800                                                                          
070900     IF KEYS-OK                                                           
071000        IF REQU-IDARTNR-KEY NOT NUMERIC                                   
071100          MOVE '024'              TO RESP-IDMSG-ERROR                     
071200*         NOT NUMERIC ***                                                 
071300          MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                    
071400          MOVE NOO                TO KEYS-SW                              
071500        ELSE                                                              
071600          MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                            
071700                                     W-IDARTNR-UTR                        
071800                                                                          
071900        END-IF                                                            
072000     END-IF                                                               
072100                                                                          
072200     IF KEYS-OK AND INDATA-OK                                             
072300     AND REQU-KDPGMACT = 'E' OR 'C'                                       
072400*      IF  REQU-KVJUSTKV-IN = ZERO                                        
072500*      AND REQU-FLSLACK NOT = YES                                         
072600*         MOVE '007'               TO RESP-IDMSG-ERROR                    
072700*         UPDATE NOT ALLOWED ***                                          
072800*         MOVE NOO                 TO INDATA-SW                           
072900*      END-IF                                                             
073000                                                                          
073100       IF  REQU-KVJUSTKV-IN NOT NUMERIC                                   
073200       AND REQU-FLSLACK NOT = YES                                         
073300          MOVE '024'               TO RESP-IDMSG-ERROR                    
073400*         NOT NUMERIC ***                                                 
073500          MOVE 'KVJUSTKV'          TO RESP-IDELMT-ERROR                   
073600*         MOVE NOO                 TO KEYS-SW                             
073700          MOVE NOO                 TO INDATA-SW                           
073800       ELSE                                                               
073900          IF REQU-FLSLACK = YES                                           
074000             IF  REQU-KVJUSTKV-IN NUMERIC                                 
074100             AND REQU-KVJUSTKV-IN > ZERO                                  
074200                MOVE '023'         TO RESP-IDMSG-ERROR                    
074300*               INVALID     ***                                           
074400                MOVE 'KVJUSTKV'    TO RESP-IDELMT-ERROR                   
074500*               MOVE NOO           TO KEYS-SW                             
074600                MOVE NOO           TO INDATA-SW                           
074700             ELSE                                                         
074800               IF REQU-KVJUSTKV-IN NOT NUMERIC                            
074900                 MOVE ZERO          TO REQU-KVJUSTKV-IN                   
075000               END-IF                                                     
075100             END-IF                                                       
075200          END-IF                                                          
075300          IF KEYS-OK AND INDATA-OK                                        
075400             PERFORM S11-KOLLA-EVALUERINGS-KATEGORI                       
075500             IF REQU-FLSLACK = YES                                        
075600               IF KAT9-YES                                                
075700                 MOVE '033'         TO RESP-IDMSG-ERROR                   
075800*                UNDEFINED ***                                            
075900                 MOVE 'DEL + INV.CAT'  TO RESP-IDELMT-ERROR               
076000*                MOVE NOO           TO KEYS-SW                            
076100                 MOVE NOO           TO INDATA-SW                          
076200               ELSE                                                       
076300                 MOVE ZERO          TO REQU-KVJUSTKV-IN                   
076400               END-IF                                                     
076500             END-IF                                                       
076600          END-IF                                                          
076700          IF KEYS-OK AND INDATA-OK                                        
076800             IF REQU-KDPGMACT  = 'C'                                      
076900                MOVE REQU-KVJUSTKV-IN TO RESP-KVJUSTKV-IN                 
077000             END-IF                                                       
077100          END-IF                                                          
077200       END-IF                                                             
077300                                                                          
077400       IF KEYS-OK AND INDATA-OK                                           
077500         IF REQU-FLANTAL NOT = ALL '+'                                    
077600          IF REQU-FLANTAL = '1'                                           
077700            CONTINUE                                                      
077800          ELSE                                                            
077900            MOVE '023'            TO RESP-IDMSG-ERROR                     
078000            MOVE 'FLANTAL'       TO RESP-IDELMT-ERROR                     
078100            MOVE NOO              TO INDATA-SW                            
078200          END-IF                                                          
078300         END-IF                                                           
078400       END-IF                                                             
078500                                                                          
078600       IF KEYS-OK AND INDATA-OK                                           
078700          IF REQU-KDAVVTYP = '+' OR '-'                                   
078800             IF REQU-KDAVVTYP = '+' AND NDC-BR AND                        
078801                REQU-FLFLYTTN = NOO                                       
078900               MOVE '023'            TO RESP-IDMSG-ERROR                  
079000*              INVALID     ***                                            
079100               MOVE 'KDAVVTYP'       TO RESP-IDELMT-ERROR                 
079200               MOVE NOO              TO INDATA-SW                         
079300             ELSE                                                         
079400               IF REQU-KDPGMACT  = 'C'                                    
079500                 MOVE REQU-KDAVVTYP TO RESP-KDAVVTYP                      
079600               END-IF                                                     
079700             END-IF                                                       
079800          ELSE                                                            
079900             MOVE '023'            TO RESP-IDMSG-ERROR                    
080000*            INVALID     ***                                              
080100             MOVE 'KDAVVTYP'       TO RESP-IDELMT-ERROR                   
080200*            MOVE NOO              TO KEYS-SW                             
080300             MOVE NOO              TO INDATA-SW                           
080400          END-IF                                                          
080500       END-IF                                                             
080600                                                                          
080700       IF KEYS-OK AND INDATA-OK                                           
080800          IF REQU-FLSLACK  = YES OR NOO                                   
080900             IF REQU-KDPGMACT  = 'C'                                      
081000               IF KAT9-YES AND REQU-FLSLACK  = YES                        
081100                 MOVE '033'         TO RESP-IDMSG-ERROR                   
081200*                UNDEFINED ***                                            
081300                 MOVE 'DEL + INV.CAT'  TO RESP-IDELMT-ERROR               
081400*                MOVE 'FLSLACK'     TO RESP-IDELMT-ERROR                  
081500*                MOVE NOO           TO KEYS-SW                            
081600                 MOVE NOO           TO INDATA-SW                          
081700               ELSE                                                       
081800                 MOVE REQU-FLSLACK TO RESP-FLSLACK                        
081900               END-IF                                                     
082000             END-IF                                                       
082100          ELSE                                                            
082200             MOVE '023'           TO RESP-IDMSG-ERROR                     
082300*            UNDEFINED   ***                                              
082400             MOVE 'FLSLACK'       TO RESP-IDELMT-ERROR                    
082500*            MOVE NOO             TO KEYS-SW                              
082600             MOVE NOO             TO INDATA-SW                            
082700          END-IF                                                          
082800       END-IF                                                             
082900                                                                          
083000       IF KEYS-OK AND INDATA-OK                                           
083100          IF REQU-FLFLYTTN = YES OR NOO                                   
083200             IF REQU-KDPGMACT  = 'C'                                      
083300                MOVE REQU-FLFLYTTN TO RESP-FLFLYTTN                       
083400             END-IF                                                       
083500          ELSE                                                            
083600             MOVE '023'            TO RESP-IDMSG-ERROR                    
083700*            UNDEFINED   ***                                              
083800             MOVE 'FLFLYTTN'       TO RESP-IDELMT-ERROR                   
083900*            MOVE NOO              TO KEYS-SW                             
084000             MOVE NOO              TO INDATA-SW                           
084100          END-IF                                                          
084200       END-IF                                                             
084300                                                                          
084400       IF KEYS-OK AND INDATA-OK                                           
084500          IF REQU-FLSLACK = YES                                           
084600             IF REQU-FLFLYTTN = YES                                       
084700                MOVE '023'            TO RESP-IDMSG-ERROR                 
084800*               INVALID     ***                                           
084900                MOVE 'FLFLYTTN'       TO RESP-IDELMT-ERROR                
085000*               MOVE NOO              TO KEYS-SW                          
085100                MOVE NOO              TO INDATA-SW                        
085200             END-IF                                                       
085300          END-IF                                                          
085400       END-IF                                                             
085500     END-IF                                                               
085600                                                                          
085700     IF KEYS-OK AND INDATA-OK                                             
085800        MOVE REQU-IDDC-KEY        TO RESP-IDDC-KEY                        
085900        MOVE REQU-IDARTNR-KEY     TO RESP-IDARTNR-KEY                     
086000     END-IF                                                               
086100     .                                                                    
086200     EJECT                                                                
086300 I-CHECK-INVENTORY        SECTION.                                        
086400     MOVE '  I-CHECK-INVENT     ' TO CURR-SECTION                         
086500     MOVE YES                TO KEYS-SW                                   
086600     MOVE YES                TO INDATA-SW                                 
086700     MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                                 
086800     PERFORM IMS-08-LAES-INV-ROT                                          
086900     IF SEGMENT-MISSING                                                   
087000       MOVE '041'            TO RESP-IDMSG-ERROR                          
087100       MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                         
087200       MOVE NOO              TO INDATA-SW                                 
087300       MOVE NOO              TO KEYS-SW                                   
087400     ELSE                                                                 
087500       MOVE REQU-IDDC-KEY    TO W-IDDC-WDH1-MIN                           
087600                                W-IDDC-WDH1-MAX                           
087700       PERFORM IMS-09-LAES-INV                                            
087800       IF SEGMENT-MISSING                                                 
087900         MOVE '007'          TO RESP-IDMSG-ERROR                          
088000*        MOVE 'SEGMENT'      TO RESP-IDELMT-ERROR                         
088100*        MOVE NOO            TO INDATA-SW                                 
088200*        MOVE NOO            TO KEYS-SW                                   
088300       END-IF                                                             
088400     END-IF                                                               
088500     PERFORM IMS-08-LAES-INV-ROT                                          
088600     .                                                                    
088700     EJECT                                                                
088800 J-CHECK-AVGCOST SECTION.                                                 
088900     MOVE '  J-CHECK-AVGCOST    ' TO CURR-SECTION                         
089000     MOVE YES                TO KEYS-SW                                   
089100     MOVE YES                TO INDATA-SW                                 
089200                                                                          
089300     MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                                 
089400     MOVE REQU-IDDC-KEY    TO W-IDDC                                      
089500     PERFORM IMS-03-GHU-WDK711                                            
089600     IF SEGMENT-MISSING                                                   
089700       MOVE '041'            TO RESP-IDMSG-ERROR                          
089800       MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                         
089900       MOVE NOO              TO INDATA-SW                                 
090000       MOVE NOO              TO KEYS-SW                                   
090100     ELSE                                                                 
090200       IF SLAG-PRAVCOST = +0                                              
090300         MOVE '260'          TO RESP-IDMSG-ERROR                          
090400         MOVE 'PRAVCOST'     TO RESP-IDELMT-ERROR                         
090500         MOVE NOO            TO INDATA-SW                                 
090600         MOVE NOO            TO KEYS-SW                                   
090700       END-IF                                                             
090800     END-IF                                                               
090900     .                                                                    
091000     EJECT                                                                
091100 C-UPDATE-INVENTORY        SECTION.                                       
091200     MOVE 'C-UPDATE-INVENTORY' TO CURR-SECTION                            
091300                                                                          
091400     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
091500                              W-IDARTNR-UTR                               
091600     MOVE REQU-IDDC-KEY    TO W-IDDC                                      
091700                              W-IDDC-WDH1-MIN                             
091800                              W-IDDC-WDH1-MAX                             
091900                              W1-IDDC-WDH1                                
092000                              W2-IDDC-WDH1                                
092100                              W-IDDC-UTR                                  
092200                                                                          
092300     MOVE REQU-KVJUSTKV-IN   TO W-KVJUSTKV-IN-X                           
092400     INSPECT W-KVJUSTKV-IN-X    REPLACING LEADING SPACE BY ZERO           
092500     MOVE W-KVJUSTKV-IN-N    TO W-KVJUSTKV-COMP                           
092600                                                                          
092700     MOVE REQU-KVJUSTKV-IN TO WS-KVJUSTKV-X                               
092800     INSPECT WS-KVJUSTKV-X REPLACING LEADING SPACE BY ZERO                
092900     MOVE WS-KVJUSTKV-N    TO WS-KVJUSTKV-COMP                            
093000                                                                          
093100     PERFORM IMS-01-READ-PART-6                                           
093200                                                                          
093300**1***  FLSLACK = YES,  DVS SLÄCKNING, BORTTAG                            
093400     IF REQU-FLSLACK = YES                                                
093500      IF W-IDDC = WC-CDC-SE                                               
093600        PERFORM IMS-02-READ-ECONOMY-SEG                                   
093700        MOVE +0                TO CLAG-KVUTRS                             
093800        PERFORM IMS-24-REPL-CLAGER-SEGM                                   
093900      ELSE                                                                
094000        PERFORM IMS-03-GHU-WDK711                                         
094100        MOVE +0                TO SLAG-KVUTRS                             
094200        PERFORM IMS-12-REPL-SLAGER-SEGM                                   
094300      END-IF                                                              
094400      PERFORM CA-RADERA-G2-UTREDNSALDO                                    
094500     ELSE                                                                 
094600                                                                          
094700**2***  FLFLYTTN = YES,  DVS UTREDNINGSSALDO-UPPD.                        
094800       IF REQU-FLFLYTTN = YES                                             
094900        IF W-IDDC = WC-CDC-SE                                             
095000          PERFORM IMS-02-READ-ECONOMY-SEG                                 
095100          IF W-KVJUSTKV-COMP NOT NUMERIC                                  
095200            MOVE ZERO TO W-KVJUSTKV-COMP                                  
095300          END-IF                                                          
095400          IF REQU-KDAVVTYP = '+'                                          
095500             COMPUTE W-SUM = CLAG-KVUTRS + W-KVJUSTKV-COMP                
095600          ELSE                                                            
095700             COMPUTE W-SUM = CLAG-KVUTRS - W-KVJUSTKV-COMP                
095800          END-IF                                                          
095900                                                                          
096000          MOVE W-SUM    TO CLAG-KVUTRS                                    
096100          PERFORM IMS-24-REPL-CLAGER-SEGM                                 
096200                                                                          
096300          IF CLAG-KVUTRS = +0                                             
096400             PERFORM CA-RADERA-G2-UTREDNSALDO                             
096500          END-IF                                                          
096600        ELSE                                                              
096700          PERFORM IMS-03-GHU-WDK711                                       
096800                                                                          
096900          IF W-KVJUSTKV-COMP NOT NUMERIC                                  
097000            MOVE ZERO TO W-KVJUSTKV-COMP                                  
097100          END-IF                                                          
097200          IF REQU-KDAVVTYP = '+'                                          
097300             COMPUTE W-SUM = SLAG-KVUTRS + W-KVJUSTKV-COMP                
097400          ELSE                                                            
097500             COMPUTE W-SUM = SLAG-KVUTRS - W-KVJUSTKV-COMP                
097600          END-IF                                                          
097700                                                                          
097800          MOVE W-SUM    TO SLAG-KVUTRS                                    
097900          PERFORM IMS-12-REPL-SLAGER-SEGM                                 
098000                                                                          
098100          IF SLAG-KVUTRS = +0                                             
098200             PERFORM CA-RADERA-G2-UTREDNSALDO                             
098300          END-IF                                                          
098400        END-IF                                                            
098500      ELSE                                                                
098600**3***  FLANTAL = '1'  DVS TYP6 JUSTERING,                                
098700*       ENDAST TILLÅTEN FÖR CDC                                           
098800       IF REQU-FLANTAL = '1'                                              
098900        IF W-IDDC = WC-CDC-SE                                             
099000         PERFORM IMS-02-READ-ECONOMY-SEG                                  
099100         IF REQU-KDAVVTYP = '+'                                           
099200           COMPUTE W-SUM = CLAG-KVLS + W-KVJUSTKV-COMP                    
099300         ELSE                                                             
099400           COMPUTE W-SUM = CLAG-KVLS - W-KVJUSTKV-COMP                    
099500         END-IF                                                           
099600         MOVE W-SUM     TO CLAG-KVLS                                      
099700         PERFORM IMS-24-REPL-CLAGER-SEGM                                  
099800         PERFORM CG-FLYTTA-LOGG-WDK6                                      
099900         PERFORM CD-UPPDATERA-LOGG                                        
100000         IF CLAG-KVUTRS = +0                                              
100100           PERFORM CA-RADERA-G2-UTREDNSALDO                               
100200         END-IF                                                           
100300         IF REQU-KDAVVTYP = '+'                                           
100400           PERFORM CB-TAECKNING-CDC                                       
100500         END-IF                                                           
100600         MOVE CLAG-PRARTSTD         TO INVH-PRARTSTD                      
100700         COMPUTE WS-KVANTAL =                                             
100800           CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                       
100900         PERFORM S10-DATE                                                 
101000         PERFORM CH-ISRT-INVENTERINGSHISTORIK                             
101100        END-IF                                                            
101200       ELSE                                                               
101300**4***  ÖVRIGT                                                            
101400         MOVE NOO                   TO PART-INV-EXISTS                    
101500         PERFORM IMS-08-LAES-INV-ROT                                      
101600         IF SEGMENT-FOUND                                                 
101700           MOVE REQU-IDDC-KEY       TO W1-IDDC-WDH1                       
101800                                       W2-IDDC-WDH1                       
101900                                       W-IDDC-UNIK                        
102000           MOVE 01                  TO W1-KDINVKAT                        
102100           MOVE 11                  TO W2-KDINVKAT                        
102200                                                                          
102300           MOVE NOO                 TO INV-CAT-EXISTS                     
102400           MOVE ' SKA LÄSA IMS-18 I C-SEC ' TO CURR-SECTION               
102500           PERFORM IMS-18-LAES-ARTIKEL-INV                                
102600           PERFORM UNTIL INVKOE-PART-EXISTS = NOO                         
102700             IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9            
102800               MOVE YES             TO INV-CAT-EXISTS                     
102900                                       PART-INV-EXISTS                    
103000             END-IF                                                       
103100             PERFORM IMS-18-LAES-ARTIKEL-INV                              
103200           END-PERFORM                                                    
103300                                                                          
103400           IF INV-CAT-EXISTS = YES                                        
103500             MOVE INVB-INV-KDINVKAT     TO KDINVKAT-WS                    
103600                                           W-KDINVKAT-UNIK                
103700             MOVE INVB-INV-TISEGKEY     TO WS-FIX-TISEGKEY                
103800                                           W-TISEGKEY-UNIK                
103900             MOVE INVB-INV-DAREGDAT-SORT    TO                            
104000                           W-DAREGDAT-SORT-UNIK                           
104100             MOVE INVB-INV-IDDC         TO W-IDDC-UNIK                    
104200             MOVE WS-TISEGKEY-3-8       TO RESP-TIM-INV                   
104300             MOVE INVB-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE                
104400             MOVE INVB-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1                
104500             MOVE INVB-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2                
104600             MOVE INVB-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3                
104700                                                                          
104800             PERFORM IMS-LAES-WDH111-UNIK                                 
104900             IF SEGMENT-FOUND                                             
105000               PERFORM IMS-LAES-WDH121                                    
105100               IF SEGMENT-FOUND                                           
105200                 PERFORM UNTIL SEGMENT-MISSING                            
105300                   MOVE 'LÄS WDH121 CAT-EXITS' TO CURR-SECTION            
105400                   IF INVB-INVL-KDSEGKEY = '0'                            
105500                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-CRE           
105600                   END-IF                                                 
105700                   IF INVB-INVL-KDSEGKEY = '1'                            
105800                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR1           
105900                   END-IF                                                 
106000                   IF INVB-INVL-KDSEGKEY = '2'                            
106100                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR2           
106200                   END-IF                                                 
106300                   IF INVB-INVL-KDSEGKEY = '3'                            
106400                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR3           
106500                   END-IF                                                 
106600                   PERFORM IMS-LAES-WDH121                                
106700                 END-PERFORM                                              
106800               END-IF                                                     
106900             END-IF                                                       
107000           ELSE                                                           
107100             PERFORM IMS-08-LAES-INV-ROT                                  
107200                                                                          
107300             MOVE REQU-IDDC-KEY       TO W-IDDC-WDH1-MIN                  
107400                                         W-IDDC-WDH1-MAX                  
107500                                         W-IDDC-UNIK                      
107600             MOVE +2                  TO W-KDINVKAT-MIN                   
107700                                         W-KDINVKAT-MAX                   
107800                                                                          
107900             PERFORM IMS-09-LAES-INV                                      
108000             IF SEGMENT-FOUND                                             
108100               MOVE INVB-INV-KDINVKAT     TO KDINVKAT-WS                  
108200                                             W-KDINVKAT-UNIK              
108300               MOVE INVB-INV-TISEGKEY     TO WS-FIX-TISEGKEY              
108400                                             W-TISEGKEY-UNIK              
108500               MOVE WS-TISEGKEY-3-8       TO RESP-TIM-INV                 
108600               MOVE INVB-INV-DAREGDAT-SORT TO                             
108700                                  W-DAREGDAT-SORT-UNIK                    
108800               MOVE INVB-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE              
108900               MOVE INVB-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1              
109000               MOVE INVB-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2              
109100               MOVE INVB-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3              
109200                                                                          
109300               PERFORM IMS-LAES-WDH111-UNIK                               
109400               IF SEGMENT-FOUND                                           
109500                 PERFORM IMS-LAES-WDH121                                  
109600                 IF SEGMENT-FOUND                                         
109700                   PERFORM UNTIL SEGMENT-MISSING                          
109800                   MOVE 'LÄS WDH121 PÅ 2A STÄLLET' TO CURR-SECTION        
109900                     IF INVB-INVL-IDUSER = '0'                            
110000                       MOVE INVB-INVL-IDUSER     TO WS-IDUSER-CRE         
110100                     END-IF                                               
110200                     IF INVB-INVL-IDUSER = '1'                            
110300                       MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR1         
110400                     END-IF                                               
110500                     IF INVB-INVL-IDUSER = '2'                            
110600                       MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR2         
110700                     END-IF                                               
110800                     IF INVB-INVL-IDUSER = '3'                            
110900                       MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR3         
111000                     END-IF                                               
111100                                                                          
111200                   PERFORM IMS-LAES-WDH121                                
111300                   END-PERFORM                                            
111400                 END-IF                                                   
111500               END-IF                                                     
111600               MOVE YES                   TO PART-INV-EXISTS              
111700             END-IF                                                       
111800           END-IF                                                         
111900         END-IF                                                           
112000                                                                          
112100         IF PART-INV-EXISTS = YES OR DCS-CDC                              
112200          IF W-IDDC = WC-CDC-SE                                           
112300            PERFORM IMS-02-READ-ECONOMY-SEG                               
112400            MOVE CLAG-KVLS TO WS-CLAG-KVLS                                
112500            MOVE +0 TO CLAG-KVUTRS                                        
112600            IF W-KVJUSTKV-COMP NOT NUMERIC                                
112700              MOVE ZERO TO W-KVJUSTKV-COMP                                
112800            END-IF                                                        
112900            IF REQU-KDAVVTYP = '+'                                        
113000               COMPUTE W-SUM = CLAG-KVLS + W-KVJUSTKV-COMP                
113100            ELSE                                                          
113200               COMPUTE W-SUM = CLAG-KVLS - W-KVJUSTKV-COMP                
113300            END-IF                                                        
113400            MOVE W-SUM             TO CLAG-KVLS                           
113500            PERFORM CA-RADERA-G2-UTREDNSALDO                              
113600                                                                          
113700            IF REQU-KDAVVTYP = '-'                                        
113800               COMPUTE CLAG-KVINVS = W-KVJUSTKV-COMP * -1                 
113900            ELSE                                                          
114000               MOVE W-KVJUSTKV-COMP TO CLAG-KVINVS                        
114100            END-IF                                                        
114200                                                                          
114300            PERFORM S10-DATE                                              
114400            MOVE DAT-TIAAVVD       TO CLAG-TIINVDAT                       
114500            PERFORM IMS-24-REPL-CLAGER-SEGM                               
114600            PERFORM CG-FLYTTA-LOGG-WDK6                                   
114700            PERFORM IMS-GHU-WDK629                                        
114800            IF SEGMENT-FOUND                                              
114900              IF CREF-FLREFNYO = NOO                                      
115000                CONTINUE                                                  
115100              ELSE                                                        
115200                MOVE NOO           TO CREF-FLREFNYO                       
115300                PERFORM IMS-REPL-WDK629                                   
115400              END-IF                                                      
115500            END-IF                                                        
115600            COMPUTE WS-ANTAL =                                            
115700             CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                     
115800          ELSE                                                            
115900           PERFORM IMS-03-GHU-WDK711                                      
116000           MOVE SLAG-KVLS TO WS-SLAG-KVLS                                 
116100           MOVE +0 TO SLAG-KVUTRS                                         
116200                                                                          
116300           IF W-KVJUSTKV-COMP NOT NUMERIC                                 
116400             MOVE ZERO TO W-KVJUSTKV-COMP                                 
116500           END-IF                                                         
116600           IF REQU-KDAVVTYP = '+'                                         
116700              COMPUTE W-SUM = SLAG-KVLS + W-KVJUSTKV-COMP                 
116800           ELSE                                                           
116900              COMPUTE W-SUM = SLAG-KVLS - W-KVJUSTKV-COMP                 
117000           END-IF                                                         
117100                                                                          
117200           MOVE W-SUM             TO SLAG-KVLS                            
117300           PERFORM CA-RADERA-G2-UTREDNSALDO                               
117400                                                                          
117500           IF REQU-KDAVVTYP = '-'                                         
117600              COMPUTE SLAG-KVINVS = W-KVJUSTKV-COMP * -1                  
117700           ELSE                                                           
117800              MOVE W-KVJUSTKV-COMP TO SLAG-KVINVS                         
117900           END-IF                                                         
118000                                                                          
118100           PERFORM S10-DATE                                               
118200           MOVE DAT-TIAAVVD       TO SLAG-TIINVDAT                        
118300           PERFORM IMS-12-REPL-SLAGER-SEGM                                
118400           PERFORM CC-FLYTTA-LOGG-WDK7                                    
118500                                                                          
118600**** IDTRACK CHANGES BEGIN ****                                           
118700           IF REQU-KDAVVTYP = '+'                                         
118800              PERFORM CE-VALIDATE-IDTRACK                                 
118900           ELSE                                                           
119000              PERFORM CF-VALIDATE-IDTRACK                                 
119100           END-IF                                                         
119200                                                                          
119300**** IDTRACK CHANGES ENDS  ****                                           
119400          END-IF                                                          
119500           PERFORM IMS-04-GHU-INVHIST-ROT                                 
119600           IF SEGMENT-MISSING                                             
119700             MOVE W-IDARTNR TO INVA-IDARTNR                               
119800             PERFORM IMS-20-ISRT-INVHIST-ROT                              
119900           END-IF                                                         
120000           MOVE ZERO                TO INVH-PRARTSTD                      
120100           MOVE CURR-DATE           TO INVH-DAREGDAT-CLO                  
120200           MOVE REQU-IDDC-KEY       TO INVH-IDDC                          
120300           MOVE REQU-IDUSER         TO INVH-IDUSER-CLO                    
120400                                                                          
120500           IF REQU-KDAVVTYP = '-'                                         
120600             COMPUTE INVH-KVJUSTKV = W-KVJUSTKV-COMP * -1                 
120700           ELSE                                                           
120800             MOVE W-KVJUSTKV-COMP TO INVH-KVJUSTKV                        
120900           END-IF                                                         
121000                                                                          
121100           EVALUATE TRUE                                                  
121200             WHEN PART-INV-EXISTS = NOO AND DCS-CDC                       
121300                  MOVE +7 TO INVH-KDJUSTYP                                
121400                  MOVE '7' TO LOGG-UREF1                                  
121500             WHEN KDINVKAT-WS = +1                                        
121600                  MOVE +1 TO INVH-KDJUSTYP                                
121700                  MOVE '1' TO LOGG-UREF1                                  
121800             WHEN KDINVKAT-WS = +2                                        
121900                  MOVE +2 TO INVH-KDJUSTYP                                
122000                  MOVE '2' TO LOGG-UREF1                                  
122100             WHEN KDINVKAT-WS = +3                                        
122200                  MOVE +3 TO INVH-KDJUSTYP                                
122300                  MOVE '3' TO LOGG-UREF1                                  
122400             WHEN KDINVKAT-WS = +4                                        
122500                  MOVE +4 TO INVH-KDJUSTYP                                
122600                  MOVE '4' TO LOGG-UREF1                                  
122700             WHEN KDINVKAT-WS = +5                                        
122800                  MOVE +5 TO INVH-KDJUSTYP                                
122900                  MOVE '5' TO LOGG-UREF1                                  
123000             WHEN KDINVKAT-WS = +8                                        
123100                  MOVE +8 TO INVH-KDJUSTYP                                
123200                  MOVE '8' TO LOGG-UREF1                                  
123300             WHEN KDINVKAT-WS = +9                                        
123400                  MOVE +9 TO INVH-KDJUSTYP                                
123500                  MOVE '9' TO LOGG-UREF1                                  
123600             WHEN OTHER                                                   
123700                  MOVE 'FELAKTIG KATEGORI = KDINVKAT-WS'                  
123800                          TO ERROR-TEXT                                   
123900                  CALL FELLOG                                             
124000           END-EVALUATE                                                   
124100                                                                          
124200           MOVE NOO                 TO INVH-FLAUTLSJ                      
124300           MOVE SPACE               TO INVH-IDPW                          
124400           IF WS-DAREGDAT-CRE = ZERO                                      
124500             MOVE DAGENS-DATUM      TO INVH-DAREGDAT-CRE                  
124600           ELSE                                                           
124700             MOVE WS-DAREGDAT-CRE   TO INVH-DAREGDAT-CRE                  
124800           END-IF                                                         
124900           MOVE WS-DAREGDAT-PR1     TO INVH-DAREGDAT-PR1                  
125000           MOVE WS-DAREGDAT-PR2     TO INVH-DAREGDAT-PR2                  
125100           MOVE WS-DAREGDAT-PR3     TO INVH-DAREGDAT-PR3                  
125200           IF WS-IDUSER-CRE = SPACE                                       
125300             MOVE REQU-IDUSER       TO INVH-IDUSER-CRE                    
125400           ELSE                                                           
125500             MOVE WS-IDUSER-CRE     TO INVH-IDUSER-CRE                    
125600           END-IF                                                         
125700           MOVE WS-IDUSER-PR1       TO INVH-IDUSER-PR1                    
125800           MOVE WS-IDUSER-PR2       TO INVH-IDUSER-PR2                    
125900           MOVE WS-IDUSER-PR3       TO INVH-IDUSER-PR3                    
126000           IF W-IDDC = WC-CDC-SE                                          
126100             MOVE WS-ANTAL          TO INVH-KVANTAL                       
126200           ELSE                                                           
126300             MOVE +0                TO INVH-KVANTAL                       
126400           END-IF                                                         
126500           PERFORM CD-UPPDATERA-LOGG                                      
126600           PERFORM S04-SKAPA-TISEGKEY                                     
126700                                                                          
126800           PERFORM IMS-22-ISRT-INVHIST-SEGM                               
126900           PERFORM UNTIL NOT SEGMENT-EXISTS                               
127000             IF SEGMENT-EXISTS                                            
127100               SUBTRACT 1 FROM INVH-TISEGKEY                              
127200               PERFORM IMS-22-ISRT-INVHIST-SEGM                           
127300             END-IF                                                       
127400           END-PERFORM                                                    
127500         ELSE                                                             
127600           MOVE '007'              TO RESP-IDMSG-ERROR                    
127700*         UPDATE NOT ALLOWED ***                                          
127800           MOVE NOO                TO INDATA-SW                           
127900         END-IF                                                           
128000**4SLUT***                                                                
128100       END-IF                                                             
128200**3SLUT***                                                                
128300       END-IF                                                             
128400**2SLUT***                                                                
128500     END-IF                                                               
128600                                                                          
128700     IF REQU-FLSLACK = YES                                                
128800      IF W-IDDC = WC-CDC-SE                                               
128900        MOVE CLAG-KVUTRS    TO RESP-KVUTRS                                
129000      ELSE                                                                
129100        MOVE SLAG-KVUTRS    TO RESP-KVUTRS                                
129200      END-IF                                                              
129300     END-IF                                                               
129400     .                                                                    
129500                                                                          
129600 CA-RADERA-G2-UTREDNSALDO SECTION.                                        
129700     MOVE 'CA-RADERA-G2      ' TO CURR-SECTION                            
129800                                                                          
129900     PERFORM IMS-13-GHU-G2-UTREDNSALDO                                    
130000     IF SEGMENT-FOUND                                                     
130100       PERFORM IMS-14-DELETE-G2-UTREDNSALDO                               
130200     END-IF                                                               
130300     .                                                                    
130400 CB-TAECKNING-CDC SECTION.                                                
130500     MOVE 'CA-TAECKNING  ' TO CURR-SECTION                                
130600     MOVE W-IDARTNR    TO 4506-IDARTNR                                    
130700     MOVE 11           TO 4506-KDTAKORS                                   
130800     MOVE ZERO         TO 4506-KVANTMOT                                   
130900     MOVE REQU-IDDC-KEY TO W-IDDC-4505                                    
131000     PERFORM IMS-ISRT-450511                                              
131100     .                                                                    
131200     EJECT                                                                
131300                                                                          
131400 CC-FLYTTA-LOGG-WDK7 SECTION.                                             
131500     MOVE 'CC-FLYTTA-LOGG    ' TO CURR-SECTION                            
131600                                                                          
131700* LÄGGER UPP SALDOLOGG I WDL9                                             
131800     MOVE W-IDARTNR            TO LOGG-IDARTNR                            
131900     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-CURR-DATE                
132000     ACCEPT TRANS-TIME FROM TIME                                          
132100     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TIME                
132200     MOVE 9                    TO LOGG-IDSEKVNR                           
132300     MOVE W-IDDC               TO LOGG-IDDC                               
132400     MOVE 'MISC'               TO LOGG-IDHUVTYP                           
132500     MOVE 'ADJ'                TO LOGG-IDSUBTYP                           
132600     MOVE IDPGM                TO LOGG-IDPGM                              
132700     MOVE 'L171'               TO LOGG-IDTRANS                            
132800     MOVE REQU-IDUSER          TO LOGG-IDUSER                             
132900     MOVE SPACE                TO LOGG-REF                                
133000     IF REQU-KDAVVTYP = '-'                                               
133100        MOVE '-'               TO LOGG-IDTECKEN-KVLS                      
133200     ELSE                                                                 
133300        MOVE '+'               TO LOGG-IDTECKEN-KVLS                      
133400     END-IF                                                               
133500     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV                 
133600     MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                    
133700     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                     
133800     IF  REQU-KVJUSTKV-IN NOT NUMERIC                                     
133900       MOVE ZERO                 TO LOGG-KVART-SALDO                      
134000     ELSE                                                                 
134100       MOVE REQU-KVJUSTKV-IN     TO LOGG-KVART-SALDO                      
134200     END-IF                                                               
134300     MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                              
134400     MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                          
134500     MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                             
134600     MOVE SLAG-KVLS            TO LOGG-KVLS                               
134700     MOVE ZERO                 TO LOGG-DAREGDAT-LADD                      
134800* NOLLA UT FÄLT PÅ WL171 VID UPDATE OK:                                   
134900     MOVE SPACE                TO RESP-KDAVVTYP                           
135000     MOVE ZERO                 TO RESP-KVJUSTKV-IN                        
135100*                                                                         
135200     .                                                                    
135300 CG-FLYTTA-LOGG-WDK6 SECTION.                                             
135400     MOVE 'CG-FLYTTA-LOGG    ' TO CURR-SECTION                            
135500                                                                          
135600* LÄGGER UPP SALDOLOGG I WDL9                                             
135700     MOVE W-IDARTNR            TO LOGG-IDARTNR                            
135800     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-CURR-DATE                
135900     ACCEPT TRANS-TIME FROM TIME                                          
136000     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TIME                
136100     MOVE 9                    TO LOGG-IDSEKVNR                           
136200     MOVE W-IDDC               TO LOGG-IDDC                               
136300     MOVE 'MISC'               TO LOGG-IDHUVTYP                           
136400     MOVE 'ADJ'                TO LOGG-IDSUBTYP                           
136500     MOVE IDPGM                TO LOGG-IDPGM                              
136600     MOVE 'L171'               TO LOGG-IDTRANS                            
136700     MOVE REQU-IDUSER          TO LOGG-IDUSER                             
136800     MOVE SPACE                TO LOGG-REF                                
136900     IF REQU-KDAVVTYP = '-'                                               
137000        MOVE '-'               TO LOGG-IDTECKEN-KVLS                      
137100     ELSE                                                                 
137200        MOVE '+'               TO LOGG-IDTECKEN-KVLS                      
137300     END-IF                                                               
137400     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV                 
137500     MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                    
137600     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                     
137700     IF  REQU-KVJUSTKV-IN NOT NUMERIC                                     
137800       MOVE ZERO                 TO LOGG-KVART-SALDO                      
137900     ELSE                                                                 
138000       MOVE REQU-KVJUSTKV-IN     TO LOGG-KVART-SALDO                      
138100     END-IF                                                               
138200     MOVE CLAG-KVAKS-CDC       TO LOGG-KVAKS                              
138300     MOVE CLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                          
138400     MOVE CLAG-KVEFRS          TO LOGG-KVEFRS                             
138500     MOVE CLAG-KVLS            TO LOGG-KVLS                               
138600     MOVE ZERO                 TO LOGG-DAREGDAT-LADD                      
138700* NOLLA UT FÄLT PÅ WL171 VID UPDATE OK:                                   
138800     MOVE SPACE                TO RESP-KDAVVTYP                           
138900     MOVE ZERO                 TO RESP-KVJUSTKV-IN                        
139000*                                                                         
139100     .                                                                    
139200                                                                          
139300 CH-ISRT-INVENTERINGSHISTORIK SECTION.                                    
139400     PERFORM IMS-04-GHU-INVHIST-ROT                                       
139500     IF SEGMENT-MISSING                                                   
139600       MOVE W-IDARTNR TO INVA-IDARTNR                                     
139700       PERFORM IMS-20-ISRT-INVHIST-ROT                                    
139800     END-IF                                                               
139900                                                                          
140000     MOVE CURR-DATE           TO INVH-DAREGDAT-CLO                        
140100                                                                          
140200     PERFORM S04-SKAPA-TISEGKEY                                           
140300                                                                          
140400     IF REQU-KDAVVTYP = '-'                                               
140500       COMPUTE INVH-KVJUSTKV = W-KVJUSTKV-COMP * -1                       
140600     ELSE                                                                 
140700       MOVE W-KVJUSTKV-COMP TO INVH-KVJUSTKV                              
140800     END-IF                                                               
140900                                                                          
141000     MOVE REQU-IDDC-KEY       TO INVH-IDDC                                
141100     MOVE 6                   TO INVH-KDJUSTYP                            
141200     MOVE '6'                 TO LOGG-UREF1                               
141300     MOVE REQU-IDUSER         TO INVH-IDUSER-CLO                          
141400     MOVE NOO                 TO INVH-FLAUTLSJ                            
141500     MOVE SPACE               TO INVH-IDPW                                
141600     IF WS-DAREGDAT-CRE = ZERO                                            
141700       MOVE DAGENS-DATUM      TO INVH-DAREGDAT-CRE                        
141800     ELSE                                                                 
141900       MOVE WS-DAREGDAT-CRE   TO INVH-DAREGDAT-CRE                        
142000     END-IF                                                               
142100     MOVE WS-DAREGDAT-PR1     TO INVH-DAREGDAT-PR1                        
142200     MOVE WS-DAREGDAT-PR2     TO INVH-DAREGDAT-PR2                        
142300     MOVE WS-DAREGDAT-PR3     TO INVH-DAREGDAT-PR3                        
142400     MOVE WS-IDUSER-PR1       TO INVH-IDUSER-PR1                          
142500     MOVE WS-IDUSER-PR2       TO INVH-IDUSER-PR2                          
142600     MOVE WS-IDUSER-PR3       TO INVH-IDUSER-PR3                          
142700     IF WS-IDUSER-CRE = SPACE                                             
142800       MOVE REQU-IDUSER       TO INVH-IDUSER-CRE                          
142900     ELSE                                                                 
143000       MOVE WS-IDUSER-CRE     TO INVH-IDUSER-CRE                          
143100     END-IF                                                               
143200                                                                          
143300     IF W-IDDC = WC-CDC-SE                                                
143400       MOVE WS-KVANTAL        TO INVH-KVANTAL                             
143500     ELSE                                                                 
143600       MOVE +0                TO INVH-KVANTAL                             
143700     END-IF                                                               
143800                                                                          
143900     PERFORM IMS-22-ISRT-INVHIST-SEGM                                     
144000     PERFORM UNTIL NOT SEGMENT-EXISTS                                     
144100       IF SEGMENT-EXISTS                                                  
144200         SUBTRACT 1 FROM INVH-TISEGKEY                                    
144300         PERFORM IMS-22-ISRT-INVHIST-SEGM                                 
144400       END-IF                                                             
144500     END-PERFORM                                                          
144600     .                                                                    
144700     EJECT                                                                
144800 CD-UPPDATERA-LOGG SECTION.                                               
144900     MOVE 'CD-UPPDATERA-LOGG ' TO CURR-SECTION                            
145000                                                                          
145100     PERFORM IMS-21-ISRT-WDL901                                           
145200     IF SEGMENT-EXISTS                                                    
145300        PERFORM UNTIL NOT SEGMENT-EXISTS                                  
145400          ADD -1 TO LOGG-IDSEKVNR                                         
145500          PERFORM IMS-21-ISRT-WDL901                                      
145600        END-PERFORM                                                       
145700     END-IF                                                               
145800     .                                                                    
145900                                                                          
146000 CE-VALIDATE-IDTRACK SECTION.                                             
146100     SKIP2                                                                
146200                                                                          
146300     IF WS-FLTRACK = 'J' AND WS-FLLOCAL = 'J'                             
146400        MOVE REQU-IDARTNR-KEY     TO W-IDARTNR                            
146500        MOVE REQU-IDDC-KEY        TO W-IDDC                               
146600        MOVE REQU-KVJUSTKV-IN     TO WS-VALD-KVAVIS                       
146700                                                                          
146800        MOVE 'N'                 TO IDTRACK-QTY-SW                        
146900        PERFORM IMS-GU-WDK711                                             
147000        MOVE 9999999999999999    TO W-DAINLEV                             
147100        PERFORM IMS-GHNP-WDK728-LAST                                      
147200        PERFORM UNTIL SEGMENT-MISSING OR IDTRACK-QTY-DONE                 
147300          IF TRCK-KVTRACK-KVAR < TRCK-KVANTMOT                            
147400             COMPUTE WS-TEMP-KVAVIS =                                     
147500              TRCK-KVANTMOT - TRCK-KVTRACK-KVAR                           
147600**** HAVE MORE THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
147700              IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                         
147800                 MOVE 'J' TO IDTRACK-QTY-SW                               
147900                 ADD WS-VALD-KVAVIS TO TRCK-KVTRACK-KVAR                  
148000                 PERFORM IMS-REPL-WDK728                                  
148100                 MOVE WS-VALD-KVAVIS TO WS-TRCK-KVANTMOT                  
148200                 MOVE '+' TO WS-LOGT-SIGN                                 
148300                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
148400                 PERFORM S12-TRACKLOG-DATA                                
148500                 PERFORM S13-ISRT-TRACKLOG                                
148600              ELSE                                                        
148700**** HAVE LESS THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
148800                 ADD WS-TEMP-KVAVIS TO TRCK-KVTRACK-KVAR                  
148900                 COMPUTE WS-VALD-KVAVIS =                                 
149000                     WS-VALD-KVAVIS - WS-TEMP-KVAVIS                      
149100                 PERFORM IMS-REPL-WDK728                                  
149200                 MOVE WS-TEMP-KVAVIS TO WS-TRCK-KVANTMOT                  
149300                 MOVE '+' TO WS-LOGT-SIGN                                 
149400                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
149500                 PERFORM S12-TRACKLOG-DATA                                
149600                 PERFORM S13-ISRT-TRACKLOG                                
149700                 MOVE 'N' TO IDTRACK-QTY-SW                               
149800              END-IF                                                      
149900                                                                          
150000          END-IF                                                          
150100          IF IDTRACK-QTY-NOT-DONE                                         
150200             PERFORM IMS-GU-WDK711                                        
150300             MOVE  TRCK-DAINLEV TO W-DAINLEV                              
150400             PERFORM IMS-GHNP-WDK728-LAST                                 
150500          END-IF                                                          
150600        END-PERFORM                                                       
150700     END-IF                                                               
150800                                                                          
150900     .                                                                    
151000     EJECT                                                                
151100 CF-VALIDATE-IDTRACK SECTION.                                             
151200     SKIP2                                                                
151300                                                                          
151400     IF WS-FLTRACK = 'J' AND WS-FLLOCAL = 'J'                             
151500        MOVE REQU-IDARTNR-KEY     TO W-IDARTNR                            
151600        MOVE REQU-IDDC-KEY        TO W-IDDC                               
151700        IF WS-USE-TEMP = 'J'                                              
151800           MOVE WS-TEMP-KVAVIS    TO WS-VALD-KVAVIS                       
151900           MOVE ZERO              TO WS-TEMP-KVAVIS                       
152000        ELSE                                                              
152100           MOVE REQU-KVJUSTKV-IN  TO WS-VALD-KVAVIS                       
152200        END-IF                                                            
152300                                                                          
152400        MOVE 'N'                 TO IDTRACK-QTY-SW                        
152500        PERFORM IMS-GU-WDK711                                             
152600        PERFORM IMS-GHNP-WDK728                                           
152700        PERFORM UNTIL SEGMENT-MISSING OR IDTRACK-QTY-DONE                 
152800          IF TRCK-KVTRACK-KVAR > 0                                        
152900             COMPUTE WS-TEMP-KVAVIS =                                     
153000                     TRCK-KVTRACK-KVAR                                    
153100**** HAVE MORE THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
153200              IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                         
153300                 MOVE 'J' TO IDTRACK-QTY-SW                               
153400                 COMPUTE TRCK-KVTRACK-KVAR =                              
153500                 TRCK-KVTRACK-KVAR - WS-VALD-KVAVIS                       
153600                 PERFORM IMS-REPL-WDK728                                  
153700                 MOVE WS-VALD-KVAVIS TO WS-TRCK-KVANTMOT                  
153800                 MOVE '-' TO WS-LOGT-SIGN                                 
153900                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
154000                 PERFORM S12-TRACKLOG-DATA                                
154100                 PERFORM S13-ISRT-TRACKLOG                                
154200              ELSE                                                        
154300**** HAVE LESS THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
154400                 COMPUTE WS-VALD-KVAVIS =                                 
154500                     WS-VALD-KVAVIS - WS-TEMP-KVAVIS                      
154600                 MOVE ZERO TO TRCK-KVTRACK-KVAR                           
154700                 PERFORM IMS-REPL-WDK728                                  
154800                 MOVE WS-TEMP-KVAVIS TO WS-TRCK-KVANTMOT                  
154900                 MOVE '-' TO WS-LOGT-SIGN                                 
155000                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
155100                 PERFORM S12-TRACKLOG-DATA                                
155200                 PERFORM S13-ISRT-TRACKLOG                                
155300                 MOVE 'N' TO IDTRACK-QTY-SW                               
155400              END-IF                                                      
155500                                                                          
155600          END-IF                                                          
155700          IF IDTRACK-QTY-NOT-DONE                                         
155800             PERFORM IMS-GHNP-WDK728                                      
155900          END-IF                                                          
156000        END-PERFORM                                                       
156100     END-IF                                                               
156200                                                                          
156300     .                                                                    
156400     EJECT                                                                
156500 D-RENSA-WDH1   SECTION.                                                  
156600     MOVE 'D-RENSA-WDH1   ' TO CURR-SECTION                               
156700                                                                          
156800*** LÄS MED STÖRSTA INTERVALL PÅ KDINVKAT 1 - 11                          
156900*** DVS FLAGGA AV ALLA INVENTERINGSKATEGORIER FÖR DENNA ART/DC            
157000     MOVE +1            TO W1-KDINVKAT                                    
157100                           W-KDINVKAT-MIN                                 
157200     MOVE +12           TO W2-KDINVKAT                                    
157300                           W-KDINVKAT-MAX                                 
157400     MOVE REQU-IDDC-KEY TO W-IDDC-WDH1-MIN                                
157500                           W-IDDC-WDH1-MAX                                
157600                           W1-IDDC-WDH1                                   
157700                           W2-IDDC-WDH1                                   
157800                                                                          
157900     PERFORM IMS-08-LAES-INV-ROT                                          
158000                                                                          
158100     IF SEGMENT-FOUND                                                     
158200       PERFORM IMS-11-LAES-INV-SEG                                        
158300                                                                          
158400       PERFORM UNTIL SEGMENT-MISSING                                      
158500                                                                          
158600         IF INVB-INV-KDINVKAT = +1 OR +2 OR +3 OR +4 OR +5 OR +9          
158700                                                                          
158800           MOVE YES   TO INVB-INV-FLINVBEH                                
158900           IF INVB-INV-KDINVKAT = +2                                      
159000             IF REQU-KDAVVTYP = '-'                                       
159100               COMPUTE INVB-INV-KVJUSTKV = W-KVJUSTKV-IN-N * -1           
159200             ELSE                                                         
159300               MOVE W-KVJUSTKV-IN-N   TO INVB-INV-KVJUSTKV                
159400             END-IF                                                       
159500           END-IF                                                         
159600           PERFORM IMS-23-REPL-INV-SEG                                    
159700         END-IF                                                           
159800         PERFORM IMS-11-LAES-INV-SEG                                      
159900       END-PERFORM                                                        
160000     END-IF                                                               
160100     .                                                                    
160200                                                                          
160300 E-SKAPA-INV-SEGM SECTION.                                                
160400     MOVE 'E-SKAPA-INV-SEGM' TO CURR-SECTION                              
160500                                                                          
160600*    INVENTERINGSSEGMENT SKAPAS PÅ WDH1                                   
160700                                                                          
160800     PERFORM IMS-08-LAES-INV-ROT                                          
160900     IF SEGMENT-MISSING                                                   
161000       MOVE W-IDARTNR     TO INVB-ART-IDARTNR                             
161100       PERFORM IMS-29-ISRT-INV-ROT                                        
161200     END-IF                                                               
161300                                                                          
161400     MOVE SPACE           TO INVB-WDH111                                  
161500     MOVE NOO             TO INVB-INV-FLINVBEH                            
161600     MOVE NOO             TO INVB-INV-FLINVSKR                            
161700     MOVE NOO             TO INVB-INV-FLINV2B                             
161800     MOVE NOO             TO INVB-INV-FLINV3E                             
161900     MOVE NOO             TO INVB-INV-FLINV4N                             
162000                                                                          
162100     IF NDC-NA                                                            
162200       MOVE NOO           TO INVB-INV-FLINV2C                             
162300                             INVB-INV-FLINV2D                             
162400       MOVE YES           TO INVB-INV-FLINV4R                             
162500                             INVB-INV-FLINV4P                             
162600     ELSE                                                                 
162700       MOVE YES           TO INVB-INV-FLINV2C                             
162800                             INVB-INV-FLINV2D                             
162900       MOVE NOO           TO INVB-INV-FLINV4R                             
163000                             INVB-INV-FLINV4P                             
163100     END-IF                                                               
163200                                                                          
163300     MOVE NOO             TO INVB-INV-FLINV85                             
163400     MOVE SPACE           TO INVB-INV-FILLER1                             
163500                             INVB-INV-FILLER2                             
163600                                                                          
163700                                                                          
163800     PERFORM IMS-30-GET-ART-ROT                                           
163900     IF SEGMENT-FOUND                                                     
164000       MOVE ART-KDSORT TO WS-KDSORT                                       
164100       PERFORM IMS-17-GNP-EKONOMISEG                                      
164200       PERFORM EA-INVENTERING                                             
164300     END-IF                                                               
164400     .                                                                    
164500                                                                          
164600 EA-INVENTERING SECTION.                                                  
164700     MOVE 'EA-INVENTERING  ' TO CURR-SECTION                              
164800                                                                          
164900     MOVE W-IDDC            TO INVB-INV-IDDC                              
165000     IF REQU-KDAVVTYP = '-'                                               
165100       COMPUTE WS-KVJUSTKV-COMP = -1 * W-KVJUSTKV-IN-N                    
165200     ELSE                                                                 
165300       MOVE W-KVJUSTKV-IN-N TO WS-KVJUSTKV-COMP                           
165400     END-IF                                                               
165500     IF WS-KVJUSTKV-COMP NOT NUMERIC                                      
165600       MOVE ZERO TO WS-KVJUSTKV-COMP                                      
165700     END-IF                                                               
165800     MOVE WS-KVJUSTKV-COMP  TO INVB-INV-KVJUSTKV                          
165900     MOVE SPACE             TO INVB-INV-TEINVANM                          
166000     IF REQU-FLANTAL = '1'                                                
166100       MOVE +6              TO INVB-INV-KDINVKAT                          
166200       MOVE +6              TO INVB-INV-KDINVKAT-OLD                      
166300     ELSE                                                                 
166400       MOVE +12               TO INVB-INV-KDINVKAT                        
166500       MOVE INVH-KDJUSTYP TO INVB-INV-KDINVKAT-OLD                        
166600     END-IF                                                               
166700     MOVE YES               TO INVB-INV-FLINVBEH                          
166800     MOVE ART-IDFKNGRP      TO INVB-INV-IDFKNGRP                          
166900     MOVE ZERO              TO INVB-INV-KDINVPRIO                         
167000     MOVE ART-KDPRODSL      TO INVB-INV-KDPRODSL                          
167100     MOVE CLAG-KDVVKL       TO INVB-INV-KDVVKL                            
167200     MOVE CLAG-KDPSLLOC     TO INVB-INV-KDPSLLOC                          
167300                                                                          
167400     IF W-IDDC NOT = WC-CDC-SE                                            
167500      PERFORM IMS-31-GU-WDK711                                            
167600      IF SEGMENT-FOUND                                                    
167700        MOVE SLAG-ADLAGOMR  TO INVB-INV-ADLAGOMR                          
167800        MOVE SLAG-ADGANG    TO INVB-INV-ADGANG                            
167900        MOVE SLAG-ADPLATS   TO INVB-INV-ADPLATS                           
168000        MOVE SLAG-PRAVCOST  TO W-PRAVCOST                                 
168100      ELSE                                                                
168200        MOVE ZERO           TO INVB-INV-ADLAGOMR                          
168300        MOVE ZERO           TO INVB-INV-ADGANG                            
168400        MOVE ZERO           TO INVB-INV-ADPLATS                           
168500        MOVE ZERO           TO W-PRAVCOST                                 
168600      END-IF                                                              
168700     ELSE                                                                 
168800        MOVE CLAG-ADLAGOMR  TO INVB-INV-ADLAGOMR                          
168900        MOVE CLAG-ADGANG    TO INVB-INV-ADGANG                            
169000        MOVE CLAG-ADPLATS   TO INVB-INV-ADPLATS                           
169100        IF CLAG-KDERS = 11 OR 14 OR 17 OR 18 OR 19                        
169200          MOVE YES             TO INVB-INV-FLINV85                        
169300        END-IF                                                            
169400     END-IF                                                               
169500     MOVE 20                  TO WS-SEKEL                                 
169600     MOVE DAGENS-DAT-WDH111   TO WS-TIAAMMDD                              
169700     MOVE WS-SEKEL            TO WS-INV-SEKEL                             
169800     MOVE DAGENS-DAT-WDH111   TO WS-INV-AAMMDD                            
169900     MOVE WS-INV-DAREGDAT     TO INVB-INV-DAREGDAT-CRE                    
170000                                 INVB-INV-DAREGDAT                        
170100     MOVE 0                   TO WS-LOPNR                                 
170200**   COMPUTE INVB-INV-DAREGDAT-SORT =                                     
170300**      99999999 - WS-INV-DAREGDAT                                        
170400     MOVE 99999999            TO  INVB-INV-DAREGDAT-SORT                  
170500     COMPUTE WS-TISEGKEY = WS-TIAAAAMMDDL                                 
170600     MOVE WS-TISEGKEY         TO INVB-INV-TISEGKEY                        
170700     MOVE ZERO                TO INVB-INV-IDPRTOMG                        
170800                                 INVB-INV-IDLOPNR                         
170900                                 INVB-INV-KVAKS-OLD                       
171000                                 INVB-INV-KVEFRS-OLD                      
171100                                 INVB-INV-KVLS-OLD                        
171200                                 INVB-INV-DAREGDAT-PR1                    
171300                                 INVB-INV-DAREGDAT-PR2                    
171400                                 INVB-INV-DAREGDAT-PR3                    
171500                                                                          
171600                                                                          
171700     PERFORM IMS-32-ISRT-INV-SEG                                          
171800                                                                          
171900     PERFORM UNTIL SEGMENT-FOUND                                          
172000       IF SEGMENT-EXISTS OR INDEX-EXISTS                                  
172100        ADD 1                TO INVB-INV-TISEGKEY                         
172200        PERFORM IMS-32-ISRT-INV-SEG                                       
172300       END-IF                                                             
172400     END-PERFORM                                                          
172500                                                                          
172600*** INSERT PÅ WDH121 SEGMENTET ***                                        
172700     MOVE REQU-IDUSER TO INVB-INVL-IDUSER                                 
172800     MOVE '0'         TO INVB-INVL-KDSEGKEY                               
172900     PERFORM IMS-INSERT-WDH121                                            
173000                                                                          
173100     MOVE SPACE       TO INVB-INVL-IDUSER                                 
173200     MOVE '1'         TO INVB-INVL-KDSEGKEY                               
173300     PERFORM IMS-INSERT-WDH121                                            
173400                                                                          
173500     MOVE SPACE       TO INVB-INVL-IDUSER                                 
173600     MOVE '2'         TO INVB-INVL-KDSEGKEY                               
173700     PERFORM IMS-INSERT-WDH121                                            
173800                                                                          
173900     MOVE SPACE       TO INVB-INVL-IDUSER                                 
174000     MOVE '3'         TO INVB-INVL-KDSEGKEY                               
174100     PERFORM IMS-INSERT-WDH121                                            
174200                                                                          
174300**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
174400     IF W-KVJUSTKV-IN-N NOT = 0                                           
174500        MOVE REQU-IDDC-KEY  TO WS-IDDC                                    
174600        IF NDC-NA AND REQU-FLANTAL NOT = '1'                              
174700           PERFORM EAE-FLYTTA-WDR8                                        
174800           PERFORM EAF-UPPDATERA-WDR8                                     
174900        ELSE                                                              
175000          IF XDC-NON-VCC-OWNED OR LDC-CN                                  
175100             PERFORM EAC-FLYTTA-WDR8                                      
175200             PERFORM EAD-UPPDATERA-WDR8                                   
175300          ELSE                                                            
175400             PERFORM EAA-FLYTTA-WDR9                                      
175500             PERFORM EAB-UPPDATERA-WDR9                                   
175600          END-IF                                                          
175700        END-IF                                                            
175800     END-IF                                                               
175900     .                                                                    
176000     EJECT                                                                
176100 EAA-FLYTTA-WDR9 SECTION.                                                 
176200     MOVE 'EAA-FLYTTA-WDR9 ' TO CURR-SECTION                              
176300                                                                          
176400     MOVE IDPGM            TO FIL-IDPGM                                   
176500     MOVE W-CURR-DATE      TO FIL-DAREGDAT                                
176600     ACCEPT FIL-TIKLOCK    FROM TIME                                      
176700     MOVE 1                TO FIL-IDSEKVNR                                
176800     MOVE 'W510EKHA'       TO FIL-IDCPYTXT                                
176900     MOVE REQU-IDUSER      TO FIL-IDUSER                                  
177000     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
177100     MOVE '403'            TO EKH-KDEKHHT                                 
177200     MOVE '40'             TO WS-KDEKSHT-1                                
177300     MOVE INVH-KDJUSTYP TO WS-KDEKSHT-2                                   
177400     MOVE WS-EKH-KDEKSHT   TO EKH-KDEKSHT                                 
177500     MOVE 'DET'            TO EKH-KDEKNIVA                                
177600     MOVE W-IDDC           TO EKH-IDDC-SEND                               
177700                              EKH-IDDC-REC                                
177800     MOVE ZERO             TO EKH-IDDISTR                                 
177900     MOVE ZERO             TO EKH-IDKUNDNR                                
178000     MOVE W-IDARTNR        TO W-EKH-IDARTNR                               
178100                                                                          
178200     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
178300     MOVE W-EKH-IDARTNR    TO CIA-IDARTBET-IN                             
178400     CALL W009CIA USING       CIA-W009CIA                                 
178500     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
178600                                                                          
178700     MOVE W-CURR-DATE      TO EKH-DAVERDAT                                
178800     MOVE ART-KDPRODSL     TO EKH-KDPRODSL                                
178900     MOVE ZERO             TO EKH-KDPSLLOC                                
179000     MOVE SPACE            TO EKH-FLLSBOK                                 
179100     MOVE 'SEK'            TO EKH-KDVALISO                                
179200     MOVE 1.00             TO EKH-PRKURS                                  
179300     MOVE ZERO             TO EKH-PRARTNTO                                
179400     MOVE ZERO             TO EKH-PRARTSJK                                
179500     MOVE ZERO             TO EKH-PRHEMTAG                                
179600     MOVE CLAG-PRARTSTD    TO EKH-PRARTSTD                                
179700     MOVE ZERO             TO EKH-PRLANDCO                                
179800     MOVE ZERO             TO EKH-PRINK                                   
179900     MOVE ZERO             TO EKH-PRDIRLON                                
180000     MOVE ZERO             TO EKH-PRDMTRL                                 
180100     MOVE ZERO             TO EKH-PROVRPAL                                
180200     MOVE ZERO             TO EKH-SUBEL                                   
180300     IF WS-KVJUSTKV-COMP NOT NUMERIC                                      
180400       MOVE ZERO TO WS-KVJUSTKV-COMP                                      
180500     END-IF                                                               
180600     MOVE WS-KVJUSTKV-COMP TO EKH-KVANTAL                                 
180700     MOVE 'L171'           TO EKH-IDTRANS                                 
180800     MOVE ZERO             TO EKH-BEVAT                                   
180900                              EKH-IDANALYS                                
181000                              EKH-IDKONTO                                 
181100                              EKH-KDANMORS                                
181200                              EKH-KDFRAKT                                 
181300                              EKH-SUVAT                                   
181400     MOVE ZERO             TO EKH-DAAVIDAT                                
181500                              EKH-IDAVINR                                 
181600                              EKH-KDAVVTYP                                
181700                              EKH-KDRT                                    
181800                              EKH-KVANTMOT                                
181900                              EKH-KVAVIS                                  
182000     MOVE WS-KDSORT        TO EKH-KDSORT                                  
182100     MOVE NOO              TO EKH-FLDCET                                  
182200     MOVE 'SEPV'           TO EKH-KDTRADP                                 
182300     MOVE SPACE            TO EKH-IDLEVNR                                 
182400                              EKH-IDKST                                   
182500     MOVE SPACE            TO EKH-IDKUNDRF                                
182600     MOVE SPACE            TO EKH-IDFAKT-EXP                              
182700     .                                                                    
182800                                                                          
182900 EAB-UPPDATERA-WDR9 SECTION.                                              
183000     MOVE 'EAB-UPPDATERA-WDR9' TO CURR-SECTION                            
183100                                                                          
183200     PERFORM IMS-33-ISRT-WDR901                                           
183300     PERFORM UNTIL SEGMENT-FOUND                                          
183400      ADD +1 TO FIL-IDSEKVNR                                              
183500      PERFORM IMS-33-ISRT-WDR901                                          
183600     END-PERFORM                                                          
183700     .                                                                    
183800     EJECT                                                                
183900                                                                          
184000 EAC-FLYTTA-WDR8 SECTION.                                                 
184100     MOVE 'EAC-FLYTTA-WDR8 ' TO CURR-SECTION                              
184200                                                                          
184300     MOVE IDPGM            TO WDR8-FIL-IDPGM                              
184400     MOVE W-CURR-DATE      TO WDR8-FIL-TIREGDAT                           
184500     ACCEPT WDR8-FIL-TIKLOCK    FROM TIME                                 
184600     MOVE 1                TO WDR8-FIL-IDSEKVNR                           
184700*    MOVE REQU-IDUSER      TO WDR8-FIL-IDUSER                             
184800     MOVE W-IDARTNR        TO WDR8-EKH-IDARTNR                            
184900     MOVE '403'            TO WDR8-EKH-KDEKHHT                            
185000     MOVE '40'             TO WS-KDEKSHT-1                                
185100     MOVE INVH-KDJUSTYP TO WS-KDEKSHT-2                                   
185200     MOVE WS-EKH-KDEKSHT   TO WDR8-EKH-KDEKSHT                            
185300     MOVE 'DET'            TO WDR8-EKH-KDEKNIVA                           
185400     MOVE W-IDDC           TO WDR8-EKH-IDDC-SEND                          
185500                              WDR8-EKH-IDDC-REC                           
185600     MOVE ZERO             TO WDR8-EKH-IDDISTR                            
185700     MOVE ZERO             TO WDR8-EKH-IDKUNDNR                           
185800     MOVE W-IDARTNR        TO W-EKH-IDARTNR                               
185900                                                                          
186000     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
186100     MOVE W-EKH-IDARTNR    TO CIA-IDARTBET-IN                             
186200     CALL W009CIA USING       CIA-W009CIA                                 
186300     MOVE CIA-IDARTBET-UT TO WDR8-EKH-IDVERGL                             
186400                                                                          
186500     MOVE W-CURR-DATE      TO WDR8-EKH-DAVERDAT                           
186600     MOVE ART-KDPRODSL     TO WDR8-EKH-KDPRODSL                           
186700     MOVE ZERO             TO WDR8-EKH-KDPSLLOC                           
186800     MOVE SPACE            TO WDR8-EKH-FLLSBOK                            
186900     MOVE 1.00             TO WDR8-EKH-PRKURS                             
187000     MOVE ZERO             TO WDR8-EKH-PRARTNTO                           
187100     MOVE ZERO             TO WDR8-EKH-PRARTSJK                           
187200     MOVE ZERO             TO WDR8-EKH-PRHEMTAG                           
187300     MOVE W-PRAVCOST       TO WDR8-EKH-PRARTSTD                           
187400     MOVE ZERO             TO WDR8-EKH-PRLANDCO                           
187500     MOVE ZERO             TO WDR8-EKH-PRINK                              
187600     MOVE ZERO             TO WDR8-EKH-PRDIRLON                           
187700     MOVE ZERO             TO WDR8-EKH-PRDMTRL                            
187800     MOVE ZERO             TO WDR8-EKH-PROVRPAL                           
187900     MOVE ZERO             TO WDR8-EKH-SUBEL                              
188000     IF WS-KVJUSTKV-COMP NOT NUMERIC                                      
188100       MOVE ZERO TO WS-KVJUSTKV-COMP                                      
188200     END-IF                                                               
188300     MOVE WS-KVJUSTKV-COMP TO WDR8-EKH-KVANTAL                            
188400     MOVE 'L171'           TO WDR8-EKH-IDTRANS                            
188500     MOVE ZERO             TO WDR8-EKH-BEVAT                              
188600                              WDR8-EKH-IDANALYS                           
188700                              WDR8-EKH-IDKONTO                            
188800                              WDR8-EKH-KDANMORS                           
188900                              WDR8-EKH-KDFRAKT                            
189000                              WDR8-EKH-SUVAT                              
189100     MOVE ZERO             TO WDR8-EKH-DAAVIDAT                           
189200                              WDR8-EKH-IDAVINR                            
189300                              WDR8-EKH-KDAVVTYP                           
189400                              WDR8-EKH-KDRT                               
189500                              WDR8-EKH-KVANTMOT                           
189600                              WDR8-EKH-KVAVIS                             
189700     MOVE WS-KDSORT        TO WDR8-EKH-KDSORT                             
189800     MOVE NOO              TO WDR8-EKH-FLDCET                             
189900     MOVE SPACE            TO WDR8-EKH-IDLEVNR                            
190000                              WDR8-EKH-IDKST                              
190100     MOVE SPACE            TO WDR8-EKH-IDKUNDRF                           
190200     MOVE SPACE            TO WDR8-EKH-IDFAKT-EXP                         
190300     PERFORM IMS-GU-WDB601                                                
190400     MOVE DCS-KDVALISO     TO WDR8-EKH-KDVALISO                           
190500     MOVE DCS-KDTRADP      TO WDR8-EKH-KDTRADP                            
190600     IF NDC-CN                                                            
190700       MOVE 'W570'         TO WDR8-FIL-IDCPYTXT(1:4)                      
190800     ELSE                                                                 
190900       IF NDC-IN                                                          
191000         MOVE 'W515'       TO WDR8-FIL-IDCPYTXT(1:4)                      
191100       ELSE                                                               
191200         MOVE DCS-KDTRADP  TO WDR8-FIL-IDCPYTXT(1:4)                      
191300       END-IF                                                             
191400     END-IF                                                               
191500     MOVE 'EKHA'           TO WDR8-FIL-IDCPYTXT(5:4)                      
191600     .                                                                    
191700                                                                          
191800 EAD-UPPDATERA-WDR8 SECTION.                                              
191900     MOVE 'EAD-UPPDATERA-WDR8' TO CURR-SECTION                            
192000                                                                          
192100     PERFORM IMS-34-ISRT-WDR801                                           
192200     PERFORM UNTIL SEGMENT-FOUND                                          
192300      ADD +1 TO WDR8-FIL-IDSEKVNR                                         
192400      PERFORM IMS-34-ISRT-WDR801                                          
192500     END-PERFORM                                                          
192600     IF DCS-KDTRADP = 'BR12'                                              
192700      PERFORM S05-FIX-LOCAL-TIME                                          
192800      PERFORM S20-SEND-OPEN                                               
192900      MOVE SEND-IDCOM         TO WZ04-SEND-IDCOM                          
193000      PERFORM S21-SEND-PUT-PROP                                           
193100      MOVE ZERO TO NOTF-IDSEKVNR                                          
193200      MOVE WDR8-EKH-KDEKHHT   TO NOTF-KDEKHHT                             
193300      MOVE WDR8-EKH-KDEKSHT   TO NOTF-KDEKSHT                             
193400      MOVE WDR8-EKH-DAVERDAT  TO NOTF-DAVERDAT                            
193500      MOVE AKTUELL-TID-X(1:6) TO NOTF-TIREGTID                            
193600      MOVE WDR8-EKH-IDVERGL   TO NOTF-IDVERGL                             
193700      MOVE WDR8-EKH-IDDC-REC  TO NOTF-IDDC                                
193800      MOVE ZERO               TO NOTF-IDFAKT                              
193900                                 NOTF-IDKUNDNR                            
194000                                 NOTF-IDORDER                             
194100                                 NOTF-IDKOLLI                             
194200      MOVE WDR8-EKH-IDARTNR   TO W-IDARTNR-EDIT-X                         
194300      MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                        
194400                              TO NOTF-IDARTNR20                           
194500      MOVE WDR8-EKH-KVANTAL   TO NOTF-KVANTAL                             
194600      ADD +1 TO NOTF-IDSEKVNR                                             
194700      PERFORM S21-SEND-PUT                                                
194800      PERFORM S22-SEND-CLOSE                                              
194900     END-IF                                                               
195000     .                                                                    
195100                                                                          
195200 EAE-FLYTTA-WDR8 SECTION.                                                 
195300     MOVE 'EAE-FLYTTA-WDR8' TO CURR-SECTION                               
195400     MOVE IDPGM             TO WDR8-FIL-IDPGM                             
195500     MOVE W-CURR-DATE(2:7)  TO WDR8-FIL-TIREGDAT                          
195600     ACCEPT WDR8-FIL-TIKLOCK   FROM TIME                                  
195700     MOVE 1                 TO WDR8-FIL-IDSEKVNR                          
195800     MOVE 'W510A08 '        TO WDR8-FIL-IDCPYTXT                          
195900     MOVE 'A08'             TO A08-IDPTYP                                 
196000     MOVE 'M10'             TO A08-KDEKOHT                                
196100     IF NDC-NA                                                            
196200       IF NDC-US                                                          
196300         MOVE 53             TO A08-IDFTG                                 
196400       ELSE                                                               
196500         MOVE 54             TO A08-IDFTG                                 
196600       END-IF                                                             
196700     ELSE                                                                 
196800       CONTINUE                                                           
196900     END-IF                                                               
197000     MOVE W-IDDC            TO A08-IDDC-SEND                              
197100                               A08-IDDC-REC                               
197200     MOVE W-CURR-DATE       TO A08-DAJUSTDA                               
197300     MOVE W-IDARTNR         TO A08-IDARTNR                                
197400     MOVE ART-KDPRODSL      TO A08-KDPRODSL                               
197500     MOVE CLAG-KDPSLLOC     TO A08-KDPSLLOC                               
197600     MOVE WS-KVJUSTKV-COMP  TO A08-KVJUSTKV                               
197700     MOVE W-PRAVCOST        TO A08-PRAVCOST                               
197800     MOVE INVB-INV-KDINVKAT TO A08-KDINVKAT                               
197900     MOVE INVB-INV-TEINVANM TO A08-TEINVANM                               
198000     .                                                                    
198100                                                                          
198200 EAF-UPPDATERA-WDR8 SECTION.                                              
198300     MOVE 'EAF-UPPDATERA-WDR8' TO CURR-SECTION                            
198400     PERFORM IMS-34-ISRT-WDR801                                           
198500     PERFORM UNTIL SEGMENT-FOUND                                          
198600       ADD +1  TO WDR8-FIL-IDSEKVNR                                       
198700       PERFORM IMS-34-ISRT-WDR801                                         
198800     END-PERFORM                                                          
198900     .                                                                    
199000                                                                          
199100 F-SKAPA-UPPDATERA-WDK7-POST  SECTION.                                    
199200     MOVE 'F-SKAPA-UPPDATERA' TO CURR-SECTION                             
199300                                                                          
199400* -- UPPDATERING AV SLAG-FLREFNYO PGA LAGERSALDOFÖRÄNDRING                
199500     MOVE REQU-IDDC-KEY      TO W-IDDC                                    
199600     IF W-IDDC NOT = WC-CDC-SE                                            
199700        PERFORM IMS-03-GHU-WDK711                                         
199800        IF SEGMENT-EXISTS                                                 
199900           MOVE NOO         TO SLAG-FLREFNYO                              
200000           PERFORM IMS-12-REPL-SLAGER-SEGM                                
200100        END-IF                                                            
200200     END-IF                                                               
200300     .                                                                    
200400     EJECT                                                                
200500                                                                          
200600 G-READ-AND-SHOW-INVENTORY SECTION.                                       
200700     MOVE 'G-READ-AND-SHOW' TO CURR-SECTION                               
200800                                                                          
200900     IF REQU-KDPGMACT = 'S'                                               
201000       MOVE SPACE              TO RESP-KDAVVTYP                           
201100                                  RESP-FLSLACK                            
201200                                  RESP-FLFLYTTN                           
201300       MOVE ZERO               TO RESP-KVJUSTKV-IN                        
201400     END-IF                                                               
201500                                                                          
201600     MOVE NOO                  TO PART-DC-EXISTS                          
201700     MOVE REQU-IDARTNR-KEY     TO W-IDARTNR                               
201800     MOVE REQU-IDDC-KEY        TO W-IDDC                                  
201900                                  W-IDDC-WDD8                             
202000                                                                          
202100     PERFORM IMS-01-READ-PART-6                                           
202200                                                                          
202300     IF SEGMENT-FOUND  AND ART-KDERS-UTG = 0                              
202400        MOVE YES                TO PART-DC-EXISTS                         
202500        MOVE +1                 TO RESP-KVRADER                           
202600        PERFORM IMS-02-READ-ECONOMY-SEG                                   
202700        IF SEGMENT-FOUND                                                  
202800         IF W-IDDC NOT = WC-CDC-SE                                        
202900           PERFORM IMS-03-GHU-WDK711                                      
203000           IF SEGMENT-FOUND                                               
203100****** OBSERVERA DE SOM HÄR LIGGER MED CLAG FINNS EJ PÅ WDK7 ****         
203200****** BASEN SKA KONTROLLERAS                                             
203300              MOVE CLAG-KDERS          TO RESP-KDERS                      
203400              COMPUTE W-PART-KVROS-SDC-NDC = SLAG-KVROS-DAG +             
203500                                             SLAG-KVROS-BULK              
203600              MOVE W-PART-KVROS-SDC-NDC TO RESP-KVROS                     
203700                                                                          
203800              MOVE SLAG-KVAKS-SDC      TO RESP-KVAKS                      
203900              MOVE SLAG-KVLS           TO RESP-KVLS                       
204000              MOVE SLAG-KVEFRS         TO RESP-KVEFRS                     
204100              MOVE SLAG-KVUTRS         TO RESP-KVUTRS                     
204200           ELSE                                                           
204300              MOVE '025'               TO RESP-IDMSG-ERROR                
204400*             IDARTNR MISSING  ***                                        
204500              MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR               
204600              MOVE NOO                 TO KEYS-SW                         
204700              MOVE NOO                 TO PART-DC-EXISTS                  
204800           END-IF                                                         
204900         ELSE                                                             
205000              MOVE CLAG-KDERS          TO RESP-KDERS                      
205100              MOVE CLAG-KVROS          TO RESP-KVROS                      
205200              MOVE CLAG-KVAKS-CDC      TO RESP-KVAKS                      
205300              MOVE CLAG-KVLS           TO RESP-KVLS                       
205400              MOVE CLAG-KVEFRS         TO RESP-KVEFRS                     
205500              MOVE CLAG-KVUTRS         TO RESP-KVUTRS                     
205600         END-IF                                                           
205700        END-IF                                                            
205800                                                                          
205900        IF PART-DC-EXISTS = YES                                           
206000           PERFORM IMS-04-GHU-INVHIST-ROT                                 
206100           IF SEGMENT-FOUND                                               
206200              PERFORM IMS-05-GNP-INVHIST-WDH7                             
206300                                                                          
206400              MOVE 1 TO INDX                                              
206500              MOVE 0 TO WS-KVRADER                                        
206600              PERFORM UNTIL NOT (SEGMENT-FOUND AND INDX < 6)              
206700                 MOVE INVH-DAREGDAT-CLO(3:6) TO DAT-I-TIDATUM             
206800                 PERFORM S03-CONV-DATE                                    
206900                 IF DAT-KDSVAR-OK                                         
207000                    MOVE DAT-TIAAVVD  TO RESP-TIJUSTDA (INDX)             
207100                 ELSE                                                     
207200                    MOVE ZERO         TO RESP-TIJUSTDA (INDX)             
207300                 END-IF                                                   
207400                 MOVE INVH-KVJUSTKV TO      RESP-KVJUSTKV-UT(INDX)        
207500                 MOVE INVH-KDJUSTYP TO RESP-KDJUSTYP       (INDX)         
207600                 ADD 1 TO INDX                                            
207700                 ADD 1 TO WS-KVRADER                                      
207800                 PERFORM IMS-05-GNP-INVHIST-WDH7                          
207900              END-PERFORM                                                 
208000              SUBTRACT 1 FROM INDX                                        
208100              IF WS-KVRADER > 1                                           
208200                MOVE WS-KVRADER TO RESP-KVRADER                           
208300              END-IF                                                      
208400           END-IF                                                         
208500        END-IF                                                            
208600                                                                          
208700        MOVE +0 TO DELIVERY-DATE                                          
208800        MOVE +0 TO DELIVERY-NUMB                                          
208900                                                                          
209000        PERFORM IMS-06-GET-WDL601                                         
209100        IF SEGMENT-FOUND                                                  
209200           PERFORM UNTIL NOT                                              
209300                          (SEGMENT-FOUND AND DELIVERY-DATE = +0)          
209400              PERFORM IMS-07-GNP-WDL611                                   
209500              IF INLC-INL-IDDC = W-IDDC                                   
209600                 IF INLC-INL-IDPTYP = 'R32'                               
209700                    MOVE INLC-INL-TIINLINL TO DELIVERY-DATE               
209800                    MOVE INLC-INL-KVANTMOT TO DELIVERY-NUMB               
209900                                              RESP-KVAVIS                 
210000                 END-IF                                                   
210100              END-IF                                                      
210200           END-PERFORM                                                    
210300        END-IF                                                            
210400                                                                          
210500        IF DELIVERY-DATE NOT = +0                                         
210600           MOVE DELIVERY-DATE   TO DAT-I-TIDATUM                          
210700           PERFORM GA-CONV-DATE                                           
210800           IF DAT-KDSVAR-OK                                               
210900              MOVE DAT-TIAAVVD TO RESP-TIAAVVD                            
211000           ELSE                                                           
211100              MOVE ZERO        TO RESP-TIAAVVD                            
211200           END-IF                                                         
211300           MOVE DELIVERY-NUMB TO RESP-KVAVIS                              
211400        END-IF                                                            
211500                                                                          
211600        PERFORM IMS-08-LAES-INV-ROT                                       
211700                                                                          
211800        IF SEGMENT-FOUND                                                  
211900           MOVE REQU-IDDC-KEY TO W-IDDC-WDH1-MIN                          
212000                                 W-IDDC-WDH1-MAX                          
212100                                 W1-IDDC-WDH1                             
212200                                 W2-IDDC-WDH1                             
212300           MOVE +9  TO W-KDINVKAT-MIN                                     
212400                       W-KDINVKAT-MAX                                     
212500                                                                          
212600           PERFORM IMS-09-LAES-INV                                        
212700           IF SEGMENT-FOUND                                               
212800              MOVE INVB-INV-KDINVKAT  TO RESP-KDINVKAT                    
212900              MOVE INVB-INV-TISEGKEY  TO WS-FIX-TISEGKEY                  
213000              MOVE WS-TISEGKEY-3-8    TO RESP-TIM-INV                     
213100           ELSE                                                           
213200              PERFORM IMS-08-LAES-INV-ROT                                 
213300                                                                          
213400              MOVE +1           TO W1-KDINVKAT                            
213500              MOVE +12          TO W2-KDINVKAT                            
213600                                                                          
213700              PERFORM IMS-11-LAES-INV-SEG                                 
213800                                                                          
213900              MOVE NOO            TO INV-EXISTS                           
214000              PERFORM UNTIL SEGMENT-MISSING OR INV-EXISTS = YES           
214100                 IF (INVB-INV-KDINVKAT = 1 OR 2 OR 3 OR 4 OR 5)           
214200                   AND  INVB-INV-FLINVBEH = NOO                           
214300                    MOVE YES               TO INV-EXISTS                  
214400                    MOVE INVB-INV-KDINVKAT TO RESP-KDINVKAT               
214500                    MOVE INVB-INV-TISEGKEY TO WS-FIX-TISEGKEY             
214600                    MOVE WS-TISEGKEY-3-8   TO RESP-TIM-INV                
214700                 ELSE                                                     
214800                    PERFORM IMS-11-LAES-INV-SEG                           
214900                 END-IF                                                   
215000              END-PERFORM                                                 
215100           END-IF                                                         
215200        END-IF                                                            
215300        MOVE REQU-IDDC-KEY  TO WS-IDDC                                    
215400        MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                           
215500        IF DCS-UNICODE-IDSKYLT                                            
215600           MOVE 'UTF8'             TO TRAUTF8-KDCP                        
215700        ELSE                                                              
215800           MOVE '278 '             TO TRAUTF8-KDCP                        
215900        END-IF                                                            
216000        PERFORM IMS-10-GU-BEN                                             
216100        IF SEGMENT-FOUND                                                  
216200           MOVE BEN-TEXT-BEART     TO TRAUTF8-TECONV-FROM                 
216300        ELSE                                                              
216400           MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                        
216500           MOVE SPACE              TO TRAUTF8-TECONV-FROM                 
216600        END-IF                                                            
216700        IF TRAUTF8-TECONV-FROM = SPACES                                   
216800         MOVE 'GB'  TO W-IDSKYLT                                          
216900         MOVE '278' TO TRAUTF8-KDCP                                       
217000         PERFORM IMS-10-GU-BEN                                            
217100         MOVE BEN-TEXT-BEART    TO TRAUTF8-TECONV-FROM                    
217200        END-IF                                                            
217300* ----  STRIP SPACE OR CONVERT TO UNICODE                                 
217400        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
217500* ----  MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
217600        MOVE TRAUTF8-TECONV-TO TO RESP-BEART-ENG                          
217700     ELSE                                                                 
217800       MOVE '025'              TO RESP-IDMSG-ERROR                        
217900*      IDARTNR MISSING  ***                                               
218000       MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                       
218100       MOVE NOO                TO KEYS-SW                                 
218200     END-IF                                                               
218300     .                                                                    
218400                                                                          
218500 GA-CONV-DATE   SECTION.                                                  
218600     MOVE 'GA-CONV-DATE               '  TO CURR-SECTION                  
218700                                                                          
218800     MOVE DELIVERY-DATE   TO DAT-I-TIDATUM                                
218900     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
219000                                                                          
219100     CALL  WDATKONV   USING  DAT-KDDATFORM                                
219200                             DAT-I-TIDATUM                                
219300                             DAT-O-TIDATUM                                
219400                             DAT-KDSVAR                                   
219500     .                                                                    
219600 H-CALCULATE-INVENTORY     SECTION.                                       
219700     MOVE 'H-CALCULATE    ' TO CURR-SECTION                               
219800                                                                          
219900     MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                                 
220000     MOVE REQU-IDDC-KEY      TO W-IDDC                                    
220100                                W-IDDC-WDD8                               
220200                                                                          
220300     MOVE REQU-KVJUSTKV-IN TO WS-KVJUSTKV-X                               
220400     INSPECT WS-KVJUSTKV-X REPLACING LEADING SPACE BY ZERO                
220500     MOVE WS-KVJUSTKV-N    TO WS-KVJUSTKV-COMP                            
220600                                                                          
220700     IF REQU-KDAVVTYP = '-'                                               
220800       COMPUTE WS-KVJUSTKV-COMP = -1 * WS-KVJUSTKV-N                      
220900     END-IF                                                               
221000                                                                          
221100     IF REQU-FLSLACK = YES                                                
221200        PERFORM HA-SLAEK-INVENTERING                                      
221300     ELSE                                                                 
221400        IF REQU-FLFLYTTN = YES                                            
221500           PERFORM HB-FLYTTNING                                           
221600        ELSE                                                              
221700         IF REQU-FLANTAL = '1'                                            
221800           PERFORM HE-JUST-ENDAST-ANTAL                                   
221900         ELSE                                                             
222000           IF REQU-KDAVVTYP = '+' OR '-' OR ' '                           
222100            IF W-IDDC NOT = WC-CDC-SE                                     
222200              PERFORM HC-BEHANDLA-SDC-NDC                                 
222300            ELSE                                                          
222400              PERFORM HD-BEHANDLA-CDC                                     
222500            END-IF                                                        
222600           END-IF                                                         
222700         END-IF                                                           
222800        END-IF                                                            
222900     END-IF                                                               
223000     MOVE '276'           TO RESP-IDMSG-INFO                              
223100*    PRESS EXECUTE TO UPDATE ***                                          
223200     MOVE SPACES          TO RESP-IDMSG-ERROR                             
223300     .                                                                    
223400                                                                          
223500     EJECT                                                                
223600 HA-SLAEK-INVENTERING SECTION.                                            
223700     MOVE 'HA-SLAEK-INVENTERING'  TO CURR-SECTION                         
223800                                                                          
223900     PERFORM IMS-08-LAES-INV-ROT                                          
224000                                                                          
224100     IF SEGMENT-FOUND                                                     
224200       MOVE REQU-IDDC-KEY  TO W1-IDDC-WDH1                                
224300                              W2-IDDC-WDH1                                
224400       MOVE 01             TO W1-KDINVKAT                                 
224500       MOVE 11             TO W2-KDINVKAT                                 
224600                                                                          
224700       MOVE NOO                           TO INV-CAT-EXISTS               
224800       MOVE 'SKA LÄSA IMS-18 I HA-SEC ' TO CURR-SECTION                   
224900       PERFORM IMS-18-LAES-ARTIKEL-INV                                    
225000       PERFORM UNTIL INVKOE-PART-EXISTS = NOO                             
225100         IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                
225200           MOVE YES                       TO INV-CAT-EXISTS               
225300         END-IF                                                           
225400         PERFORM IMS-18-LAES-ARTIKEL-INV                                  
225500       END-PERFORM                                                        
225600                                                                          
225700       IF INV-CAT-EXISTS = YES                                            
225800         MOVE ZERO                        TO RESP-KVUTRS                  
225900       ELSE                                                               
226000                                                                          
226100         PERFORM IMS-08-LAES-INV-ROT                                      
226200                                                                          
226300         MOVE REQU-IDDC-KEY               TO W-IDDC-WDH1-MIN              
226400                                             W-IDDC-WDH1-MAX              
226500         MOVE +2                          TO W-KDINVKAT-MIN               
226600                                             W-KDINVKAT-MAX               
226700                                                                          
226800         PERFORM IMS-09-LAES-INV                                          
226900         IF SEGMENT-FOUND                                                 
227000           MOVE ZERO                      TO RESP-KVUTRS                  
227100         END-IF                                                           
227200       END-IF                                                             
227300     END-IF                                                               
227400     .                                                                    
227500                                                                          
227600 HB-FLYTTNING SECTION.                                                    
227700     MOVE 'HB-FLYTTNING  '  TO CURR-SECTION                               
227800                                                                          
227900     IF DCS-CDC                                                           
228000      PERFORM IMS-25-GHU-WDK611                                           
228100     ELSE                                                                 
228200      PERFORM IMS-03-GHU-WDK711                                           
228300     END-IF                                                               
228400                                                                          
228500     IF WS-KVJUSTKV-COMP NOT NUMERIC                                      
228600       MOVE ZERO TO WS-KVJUSTKV-COMP                                      
228700     END-IF                                                               
228800     IF REQU-KDAVVTYP = '+' OR '-'                                        
228900        IF DCS-CDC                                                        
229000         COMPUTE W-SUM = CLAG-KVUTRS + WS-KVJUSTKV-COMP                   
229100        ELSE                                                              
229200         COMPUTE W-SUM = SLAG-KVUTRS + WS-KVJUSTKV-COMP                   
229300        END-IF                                                            
229400     ELSE                                                                 
229500        IF REQU-KDAVVTYP = ' ' AND REQU-KVJUSTKV-IN = ZERO                
229600         IF DCS-CDC                                                       
229700          COMPUTE W-SUM = CLAG-KVUTRS + WS-KVJUSTKV-COMP                  
229800         ELSE                                                             
229900          COMPUTE W-SUM = SLAG-KVUTRS + WS-KVJUSTKV-COMP                  
230000         END-IF                                                           
230100        END-IF                                                            
230200     END-IF                                                               
230300                                                                          
230400     MOVE W-SUM                 TO RESP-KVUTRS                            
230500     .                                                                    
230600                                                                          
230700 HC-BEHANDLA-SDC-NDC SECTION.                                             
230800     MOVE 'HC-BEHANDLA-SDC-NDC '  TO CURR-SECTION                         
230900                                                                          
231000     PERFORM IMS-03-GHU-WDK711                                            
231100     IF WS-KVJUSTKV-COMP NOT NUMERIC                                      
231200       MOVE ZERO TO WS-KVJUSTKV-COMP                                      
231300     END-IF                                                               
231400     IF REQU-KDAVVTYP = '+' OR '-'                                        
231500*  TECKN REDAN ÅTGÄRDAT VID BERÄKNING AV WS-KVJUSTKV-COMP                 
231600       COMPUTE W-SUM = SLAG-KVLS + WS-KVJUSTKV-COMP                       
231700     ELSE                                                                 
231800       IF REQU-KDAVVTYP = ' ' AND WS-KVJUSTKV-COMP = ZERO                 
231900         COMPUTE W-SUM = SLAG-KVUTRS + WS-KVJUSTKV-COMP                   
232000       END-IF                                                             
232100     END-IF                                                               
232200                                                                          
232300     MOVE W-SUM                 TO RESP-KVLS                              
232400     MOVE +0                    TO RESP-KVUTRS                            
232500                                                                          
232600     PERFORM S09-FLYTTA-SKAPA-INVINFO                                     
232700     PERFORM S10-DATE                                                     
232800                                                                          
232900     PERFORM IMS-08-LAES-INV-ROT                                          
233000                                                                          
233100     MOVE NOO                     TO PART-INV-EXISTS                      
233200     IF SEGMENT-FOUND                                                     
233300       MOVE REQU-IDDC-KEY         TO W1-IDDC-WDH1                         
233400                                     W2-IDDC-WDH1                         
233500       MOVE 01                    TO W1-KDINVKAT                          
233600       MOVE 11                    TO W2-KDINVKAT                          
233700                                                                          
233800       MOVE NOO                   TO INV-CAT-EXISTS                       
233900       MOVE 'SKA LÄSA IMS-18 I HC- SEC'  TO CURR-SECTION                  
234000       PERFORM IMS-18-LAES-ARTIKEL-INV                                    
234100       PERFORM UNTIL INVKOE-PART-EXISTS = NOO                             
234200         IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                
234300           MOVE YES               TO INV-CAT-EXISTS                       
234400                                     PART-INV-EXISTS                      
234500         END-IF                                                           
234600         PERFORM IMS-18-LAES-ARTIKEL-INV                                  
234700       END-PERFORM                                                        
234800                                                                          
234900       IF INV-CAT-EXISTS = NOO                                            
235000         PERFORM IMS-08-LAES-INV-ROT                                      
235100                                                                          
235200         MOVE REQU-IDDC-KEY       TO W-IDDC-WDH1-MIN                      
235300                                     W-IDDC-WDH1-MAX                      
235400         MOVE +2                  TO W-KDINVKAT-MIN                       
235500                                     W-KDINVKAT-MAX                       
235600         PERFORM IMS-09-LAES-INV                                          
235700         IF SEGMENT-FOUND                                                 
235800           MOVE YES              TO PART-INV-EXISTS                       
235900           PERFORM S08-EVALUERA-INVENT-KATEGORI                           
236000         END-IF                                                           
236100       ELSE                                                               
236200         MOVE YES              TO PART-INV-EXISTS                         
236300         PERFORM S08-EVALUERA-INVENT-KATEGORI                             
236400       END-IF                                                             
236500     END-IF                                                               
236600                                                                          
236700*    PERFORM S08-EVALUERA-INVENT-KATEGORI                                 
236800     .                                                                    
236900 HD-BEHANDLA-CDC SECTION.                                                 
237000     MOVE 'HD-BEHANDLA-CDC '      TO CURR-SECTION                         
237100                                                                          
237200     PERFORM IMS-25-GHU-WDK611                                            
237300     IF WS-KVJUSTKV-COMP NOT NUMERIC                                      
237400       MOVE ZERO TO WS-KVJUSTKV-COMP                                      
237500     END-IF                                                               
237600     IF REQU-KDAVVTYP = '+' OR '-'                                        
237700*  TECKN REDAN ÅTGÄRDAT VID BERÄKNING AV WS-KVJUSTKV-COMP                 
237800       COMPUTE W-SUM = CLAG-KVLS + WS-KVJUSTKV-COMP                       
237900     ELSE                                                                 
238000       IF REQU-KDAVVTYP = ' ' AND WS-KVJUSTKV-COMP = ZERO                 
238100         COMPUTE W-SUM = CLAG-KVUTRS + WS-KVJUSTKV-COMP                   
238200       END-IF                                                             
238300     END-IF                                                               
238400                                                                          
238500     MOVE W-SUM                 TO RESP-KVLS                              
238600     MOVE +0                    TO RESP-KVUTRS                            
238700                                                                          
238800     PERFORM S09-FLYTTA-SKAPA-INVINFO                                     
238900     PERFORM S10-DATE                                                     
239000                                                                          
239100     PERFORM IMS-08-LAES-INV-ROT                                          
239200                                                                          
239300     MOVE NOO                     TO PART-INV-EXISTS                      
239400     IF SEGMENT-FOUND                                                     
239500       MOVE REQU-IDDC-KEY         TO W1-IDDC-WDH1                         
239600                                     W2-IDDC-WDH1                         
239700       MOVE 01                    TO W1-KDINVKAT                          
239800       MOVE 11                    TO W2-KDINVKAT                          
239900                                                                          
240000       MOVE NOO                   TO INV-CAT-EXISTS                       
240100       MOVE 'SKA LÄSA IMS-18 I HC- SEC'  TO CURR-SECTION                  
240200       PERFORM IMS-18-LAES-ARTIKEL-INV                                    
240300       PERFORM UNTIL INVKOE-PART-EXISTS = NOO                             
240400         IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                
240500           MOVE YES               TO INV-CAT-EXISTS                       
240600                                     PART-INV-EXISTS                      
240700         END-IF                                                           
240800         PERFORM IMS-18-LAES-ARTIKEL-INV                                  
240900       END-PERFORM                                                        
241000                                                                          
241100       IF INV-CAT-EXISTS = NOO                                            
241200         PERFORM IMS-08-LAES-INV-ROT                                      
241300                                                                          
241400         MOVE REQU-IDDC-KEY       TO W-IDDC-WDH1-MIN                      
241500                                     W-IDDC-WDH1-MAX                      
241600         MOVE +2                  TO W-KDINVKAT-MIN                       
241700                                     W-KDINVKAT-MAX                       
241800         PERFORM IMS-09-LAES-INV                                          
241900         IF SEGMENT-FOUND                                                 
242000           MOVE YES              TO PART-INV-EXISTS                       
242100           PERFORM S08-EVALUERA-INVENT-KATEGORI                           
242200         END-IF                                                           
242300       ELSE                                                               
242400         MOVE YES              TO PART-INV-EXISTS                         
242500         PERFORM S08-EVALUERA-INVENT-KATEGORI                             
242600       END-IF                                                             
242700     END-IF                                                               
242800                                                                          
242900*    PERFORM S08-EVALUERA-INVENT-KATEGORI                                 
243000     .                                                                    
243100 HE-JUST-ENDAST-ANTAL SECTION.                                            
243200     IF W-IDDC = WC-CDC-SE                                                
243300       PERFORM IMS-01-READ-PART-6                                         
243400       PERFORM IMS-02-READ-ECONOMY-SEG                                    
243500       IF REQU-KDAVVTYP = '+' OR '-'                                      
243600         IF REQU-KDAVVTYP = '+'                                           
243700           COMPUTE W-SUM = CLAG-KVLS + WS-KVJUSTKV-COMP                   
243800         ELSE                                                             
243900           COMPUTE W-SUM = CLAG-KVLS - WS-KVJUSTKV-COMP                   
244000         END-IF                                                           
244100       ELSE                                                               
244200         IF REQU-KDAVVTYP = ' ' AND REQU-KVJUSTKV-IN = ZERO               
244300           COMPUTE W-SUM = CLAG-KVUTRS + WS-KVJUSTKV-COMP                 
244400         END-IF                                                           
244500       END-IF                                                             
244600       MOVE W-SUM                TO RESP-KVLS                             
244700       PERFORM S09-FLYTTA-SKAPA-INVINFO                                   
244800       PERFORM S10-DATE                                                   
244900       MOVE 6                     TO RESP-KDJUSTYP (1)                    
245000     END-IF                                                               
245100     .                                                                    
245200     EJECT                                                                
245300                                                                          
245400                                                                          
245500 KA-VALIDATE-WDK728  SECTION.                                             
245600     MOVE ZERO                TO WS-VALD-KVAVIS                           
245700                                 WS-TEMP-KVAVIS                           
245800** IDTRACK-QTY-SW  IS USED TO CHECK IF THE IDTRACK                        
245900** HAVE ENOUGH QUANTITY FOR UPDATES!                                      
246000     MOVE 'N'                 TO IDTRACK-QTY-SW                           
246100     MOVE REQU-IDDC-KEY       TO W-IDDC                                   
246200     MOVE REQU-IDARTNR-KEY    TO W-IDARTNR                                
246300     MOVE REQU-KVJUSTKV-IN    TO WS-VALD-KVAVIS                           
246400     PERFORM IMS-GU-WDK711                                                
246500     MOVE 9999999999999999    TO W-DAINLEV                                
246600     PERFORM IMS-GHNP-WDK728-LAST                                         
246700     PERFORM UNTIL SEGMENT-MISSING OR IDTRACK-QTY-DONE                    
246800       IF TRCK-KVTRACK-KVAR < TRCK-KVANTMOT                               
246900          COMPUTE WS-TEMP-KVAVIS = WS-TEMP-KVAVIS +                       
247000          (TRCK-KVANTMOT - TRCK-KVTRACK-KVAR)                             
247100          IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                             
247200             MOVE 'J' TO IDTRACK-QTY-SW                                   
247300          END-IF                                                          
247400       END-IF                                                             
247500       IF IDTRACK-QTY-NOT-DONE                                            
247600          PERFORM IMS-GU-WDK711                                           
247700          MOVE  TRCK-DAINLEV TO W-DAINLEV                                 
247800          PERFORM IMS-GHNP-WDK728-LAST                                    
247900       END-IF                                                             
248000     END-PERFORM                                                          
248100     .                                                                    
248200     EJECT                                                                
248300 KB-VALIDATE-WDK728  SECTION.                                             
248400     MOVE ZERO                TO WS-VALD-KVAVIS                           
248500                                 WS-TEMP-KVAVIS                           
248600** IDTRACK-QTY-SW  IS USED TO CHECK IF THE IDTRACK                        
248700** HAVE ENOUGH QUANTITY FOR UPDATES!                                      
248800     MOVE 'N'                 TO IDTRACK-QTY-SW                           
248900     MOVE REQU-IDDC-KEY       TO W-IDDC                                   
249000     MOVE REQU-IDARTNR-KEY    TO W-IDARTNR                                
249100     MOVE REQU-KVJUSTKV-IN    TO WS-VALD-KVAVIS                           
249200     PERFORM IMS-GU-WDK711                                                
249300     MOVE 0000000000000000    TO W-DAINLEV                                
249400     PERFORM IMS-GHNP-WDK728                                              
249500     PERFORM UNTIL SEGMENT-MISSING OR IDTRACK-QTY-DONE                    
249600       IF TRCK-KVTRACK-KVAR > 0                                           
249700          COMPUTE WS-TEMP-KVAVIS = WS-TEMP-KVAVIS +                       
249800                  TRCK-KVTRACK-KVAR                                       
249900          IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                             
250000             MOVE 'J' TO IDTRACK-QTY-SW                                   
250100             MOVE 'N' TO WS-USE-TEMP                                      
250200          END-IF                                                          
250300       END-IF                                                             
250400       IF IDTRACK-QTY-NOT-DONE                                            
250500          PERFORM IMS-GHNP-WDK728                                         
250600       END-IF                                                             
250700     END-PERFORM                                                          
250800     .                                                                    
250900     EJECT                                                                
251000*    --- DISPATCHER SECTIONS                                              
251100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
251200     MOVE 'S01-FETCH-REQUEST ' TO CURR-SECTION                            
251300                                                                          
251400     MOVE 'GETARG'                     TO SUB-KDFUNC                      
251500     MOVE 'CARPARTS.LDC.INVADJUSTMENT' TO SUB-ADDISPABS                   
251600     MOVE LENGTH OF REQU-AREA          TO SUB-KVDLEN                      
251700                                                                          
251800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
251900                                                                          
252000     IF SUB-KDRC > 0                                                      
252100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
252200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
252300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
252400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
252500     END-IF                                                               
252600     .                                                                    
252700                                                                          
252800 S02-RETURN-RESPONSE SECTION.                                             
252900     MOVE 'S02-RETURN-RESP   ' TO CURR-SECTION                            
253000                                                                          
253100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
253200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
253300                                                                          
253400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
253500                                                                          
253600     IF SUB-KDRC > 0                                                      
253700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
253800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
253900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
254000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
254100     END-IF                                                               
254200     .                                                                    
254300                                                                          
254400 S03-CONV-DATE  SECTION.                                                  
254500     MOVE 'S03-CONV-DATE' TO CURR-SECTION                                 
254600                                                                          
254700     MOVE INVH-DAREGDAT-CLO(3:6) TO DAT-I-TIDATUM                         
254800     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
254900                                                                          
255000     CALL WDATKONV USING DAT-KDDATFORM                                    
255100                         DAT-I-TIDATUM                                    
255200                         DAT-O-TIDATUM                                    
255300                         DAT-KDSVAR                                       
255400     .                                                                    
255500                                                                          
255600 S04-SKAPA-TISEGKEY SECTION.                                              
255700     MOVE 'S04-SKAPA-TISEGKEY' TO CURR-SECTION                            
255800                                                                          
255900     MOVE 20          TO WS-SEKEL                                         
256000     MOVE 9           TO WS-LOPNR                                         
256100                                                                          
256200     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
256300                                                                          
256400     MOVE WS-TISEGKEY TO INVH-TISEGKEY                                    
256500     .                                                                    
256600 S05-FIX-LOCAL-TIME SECTION.                                              
256700                                                                          
256800******* ADAPT DATE AND TIME FOR TIMEZONES                                 
256900     PERFORM IMS-GU-WDB601                                                
257000                                                                          
257100     MOVE '011'                TO MSGI-KDCALL                             
257200     MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
257300     MOVE DCS-IDDC             TO MSGI-IDDC                               
257400     MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                           
257500     MOVE DAGENS-TID           TO MSGI-TILOKTID                           
257600     CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
257700       MOVE MSGI-TILOKDAT(1:6) TO W-DATUM-Y                               
257800       MOVE MSGI-TILOKTID(1:4) TO AKTUELL-TID-X(1:4)                      
257900     .                                                                    
258000     EJECT                                                                
258100                                                                          
258200 S08-EVALUERA-INVENT-KATEGORI SECTION.                                    
258300                                                                          
258400     IF PART-INV-EXISTS = NOO AND DCS-CDC                                 
258500       MOVE 7 TO RESP-KDJUSTYP (1)                                        
258600     ELSE                                                                 
258700                                                                          
258800       EVALUATE TRUE                                                      
258900                                                                          
259000       WHEN INVB-INV-KDINVKAT = 1                                         
259100         MOVE 1 TO RESP-KDJUSTYP (1)                                      
259200                                                                          
259300       WHEN INVB-INV-KDINVKAT = 2                                         
259400         MOVE 2 TO RESP-KDJUSTYP (1)                                      
259500                                                                          
259600       WHEN INVB-INV-KDINVKAT = 3                                         
259700         MOVE 3 TO RESP-KDJUSTYP (1)                                      
259800                                                                          
259900       WHEN INVB-INV-KDINVKAT = 4                                         
260000         MOVE 4 TO RESP-KDJUSTYP (1)                                      
260100                                                                          
260200       WHEN INVB-INV-KDINVKAT = 5                                         
260300         MOVE 5 TO RESP-KDJUSTYP (1)                                      
260400                                                                          
260500       WHEN INVB-INV-KDINVKAT = 8                                         
260600         MOVE 8 TO RESP-KDJUSTYP (1)                                      
260700                                                                          
260800       WHEN INVB-INV-KDINVKAT = 9                                         
260900         MOVE 9 TO RESP-KDJUSTYP (1)                                      
261000                                                                          
261100        WHEN OTHER                                                        
261200          MOVE 'FELAKTIG KATEGORI = KDINVKAT-WS'                          
261300                TO ERROR-TEXT                                             
261400                CALL FELLOG                                               
261500       END-EVALUATE                                                       
261600                                                                          
261700     END-IF                                                               
261800     .                                                                    
261900 S09-FLYTTA-SKAPA-INVINFO SECTION.                                        
262000     MOVE 'S09-FLYTTA-SKAPA-INVINFO   '  TO CURR-SECTION                  
262100                                                                          
262200     MOVE 5 TO INDX                                                       
262300     MOVE 6 TO INDY                                                       
262400                                                                          
262500     PERFORM UNTIL INDX = 0                                               
262600                                                                          
262700       IF NOT RESP-TIJUSTDA(INDX) = ALL '+'                               
262800          MOVE RESP-TIJUSTDA    (INDX) TO RESP-TIJUSTDA    (INDY)         
262900          MOVE RESP-KVJUSTKV-UT (INDX) TO RESP-KVJUSTKV-UT (INDY)         
263000          MOVE RESP-KDJUSTYP    (INDX) TO RESP-KDJUSTYP    (INDY)         
263100       END-IF                                                             
263200                                                                          
263300       SUBTRACT 1 FROM INDX INDY                                          
263400                                                                          
263500     END-PERFORM                                                          
263600                                                                          
263700     IF WS-KVJUSTKV-COMP NOT NUMERIC                                      
263800       MOVE ZERO TO WS-KVJUSTKV-COMP                                      
263900     END-IF                                                               
264000     MOVE WS-KVJUSTKV-COMP    TO RESP-KVJUSTKV-UT (1)                     
264100     IF WS-KVRADER < 6                                                    
264200        ADD 1 TO WS-KVRADER                                               
264300        MOVE WS-KVRADER TO RESP-KVRADER                                   
264400     END-IF                                                               
264500     .                                                                    
264600                                                                          
264700 S10-DATE  SECTION.                                                       
264800     MOVE 'S10-DATE          ' TO CURR-SECTION                            
264900                                                                          
265000     MOVE 'IDAG  '      TO DAT-KDDATFORM                                  
265100     CALL  WDATKONV  USING DAT-KDDATFORM                                  
265200                           DAT-I-TIDATUM                                  
265300                           DAT-O-TIDATUM                                  
265400                           DAT-KDSVAR                                     
265500                                                                          
265600     IF DAT-KDSVAR-OK                                                     
265700         CONTINUE                                                         
265800     ELSE                                                                 
265900         CALL  FELLOG                                                     
266000     END-IF                                                               
266100                                                                          
266200     MOVE DAT-TIAAVVD  TO RESP-TIJUSTDA (1)                               
266300     MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                                     
266400                          CURR-DATE(3:6)                                  
266500     MOVE DAT-TISEKEL  TO CURR-DATE(1:2)                                  
266600     .                                                                    
266700                                                                          
266800 S11-KOLLA-EVALUERINGS-KATEGORI SECTION.                                  
266900                                                                          
267000     MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                                 
267100     PERFORM IMS-08-LAES-INV-ROT                                          
267200     IF SEGMENT-FOUND                                                     
267300       MOVE REQU-IDDC-KEY       TO W-IDDC-WDH1-MIN                        
267400                                   W-IDDC-WDH1-MAX                        
267500       MOVE +2                  TO W-KDINVKAT-MIN                         
267600       MOVE +12                 TO W-KDINVKAT-MAX                         
267700       PERFORM IMS-09-LAES-INV                                            
267800       IF SEGMENT-FOUND                                                   
267900         MOVE YES TO KAT9-SW                                              
268000       END-IF                                                             
268100     END-IF                                                               
268200     .                                                                    
268300                                                                          
268400 S12-TRACKLOG-DATA SECTION.                                               
268500     INITIALIZE LOGT-WDL301                                               
268600     COMPUTE LOGT-DAREGDAT-9KOMPL = 99999999 - W-CURR-DATE                
268700     ACCEPT TRANS-TIME FROM TIME                                          
268800     COMPUTE LOGT-TIKLOCK-9KOMPL  = 999999999 - TRANS-TIME                
268900     MOVE W-IDARTNR          TO LOGT-IDARTNR                              
269000     MOVE 9                  TO LOGT-IDSEKVNR                             
269100     MOVE 'MISC'             TO LOGT-IDHUVTYP                             
269200     MOVE 'ADJ'              TO LOGT-IDSUBTYP                             
269300     MOVE 'WL017100'         TO LOGT-IDPGM                                
269400     MOVE 'L171'             TO LOGT-IDTRANS                              
269500     MOVE MSG-SIGNON-USERID  TO LOGT-IDUSER                               
269600     MOVE +0                 TO LOGT-IDKUNDNR                             
269700     MOVE '00000000'         TO LOGT-DAREGDAT-LADD                        
269800     MOVE W-IDDC             TO LOGT-IDDC                                 
269900     MOVE WS-TRCK-KVANTMOT   TO LOGT-KVART-SALDO                          
270000     IF WS-LOGT-SIGN = '+'                                                
270100        COMPUTE WS-SLAG-KVLS = WS-SLAG-KVLS +                             
270200                               WS-TRCK-KVANTMOT                           
270300     ELSE                                                                 
270400        COMPUTE WS-SLAG-KVLS = WS-SLAG-KVLS -                             
270500                               WS-TRCK-KVANTMOT                           
270600     END-IF                                                               
270700     MOVE WS-SLAG-KVLS       TO LOGT-KVLS                                 
270800     MOVE WS-LOGT-SIGN       TO LOGT-IDTECKEN-KVLS                        
270900                                LOGT-IDTECKEN-KVTRACK-KVAR                
271000     MOVE TRCK-KVTRACK-KVAR  TO LOGT-KVTRACK-KVAR                         
271100     MOVE WS-TRCK-IDTRACK    TO LOGT-IDTRACK                              
271200     .                                                                    
271300     EJECT                                                                
271400                                                                          
271500 S13-ISRT-TRACKLOG SECTION.                                               
271600     PERFORM IMS-ISRT-WDL301                                              
271700     IF SEGMENT-EXISTS                                                    
271800       PERFORM UNTIL NOT SEGMENT-EXISTS                                   
271900         SUBTRACT 1 FROM LOGT-IDSEKVNR                                    
272000         PERFORM IMS-ISRT-WDL301                                          
272100       END-PERFORM                                                        
272200     END-IF                                                               
272300     .                                                                    
272400     EJECT                                                                
272500 S20-SEND-OPEN SECTION.                                                   
272600     MOVE 'OPEN'                        TO SEND-KDFUNC                    
272700     MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
272800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
272900                         SEND-OPEN-AREA                                   
273000     IF SEND-KDRC > 0                                                     
273100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
273200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
273300       DELIMITED BY SIZE INTO FELTEXT                                     
273400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
273500     END-IF                                                               
273600     .                                                                    
273700                                                                          
273800 S21-SEND-PUT-PROP SECTION.                                               
273900                                                                          
274000     SET PROP-IX                 TO +1                                    
274100*    Mandatory property that specifies the actual destination             
274200     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
274300     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
274400     MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
274500                                                                          
274600     SET PROP-IX              UP BY +1                                    
274700*    Optional MQ message properties. Can be case-sensitive                
274800     MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
274900     MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
275000     MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
275100                                                                          
275200*    Set the number of properties (KVANTAL) so correct length             
275300*    is calculated.                                                       
275400     SET PROP-KVANTAL            TO PROP-IX                               
275500                                                                          
275600     MOVE 'PUT'                            TO SEND-KDFUNC                 
275700     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
275800     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
275900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
276000                         SEND-KVDLEN                                      
276100                         PROP-WZ04PROP                                    
276200     IF SEND-KDRC > 1                                                     
276300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
276400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
276500       DELIMITED BY SIZE INTO FELTEXT                                     
276600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
276700     END-IF                                                               
276800     .                                                                    
276900     EJECT                                                                
277000 S21-SEND-PUT SECTION.                                                    
277100                                                                          
277200     MOVE 'PUT'                            TO SEND-KDFUNC                 
277300     MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
277400     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
277500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
277600                         SEND-KVDLEN                                      
277700                         NOTF-AREA                                        
277800     IF SEND-KDRC > 1                                                     
277900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
278000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
278100       DELIMITED BY SIZE INTO FELTEXT                                     
278200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
278300     END-IF                                                               
278400     .                                                                    
278500     EJECT                                                                
278600 S22-SEND-CLOSE SECTION.                                                  
278700     MOVE 'CLOSE'            TO SEND-KDFUNC                               
278800     MOVE SEND-IDCOM         TO WZ04-SEND-IDCOM                           
278900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
279000                                                                          
279100     IF SEND-KDRC > 0                                                     
279200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
279300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
279400       DELIMITED BY SIZE INTO FELTEXT                                     
279500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
279600     END-IF                                                               
279700     .                                                                    
279800     EJECT                                                                
279900                                                                          
280000 IMS-01-READ-PART-6 SECTION.                                              
280100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
280200            DELIMITED BY SIZE INTO SSA1                                   
280300     MOVE '  GE' TO GOOD-STATUSCODES                                      
280400     CALL  CBLTDLI  USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                 
280500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
280600     PERFORM IMS-STATUS-CHECK                                             
280700     .                                                                    
280800                                                                          
280900 IMS-02-READ-ECONOMY-SEG SECTION.                                         
281000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
281100            DELIMITED BY SIZE INTO SSA1                                   
281200     MOVE '  GE' TO GOOD-STATUSCODES                                      
281300     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                
281400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
281500     PERFORM IMS-STATUS-CHECK                                             
281600     .                                                                    
281700                                                                          
281800 IMS-03-GHU-WDK711 SECTION.                                               
281900     MOVE 'IMS-03     ' TO CURR-IMS-SECTION                               
282000                                                                          
282100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
282200            DELIMITED BY SIZE INTO SSA1                                   
282300     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
282400            DELIMITED BY SIZE INTO SSA2                                   
282500     MOVE '  GE'                TO GOOD-STATUSCODES                       
282600     CALL  CBLTDLI  USING GHU ARTS-PCB WLARTS11 SSA1 SSA2                 
282700     MOVE ARTS-STATUS-CODE      TO STATUS-WS                              
282800     PERFORM IMS-STATUS-CHECK                                             
282900     .                                                                    
283000                                                                          
283100 IMS-25-GHU-WDK611 SECTION.                                               
283200     MOVE 'IMS-25     ' TO CURR-IMS-SECTION                               
283300                                                                          
283400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
283500            DELIMITED BY SIZE INTO SSA1                                   
283600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
283700            DELIMITED BY SIZE INTO SSA2                                   
283800     MOVE '  GE'                TO GOOD-STATUSCODES                       
283900     CALL  CBLTDLI  USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2            
284000     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
284100     PERFORM IMS-STATUS-CHECK                                             
284200     .                                                                    
284300 IMS-04-GHU-INVHIST-ROT SECTION.                                          
284400     MOVE 'IMS-04     ' TO CURR-IMS-SECTION                               
284500                                                                          
284600     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
284700            DELIMITED BY SIZE INTO SSA1                                   
284800     MOVE '  GE'                TO GOOD-STATUSCODES                       
284900     CALL  CBLTDLI  USING GHU INVC-PCB WLINVC01 SSA1                      
285000     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
285100     PERFORM IMS-STATUS-CHECK                                             
285200     .                                                                    
285300                                                                          
285400 IMS-05-GNP-INVHIST-WDH7 SECTION.                                         
285500     MOVE 'IMS-05     ' TO CURR-IMS-SECTION                               
285600                                                                          
285700     STRING 'WLINVC11(TISEGKEY<=' W-TISEGKEY-X                            
285800                    '&IDDC     =' W-IDDC-X ')'                            
285900            DELIMITED BY SIZE INTO SSA1                                   
286000     MOVE '  GE'                TO GOOD-STATUSCODES                       
286100     CALL  CBLTDLI  USING GHNP INVC-PCB WLINVC11 SSA1                     
286200     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
286300     PERFORM IMS-STATUS-CHECK                                             
286400     .                                                                    
286500                                                                          
286600 IMS-06-GET-WDL601      SECTION.                                          
286700     MOVE 'IMS-06     ' TO CURR-IMS-SECTION                               
286800                                                                          
286900     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X  ') '                       
287000            DELIMITED BY SIZE INTO SSA1                                   
287100     MOVE '  GE'                TO GOOD-STATUSCODES                       
287200     CALL  CBLTDLI  USING GU INLC-PCB INLC-WLINLC01 SSA1                  
287300     MOVE INLC-STATUS-CODE      TO STATUS-WS                              
287400     PERFORM IMS-STATUS-CHECK                                             
287500     .                                                                    
287600                                                                          
287700 IMS-07-GNP-WDL611      SECTION.                                          
287800     MOVE 'IMS-07     ' TO CURR-IMS-SECTION                               
287900                                                                          
288000     MOVE 'WLINLC11 '      TO SSA1                                        
288100     MOVE '  GE'           TO GOOD-STATUSCODES                            
288200     CALL  CBLTDLI  USING GNP INLC-PCB INLC-WLINLC11 SSA1                 
288300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
288400     PERFORM IMS-STATUS-CHECK                                             
288500     .                                                                    
288600                                                                          
288700 IMS-ISRT-WDL623    SECTION.                                              
288800     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
288900          DELIMITED BY SIZE INTO SSA1                                     
289000     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
289100          DELIMITED BY SIZE INTO SSA2                                     
289200     MOVE 'WDL623 ' TO SSA3                                               
289300     MOVE '  II' TO GOOD-STATUSCODES                                      
289400     CALL CBLTDLI USING ISRT WDL6-PCB WDL623                              
289500                             SSA1 SSA2 SSA3                               
289600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
289700     PERFORM IMS-STATUS-CHECK                                             
289800     .                                                                    
289900     EJECT                                                                
290000 IMS-08-LAES-INV-ROT  SECTION.                                            
290100     MOVE 'IMS-08     ' TO CURR-IMS-SECTION                               
290200                                                                          
290300     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
290400            DELIMITED BY SIZE INTO SSA1                                   
290500     MOVE '  GE'                TO GOOD-STATUSCODES                       
290600     CALL  CBLTDLI  USING GU INVREG-PCB INVB-WDH101   SSA1                
290700     MOVE INVREG-STATUS-CODE    TO STATUS-WS                              
290800     PERFORM IMS-STATUS-CHECK                                             
290900     .                                                                    
291000                                                                          
291100 IMS-09-LAES-INV  SECTION.                                                
291200     MOVE 'IMS-09     ' TO CURR-IMS-SECTION                               
291300                                                                          
291400     STRING 'WDH111  (WDH111KY >' W-WDH1KEY-MIN-X                         
291500                    '&WDH111KY <' W-WDH1KEY-MAX-X                         
291600                    '&FLINVBEH =' NOO ')'                                 
291700            DELIMITED BY SIZE INTO SSA1                                   
291800     MOVE '  GE'                TO GOOD-STATUSCODES                       
291900     CALL CBLTDLI USING GNP INVREG-PCB INVB-WDH111   SSA1                 
292000     MOVE INVREG-STATUS-CODE    TO STATUS-WS                              
292100     PERFORM IMS-STATUS-CHECK                                             
292200     .                                                                    
292300 IMS-LAES-WDH111-UNIK  SECTION.                                           
292400     MOVE 'IMS-WDH111-UNIK ' TO CURR-IMS-SECTION                          
292500                                                                          
292600     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
292700            DELIMITED BY SIZE INTO SSA1                                   
292800     STRING 'WDH111  (WDH111KY =' W-WDH1KEY-UNIK-X ')'                    
292900            DELIMITED BY SIZE INTO SSA2                                   
293000     MOVE '  GE'                TO GOOD-STATUSCODES                       
293100     CALL CBLTDLI USING GHU INVREG-PCB INVB-WDH111 SSA1 SSA2              
293200     MOVE INVREG-STATUS-CODE    TO STATUS-WS                              
293300     PERFORM IMS-STATUS-CHECK                                             
293400     .                                                                    
293500 IMS-LAES-WDH121 SECTION.                                                 
293600     MOVE 'IMS-LAES-WDH121     ' TO CURR-IMS-SECTION                      
293700                                                                          
293800*    STRING 'WDH111  (WDH111KY =' W-WDH111KY-X ')'                        
293900*      DELIMITED BY SIZE INTO SSA1                                        
294000                                                                          
294100     MOVE 'WDH121   '  TO SSA1                                            
294200     MOVE '  GE' TO GOOD-STATUSCODES                                      
294300     CALL  CBLTDLI  USING GNP INVREG-PCB INVB-WDH121  SSA1                
294400     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
294500     PERFORM IMS-STATUS-CHECK                                             
294600     .                                                                    
294700     SKIP2                                                                
294800                                                                          
294900 IMS-10-GU-BEN SECTION.                                                   
295000     MOVE 'IMS-10     ' TO CURR-IMS-SECTION                               
295100                                                                          
295200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X  ') '                       
295300            DELIMITED BY SIZE INTO SSA1                                   
295400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
295500            DELIMITED BY SIZE INTO SSA2                                   
295600     MOVE '  GE'                TO GOOD-STATUSCODES                       
295700     CALL  CBLTDLI  USING GU BEN-PCB BEN-WLBENA11 SSA1 SSA2               
295800     MOVE BEN-STATUS-CODE       TO STATUS-WS                              
295900     PERFORM IMS-STATUS-CHECK                                             
296000     .                                                                    
296100                                                                          
296200 IMS-11-LAES-INV-SEG  SECTION.                                            
296300     MOVE 'IMS-11     ' TO CURR-IMS-SECTION                               
296400                                                                          
296500     STRING 'WDH111  (WDH111KY >' W1-WDH1KEY-X '&WDH111KY <'              
296600       W2-WDH1KEY-X ')'                                                   
296700            DELIMITED BY SIZE INTO SSA1                                   
296800     MOVE '  GE'                TO GOOD-STATUSCODES                       
296900     CALL  CBLTDLI  USING GHNP INVREG-PCB INVB-WDH111   SSA1              
297000     MOVE INVREG-STATUS-CODE    TO STATUS-WS                              
297100     PERFORM IMS-STATUS-CHECK                                             
297200     .                                                                    
297300                                                                          
297400 IMS-12-REPL-SLAGER-SEGM SECTION.                                         
297500     MOVE 'IMS-12     ' TO CURR-IMS-SECTION                               
297600                                                                          
297700     MOVE '  '             TO GOOD-STATUSCODES                            
297800     CALL  CBLTDLI  USING REPL ARTS-PCB WLARTS11                          
297900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
298000     PERFORM IMS-STATUS-CHECK                                             
298100     .                                                                    
298200 IMS-24-REPL-CLAGER-SEGM SECTION.                                         
298300     MOVE 'IMS-24     ' TO CURR-IMS-SECTION                               
298400                                                                          
298500     MOVE '  '             TO GOOD-STATUSCODES                            
298600     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-WDK611                     
298700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
298800     PERFORM IMS-STATUS-CHECK                                             
298900     .                                                                    
299000                                                                          
299100 IMS-13-GHU-G2-UTREDNSALDO SECTION.                                       
299200     MOVE 'IMS-13     ' TO CURR-IMS-SECTION                               
299300                                                                          
299400     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
299500           DELIMITED BY SIZE INTO SSA1                                    
299600     STRING 'WLXXEF11(WDGXKEY  =' W-IDARTNR-UTR-X ')'                     
299700           DELIMITED BY SIZE INTO SSA2                                    
299800     MOVE '  GE'               TO GOOD-STATUSCODES                        
299900     CALL  CBLTDLI  USING GHU XXEF-PCB WLXXEF11 SSA1 SSA2                 
300000     MOVE XXEF-STATUS-CODE     TO STATUS-WS                               
300100     PERFORM IMS-STATUS-CHECK                                             
300200     .                                                                    
300300                                                                          
300400 IMS-14-DELETE-G2-UTREDNSALDO SECTION.                                    
300500     MOVE 'IMS-14     ' TO CURR-IMS-SECTION                               
300600                                                                          
300700     MOVE '  '             TO GOOD-STATUSCODES                            
300800     CALL  CBLTDLI  USING DLET XXEF-PCB WLXXEF11                          
300900     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
301000     PERFORM IMS-STATUS-CHECK                                             
301100     .                                                                    
301200                                                                          
301300 IMS-17-GNP-EKONOMISEG SECTION.                                           
301400     MOVE 'IMS-17     ' TO CURR-IMS-SECTION                               
301500                                                                          
301600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
301700            DELIMITED BY SIZE INTO SSA1                                   
301800     MOVE '  '                  TO GOOD-STATUSCODES                       
301900     CALL  CBLTDLI  USING GNP WDK6-PCB   DLI-IO-WDK611 SSA1               
302000     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
302100     PERFORM IMS-STATUS-CHECK                                             
302200     .                                                                    
302300                                                                          
302400 IMS-18-LAES-ARTIKEL-INV SECTION.                                         
302500     MOVE 'IMS-18     ' TO CURR-IMS-SECTION                               
302600                                                                          
302700     STRING 'WDH111  (WDH111KY >' W1-WDH1KEY-X                            
302800                    '&WDH111KY <' W2-WDH1KEY-X                            
302900                    '&FLINVBEH =' NOO ')'                                 
303000            DELIMITED BY SIZE INTO SSA1                                   
303100     MOVE '  GE'                TO GOOD-STATUSCODES                       
303200     CALL  CBLTDLI  USING GNP INVREG-PCB INVB-WDH111   SSA1               
303300     MOVE INVREG-STATUS-CODE    TO STATUS-WS                              
303400     PERFORM IMS-STATUS-CHECK                                             
303500                                                                          
303600     IF SEGMENT-FOUND AND INVB-INV-FLINVBEH = NOO                         
303700       MOVE YES   TO INVKOE-PART-EXISTS                                   
303800     ELSE                                                                 
303900       MOVE NOO   TO INVKOE-PART-EXISTS                                   
304000     END-IF                                                               
304100     .                                                                    
304200                                                                          
304300 IMS-20-ISRT-INVHIST-ROT SECTION.                                         
304400     MOVE 'IMS-20     ' TO CURR-IMS-SECTION                               
304500                                                                          
304600     MOVE 'WLINVC01 '      TO SSA1                                        
304700     MOVE '  '             TO GOOD-STATUSCODES                            
304800     CALL  CBLTDLI  USING ISRT INVC-PCB WLINVC01  SSA1                    
304900     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
305000     PERFORM IMS-STATUS-CHECK                                             
305100     .                                                                    
305200                                                                          
305300 IMS-21-ISRT-WDL901 SECTION.                                              
305400     MOVE 'IMS-21     ' TO CURR-IMS-SECTION                               
305500                                                                          
305600     SKIP2                                                                
305700     MOVE 'WLLOGA01 '      TO SSA1                                        
305800     MOVE '  II'           TO GOOD-STATUSCODES                            
305900     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
306000     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
306100     PERFORM IMS-STATUS-CHECK                                             
306200     .                                                                    
306300                                                                          
306400 IMS-ISRT-WDL301 SECTION.                                                 
306500     MOVE 'WDL301 ' TO SSA1                                               
306600     MOVE '  II' TO GOOD-STATUSCODES                                      
306700     CALL CBLTDLI USING ISRT WDL3-PCB WDL301 SSA1                         
306800     MOVE WDL3-STATUS-CODE TO STATUS-WS                                   
306900     PERFORM IMS-STATUS-CHECK                                             
307000     .                                                                    
307100     EJECT                                                                
307200 IMS-22-ISRT-INVHIST-SEGM SECTION.                                        
307300     MOVE 'IMS-22     ' TO CURR-IMS-SECTION                               
307400                                                                          
307500     MOVE 'WLINVC11 '      TO SSA1                                        
307600     MOVE '  II'           TO GOOD-STATUSCODES                            
307700     CALL  CBLTDLI  USING ISRT INVC-PCB WLINVC11 SSA1                     
307800     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
307900     PERFORM IMS-STATUS-CHECK                                             
308000     .                                                                    
308100                                                                          
308200 IMS-23-REPL-INV-SEG  SECTION.                                            
308300     MOVE 'IMS-23     ' TO CURR-IMS-SECTION                               
308400                                                                          
308500     MOVE '    ' TO GOOD-STATUSCODES                                      
308600     CALL  CBLTDLI  USING REPL INVREG-PCB INVB-WDH111                     
308700     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
308800     PERFORM IMS-STATUS-CHECK                                             
308900     .                                                                    
309000 IMS-29-ISRT-INV-ROT  SECTION.                                            
309100     MOVE 'IMS-29     ' TO CURR-IMS-SECTION                               
309200                                                                          
309300     MOVE 'WDH101   '  TO SSA1                                            
309400     MOVE '  II' TO GOOD-STATUSCODES                                      
309500     CALL  CBLTDLI  USING ISRT INVREG-PCB INVB-WDH101   SSA1              
309600     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
309700     PERFORM IMS-STATUS-CHECK                                             
309800     .                                                                    
309900 IMS-30-GET-ART-ROT  SECTION.                                             
310000     MOVE 'IMS-30     ' TO CURR-IMS-SECTION                               
310100                                                                          
310200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X                             
310300                    '&KDERS    =' W-KDERS-0-X ')'                         
310400            DELIMITED BY SIZE INTO SSA1                                   
310500     MOVE '  ' TO GOOD-STATUSCODES                                        
310600     CALL  CBLTDLI  USING GU WDK6-PCB   DLI-IO-WDK601 SSA1                
310700     MOVE WDK6-STATUS-CODE   TO STATUS-WS                                 
310800     PERFORM IMS-STATUS-CHECK                                             
310900     .                                                                    
311000 IMS-31-GU-WDK711 SECTION.                                                
311100     MOVE 'IMS-31     ' TO CURR-IMS-SECTION                               
311200                                                                          
311300     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
311400            DELIMITED BY SIZE INTO SSA1                                   
311500     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
311600            DELIMITED BY SIZE INTO SSA2                                   
311700     MOVE '    ' TO GOOD-STATUSCODES                                      
311800     CALL  CBLTDLI  USING GU ARTS-PCB WLARTS11 SSA1 SSA2                  
311900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
312000     PERFORM IMS-STATUS-CHECK                                             
312100     .                                                                    
312200 IMS-GU-WDK711 SECTION.                                                   
312300                                                                          
312400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
312500          DELIMITED BY SIZE INTO SSA1                                     
312600     STRING 'WDK711  (IDDC     =' W-IDDC ')'                              
312700          DELIMITED BY SIZE INTO SSA2                                     
312800     MOVE '  GE' TO GOOD-STATUSCODES                                      
312900     CALL CBLTDLI USING GU WDK7-PCB WLARTS11 SSA1 SSA2                    
313000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
313100     PERFORM IMS-STATUS-CHECK                                             
313200     .                                                                    
313300     EJECT                                                                
313400 IMS-GHNP-WDK728 SECTION.                                                 
313500                                                                          
313600     MOVE 'WDK728 ' TO SSA1                                               
313700     MOVE '  GE' TO GOOD-STATUSCODES                                      
313800     CALL CBLTDLI USING GHNP WDK7-PCB WDK728 SSA1                         
313900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
314000     PERFORM IMS-STATUS-CHECK                                             
314100     .                                                                    
314200     EJECT                                                                
314300 IMS-GHNP-WDK728-LAST SECTION.                                            
314400                                                                          
314500     STRING 'WDK728  *L(DAINLEV < ' W-DAINLEV ')'                         
314600          DELIMITED BY SIZE INTO SSA1                                     
314700     MOVE '  GE' TO GOOD-STATUSCODES                                      
314800     CALL CBLTDLI USING GHNP WDK7-PCB WDK728 SSA1                         
314900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
315000     PERFORM IMS-STATUS-CHECK                                             
315100     .                                                                    
315200     EJECT                                                                
315300 IMS-REPL-WDK728 SECTION.                                                 
315400                                                                          
315500     MOVE '  ' TO GOOD-STATUSCODES                                        
315600     CALL CBLTDLI USING REPL WDK7-PCB WDK728                              
315700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
315800     PERFORM IMS-STATUS-CHECK                                             
315900     .                                                                    
316000     EJECT                                                                
316100 IMS-32-ISRT-INV-SEG  SECTION.                                            
316200     MOVE 'IMS-32     ' TO CURR-IMS-SECTION                               
316300                                                                          
316400     MOVE 'WDH111   ' TO SSA1                                             
316500     MOVE '  IINI' TO GOOD-STATUSCODES                                    
316600     CALL  CBLTDLI  USING ISRT INVREG-PCB INVB-WDH111   SSA1              
316700     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
316800     PERFORM IMS-STATUS-CHECK                                             
316900     .                                                                    
317000 IMS-INSERT-WDH121      SECTION.                                          
317100     MOVE 'IMS-INSERT-WDH121   ' TO CURR-IMS-SECTION                      
317200     MOVE 'WDH121 ' TO SSA1                                               
317300     MOVE '  II' TO GOOD-STATUSCODES                                      
317400     CALL CBLTDLI USING ISRT INVREG-PCB  INVB-WDH121   SSA1               
317500     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
317600     PERFORM IMS-STATUS-CHECK                                             
317700     .                                                                    
317800     EJECT                                                                
317900 IMS-33-ISRT-WDR901 SECTION.                                              
318000     MOVE 'IMS-33     ' TO CURR-IMS-SECTION                               
318100                                                                          
318200     MOVE 'WLSAPA01 ' TO SSA1                                             
318300     MOVE '  II' TO GOOD-STATUSCODES                                      
318400     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
318500     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
318600     PERFORM IMS-STATUS-CHECK                                             
318700     .                                                                    
318800 IMS-34-ISRT-WDR801 SECTION.                                              
318900     MOVE 'IMS-34     ' TO CURR-IMS-SECTION                               
319000                                                                          
319100     MOVE 'WDR801  ' TO SSA1                                              
319200     MOVE '  II' TO GOOD-STATUSCODES                                      
319300     CALL CBLTDLI USING ISRT WDR8-PCB WDR8-WDR801 SSA1                    
319400     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
319500     PERFORM IMS-STATUS-CHECK                                             
319600     .                                                                    
319700 IMS-GU-WDB601    SECTION.                                                
319800     MOVE 'IMS-GU-WDB601   '  TO CURR-IMS-SECTION                         
319900                                                                          
320000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
320100          DELIMITED BY SIZE INTO SSA1                                     
320200     MOVE '  ' TO GOOD-STATUSCODES                                        
320300     CALL CBLTDLI USING GU WDB6-PCB WDB601 SSA1                           
320400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
320500     PERFORM IMS-STATUS-CHECK                                             
320600     .                                                                    
320700     EJECT                                                                
320800 IMS-GHU-WDK629  SECTION.                                                 
320900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
321000          DELIMITED BY SIZE INTO SSA1                                     
321100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
321200          DELIMITED BY SIZE INTO SSA2                                     
321300     MOVE   'WDK629  '        TO SSA3                                     
321400     MOVE '  GE' TO GOOD-STATUSCODES                                      
321500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
321600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
321700     PERFORM IMS-STATUS-CHECK                                             
321800     .                                                                    
321900     SKIP3                                                                
322000 IMS-REPL-WDK629 SECTION.                                                 
322100     MOVE '  ' TO GOOD-STATUSCODES                                        
322200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
322300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
322400     PERFORM IMS-STATUS-CHECK                                             
322500     .                                                                    
322600     EJECT                                                                
322700************* WDR4 4505 HÄNDELSEBAS ( TÄCKNING ORDERADSREG.) ****         
322800 IMS-ISRT-450511 SECTION.                                                 
322900                                                                          
323000     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
323100            DELIMITED BY SIZE INTO SSA1                                   
323200     MOVE 'WL450511 '                   TO SSA2                           
323300     MOVE '  ' TO GOOD-STATUSCODES                                        
323400     CALL  CBLTDLI  USING ISRT 4505-PCB WL450511 SSA1 SSA2                
323500     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
323600     PERFORM IMS-STATUS-CHECK                                             
323700     .                                                                    
323800     EJECT                                                                
323900 IMS-STATUS-CHECK   SECTION.                                              
324000                                                                          
324100     SET STATUS-IX TO 1                                                   
324200     SEARCH GOOD-STATUS                                                   
324300       AT END                                                             
324400         CALL FELLOG                                                      
324500     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
324600       CONTINUE                                                           
324700     END-SEARCH                                                           
324800     .                                                                    
