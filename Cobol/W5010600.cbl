000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5010600.                                                
000300 AUTHOR.         MARIE-ANN EVERBÄCK.                                      
000400 DATE-WRITTEN.   MARS 1979.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000900*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0171               
001000*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001100*                TP-PROGRAM SOM UPPDATERAR ARTIKELREG-WDK6 OCH            
001200*                ARTIKELREG-WDK7 MED INVENTERINGSJUSTERINGAR.             
001300*                UPPDATERAR OCKSÅ WDR9-PEDAL BASEN                        
001500* CHANGES:                                                                
001600*        ETRACKER: 4637061                                                
001700*        - LÄGGA UPP EN ARTIKELS LAGERVÄRDE EFTER EN INVENTERING          
001800*          FÖR SENARE KUNNA SKAPA INVENTERINGS HIST LISTOR                
001900*          GAO KRAV PÅ DETTA                                              
002000*                                                                         
002100*        2014-04-07                                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T106                                              
002700*        MID:         W5I10601                                            
002800*    UTDATA.                                                              
002900*        MOD:         W5O10601                                            
003000*                                                                         
003100                                                                          
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W1                                                       
003900                                                                          
004000 77  IDPGM                   PIC X(8)   VALUE 'W5010600'.                 
004100 77  WS-SECTION               PIC X(32).                                  
004200 77  WS-IMS                   PIC X(32).                                  
004300 77  WS-ANTAL                 PIC S9(9) VALUE ZERO.                       
004400 77  FELTEXT                 PIC X(80)  VALUE SPACE.                      
004500 77  W-SUMMA                 PIC S9(7)  COMP-3.                           
004600 77  W-SUMMA-STD             PIC S9(11)V9(2)  COMP-3.                     
004700 77  SPAR-PRARTSTD           PIC S9(7)V9(2)  COMP-3.                      
004800 77  IX                      PIC 9(3)   VALUE ZERO.                       
004900 77  IX2                     PIC 9(3)   VALUE ZERO.                       
005000 77  INDX                    PIC S9(9)  COMP SYNC.                        
005100 77  INDY                    PIC S9(9)  COMP SYNC.                        
005200 77  KDINVKAT-WS             PIC S9(3)  COMP-3.                           
005300 77  W-KDSPRAK               PIC S9     COMP-3.                           
005400 77  WS-KDSORT               PIC X(2)   VALUE SPACE.                      
005500 77  W-ART-KVROS-SDC-NDC     PIC S9(7)  VALUE ZERO COMP-3.                
005600 77  DAGENS-DATUM            PIC 9(8).                                    
005700 77  W-DAGENS-DATUM          PIC 9(8).                                    
005800 77  TRANS-TID               PIC 9(9).                                    
005900 77  RETURKOD                PIC S9(4)  COMP SYNC VALUE +0.               
006000 77  KVJUSTKV-WS             PIC X(6).                                    
006100 77  INLEV-DATUM             PIC S9(7)   COMP-3.                          
006200 77  INLEV-ANTAL             PIC S9(7)   COMP-3.                          
006300 77  W-PRAVCOST              PIC S9(7)V9(2) VALUE ZERO COMP-3.            
006400 77  W-IDRT-KEY              PIC X(2)    VALUE SPACE.                     
006500 77  WS-KVANTAL              PIC S9(7)   COMP-3.                          
006600                                                                          
006700*01 -COPY WWDCKONS                                                        
006800                                                                          
006900 77    WDB6-SW               PIC X   VALUE 'J'.                           
007000       88  WDB6-FINNS                VALUE 'J'.                           
007100       88  WDB6-SAKNAS               VALUE 'N'.                           
007200                                                                          
007300 01  WS-SPAR-HIST.                                                        
007400     03 WS-DAREGDAT-CRE          PIC 9(8) VALUE ZERO.                     
007500     03 WS-DAREGDAT-PR1          PIC 9(8) VALUE ZERO.                     
007600     03 WS-DAREGDAT-PR2          PIC 9(8) VALUE ZERO.                     
007700     03 WS-DAREGDAT-PR3          PIC 9(8) VALUE ZERO.                     
007800     03 WS-IDUSER-PR1            PIC X(8) VALUE SPACE.                    
007900     03 WS-IDUSER-PR2            PIC X(8) VALUE SPACE.                    
008000     03 WS-IDUSER-PR3            PIC X(8) VALUE SPACE.                    
008100     03 WS-IDUSER-CRE            PIC X(8) VALUE 'W5010600'.               
008200                                                                          
008300 01  DIVERSE.                                                             
008400     03  DAGENS-DAT          PIC X(6).                                    
008500     03  DAGENS-DAT-N        PIC 9(6).                                    
008600     03  DAGENS-DAT-WDH111   PIC 9(6).                                    
008700     03  WS-EKH-KDEKSHT.                                                  
008800         05 WS-KDEKSHT-1     PIC X(2).                                    
008900         05 WS-KDEKSHT-2     PIC X.                                       
009000     03  WS-EKH-KDEKSHT1.                                                 
009100         05 WS-KDEKSHT-3     PIC X(2).                                    
009200         05 WS-KDEKSHT-4     PIC X.                                       
009300     03  W-EKH-IDARTNR       PIC X(9)  VALUE SPACE.                       
009400                                                                          
009500                                                                          
009600 01  WS-FIX-DATUM.                                                        
009700     03  WS-FIX-TISEGKEY     PIC 9(9).                                    
009800     03  WS-FILLER1 REDEFINES WS-FIX-TISEGKEY.                            
009900         05 WS-FILLER1-1-2   PIC 9(2).                                    
010000         05 WS-TISEGKEY-3-8  PIC 9(6).                                    
010100         05 WS-FILLER1-9     PIC 9(1).                                    
010200     03  WS-FILLER2 REDEFINES WS-FIX-TISEGKEY.                            
010300         05 WS-TISEGKEY-1-8  PIC 9(8).                                    
010400         05 WS-FILLER2-9     PIC 9(1).                                    
010500 01  WS-INV-DAREGDAT-AREA.                                                
010600     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
010700     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
010800       05  WS-INV-NOLL         PIC 9(1).                                  
010900       05  WS-INV-SEKEL        PIC 9(2).                                  
011000       05  WS-INV-AAMMDD       PIC 9(6).                                  
011100                                                                          
011200 01  KONSTANTER.                                                          
011300     03  JA                  PIC X       VALUE 'J'.                       
011400     03  NEJ                 PIC X       VALUE 'N'.                       
011500     03  ARTIKEL-DC-RETT     PIC X.                                       
011600     03  ARTIKEL-DC-FINNS    PIC X.                                       
011700     03  UPPDATERINGSRAD-IFYLLD       PIC X.                              
011800     03  UPPDATERINGSRAD-GODKEND      PIC X.                              
011900     03  INV-FINNS           PIC X.                                       
012000     03  TOTAL-STD           PIC X.                                       
012100     03  ART-INV-FINNS       PIC X.                                       
012200     03  INVKOE-ART-FINNS    PIC X.                                       
012300     03  EGEN-BILD           PIC X    VALUE 'J'.                          
012400     03  INV-KAT-FINNS       PIC X    VALUE 'N'.                          
012500                                                                          
012600 01  WS-KVJUSTKV-X.                                                       
012700     03  WS-KVJUSTKV-N       PIC 9(6).                                    
012800                                                                          
012900 01  WS-KVJUSTKV-COMP        PIC S9(7)    COMP-3.                         
013000 01  W-KVLS-COMP             PIC S9(7)    COMP-3  VALUE +0.               
013100                                                                          
013200 01  IDARTNR-WS              PIC X(9).                                    
013300 01  FILLER REDEFINES IDARTNR-WS.                                         
013400     03  NOLL-ARTIKEL        PIC X(1).                                    
013500     03  IDARTNR-WS-X        PIC X(8).                                    
013600                                                                          
013700 01  WS-TISEGKEYAREA.                                                     
013800     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
013900     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
014000         05  WS-SEKEL        PIC 9(2).                                    
014100         05  WS-TIAAMMDD     PIC 9(6).                                    
014200         05  WS-LOPNR        PIC 9(1).                                    
014300     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
014400                                                                          
014500     EJECT                                                                
014600 01  DYNAMISKA-SUBPROGRAM.                                                
014700     03  WDATKONV            PIC X(8) VALUE 'WDATKONV'.                   
014800     03  CBLTDLI             PIC X(8) VALUE 'CBLTDLI '.                   
014900     03  FELLOG              PIC X(8) VALUE 'FELLOG  '.                   
015000     03  W005INIT            PIC X(8) VALUE 'W005INIT'.                   
015200     03  W009CIA             PIC X(8) VALUE 'W009CIA '.                   
015300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015400*01 -COPY WMSGINIT                                                        
015500                                                                          
015600     EJECT                                                                
015700 01  MESSAGE-CODES.                                                       
015800     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
015900     03  ARTIKEL-EJ-NUMERISK     PIC X(3)    VALUE '020'.                 
016000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016100     EJECT                                                                
016500*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
016600*01 -COPY W009CIA                                                         
016700     EJECT                                                                
016800*********** NYCKLAR TILL LÄSNINGAR                                        
016900                                                                          
017000 01      W-IDARTNR-X.                                                     
017100   03    W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
017200                                                                          
017300 01      W-KDSEGKEY-X.                                                    
017400   03    W-KDSEGKEY      PIC X       VALUE '1'.                           
017500                                                                          
017600 01      W-WDD811KY-X.                                                    
017700   03    W-IDDC-WDD8     PIC X(2)           VALUE  SPACE.                 
017800   03    W-ADBUFFOMR     PIC S9(3)  COMP-3  VALUE  +1.                    
017900   03    W-DABUFPAF      PIC  9(8)          VALUE  ZERO.                  
018000   03    W-ADBUFFGANG    PIC S9(3)  COMP-3  VALUE  ZERO.                  
018100   03    W-ADBUFFPL      PIC S9(5)  COMP-3  VALUE  ZERO.                  
018200                                                                          
018300 01  W-DAINLEVNYCK-X.                                                     
018400     03  W-DAINLEV      PIC  9(16).                                       
018500                                                                          
018600 01  W-IDLEVNYCK-X.                                                       
018700     03  W-IDPTYP       PIC X(3)   VALUE 'R34'.                           
018800                                                                          
018900 01  W-IDSKYLT-X.                                                         
019000     03  W-IDSKYLT      PIC X(3)   VALUE 'S  '.                           
019100     EJECT                                                                
019200 01  W-IDDC-X.                                                            
019300     03  W-IDDC              PIC X(2)   VALUE SPACE.                      
019400     SKIP2                                                                
019500                                                                          
019600 01  W-TISEGKEY-X.                                                        
019700     03  W-TISEGKEY          PIC S9(9)  VALUE +999999999 COMP-3.          
019800     SKIP2                                                                
019900                                                                          
020000 01  W-WDGXKEY-ROT-X.                                                     
020100     03  FILLER              PIC X(4)  VALUE '5115'.                      
020200     03  FILLER              PIC X(26) VALUE LOW-VALUE.                   
020300                                                                          
020400 01  W-IDARTNR-UTR-X.                                                     
020500     03  W-IDDC-UTR          PIC X(2)   VALUE SPACE.                      
020600     03  W-IDARTNR-UTR       PIC S9(9)  VALUE ZERO COMP-3.                
020700     03  FILLER              PIC  X(8)  VALUE LOW-VALUE.                  
020800     EJECT                                                                
020900                                                                          
021000 01  W-KVJUSTKV-IN-X.                                                     
021100     03  W-KVJUSTKV-IN-N     PIC 9(6).                                    
021200     SKIP2                                                                
021300 01  W-KVJUSTKV-COMP         PIC 9(7)     COMP-3.                         
021400     SKIP2                                                                
021500 01  FILLER              PIC X(8)   VALUE 'W1-WDH1='.                     
021600 01  W1-WDH1KEY-X.                                                        
021700     03  W1-IDDC-WDH1    PIC X(2)   VALUE SPACE.                          
021800     03  W1-KDINVKAT     PIC S9(3)  COMP-3.                               
021900     03  W1-TISEGKEY     PIC S9(9)  COMP-3 VALUE ZERO.                    
022000     03  W1-DAREGDAT-SORT    PIC 9(8) VALUE ZERO.                         
022100     SKIP2                                                                
022200 01  FILLER              PIC X(8)   VALUE 'W2-WDH1='.                     
022300 01  W2-WDH1KEY-X.                                                        
022400     03  W2-IDDC-WDH1    PIC X(2)   VALUE SPACE.                          
022500     03  W2-KDINVKAT     PIC S9(3)  COMP-3.                               
022600     03  W2-TISEGKEY     PIC S9(9)  COMP-3 VALUE +999999999.              
022700     03  W2-DAREGDAT-SORT    PIC 9(8) VALUE 99999999.                     
022800     SKIP2                                                                
022900 01  FILLER              PIC X(8)   VALUE 'WDH1MIN='.                     
023000 01  W-WDH1KEY-MIN-X.                                                     
023100     03  W-IDDC-WDH1-MIN PIC X(2).                                        
023200     03  W-KDINVKAT-MIN  PIC S9(3)  COMP-3.                               
023300     03  W-TISEGKEY-MIN  PIC S9(9)  COMP-3 VALUE ZERO.                    
023400     03  W-DAREGDAT-SORT-MIN    PIC 9(8) VALUE ZERO.                      
023500 01  FILLER              PIC X(8)   VALUE 'WDH1MAX='.                     
023600 01  W-WDH1KEY-MAX-X.                                                     
023700     03  W-IDDC-WDH1-MAX PIC X(2).                                        
023800     03  W-KDINVKAT-MAX  PIC S9(3)  COMP-3.                               
023900     03  W-TISEGKEY-MAX  PIC S9(9)  COMP-3 VALUE +999999999.              
024000     03  W-DAREGDAT-SORT-MAX PIC 9(8)    VALUE 99999999.                  
024100 01  W-WDH1KEY-UNIK-X.                                                    
024200     03  W-IDDC-UNIK         PIC X(2).                                    
024300     03  W-KDINVKAT-UNIK     PIC S9(3)  COMP-3.                           
024400     03  W-TISEGKEY-UNIK     PIC S9(9)  COMP-3 VALUE +999999999.          
024500     03  W-DAREGDAT-SORT-UNIK PIC 9(8)    VALUE 99999999.                 
024600     SKIP2                                                                
024700 01  W-KDERS-0-X.                                                         
024800     03  W-KDERS-0           PIC S9(3)  COMP-3  VALUE +0.                 
024900     EJECT                                                                
025000 01  W-KVUTRS-X.                                                          
025100     03  W-KVUTRS-N          PIC 9(7).                                    
025200     03  W-KVUTRS-TECKEN     PIC X.                                       
025300                                                                          
025400 01  W-KVUTRS-COMP           PIC S9(7)    COMP-3  VALUE +0.               
025500     SKIP2                                                                
025600 01  W-KVLS-X.                                                            
025700     03  W-KVLS-N            PIC 9(7).                                    
025800     03  W-KVLS-TECKEN       PIC X.                                       
025900                                                                          
026000 01  W-WDGX-4505-KEY-X.                                                   
026100     03  W-IDHTYP-4505       PIC X(4)    VALUE '4505'.                    
026200     03  W-IDDC-4505         PIC X(2)    VALUE '11'.                      
026300     03  FILLER              PIC X(24) VALUE LOW-VALUE.                   
026400                                                                          
026500  01  W-IDDC-B6-X.                                                        
026600      03 W-IDDC-B6           PIC X(2).                                    
026700                                                                          
026800     EJECT                                                                
026900 01      FELMEDDELANDE.                                                   
027000   03  FILLER           PIC X(42)                                         
027100      VALUE 'FELAKTIG TRANSAKTION'.                                       
027200   03  FILLER           PIC X(42)                                         
027300      VALUE 'TRANSACTION INCORRECT'.                                      
027400   03  FILLER           PIC X(42)                                         
027500      VALUE 'ARTIKELNR SAKNAS PÅ ARTIKELREG'.                             
027600   03  FILLER           PIC X(42)                                         
027700      VALUE 'PART NO  MISSING ON STOCK FILE'.                             
027800   03  FILLER           PIC X(55)  VALUE                                  
027900      ' TRYCK PF11 VID UPPDATERING                         '.             
028000   03  FILLER           PIC X(55)                                         
028100      VALUE ' PRESS PF11 TO UPDATE                       '.               
028200   03  FILLER           PIC X(42)                                         
028300      VALUE 'INGEN INV. PÅGÅR FÖR DETTA ARTIKELNR'.                       
028400   03  FILLER           PIC X(42)                                         
028500      VALUE 'NO STOCKT. IN PROCES FOR THIS PART NO'.                      
028600   03  FILLER           PIC X(42)                                         
028700      VALUE 'SLÄCKNING KAT. EJ TILLÅTEN'.                                 
028800   03  FILLER           PIC X(42)                                         
028900      VALUE 'DELETE CAT. NOT ALLOWED'.                                    
029000   03  FILLER           PIC X(42)                                         
029100      VALUE 'ADRESS OCH SALDO SAKNAS'.                                    
029200   03  FILLER           PIC X(42)                                         
029300      VALUE 'ADRESS AND SALDO MISSING'.                                   
029400   03  FILLER           PIC X(42)                                         
029500      VALUE 'INGEN UPPDATERING HAR GJORTS'.                               
029600   03  FILLER           PIC X(42)                                         
029700      VALUE 'NO UPDATE HAS BEEN EXECUTED'.                                
029800   03  FILLER           PIC X(42)                                         
029900      VALUE 'UPPDATERING HAR GJORTS'.                                     
030000   03  FILLER           PIC X(42)                                         
030100      VALUE 'UPDATE HAS BEEN EXECUTED'.                                   
030200   03  FILLER           PIC X(42)                                         
030300      VALUE 'FELAKTIGT ANTAL'.                                            
030400   03  FILLER           PIC X(42)                                         
030500      VALUE 'WRONG QUANTITY'.                                             
030600   03  FILLER           PIC X(42)                                         
030700      VALUE 'FEL! PF11 UTAN FÖREGÅENDE ENTER'.                            
030800   03  FILLER       PIC X(42)                                             
030900      VALUE 'WRONG! PF11 WITHOUT PRECEDING ENTER'.                        
031000   03  FILLER       PIC X(42)                                             
031100      VALUE 'TRYCK NU PF11 FÖR UPPDATERING'.                              
031200   03  FILLER       PIC X(42)                                             
031300      VALUE 'NOW PUSH PF11 FOR UPPDATE'.                                  
031400   03  FILLER       PIC X(42)                                             
031500      VALUE 'TYP 6 JUSTERING EJ TILLÅTEN FÖR DETTA DC'.                   
031600   03  FILLER       PIC X(42)                                             
031700      VALUE 'ADJUSTMENT TYPE 6 NOT ALLOWED'.                              
031800                                                                          
031900                                                                          
032000 01      FELMEDD  REDEFINES FELMEDDELANDE.                                
032100   03    MEDDELANDE-1    PIC X(42)  OCCURS 2.                             
032200   03    MEDDELANDE-2    PIC X(42)  OCCURS 2.                             
032300   03    MEDDELANDE-3    PIC X(55)  OCCURS 2.                             
032400   03    MEDDELANDE-4    PIC X(42)  OCCURS 2.                             
032500   03    MEDDELANDE-5    PIC X(42)  OCCURS 2.                             
032600   03    MEDDELANDE-6    PIC X(42)  OCCURS 2.                             
032700   03    MEDDELANDE-7    PIC X(42)  OCCURS 2.                             
032800   03    MEDDELANDE-8    PIC X(42)  OCCURS 2.                             
032900   03    MEDDELANDE-9    PIC X(42)  OCCURS 2.                             
033000   03    MEDDELANDE-10   PIC X(42)  OCCURS 2.                             
033100   03    MEDDELANDE-11   PIC X(42)  OCCURS 2.                             
033200   03    MEDDELANDE-12   PIC X(42)  OCCURS 2.                             
033300     EJECT                                                                
033400 01  FILLER              PIC X(16)  VALUE 'WDATAREA'.                     
033500*01  -COPY WDATAREA                                                       
033600     EJECT                                                                
033700*                        ****    TP-AREOR                                 
033800 01  FILLER              PIC X(16) VALUE '   TP-AREAOR   '.               
033900     SKIP2                                                                
034000 01  MID-INPUT           PIC X(150).                                      
034100*01  MID -COPY W5I10601 -PRE MID-  -RED MID-INPUT.                        
034200     EJECT                                                                
034300*01      -COPY WMSGAREA                                                   
034400     EJECT                                                                
034500*  03    MOD -COPY W5O10601 -PRE MOD- -RED MSG-AREA.                      
034600     EJECT                                                                
034700*01  -COPY WMFSAREA.                                                      
034800     EJECT                                                                
034900******************************************************************        
035000*****                                                                     
035100*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
035200*****                    **** STATUS-KOD FRÅN IMS                         
035300   03    STATUS-WS       PIC XX.                                          
035400         88  SEGMENT-FINNS       VALUE '  '.                              
035500         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
035600         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
035700         88  INDEX-FINNS-REDAN   VALUE 'NI'.                              
035800     SKIP3                                                                
035900   03    GODK-STATUSKODER.                                                
036000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036100     SKIP3                                                                
036200 01      SSA1            PIC X(128).                                      
036300 01      SSA2            PIC X(128).                                      
036400 01      SSA3            PIC X(128).                                      
036500     EJECT                                                                
036600*                            IMS FUNKTIONSKODER                           
036700*01      -COPY W0003                                                      
036800     EJECT                                                                
036900*-------- WDH1-INVENTERINGSREG                                            
037000                                                                          
037100 01  FILLER              PIC X(16)   VALUE 'WDH101'.                      
037200*                                                                         
037300*01  WDH101       -COPY WDH101 -PRE INVB-.                                
037400     EJECT                                                                
037500                                                                          
037600 01  FILLER              PIC X(16)   VALUE 'WDH111'.                      
037700*                                                                         
037800*01  WDH111       -COPY WDH111 -PRE INVB-.                                
037900     EJECT                                                                
038000 01  FILLER              PIC X(16)   VALUE 'WDH121'.                      
038100*                                                                         
038200*01  WDH121       -COPY WDH121 -PRE INVB-.                                
038300     EJECT                                                                
038400*-------- WDK6-ARTIKELREG                                                 
038500                                                                          
038600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
038700 01  DLI-IO-WDK601.                                                       
038800*  03  -COPY WDK601.                                                      
038900                                                                          
039000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
039100 01  DLI-IO-WDK611.                                                       
039200*  03  -COPY WDK611.                                                      
039300                                                                          
039400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
039500 01  DLI-IO-WDK629.                                                       
039600*    03  -COPY WDK629                                                     
039700                                                                          
039800 01  FILLER              PIC X(16)   VALUE 'WDGX4506'.                    
039900*                                                                         
040000*01  WL450511     -COPY WDGX4506.                                         
040100     EJECT                                                                
040200                                                                          
040300 01  FILLER              PIC X(16)   VALUE 'WDGX5116'.                    
040400*                                                                         
040500*01  WLXXEF11     -COPY WDGX5116.                                         
040600     EJECT                                                                
040700                                                                          
040800 01  FILLER              PIC X(16)   VALUE 'WDL201'.                      
040900*                                                                         
041000*01  WLINLE01 -COPY WDL201 -PRE INL-                                      
041100     EJECT                                                                
041200                                                                          
041300 01  FILLER              PIC X(16)   VALUE 'WDL211'.                      
041400*                                                                         
041500*01  WLINLE11 -COPY WDL211 -PRE INL-                                      
041600     EJECT                                                                
041700                                                                          
041800 01  FILLER              PIC X(16)   VALUE 'WDL221'.                      
041900*                                                                         
042000*01  WLINLE21 -COPY WDL221 -PRE INL-                                      
042100     EJECT                                                                
042200                                                                          
042300 01  FILLER              PIC X(16)   VALUE 'WDL231'.                      
042400*                                                                         
042500*01  WLINLE31 -COPY WDL231 -PRE INL-                                      
042600     EJECT                                                                
042700                                                                          
042800 01  FILLER              PIC X(16)   VALUE 'WDL222'.                      
042900*                                                                         
043000*01  WLINLE22 -COPY WDL222 -PRE INL-                                      
043100     EJECT                                                                
043200                                                                          
043300 01  FILLER              PIC X(16)   VALUE 'WDD801'.                      
043400*                                                                         
043500*01  WLARTD01 -COPY WDD801 -PRE ART-                                      
043600     EJECT                                                                
043700                                                                          
043800 01  FILLER              PIC X(16)   VALUE 'WDD811'.                      
043900*                                                                         
044000*01  WLARTD11 -COPY WDD811 -PRE ART-                                      
044100     EJECT                                                                
044200                                                                          
044300 01  FILLER              PIC X(16)   VALUE 'WDD311'.                      
044400*                                                                         
044500*01  WLBENA11 -COPY WDD311 -PRE BEN-                                      
044600     EJECT                                                                
044700                                                                          
044800 01  FILLER              PIC X(16)   VALUE 'WDL601'.                      
044900*                                                                         
045000*01  WLINLC01 -COPY WDL601 -PRE INLC-                                     
045100     EJECT                                                                
045200                                                                          
045300 01  FILLER              PIC X(16)   VALUE 'WDL611'.                      
045400*                                                                         
045500*01  WLINLC11 -COPY WDL611 -PRE INLC-                                     
045600     EJECT                                                                
045700*-------- WDK7-ARTIKELREG                                                 
045800                                                                          
045900 01  FILLER              PIC X(16)   VALUE 'WDK701'.                      
046000*                                                                         
046100*01  WLARTS01     -COPY WDK701                                            
046200     EJECT                                                                
046300                                                                          
046400 01  FILLER              PIC X(16)   VALUE 'WDK711'.                      
046500*                                                                         
046600*01  WLARTS11     -COPY WDK711                                            
046700     EJECT                                                                
046800*-------- WDH7-INVENTERINGSHISTORIK                                       
046900                                                                          
047000 01  FILLER              PIC X(16)   VALUE 'WDH701'.                      
047100*                                                                         
047200*01  WLINVC01     -COPY WDH701                                            
047300     EJECT                                                                
047400                                                                          
047500 01  FILLER              PIC X(16)   VALUE 'WDH711'.                      
047600*                                                                         
047700*01  WLINVC11     -COPY WDH711                                            
047800     EJECT                                                                
047900                                                                          
048000 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
048100*01  WLLOGA01    -COPY WDL901                                             
048200     EJECT                                                                
048300 01  FILLER                      PIC X(16) VALUE 'WLSAPA01'.              
048400*01  WLSAPA01    -COPY WDR901                                             
048500*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
048600     EJECT                                                                
048700 01  FILLER                      PIC X(16) VALUE 'WFILBO1'.               
048800*01  WFILB01     -COPY WDR801 -PRE A08-                                   
048900*    05 -COPY W510A08  -RED A08-FIL-WDR801-DATA -PRE A08-                 
049000     EJECT                                                                
049600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
049700 01   DLI-IO-AREA-B601.                                                   
049800*     03  -COPY WDB601                                                    
049900                                                                          
050000     EJECT                                                                
050100                                                                          
050200 LINKAGE SECTION.                                                         
050300*01  -COPY W0009     -PRE MSG-                                            
050400     EJECT                                                                
050500*01  -COPY W0008     -PRE USEA-.                                          
050600         05  FILLER           PIC X.                                      
050700                                                                          
050800*01  -COPY W0008     -PRE INVREG-.                                        
050900         05  FILLER           PIC X.                                      
051000     EJECT                                                                
051100*01  -COPY W0008     -PRE WDK6-                                           
051200         05  FILLER           PIC X.                                      
051300*01  -COPY W0008     -PRE 4505-                                           
051400         05  FILLER           PIC X.                                      
051500     EJECT                                                                
051600*01  -COPY W0008     -PRE XXEF-                                           
051700         05  FILLER           PIC X.                                      
051800                                                                          
051900*01  -COPY W0008     -PRE INLE-                                           
052000         05  FILLER           PIC X.                                      
052100     EJECT                                                                
052200*01  -COPY W0008     -PRE ARTD-                                           
052300         05  FILLER           PIC X.                                      
052400                                                                          
052500*01  -COPY W0008     -PRE BEN-                                            
052600         05  FILLER           PIC X.                                      
052700     EJECT                                                                
052800*01  -COPY W0008     -PRE ARTS-                                           
052900         05  FILLER           PIC X.                                      
053000                                                                          
053100*01  -COPY W0008     -PRE INLC-                                           
053200         05  FILLER           PIC X.                                      
053300     EJECT                                                                
053400*01  -COPY W0008     -PRE INVC-                                           
053500         05  FILLER           PIC X.                                      
053600     EJECT                                                                
053700*01  -COPY W0008     -PRE LOGA-                                           
053800         05  FILLER           PIC X.                                      
053900     EJECT                                                                
054000*01  -COPY W0008     -PRE SAPA-                                           
054100         05  FILLER           PIC X.                                      
054200*01  -COPY W0008     -PRE WFILB-                                          
054300         05  FILLER           PIC X.                                      
054600*01  -COPY W0008     -PRE WDB6-                                           
054700     05  FILLER               PIC X.                                      
054800                                                                          
054900     EJECT                                                                
055000 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
055100               INVREG-PCB WDK6-PCB                                        
055200               4505-PCB XXEF-PCB                                          
055300               INLE-PCB ARTD-PCB BEN-PCB ARTS-PCB INLC-PCB                
055400               INVC-PCB LOGA-PCB SAPA-PCB WFILB-PCB                       
055500               WDB6-PCB.                                                  
055600 MAIN SECTION.                                                            
055700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
055800               INVREG-PCB WDK6-PCB                                        
055900               4505-PCB XXEF-PCB                                          
056000               INLE-PCB ARTD-PCB BEN-PCB ARTS-PCB INLC-PCB                
056100               INVC-PCB LOGA-PCB SAPA-PCB WFILB-PCB                       
056200               WDB6-PCB.                                                  
056300                                                                          
056400     PERFORM IMS-GET-MSG                                                  
056500     IF SEGMENT-FINNS                                                     
056600       ACCEPT DAGENS-DAT FROM DATE                                        
056700       ACCEPT DAGENS-DAT-WDH111 FROM DATE                                 
056800       PERFORM A-KOLLA-NYCKLAR                                            
056900       IF ARTIKEL-DC-RETT = NEJ                                           
057000         PERFORM G-ARTIKEL-DC-SAKNAS                                      
057100       ELSE                                                               
057200         IF MFS-UPDATE                                                    
057300           PERFORM I-INFORMATIONSBILD                                     
057400           IF ARTIKEL-DC-FINNS = JA                                       
057500             IF UPPDATERINGSRAD-IFYLLD = JA                               
057600               PERFORM J-UPPDATERINGSRAD-GODKEND                          
057700                                                                          
057800               IF UPPDATERINGSRAD-GODKEND = JA                            
057900                 PERFORM B-UPPDATERING                                    
058000                                                                          
058100                 IF MID-FLANTAL = '1' OR MID-FLFLYTTN = '1'               
058200                   CONTINUE                                               
058300                 ELSE                                                     
058400                   PERFORM H-RENSA-WDH1                                   
058500                 END-IF                                                   
058600                                                                          
058700                 IF MID-FLFLYTTN NOT = '1'                                
058800                   IF MID-FLSLACK = SPACE                                 
058900                     PERFORM E-SKAPA-INV-SEGM                             
059000                   END-IF                                                 
059100                 END-IF                                                   
059200                                                                          
059300                 PERFORM F-SKAPA-UPPDATERA-WDK7-POST                      
059400                                                                          
059500                 PERFORM I-INFORMATIONSBILD                               
059600               END-IF                                                     
059700             END-IF                                                       
059800           END-IF                                                         
059900         ELSE                                                             
060000           PERFORM I-INFORMATIONSBILD                                     
060100           IF ARTIKEL-DC-FINNS = JA                                       
060200             IF UPPDATERINGSRAD-IFYLLD = JA                               
060300               PERFORM J-UPPDATERINGSRAD-GODKEND                          
060400                                                                          
060500               IF UPPDATERINGSRAD-GODKEND = JA                            
060600                 PERFORM K-UPPDATERINGSBILD                               
060700               END-IF                                                     
060800             END-IF                                                       
060900           ELSE                                                           
061000             PERFORM G-ARTIKEL-DC-SAKNAS                                  
061100           END-IF                                                         
061200         END-IF                                                           
061300       END-IF                                                             
061400                                                                          
061500       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O10601 + 4                      
061600       PERFORM IMS-INSERT-MSG                                             
061700     END-IF                                                               
061800                                                                          
061900     MOVE ZERO TO RETURN-CODE                                             
062000     GOBACK                                                               
062100     .                                                                    
062200     EJECT                                                                
062300 A-KOLLA-NYCKLAR   SECTION.                                               
062400                                                                          
062500     IF MSG-DUBBLA-TRANSKODER                                             
062600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10601                 
062700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
062800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
062900     ELSE                                                                 
063000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I10601                  
063100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
063200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
063300     END-IF                                                               
063400     MOVE MSG-KDTRTYP    TO MFS-KDTRTYP                                   
063500                                                                          
063600     IF MID-KVJUSTKV-IN = SPACE AND                                       
063700        MID-KDAVVTYP    = SPACE AND                                       
063800        MID-FLANTAL     = SPACE AND                                       
063900        MID-FLSLACK     = SPACE AND                                       
064000        MID-FLFLYTTN    = SPACE                                           
064100                                                                          
064200       MOVE NEJ TO UPPDATERINGSRAD-IFYLLD                                 
064300     ELSE                                                                 
064400       MOVE JA  TO UPPDATERINGSRAD-IFYLLD                                 
064500     END-IF                                                               
064600                                                                          
064700                                                                          
064800     IF MFS-IDTRANS NOT = '5106'                                          
064900       MOVE NEJ TO UPPDATERINGSRAD-IFYLLD                                 
065000                   EGEN-BILD                                              
065100     END-IF                                                               
065200                                                                          
065300     MOVE ALL '+'              TO MSGI-WMSGINIT                           
065400     MOVE '001'                TO MSGI-KDCALL                             
065500     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
065600     MOVE '5106'               TO MSGI-IDTRANS                            
065700     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
065800     IF MFS-IDTRANS = '5106'                                              
065900     OR (MID-IDARTNR-IN NUMERIC                                           
066000     AND MID-IDARTNR-IN > ZERO)                                           
066100         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
066200     END-IF                                                               
066300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
066400     MOVE MSGI-IDARTNR         TO IDARTNR-WS                              
066500     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
066600                                                                          
066700     IF MID-IDARTNR-IN = ALL '+'                                          
066800       CONTINUE                                                           
066900     ELSE                                                                 
067000       MOVE NEJ TO UPPDATERINGSRAD-IFYLLD                                 
067100     END-IF                                                               
067200                                                                          
067300     MOVE JA          TO ARTIKEL-DC-RETT                                  
067400     IF IDARTNR-WS NUMERIC                                                
067500       IF IDARTNR-WS = ZERO                                               
067600         MOVE NEJ     TO ARTIKEL-DC-RETT                                  
067700       END-IF                                                             
067800     ELSE                                                                 
067900       MOVE NEJ       TO ARTIKEL-DC-RETT                                  
068000     END-IF                                                               
068100                                                                          
068200     MOVE MSGI-IDRT-KEY TO W-IDRT-KEY                                     
068300     MOVE MSGI-IDDC   TO W-IDDC-B6                                        
068400                                                                          
068500     MOVE JA          TO WDB6-SW                                          
068600     PERFORM IMS-GU-WDB601                                                
068700     IF SEGMENT-SAKNAS                                                    
068800        MOVE NEJ      TO WDB6-SW                                          
068900     END-IF                                                               
069000                                                                          
069100     IF DCS-CDC-TR                                                        
069200       MOVE NEJ       TO ARTIKEL-DC-RETT                                  
069300     END-IF                                                               
069400                                                                          
069500     IF NOT DCS-CDC AND MID-FLANTAL = '1'                                 
069600       MOVE NEJ       TO ARTIKEL-DC-RETT                                  
069700     END-IF                                                               
069800                                                                          
069900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
070000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
070100                                                                          
070200     MOVE LOW-VALUE   TO MSG-AREA                                         
070300     MOVE 'W5O106N1'  TO MFS-IDMOD                                        
070400     MOVE '5106'      TO MOD-IDTRANS                                      
070500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
070600                                                                          
070700     IF SWEDISH-TEXT                                                      
070800       MOVE 1 TO W-KDSPRAK                                                
070900     ELSE                                                                 
071000       MOVE 2 TO W-KDSPRAK                                                
071100     END-IF                                                               
071200                                                                          
071300     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
071400     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
071500     MOVE DCS-IDDC   TO MOD-IDDC-UT                                       
071600                                                                          
071700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
071800     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
071900                                                                          
072000     IF MFS-UPDATE                                                        
072100       IF MID-KVJUSTKV-IN = SPACE AND                                     
072200          MID-KDAVVTYP = SPACE AND                                        
072300          MID-FLANTAL = SPACE AND                                         
072400          MID-FLSLACK = SPACE AND                                         
072500          MID-FLFLYTTN = SPACE                                            
072600                                                                          
072700         MOVE NEJ TO UPPDATERINGSRAD-IFYLLD                               
072800       ELSE                                                               
072900         IF MID-FLANTAL = SPACE AND                                       
073000            MID-FLSLACK = SPACE AND                                       
073100            MID-FLFLYTTN = SPACE                                          
073200            MOVE MID-KVJUSTKV-IN TO W-KVJUSTKV-IN-X                       
073300            INSPECT W-KVJUSTKV-IN-X                                       
073400            REPLACING LEADING SPACE BY ZERO                               
073500            IF W-KVJUSTKV-IN-N = ZERO                                     
073600              MOVE JA  TO UPPDATERINGSRAD-IFYLLD                          
073700            ELSE                                                          
073800              MOVE MEDDELANDE-10 (W-KDSPRAK) TO MOD-TEMFSFEL              
073900*             MOVE MEDDELANDE-11 (W-KDSPRAK) TO MOD-TEMFSINF              
074000* - - - - - - - - KDTRTYP = MFS-UPDATE SOM HÄR BLANKAS UT                 
074100              IF MID-KOLLA-PF11-DOLD NOT = JA                             
074200                MOVE JA TO MOD-KOLLA-PF11-DOLD                            
074300                MOVE SPACE TO MFS-KDTRTYP                                 
074400                MOVE JA  TO UPPDATERINGSRAD-IFYLLD                        
074500              ELSE                                                        
074600                MOVE NEJ TO MOD-KOLLA-PF11-DOLD                           
074700                MOVE SPACE TO MOD-TEMFSFEL                                
074800                              MOD-TEMFSINF                                
074900              END-IF                                                      
075000            END-IF                                                        
075100         END-IF                                                           
075200       END-IF                                                             
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600*****************************************************************         
075700*    WDK6-ARTREG UPPDATERAS OCH RESULTATET LÄGGS UT PÅ SKÄRMEN            
075800*****************************************************************         
075900                                                                          
076000 B-UPPDATERING SECTION.                                                   
076100     MOVE 'B-UPPDAT' TO WS-SECTION                                        
076200     MOVE IDARTNR-WS TO W-IDARTNR                                         
076300                        W-IDARTNR-UTR                                     
076400     MOVE DCS-IDDC   TO W-IDDC                                            
076500                        W-IDDC-WDH1-MIN                                   
076600                        W-IDDC-WDH1-MAX                                   
076700                        W1-IDDC-WDH1                                      
076800                        W2-IDDC-WDH1                                      
076900                        W-IDDC-UTR                                        
077000                                                                          
077100     MOVE MID-KVJUSTKV-IN TO W-KVJUSTKV-IN-X                              
077200     INSPECT W-KVJUSTKV-IN-X REPLACING LEADING SPACE BY ZERO              
077300     MOVE W-KVJUSTKV-IN-N TO W-KVJUSTKV-COMP                              
077400                                                                          
077500     MOVE MID-KVUTRS-LAGR TO W-KVUTRS-X                                   
077600     INSPECT W-KVUTRS-X REPLACING LEADING SPACE BY ZERO                   
077700                                                                          
077800     IF W-KVUTRS-TECKEN = '-'                                             
077900       MULTIPLY W-KVUTRS-N BY -1 GIVING W-KVUTRS-COMP                     
078000     ELSE                                                                 
078100       MOVE W-KVUTRS-N TO W-KVUTRS-COMP                                   
078200     END-IF                                                               
078300                                                                          
078400     MOVE MID-KVLS-LAGR TO W-KVLS-X                                       
078500     INSPECT W-KVLS-X REPLACING LEADING SPACE BY ZERO                     
078600                                                                          
078700     IF W-KVLS-TECKEN = '-'                                               
078800       MULTIPLY W-KVLS-N BY -1 GIVING W-KVLS-COMP                         
078900     ELSE                                                                 
079000       MOVE W-KVLS-N TO W-KVLS-COMP                                       
079100     END-IF                                                               
079200                                                                          
079300     PERFORM IMS-LAES-ARTIKEL-6                                           
079400                                                                          
079500**1***  FLSLACK = '1',  DVS SLÄCKNING, BORTTAG                            
079600     IF MID-FLSLACK = '1'                                                 
079700       IF DCS-CDC                                                         
079800         PERFORM IMS-LAES-EKONOMISEG                                      
079900         MOVE +0                TO CLAG-KVUTRS                            
080000         PERFORM IMS-REPL-CLAGER-SEGM                                     
080100       ELSE                                                               
080200         PERFORM IMS-GHU-WDK711                                           
080300         MOVE +0                  TO SLAG-KVUTRS                          
080400         PERFORM IMS-REPL-SLAGER-SEGM                                     
080500       END-IF                                                             
080600       PERFORM BC-RADERA-G2-UTREDNSALDO                                   
080700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                      
080800     ELSE                                                                 
080900                                                                          
081000**2***  FLFLYTTN = '1',  DVS UTREDNINGSSALDO-UPPD.                        
081100       IF MID-FLFLYTTN = '1'                                              
081200         IF DCS-CDC                                                       
081300           PERFORM IMS-LAES-EKONOMISEG                                    
081400                                                                          
081500           IF MID-KDAVVTYP = '+'                                          
081600             COMPUTE W-SUMMA = CLAG-KVUTRS + W-KVJUSTKV-COMP              
081700           ELSE                                                           
081800             COMPUTE W-SUMMA = CLAG-KVUTRS - W-KVJUSTKV-COMP              
081900           END-IF                                                         
082000                                                                          
082100           MOVE W-SUMMA TO CLAG-KVUTRS                                    
082200           PERFORM IMS-REPL-CLAGER-SEGM                                   
082300                                                                          
082400           IF CLAG-KVUTRS = +0                                            
082500             PERFORM BC-RADERA-G2-UTREDNSALDO                             
082600           END-IF                                                         
082700                                                                          
082800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                  
082900           IF MID-KDAVVTYP = '-'                                          
083000              PERFORM BE-TAECKNING-CDC                                    
083100           END-IF                                                         
083200         ELSE                                                             
083300           PERFORM IMS-GHU-WDK711                                         
083400                                                                          
083500           IF MID-KDAVVTYP = '+'                                          
083600             COMPUTE W-SUMMA = SLAG-KVUTRS + W-KVJUSTKV-COMP              
083700           ELSE                                                           
083800             COMPUTE W-SUMMA = SLAG-KVUTRS - W-KVJUSTKV-COMP              
083900           END-IF                                                         
084000                                                                          
084100           MOVE W-SUMMA TO SLAG-KVUTRS                                    
084200           PERFORM IMS-REPL-SLAGER-SEGM                                   
084300                                                                          
084400           IF SLAG-KVUTRS = +0                                            
084500             PERFORM BC-RADERA-G2-UTREDNSALDO                             
084600           END-IF                                                         
084700                                                                          
084800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                  
084900           IF MID-KDAVVTYP = '-'                                          
085000             IF DCS-NDC                                                   
085100               PERFORM BF-TAECKNING-NDC                                   
085200             END-IF                                                       
085300           END-IF                                                         
085400         END-IF                                                           
085500       ELSE                                                               
085600                                                                          
085700**3***  FLANTAL = '1'  DVS TYP6 JUSTERING,                                
085800*       ENDAST TILLÅTEN FÖR CDC                                           
085900         IF MID-FLANTAL = '1'                                             
086000           IF DCS-CDC                                                     
086100             PERFORM IMS-LAES-EKONOMISEG                                  
086200                                                                          
086300             IF MID-KDAVVTYP = '+'                                        
086400               COMPUTE W-SUMMA = CLAG-KVLS + W-KVJUSTKV-COMP              
086500             ELSE                                                         
086600               COMPUTE W-SUMMA = CLAG-KVLS - W-KVJUSTKV-COMP              
086700             END-IF                                                       
086800                                                                          
086900             MOVE W-SUMMA     TO CLAG-KVLS                                
087000             PERFORM IMS-REPL-CLAGER-SEGM                                 
087100             PERFORM BG-FLYTTA-LOGG-WDK6                                  
087200             PERFORM BI-UPPDATERA-LOGG                                    
087300                                                                          
087400             IF CLAG-KVUTRS = +0                                          
087500               PERFORM BC-RADERA-G2-UTREDNSALDO                           
087600             END-IF                                                       
087700                                                                          
087800             IF MID-KDAVVTYP = '+'                                        
087900               PERFORM BE-TAECKNING-CDC                                   
088000             END-IF                                                       
088100                                                                          
088200             MOVE SPAR-PRARTSTD         TO INVH-PRARTSTD                  
088300             COMPUTE WS-KVANTAL =                                         
088400               CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                   
088500             PERFORM S10-DATUM                                            
088600             PERFORM BD-ISRT-INVENTERINGSHISTORIK                         
088700             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVLS-ATTR                  
088800                                          MOD-TIJUSTDA-ATTR   (1)         
088900                                          MOD-KVJUSTKV-UT-ATTR(1)         
089000                                          MOD-KDJUSTYP        (1)         
089100           END-IF                                                         
089200         ELSE                                                             
089300                                                                          
089400**4***  ÖVRIGT                                                            
089500           MOVE NEJ                   TO ART-INV-FINNS                    
089600           PERFORM IMS-LAES-INV-ROT                                       
089700           IF SEGMENT-FINNS                                               
089800             MOVE DCS-IDDC            TO W1-IDDC-WDH1                     
089900                                         W2-IDDC-WDH1                     
090000                                         W-IDDC-UNIK                      
090100             MOVE 01                  TO W1-KDINVKAT                      
090200             MOVE 11                  TO W2-KDINVKAT                      
090300                                                                          
090400             MOVE NEJ                 TO INV-KAT-FINNS                    
090500             PERFORM IMS-LAES-ARTIKEL-INV                                 
090600             PERFORM UNTIL INVKOE-ART-FINNS = NEJ                         
090700               IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9          
090800                 MOVE JA              TO INV-KAT-FINNS                    
090900                                         ART-INV-FINNS                    
091000               END-IF                                                     
091100               PERFORM IMS-LAES-ARTIKEL-INV                               
091200             END-PERFORM                                                  
091300                                                                          
091400             IF INV-KAT-FINNS = JA                                        
091500               MOVE INVB-INV-KDINVKAT     TO KDINVKAT-WS                  
091600                                             W-KDINVKAT-UNIK              
091700               MOVE INVB-INV-TISEGKEY     TO WS-FIX-TISEGKEY              
091800                                             W-TISEGKEY-UNIK              
091900               MOVE WS-TISEGKEY-3-8       TO MOD-TIM-INV                  
092000               MOVE INVB-INV-DAREGDAT-SORT TO                             
092100                                      W-DAREGDAT-SORT-UNIK                
092200               MOVE INVB-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE              
092300               MOVE INVB-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1              
092400               MOVE INVB-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2              
092500               MOVE INVB-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3              
092600                                                                          
092700               PERFORM IMS-LAES-WDH111-UNIK                               
092800               IF SEGMENT-FINNS                                           
092900                 PERFORM IMS-LAES-WDH121                                  
093000                 IF SEGMENT-FINNS                                         
093100                   PERFORM UNTIL SEGMENT-SAKNAS                           
093200                   IF INVB-INVL-KDSEGKEY = '0'                            
093300                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-CRE           
093400                   END-IF                                                 
093500                   IF INVB-INVL-KDSEGKEY = '1'                            
093600                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR1           
093700                   END-IF                                                 
093800                   IF INVB-INVL-KDSEGKEY = '2'                            
093900                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR2           
094000                   END-IF                                                 
094100                   IF INVB-INVL-KDSEGKEY = '3'                            
094200                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR3           
094300                   END-IF                                                 
094400                   PERFORM IMS-LAES-WDH121                                
094500                   END-PERFORM                                            
094600                 END-IF                                                   
094700               END-IF                                                     
094800             ELSE                                                         
094900               PERFORM IMS-LAES-INV-ROT                                   
095000                                                                          
095100               MOVE DCS-IDDC            TO W-IDDC-WDH1-MIN                
095200                                           W-IDDC-WDH1-MAX                
095300                                           W-IDDC-UNIK                    
095400               MOVE +2                  TO W-KDINVKAT-MIN                 
095500                                           W-KDINVKAT-MAX                 
095600                                                                          
095700               PERFORM IMS-LAES-INV                                       
095800               IF SEGMENT-FINNS                                           
095900                 MOVE INVB-INV-KDINVKAT     TO KDINVKAT-WS                
096000                                               W-KDINVKAT-UNIK            
096100                 MOVE INVB-INV-TISEGKEY     TO WS-FIX-TISEGKEY            
096200                                               W-TISEGKEY-UNIK            
096300                 MOVE WS-TISEGKEY-3-8       TO MOD-TIM-INV                
096400                 MOVE JA                    TO ART-INV-FINNS              
096500                 MOVE INVB-INV-DAREGDAT-SORT TO                           
096600                                          W-DAREGDAT-SORT-UNIK            
096700                 MOVE INVB-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE            
096800                 MOVE INVB-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1            
096900                 MOVE INVB-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2            
097000                 MOVE INVB-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3            
097100                 PERFORM IMS-LAES-WDH111-UNIK                             
097200                 PERFORM IMS-LAES-WDH121                                  
097300                 PERFORM UNTIL SEGMENT-SAKNAS                             
097400                   IF INVB-INVL-IDUSER = '0'                              
097500                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-CRE           
097600                   END-IF                                                 
097700                   IF INVB-INVL-IDUSER = '1'                              
097800                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR1           
097900                   END-IF                                                 
098000                   IF INVB-INVL-IDUSER = '2'                              
098100                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR2           
098200                   END-IF                                                 
098300                   IF INVB-INVL-IDUSER = '3'                              
098400                     MOVE INVB-INVL-IDUSER     TO WS-IDUSER-PR3           
098500                   END-IF                                                 
098600                 PERFORM IMS-LAES-WDH121                                  
098700                 END-PERFORM                                              
098800               END-IF                                                     
098900             END-IF                                                       
099000           END-IF                                                         
099100                                                                          
099200           IF DCS-CDC                                                     
099300             PERFORM IMS-LAES-EKONOMISEG                                  
099400                                                                          
099500             IF MID-KDAVVTYP = '+'                                        
099600               COMPUTE W-SUMMA = CLAG-KVLS + W-KVJUSTKV-COMP              
099700               MOVE W-KVJUSTKV-COMP TO CLAG-KVINVS                        
099800               PERFORM BE-TAECKNING-CDC                                   
099900             ELSE                                                         
100000               COMPUTE W-SUMMA = CLAG-KVLS - W-KVJUSTKV-COMP              
100100               COMPUTE CLAG-KVINVS = W-KVJUSTKV-COMP * -1                 
100200             END-IF                                                       
100300                                                                          
100400             MOVE +0                TO CLAG-KVUTRS                        
100500             MOVE W-SUMMA           TO CLAG-KVLS                          
100600             PERFORM BG-FLYTTA-LOGG-WDK6                                  
100700                                                                          
100800             PERFORM BC-RADERA-G2-UTREDNSALDO                             
100900                                                                          
101000             PERFORM S10-DATUM                                            
101100             MOVE DAT-TIAAVVD       TO CLAG-TIINVDAT                      
101200             PERFORM IMS-REPL-CLAGER-SEGM                                 
101300                                                                          
101400**** OM DET ÄR EN REFILLARTIKEL SÅ SKALL FLAGGA SÄTTAS TILL N             
101500             PERFORM IMS-GHU-WDK629                                       
101600             IF SEGMENT-FINNS                                             
101700               IF CREF-FLREFNYO = NEJ                                     
101800                 CONTINUE                                                 
101900               ELSE                                                       
102000                 MOVE NEJ           TO CREF-FLREFNYO                      
102100                 PERFORM IMS-REPL-WDK629                                  
102200               END-IF                                                     
102300             END-IF                                                       
102400                                                                          
102500             MOVE SPAR-PRARTSTD     TO INVH-PRARTSTD                      
102600             COMPUTE WS-ANTAL =                                           
102700              CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                    
102800                                                                          
102900           ELSE                                                           
103000             PERFORM IMS-GHU-WDK711                                       
103100                                                                          
103200             MOVE +0 TO SLAG-KVUTRS                                       
103300                                                                          
103400             IF MID-KDAVVTYP = '+'                                        
103500               COMPUTE W-SUMMA = SLAG-KVLS + W-KVJUSTKV-COMP              
103600             ELSE                                                         
103700               COMPUTE W-SUMMA = SLAG-KVLS - W-KVJUSTKV-COMP              
103800             END-IF                                                       
103900                                                                          
104000             MOVE W-SUMMA           TO SLAG-KVLS                          
104100             PERFORM BC-RADERA-G2-UTREDNSALDO                             
104200                                                                          
104300             IF MID-KDAVVTYP = '+'                                        
104400               IF DCS-NDC                                                 
104500                 PERFORM BF-TAECKNING-NDC                                 
104600               END-IF                                                     
104700             END-IF                                                       
104800                                                                          
104900             IF MID-KDAVVTYP = '-'                                        
105000               COMPUTE SLAG-KVINVS = W-KVJUSTKV-COMP * -1                 
105100             ELSE                                                         
105200               MOVE W-KVJUSTKV-COMP TO SLAG-KVINVS                        
105300             END-IF                                                       
105400                                                                          
105500             PERFORM S10-DATUM                                            
105600             MOVE DAT-TIAAVVD       TO SLAG-TIINVDAT                      
105700             PERFORM IMS-REPL-SLAGER-SEGM                                 
105800             PERFORM BH-FLYTTA-LOGG-WDK7                                  
105900                                                                          
106000             MOVE ZERO              TO INVH-PRARTSTD                      
106100           END-IF                                                         
106200                                                                          
106300           PERFORM IMS-GHU-INVHIST-ROT                                    
106400           IF SEGMENT-SAKNAS                                              
106500             MOVE W-IDARTNR TO INVA-IDARTNR                               
106600             PERFORM IMS-ISRT-INVHIST-ROT                                 
106700           END-IF                                                         
106800                                                                          
106900                                                                          
107000           IF MID-KDAVVTYP = '-'                                          
107100             COMPUTE INVH-KVJUSTKV = W-KVJUSTKV-COMP * -1                 
107200           ELSE                                                           
107300             MOVE W-KVJUSTKV-COMP TO INVH-KVJUSTKV                        
107400           END-IF                                                         
107500                                                                          
107600           EVALUATE TRUE                                                  
107700             WHEN ART-INV-FINNS = NEJ                                     
107800                  MOVE +7 TO INVH-KDJUSTYP                                
107900                  MOVE '7' TO LOGG-UREF1                                  
108000             WHEN KDINVKAT-WS = +1                                        
108100                  MOVE +1 TO INVH-KDJUSTYP                                
108200                  MOVE '1' TO LOGG-UREF1                                  
108300             WHEN KDINVKAT-WS = +2                                        
108400                  MOVE +2 TO INVH-KDJUSTYP                                
108500                  MOVE '2' TO LOGG-UREF1                                  
108600             WHEN KDINVKAT-WS = +3                                        
108700                  MOVE +3 TO INVH-KDJUSTYP                                
108800                  MOVE '3' TO LOGG-UREF1                                  
108900             WHEN KDINVKAT-WS = +4                                        
109000                  MOVE +4 TO INVH-KDJUSTYP                                
109100                  MOVE '4' TO LOGG-UREF1                                  
109200             WHEN KDINVKAT-WS = +5                                        
109300                  MOVE +5 TO INVH-KDJUSTYP                                
109400                  MOVE '5' TO LOGG-UREF1                                  
109500             WHEN KDINVKAT-WS = +8                                        
109600                  MOVE +8 TO INVH-KDJUSTYP                                
109700                  MOVE '8' TO LOGG-UREF1                                  
109800             WHEN KDINVKAT-WS = +9                                        
109900                  MOVE +9 TO INVH-KDJUSTYP                                
110000                  MOVE '9' TO LOGG-UREF1                                  
110100             WHEN OTHER                                                   
110200                  MOVE 'FELAKTIG KATEGORI = KDINVKAT-WS'                  
110300                          TO FELTEXT                                      
110400                  CALL FELLOG                                             
110500           END-EVALUATE                                                   
110600                                                                          
110700           MOVE NEJ                 TO INVH-FLAUTLSJ                      
110800           MOVE SPACE               TO INVH-IDPW                          
110900                                                                          
111000           MOVE DAGENS-DATUM        TO INVH-DAREGDAT-CLO                  
111100           MOVE DCS-IDDC            TO INVH-IDDC                          
111200           MOVE MSGI-IDUSER         TO INVH-IDUSER-CLO                    
111300                                                                          
111400           MOVE DAGENS-DATUM        TO INVH-DAREGDAT-CRE                  
111500           MOVE WS-DAREGDAT-PR1     TO INVH-DAREGDAT-PR1                  
111600           MOVE WS-DAREGDAT-PR2     TO INVH-DAREGDAT-PR2                  
111700           MOVE WS-DAREGDAT-PR3     TO INVH-DAREGDAT-PR3                  
111800           MOVE WS-IDUSER-PR1       TO INVH-IDUSER-PR1                    
111900           MOVE WS-IDUSER-PR2       TO INVH-IDUSER-PR2                    
112000           MOVE WS-IDUSER-PR3       TO INVH-IDUSER-PR3                    
112100           MOVE MSGI-IDUSER         TO INVH-IDUSER-CRE                    
112200           PERFORM BI-UPPDATERA-LOGG                                      
112300           PERFORM S02-SKAPA-TISEGKEY                                     
112400           IF DCS-CDC                                                     
112500             MOVE WS-ANTAL          TO INVH-KVANTAL                       
112600           ELSE                                                           
112700             MOVE +0                TO INVH-KVANTAL                       
112800           END-IF                                                         
112900                                                                          
113000           PERFORM IMS-ISRT-INVHIST-SEGM                                  
113100           PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                          
113200             IF SEGMENT-FINNS-REDAN                                       
113300               SUBTRACT 1 FROM INVH-TISEGKEY                              
113400               PERFORM IMS-ISRT-INVHIST-SEGM                              
113500             END-IF                                                       
113600           END-PERFORM                                                    
113700                                                                          
113800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                  
113900                                         MOD-KVLS-ATTR                    
114000                                         MOD-TIJUSTDA-ATTR   (1)          
114100                                         MOD-KVJUSTKV-UT-ATTR(1)          
114200                                         MOD-KDJUSTYP-ATTR   (1)          
114300**4SLUT***                                                                
114400         END-IF                                                           
114500**3SLUT***                                                                
114600       END-IF                                                             
114700**2SLUT***                                                                
114800     END-IF                                                               
114900                                                                          
115000     MOVE MEDDELANDE-8 (W-KDSPRAK) TO MOD-TEMFSINF                        
115100                                                                          
115200     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-ENG                              
115300                               MOD-PRARTSTD                               
115400                               MOD-TIAAVVD                                
115500                               MOD-KVAVIS                                 
115600                               MOD-KDERS                                  
115700                               MOD-KDINVKAT                               
115800                               MOD-TIM-INV                                
115900                               MOD-SALDO-KVBUFF-OF                        
116000                               MOD-SALDO-KVBUFF-F                         
116100                               MOD-KVAKS                                  
116200                               MOD-KVLS                                   
116300                               MOD-KVEFRS                                 
116400                               MOD-KVROS                                  
116500                               MOD-KVUTRS                                 
116600                               MOD-KVLS-LAGR                              
116700                               MOD-KVUTRS-LAGR                            
116800                                                                          
116900     IF MID-FLSLACK = '1'                                                 
117000       IF DCS-CDC                                                         
117100         MOVE CLAG-KVUTRS    TO MOD-KVUTRS                                
117200         MOVE MOD-KVUTRS-X   TO MOD-KVUTRS-LAGR                           
117300       ELSE                                                               
117400         MOVE SLAG-KVUTRS    TO MOD-KVUTRS                                
117500         MOVE MOD-KVUTRS-X   TO MOD-KVUTRS-LAGR                           
117600         MOVE 'AVERAGE COST' TO MOD-HEADING                               
117700         MOVE SLAG-PRAVCOST  TO MOD-PRAVCOST                              
117800       END-IF                                                             
117900     END-IF                                                               
118000                                                                          
118100     IF MID-FLSLACK = '1' OR MID-FLANTAL = '1'                            
118200       CONTINUE                                                           
118300     ELSE                                                                 
118400       MOVE MFS-RENSA-FAELT TO MOD-KDINVKAT                               
118500       MOVE MFS-RENSA-FAELT TO MOD-TIM-INV                                
118600     END-IF                                                               
118700                                                                          
118800     MOVE +1 TO INDX                                                      
118900                                                                          
119000     PERFORM UNTIL INDX > +6                                              
119100       MOVE MFS-ROER-EJ-FAELT TO MOD-TIJUSTDA-X (INDX)                    
119200                                 MOD-KVJUSTKV-UT-X (INDX)                 
119300                                 MOD-KDJUSTYP-X (INDX)                    
119400                                                                          
119500       ADD +1 TO INDX                                                     
119600     END-PERFORM                                                          
119700                                                                          
119800     MOVE MFS-RENSA-FAELT TO MOD-KVJUSTKV-IN                              
119900                             MOD-KDAVVTYP                                 
120000                             MOD-FLANTAL                                  
120100                             MOD-FLSLACK                                  
120200                             MOD-FLFLYTTN                                 
120300     .                                                                    
120400                                                                          
120500     EJECT                                                                
120600 BC-RADERA-G2-UTREDNSALDO SECTION.                                        
120700     MOVE 'BC      ' TO WS-SECTION                                        
120800     PERFORM IMS-GHU-G2-UTREDNSALDO                                       
120900                                                                          
121000     IF SEGMENT-FINNS                                                     
121100       PERFORM IMS-DELETE-G2-UTREDNSALDO                                  
121200     END-IF                                                               
121300     .                                                                    
121400                                                                          
121500     EJECT                                                                
121600 BD-ISRT-INVENTERINGSHISTORIK SECTION.                                    
121700     MOVE 'BD      ' TO WS-SECTION                                        
121800     PERFORM IMS-GHU-INVHIST-ROT                                          
121900     IF SEGMENT-SAKNAS                                                    
122000       MOVE W-IDARTNR TO INVA-IDARTNR                                     
122100       PERFORM IMS-ISRT-INVHIST-ROT                                       
122200     END-IF                                                               
122300                                                                          
122400     MOVE DAGENS-DATUM        TO INVH-DAREGDAT-CLO                        
122500                                                                          
122600     PERFORM S02-SKAPA-TISEGKEY                                           
122700                                                                          
122800     IF MID-KDAVVTYP = '-'                                                
122900       COMPUTE INVH-KVJUSTKV = W-KVJUSTKV-COMP * -1                       
123000     ELSE                                                                 
123100       MOVE W-KVJUSTKV-COMP TO INVH-KVJUSTKV                              
123200     END-IF                                                               
123300                                                                          
123400     MOVE DCS-IDDC            TO INVH-IDDC                                
123500     MOVE 6                   TO INVH-KDJUSTYP                            
123600     MOVE '6'                 TO LOGG-UREF1                               
123700     MOVE MSGI-IDUSER         TO INVH-IDUSER-CLO                          
123800     MOVE NEJ                 TO INVH-FLAUTLSJ                            
123900     MOVE SPACE               TO INVH-IDPW                                
124000     IF WS-DAREGDAT-CRE = ZERO                                            
124100       MOVE DAGENS-DATUM(3:6) TO INVH-DAREGDAT-CRE                        
124200     ELSE                                                                 
124300       MOVE WS-DAREGDAT-CRE   TO INVH-DAREGDAT-CRE                        
124400     END-IF                                                               
124500     MOVE WS-DAREGDAT-PR1     TO INVH-DAREGDAT-PR1                        
124600     MOVE WS-DAREGDAT-PR2     TO INVH-DAREGDAT-PR2                        
124700     MOVE WS-DAREGDAT-PR3     TO INVH-DAREGDAT-PR3                        
124800     MOVE WS-IDUSER-PR1       TO INVH-IDUSER-PR1                          
124900     MOVE WS-IDUSER-PR2       TO INVH-IDUSER-PR2                          
125000     MOVE WS-IDUSER-PR3       TO INVH-IDUSER-PR3                          
125100     IF WS-IDUSER-CRE = SPACE                                             
125200       MOVE MSGI-IDUSER       TO INVH-IDUSER-CRE                          
125300     ELSE                                                                 
125400       MOVE WS-IDUSER-CRE     TO INVH-IDUSER-CRE                          
125500     END-IF                                                               
125600                                                                          
125700     IF DCS-CDC                                                           
125800       MOVE WS-KVANTAL        TO INVH-KVANTAL                             
125900     ELSE                                                                 
126000       MOVE +0                TO INVH-KVANTAL                             
126100     END-IF                                                               
126200                                                                          
126300     PERFORM IMS-ISRT-INVHIST-SEGM                                        
126400                                                                          
126500     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
126600       IF SEGMENT-FINNS-REDAN                                             
126700         SUBTRACT 1 FROM INVH-TISEGKEY                                    
126800         PERFORM IMS-ISRT-INVHIST-SEGM                                    
126900       END-IF                                                             
127000     END-PERFORM                                                          
127100     .                                                                    
127200     EJECT                                                                
127300 BE-TAECKNING-CDC SECTION.                                                
127400     MOVE 'BE      ' TO WS-SECTION                                        
127500     MOVE W-IDARTNR    TO 4506-IDARTNR                                    
127600     MOVE 11           TO 4506-KDTAKORS                                   
127700     MOVE ZERO         TO 4506-KVANTMOT                                   
127800     MOVE DCS-IDDC     TO W-IDDC-4505                                     
127900     PERFORM IMS-ISRT-450511                                              
128000     .                                                                    
128100     EJECT                                                                
128200 BF-TAECKNING-NDC SECTION.                                                
128300     MOVE 'BF      ' TO WS-SECTION                                        
128400     PERFORM IMS-GU-ARTIKEL-6                                             
128500     PERFORM IMS-GNP-EKONOMISEG                                           
128600                                                                          
128700     MOVE W-IDARTNR    TO 4506-IDARTNR                                    
128800     MOVE 11           TO 4506-KDTAKORS                                   
128900     MOVE ZERO         TO 4506-KVANTMOT                                   
129000     MOVE DCS-IDDC     TO W-IDDC-4505                                     
129100     PERFORM IMS-ISRT-450511                                              
129200     .                                                                    
129300     EJECT                                                                
129400                                                                          
129500 BG-FLYTTA-LOGG-WDK6 SECTION.                                             
129600     MOVE 'BG      ' TO WS-SECTION                                        
129700* LÄGGER UPP SALDOLOGG I WDL9                                             
129800     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
129900     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DAGENS-DATUM                   
130000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-DAGENS-DATUM             
130100     ACCEPT TRANS-TID FROM TIME                                           
130200     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
130300     MOVE 9                       TO LOGG-IDSEKVNR                        
130400     MOVE W-IDDC                  TO LOGG-IDDC                            
130500     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
130600     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
130700     MOVE IDPGM                   TO LOGG-IDPGM                           
130800     MOVE '5106'                  TO LOGG-IDTRANS                         
130900     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
131000     MOVE SPACE                   TO LOGG-REF                             
131100     IF MID-KDAVVTYP = '-'                                                
131200        MOVE '-'                  TO LOGG-IDTECKEN-KVLS                   
131300     ELSE                                                                 
131400        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
131500     END-IF                                                               
131600     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
131700     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
131800     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
131900     MOVE MID-KVJUSTKV-IN         TO LOGG-KVART-SALDO                     
132000     MOVE CLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
132100     MOVE CLAG-KVEFRS             TO LOGG-KVEFRS                          
132200     MOVE CLAG-KVLS               TO LOGG-KVLS                            
132300     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC +                                
132400                          CLAG-KVAKS-T                                    
132500     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
132600     .                                                                    
132700     EJECT                                                                
132800 BH-FLYTTA-LOGG-WDK7 SECTION.                                             
132900     MOVE 'BH      ' TO WS-SECTION                                        
133000* LÄGGER UPP SALDOLOGG I WDL9                                             
133100     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
133200     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DAGENS-DATUM                   
133300     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-DAGENS-DATUM             
133400     ACCEPT TRANS-TID FROM TIME                                           
133500     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
133600     MOVE 9                       TO LOGG-IDSEKVNR                        
133700     MOVE W-IDDC                  TO LOGG-IDDC                            
133800     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
133900     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
134000     MOVE IDPGM                   TO LOGG-IDPGM                           
134100     MOVE '5106'                  TO LOGG-IDTRANS                         
134200     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
134300     MOVE SPACE                   TO LOGG-REF                             
134400     IF MID-KDAVVTYP = '-'                                                
134500        MOVE '-'                  TO LOGG-IDTECKEN-KVLS                   
134600     ELSE                                                                 
134700        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
134800     END-IF                                                               
134900     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
135000     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
135100     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
135200     MOVE MID-KVJUSTKV-IN         TO LOGG-KVART-SALDO                     
135300     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
135400     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
135500     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
135600     MOVE SLAG-KVLS               TO LOGG-KVLS                            
135700     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
135800     .                                                                    
135900     EJECT                                                                
136000 BI-UPPDATERA-LOGG SECTION.                                               
136100     MOVE 'BI      ' TO WS-SECTION                                        
136200     PERFORM IMS-ISRT-WDL901                                              
136300     IF SEGMENT-FINNS-REDAN                                               
136400        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
136500          ADD -1 TO LOGG-IDSEKVNR                                         
136600          PERFORM IMS-ISRT-WDL901                                         
136700        END-PERFORM                                                       
136800     END-IF                                                               
136900     .                                                                    
137000     EJECT                                                                
137100 E-SKAPA-INV-SEGM SECTION.                                                
137200                                                                          
137300     MOVE 'E-SKAPA ' TO WS-SECTION                                        
137400*    INVENTERINGSSEGMENT SKAPAS PÅ WDH1                                   
137500                                                                          
137600     PERFORM IMS-LAES-INV-ROT                                             
137700     IF SEGMENT-SAKNAS                                                    
137800       MOVE W-IDARTNR     TO INVB-ART-IDARTNR                             
137900       PERFORM IMS-ISRT-INV-ROT                                           
138000     END-IF                                                               
138100                                                                          
138200     MOVE SPACE           TO INVB-WDH111                                  
138300     MOVE NEJ             TO INVB-INV-FLINVBEH                            
138400     MOVE NEJ             TO INVB-INV-FLINVSKR                            
138500     MOVE NEJ             TO INVB-INV-FLINV2B                             
138600     MOVE NEJ             TO INVB-INV-FLINV3E                             
138700     MOVE NEJ             TO INVB-INV-FLINV4N                             
138800                                                                          
138900     IF DCS-NDC                                                           
139000       MOVE NEJ           TO INVB-INV-FLINV2C                             
139100                             INVB-INV-FLINV2D                             
139200       MOVE JA            TO INVB-INV-FLINV4R                             
139300                             INVB-INV-FLINV4P                             
139400     ELSE                                                                 
139500       MOVE JA            TO INVB-INV-FLINV2C                             
139600                             INVB-INV-FLINV2D                             
139700       MOVE NEJ           TO INVB-INV-FLINV4R                             
139800                             INVB-INV-FLINV4P                             
139900     END-IF                                                               
140000                                                                          
140100     MOVE NEJ             TO INVB-INV-FLINV85                             
140200     MOVE SPACE           TO INVB-INV-FILLER1                             
140300                             INVB-INV-FILLER2                             
140400                                                                          
140500     PERFORM IMS-GET-ART-ROT                                              
140600     IF SEGMENT-FINNS                                                     
140700       MOVE ART-KDSORT TO WS-KDSORT                                       
140800       PERFORM IMS-GNP-EKONOMISEG                                         
140900       PERFORM EA-INVENTERING                                             
141000     END-IF                                                               
141100     .                                                                    
141200     EJECT                                                                
141300 EA-INVENTERING SECTION.                                                  
141400     MOVE 'EA      ' TO WS-SECTION                                        
141500     MOVE W-IDDC            TO INVB-INV-IDDC                              
141600     IF MID-KDAVVTYP = '-'                                                
141700       COMPUTE WS-KVJUSTKV-COMP = -1 * W-KVJUSTKV-IN-N                    
141800     ELSE                                                                 
141900       MOVE W-KVJUSTKV-IN-N TO WS-KVJUSTKV-COMP                           
142000     END-IF                                                               
142100     MOVE WS-KVJUSTKV-COMP  TO INVB-INV-KVJUSTKV                          
142200     MOVE SPACE             TO INVB-INV-TEINVANM                          
142300     IF MID-FLANTAL = '1'                                                 
142400       MOVE +6              TO INVB-INV-KDINVKAT                          
142500       MOVE +6              TO INVB-INV-KDINVKAT-OLD                      
142600     ELSE                                                                 
142700       MOVE +12             TO INVB-INV-KDINVKAT                          
142800       MOVE INVH-KDJUSTYP TO INVB-INV-KDINVKAT-OLD                        
142900     END-IF                                                               
143000                                                                          
143100     MOVE JA                TO INVB-INV-FLINVBEH                          
143200     MOVE ART-IDFKNGRP      TO INVB-INV-IDFKNGRP                          
143300     MOVE ZERO              TO INVB-INV-KDINVPRIO                         
143400     MOVE ART-KDPRODSL      TO INVB-INV-KDPRODSL                          
143500     MOVE CLAG-KDVVKL       TO INVB-INV-KDVVKL                            
143600     MOVE CLAG-KDPSLLOC     TO INVB-INV-KDPSLLOC                          
143700                                                                          
143800     IF NOT DCS-CDC                                                       
143900       PERFORM IMS-GU-WDK711                                              
144000       IF SEGMENT-FINNS                                                   
144100         MOVE SLAG-ADLAGOMR  TO INVB-INV-ADLAGOMR                         
144200         MOVE SLAG-ADGANG    TO INVB-INV-ADGANG                           
144300         MOVE SLAG-ADPLATS   TO INVB-INV-ADPLATS                          
144400         MOVE SLAG-PRAVCOST  TO W-PRAVCOST                                
144500       ELSE                                                               
144600         MOVE ZERO           TO INVB-INV-ADLAGOMR                         
144700         MOVE ZERO           TO INVB-INV-ADGANG                           
144800         MOVE ZERO           TO INVB-INV-ADPLATS                          
144900       END-IF                                                             
145000     ELSE                                                                 
145100       MOVE CLAG-ADLAGOMR    TO INVB-INV-ADLAGOMR                         
145200       MOVE CLAG-ADGANG      TO INVB-INV-ADGANG                           
145300       MOVE CLAG-ADPLATS     TO INVB-INV-ADPLATS                          
145400     END-IF                                                               
145500                                                                          
145600     IF DCS-CDC                                                           
145700       IF CLAG-KDERS = 11 OR 14 OR 17 OR 18 OR 19                         
145800         MOVE JA              TO INVB-INV-FLINV85                         
145900       END-IF                                                             
146000     END-IF                                                               
146100                                                                          
146200     MOVE 20                  TO WS-SEKEL                                 
146300     MOVE DAGENS-DAT-WDH111   TO WS-TIAAMMDD                              
146400     MOVE WS-SEKEL            TO WS-INV-SEKEL                             
146500     MOVE DAGENS-DAT-WDH111   TO WS-INV-AAMMDD                            
146600     MOVE WS-INV-DAREGDAT     TO INVB-INV-DAREGDAT-CRE                    
146700     MOVE WS-INV-DAREGDAT     TO INVB-INV-DAREGDAT                        
146800                                                                          
146900**   COMPUTE INVB-INV-DAREGDAT-SORT =                                     
147000**      99999999 - WS-INV-DAREGDAT                                        
147100     MOVE 99999999        TO  INVB-INV-DAREGDAT-SORT                      
147200     MOVE 0                   TO WS-LOPNR                                 
147300     COMPUTE WS-TISEGKEY = WS-TIAAAAMMDDL                                 
147400     MOVE WS-TISEGKEY         TO INVB-INV-TISEGKEY                        
147500     MOVE ZERO                TO INVB-INV-IDPRTOMG                        
147600                                 INVB-INV-IDLOPNR                         
147700                                 INVB-INV-KVAKS-OLD                       
147800                                 INVB-INV-KVEFRS-OLD                      
147900                                 INVB-INV-KVLS-OLD                        
148000                                 INVB-INV-DAREGDAT-PR1                    
148100                                 INVB-INV-DAREGDAT-PR2                    
148200                                 INVB-INV-DAREGDAT-PR3                    
148300                                                                          
148400     MOVE 'INSRT 1 PÅ WDH111' TO WS-SECTION                               
148500     PERFORM IMS-ISRT-INV-SEG                                             
148600     ADD +1 TO WS-ANTAL                                                   
148700     PERFORM UNTIL SEGMENT-FINNS                                          
148800       IF SEGMENT-FINNS-REDAN OR INDEX-FINNS-REDAN                        
148900         ADD 1                TO INVB-INV-TISEGKEY                        
149000         MOVE 'INSRT 2 PÅ WDH111' TO WS-SECTION                           
149100         ADD +1 TO WS-ANTAL                                               
149200         PERFORM IMS-ISRT-INV-SEG                                         
149300       END-IF                                                             
149400     END-PERFORM                                                          
149500     MOVE 'SKA GÖRA INSERT1PÅ WDH121' TO WS-SECTION                       
149600*** INSERT PÅ WDH121 SEGMENTET ***                                        
149700     MOVE MSGI-IDUSER TO INVB-INVL-IDUSER                                 
149800     MOVE '0'         TO INVB-INVL-KDSEGKEY                               
149900     PERFORM IMS-INSERT-WDH121                                            
150000                                                                          
150100     MOVE 'SKA GÖRA INSERT2PÅ WDH121' TO WS-SECTION                       
150200     MOVE SPACE       TO INVB-INVL-IDUSER                                 
150300     MOVE '1'         TO INVB-INVL-KDSEGKEY                               
150400     PERFORM IMS-INSERT-WDH121                                            
150500                                                                          
150600     MOVE 'SKA GÖRA INSERT3PÅ WDH121' TO WS-SECTION                       
150700     MOVE SPACE       TO INVB-INVL-IDUSER                                 
150800     MOVE '2'         TO INVB-INVL-KDSEGKEY                               
150900     PERFORM IMS-INSERT-WDH121                                            
151000                                                                          
151100     MOVE 'SKA GÖRA INSERT3PÅ WDH121' TO WS-SECTION                       
151200     MOVE SPACE       TO INVB-INVL-IDUSER                                 
151300     MOVE '3'         TO INVB-INVL-KDSEGKEY                               
151400     PERFORM IMS-INSERT-WDH121                                            
151500                                                                          
151600**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
151700                                                                          
151800     IF DCS-NDC-NA                                                        
151810       IF W-KVJUSTKV-IN-N NOT = 0 AND MID-FLANTAL NOT = '1'               
151820         PERFORM ED-FLYTTA-NDC-WDR8                                       
151830         PERFORM EDA-UPPDATERA-WDR8                                       
151840       END-IF                                                             
152300     ELSE                                                                 
152400       IF W-KVJUSTKV-IN-N NOT = 0                                         
152500         PERFORM EC-FLYTTA-WDR9                                           
152600         PERFORM ECA-UPPDATERA-WDR9                                       
152700       END-IF                                                             
152800     END-IF                                                               
152900     .                                                                    
153000     EJECT                                                                
153100 EC-FLYTTA-WDR9 SECTION.                                                  
153200     MOVE 'EC      ' TO WS-SECTION                                        
153300     MOVE 'W5010600'       TO FIL-IDPGM                                   
153400     MOVE DAGENS-DATUM     TO FIL-DAREGDAT                                
153500     ACCEPT FIL-TIKLOCK    FROM TIME                                      
153600     MOVE 1                TO FIL-IDSEKVNR                                
153700     MOVE 'W510EKHA'       TO FIL-IDCPYTXT                                
153800     MOVE MSGI-IDUSER      TO FIL-IDUSER                                  
153900     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
154000     MOVE '403'            TO EKH-KDEKHHT                                 
154100     MOVE '40'             TO WS-KDEKSHT-1                                
154200     MOVE INVH-KDJUSTYP TO WS-KDEKSHT-2                                   
154300     MOVE WS-EKH-KDEKSHT   TO EKH-KDEKSHT                                 
154400     MOVE 'DET'            TO EKH-KDEKNIVA                                
154500     MOVE W-IDDC           TO EKH-IDDC-SEND                               
154600                              EKH-IDDC-REC                                
154700     MOVE ZERO             TO EKH-IDDISTR                                 
154800     MOVE ZERO             TO EKH-IDKUNDNR                                
154900     MOVE W-IDARTNR        TO W-EKH-IDARTNR                               
155000                                                                          
155100     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
155200     MOVE W-EKH-IDARTNR    TO CIA-IDARTBET-IN                             
155300     CALL W009CIA USING       CIA-W009CIA                                 
155400     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
155500                                                                          
155600     MOVE DAGENS-DATUM     TO EKH-DAVERDAT                                
155700     MOVE ART-KDPRODSL     TO EKH-KDPRODSL                                
155800     MOVE ZERO             TO EKH-KDPSLLOC                                
155900     MOVE SPACE            TO EKH-FLLSBOK                                 
156000     MOVE 'SEK'            TO EKH-KDVALISO                                
156100     MOVE 1.00             TO EKH-PRKURS                                  
156200     MOVE ZERO             TO EKH-PRARTNTO                                
156300     MOVE ZERO             TO EKH-PRARTSJK                                
156400     MOVE ZERO             TO EKH-PRHEMTAG                                
156500     MOVE CLAG-PRARTSTD    TO EKH-PRARTSTD                                
156600     MOVE ZERO             TO EKH-PRLANDCO                                
156700     MOVE ZERO             TO EKH-PRINK                                   
156800     MOVE ZERO             TO EKH-PRDIRLON                                
156900     MOVE ZERO             TO EKH-PRDMTRL                                 
157000     MOVE ZERO             TO EKH-PROVRPAL                                
157100     MOVE ZERO             TO EKH-SUBEL                                   
157200     MOVE WS-KVJUSTKV-COMP TO EKH-KVANTAL                                 
157300     MOVE '5106'           TO EKH-IDTRANS                                 
157400     MOVE ZERO             TO EKH-BEVAT                                   
157500                              EKH-IDANALYS                                
157600                              EKH-IDKONTO                                 
157700                              EKH-KDANMORS                                
157800                              EKH-KDFRAKT                                 
157900                              EKH-SUVAT                                   
158000     MOVE ZERO             TO EKH-DAAVIDAT                                
158100                              EKH-IDAVINR                                 
158200                              EKH-KDAVVTYP                                
158300                              EKH-KDRT                                    
158400                              EKH-KVANTMOT                                
158500                              EKH-KVAVIS                                  
158600     MOVE WS-KDSORT        TO EKH-KDSORT                                  
158700     IF W-IDDC = WC-SDC-NL-ET                                             
158800       MOVE JA             TO EKH-FLDCET                                  
158900     ELSE                                                                 
159000       MOVE NEJ            TO EKH-FLDCET                                  
159100     END-IF                                                               
159200     MOVE SPACE            TO EKH-KDTRADP                                 
159300                              EKH-IDKST                                   
159400                              EKH-IDLEVNR                                 
159500                              EKH-IDKUNDRF                                
159510                              EKH-IDFAKT-EXP                              
159600     .                                                                    
159700     EJECT                                                                
159800 ECA-UPPDATERA-WDR9 SECTION.                                              
159900     MOVE 'ECA     ' TO WS-SECTION                                        
160000     PERFORM IMS-ISRT-WDR901                                              
160100     PERFORM UNTIL SEGMENT-FINNS                                          
160200      ADD +1 TO FIL-IDSEKVNR                                              
160300      PERFORM IMS-ISRT-WDR901                                             
160400     END-PERFORM                                                          
160500     .                                                                    
160600     EJECT                                                                
160700 ED-FLYTTA-NDC-WDR8 SECTION.                                              
160800     MOVE 'ED      ' TO WS-SECTION                                        
160900     MOVE 'W5010600'       TO A08-FIL-IDPGM                               
161000     MOVE DAGENS-DATUM(2:7) TO A08-FIL-TIREGDAT                           
161100     ACCEPT A08-FIL-TIKLOCK    FROM TIME                                  
161200     MOVE 1                TO A08-FIL-IDSEKVNR                            
161300     MOVE 'W510A08 '       TO A08-FIL-IDCPYTXT                            
161400     MOVE 'A08'            TO A08-IDPTYP                                  
161500     MOVE 'M10'            TO A08-KDEKOHT                                 
162010     IF DCS-NDC AND DCS-CANADA                                            
162020       MOVE 54             TO A08-IDFTG                                   
162030     ELSE                                                                 
162040       MOVE 53             TO A08-IDFTG                                   
162050     END-IF                                                               
162100     MOVE W-IDDC           TO A08-IDDC-SEND                               
162200                              A08-IDDC-REC                                
162300     MOVE DAGENS-DATUM     TO A08-DAJUSTDA                                
162400     MOVE W-IDARTNR        TO A08-IDARTNR                                 
162500     MOVE ART-KDPRODSL     TO A08-KDPRODSL                                
162600     MOVE CLAG-KDPSLLOC    TO A08-KDPSLLOC                                
162700     MOVE WS-KVJUSTKV-COMP TO A08-KVJUSTKV                                
162800     MOVE W-PRAVCOST       TO A08-PRAVCOST                                
162900     MOVE INVB-INV-KDINVKAT TO A08-KDINVKAT                               
163000     MOVE INVB-INV-TEINVANM TO A08-TEINVANM                               
163100     .                                                                    
163200     EJECT                                                                
163300 EDA-UPPDATERA-WDR8 SECTION.                                              
163400     MOVE 'EDA     ' TO WS-SECTION                                        
163500     PERFORM IMS-ISRT-WDR801                                              
163600     PERFORM UNTIL SEGMENT-FINNS                                          
163700      ADD +1 TO A08-FIL-IDSEKVNR                                          
163800      PERFORM IMS-ISRT-WDR801                                             
163900     END-PERFORM                                                          
164000     .                                                                    
164100     EJECT                                                                
164200                                                                          
164300 F-SKAPA-UPPDATERA-WDK7-POST SECTION.                                     
164400     MOVE 'F-SKAPA ' TO WS-SECTION                                        
164500* -- UPPDATERING AV DC-FLREFNYO PGA LAGERSALDOFÖRÄNDRING                  
164600     MOVE DCS-IDDC           TO W-IDDC                                    
164700     IF W-IDDC = WC-CDC-SE                                                
164800       CONTINUE                                                           
164900     ELSE                                                                 
165700       PERFORM IMS-GHU-WDK711                                             
165800       IF SEGMENT-FINNS                                                   
165900          MOVE NEJ          TO SLAG-FLREFNYO                              
166000          PERFORM IMS-REPL-SLAGER-SEGM                                    
166100       END-IF                                                             
166300     END-IF                                                               
166400     .                                                                    
166500     EJECT                                                                
166600                                                                          
166700 G-ARTIKEL-DC-SAKNAS SECTION.                                             
166800     MOVE 'G-ART   ' TO WS-SECTION                                        
166900     IF NOT DCS-CDC AND MID-FLANTAL = '1'                                 
167000       MOVE MEDDELANDE-12(W-KDSPRAK) TO MOD-TEMFSFEL                      
167100     ELSE                                                                 
167200       MOVE MEDDELANDE-2 (W-KDSPRAK) TO MOD-TEMFSFEL                      
167300     END-IF                                                               
167400                                                                          
167500     MOVE MFS-RENSA-FAELT         TO                                      
167600                                     MOD-BEART-ENG                        
167700                                     MOD-PRARTSTD                         
167800                                     MOD-TIAAVVD                          
167900                                     MOD-KVAVIS                           
168000                                     MOD-KDERS                            
168100                                     MOD-KDINVKAT                         
168200                                     MOD-TIM-INV                          
168300                                     MOD-SALDO-KVBUFF-OF                  
168400                                     MOD-SALDO-KVBUFF-F                   
168500                                     MOD-KVAKS                            
168600                                     MOD-KVLS                             
168700                                     MOD-KVEFRS                           
168800                                     MOD-KVROS                            
168900                                     MOD-KVUTRS                           
169000                                     MOD-KVJUSTKV-IN                      
169100                                     MOD-KDAVVTYP                         
169200                                     MOD-FLANTAL                          
169300                                     MOD-FLSLACK                          
169400                                     MOD-FLFLYTTN                         
169500                                     MOD-KVLS-LAGR                        
169600                                     MOD-KVUTRS-LAGR                      
169700                                     MOD-TEMFSINF                         
169800                                                                          
169900     MOVE +1 TO INDX                                                      
170000                                                                          
170100     PERFORM UNTIL                                                        
170200      NOT ( INDX < +7 )                                                   
170300       MOVE MFS-RENSA-FAELT       TO MOD-TIJUSTDA    (INDX)               
170400                                     MOD-KVJUSTKV-UT (INDX)               
170500                                     MOD-KDJUSTYP    (INDX)               
170600       ADD +1 TO INDX                                                     
170700     END-PERFORM                                                          
170800     .                                                                    
170900     EJECT                                                                
171000 H-RENSA-WDH1   SECTION.                                                  
171100     MOVE 'H-RENSA ' TO WS-SECTION                                        
171200***  LÄS MED STÖRSTA INTERVALL PÅ KDINVKAT 1 - 11                         
171300***  DVS FLAGGA AV ALLA INVENTERINGSKATEGORIER FÖR DENNA ART/DC           
171400     MOVE +1         TO W1-KDINVKAT                                       
171500                        W-KDINVKAT-MIN                                    
171600     MOVE +12        TO W2-KDINVKAT                                       
171700                        W-KDINVKAT-MAX                                    
171800     MOVE DCS-IDDC   TO W-IDDC-WDH1-MIN                                   
171900                        W-IDDC-WDH1-MAX                                   
172000                        W1-IDDC-WDH1                                      
172100                        W2-IDDC-WDH1                                      
172200                                                                          
172300     PERFORM IMS-LAES-INV-ROT                                             
172400                                                                          
172500     IF SEGMENT-FINNS                                                     
172600       PERFORM IMS-LAES-INV-SEG                                           
172700                                                                          
172800       PERFORM UNTIL SEGMENT-SAKNAS                                       
172900                                                                          
173000         IF INVB-INV-KDINVKAT = +1 OR +2 OR +3 OR +4 OR +5 OR +9          
173100                                                                          
173200           MOVE JA    TO INVB-INV-FLINVBEH                                
173300           IF INVB-INV-KDINVKAT = +2                                      
173400             IF MID-KDAVVTYP = '-'                                        
173500               COMPUTE INVB-INV-KVJUSTKV = W-KVJUSTKV-IN-N * -1           
173600             ELSE                                                         
173700               MOVE W-KVJUSTKV-IN-N   TO INVB-INV-KVJUSTKV                
173800             END-IF                                                       
173900           END-IF                                                         
174000           PERFORM IMS-REPL-INV-SEG                                       
174100         END-IF                                                           
174200         PERFORM IMS-LAES-INV-SEG                                         
174300       END-PERFORM                                                        
174400     END-IF                                                               
174500     .                                                                    
174600     EJECT                                                                
174700 I-INFORMATIONSBILD SECTION.                                              
174800*****************************************************************         
174900*    INFORMATIONSVÄRDEN PÅ ÖNSKAT ARTIKELNR/CL LÄGGS UT                   
175000*    INFORMATIONEN HÄMTAS FRÅN  WDK6, WDH1 OCH WDL2                       
175100*****************************************************************         
175200     MOVE 'I-INFO  ' TO WS-SECTION                                        
175300     MOVE NEJ                  TO ARTIKEL-DC-FINNS                        
175400     MOVE IDARTNR-WS           TO W-IDARTNR                               
175500     MOVE DCS-IDDC             TO W-IDDC                                  
175600                                  W-IDDC-WDD8                             
175700                                                                          
175800     PERFORM IMS-LAES-ARTIKEL-6                                           
175900                                                                          
176000     IF SEGMENT-FINNS AND ART-KDERS-UTG = 0                               
176100       MOVE JA                 TO ARTIKEL-DC-FINNS                        
176200       PERFORM IMS-LAES-EKONOMISEG                                        
176300       IF SEGMENT-FINNS                                                   
176400         MOVE CLAG-PRARTSTD    TO SPAR-PRARTSTD                           
176500         IF  WDB6-FINNS                                                   
176600         AND DCS-FLPRISSPR = JA                                           
176700           MOVE ZERO           TO MOD-PRARTSTD                            
176800         ELSE                                                             
176900           MOVE CLAG-PRARTSTD  TO MOD-PRARTSTD                            
177000         END-IF                                                           
177100         IF  WDB6-FINNS                                                   
177200         AND (DCS-CDC                                                     
177300         OR   DCS-CDC-TR)                                                 
177400           MOVE CLAG-KDERS     TO MOD-KDERS                               
177500           MOVE CLAG-KVAKS-CDC TO MOD-KVAKS                               
177600           MOVE CLAG-KVLS      TO MOD-KVLS                                
177700           MOVE MOD-KVLS-X     TO MOD-KVLS-LAGR                           
177800           MOVE CLAG-KVEFRS    TO MOD-KVEFRS                              
177900           MOVE CLAG-KVROS     TO MOD-KVROS                               
178000           MOVE CLAG-KVUTRS    TO MOD-KVUTRS                              
178100           MOVE SPACE          TO MOD-HEADING                             
178200           MOVE ZERO           TO MOD-PRAVCOST                            
178300           MOVE MOD-KVUTRS-X   TO MOD-KVUTRS-LAGR                         
178400         ELSE                                                             
178500           PERFORM IMS-GHU-WDK711                                         
178600           IF SEGMENT-FINNS                                               
178700****** OBSERVERA DE SOM HÄR LIGGER MED CLAG FINNS EJ PÅ WDK7 ****         
178800****** BASEN SKA KONTROLLERAS                                             
178900             MOVE CLAG-KDERS          TO MOD-KDERS                        
179000             COMPUTE W-ART-KVROS-SDC-NDC = SLAG-KVROS-DAG +               
179100                                           SLAG-KVROS-BULK                
179200             MOVE W-ART-KVROS-SDC-NDC TO MOD-KVROS                        
179300                                                                          
179400             MOVE SLAG-KVAKS-SDC      TO MOD-KVAKS                        
179500             MOVE SLAG-KVLS           TO MOD-KVLS                         
179600             MOVE MOD-KVLS-X          TO MOD-KVLS-LAGR                    
179700             MOVE SLAG-KVEFRS         TO MOD-KVEFRS                       
179800             MOVE SLAG-KVUTRS         TO MOD-KVUTRS                       
179900             MOVE 'AVERAGE COST'      TO MOD-HEADING                      
180000             MOVE SLAG-PRAVCOST       TO MOD-PRAVCOST                     
180100             MOVE MOD-KVUTRS-X        TO MOD-KVUTRS-LAGR                  
180200           ELSE                                                           
180300             MOVE NEJ                 TO ARTIKEL-DC-FINNS                 
180400           END-IF                                                         
180500         END-IF                                                           
180600       ELSE                                                               
180700         MOVE ZERO                    TO SPAR-PRARTSTD                    
180800       END-IF                                                             
180900                                                                          
181000       IF ARTIKEL-DC-FINNS = JA                                           
181100         PERFORM IMS-GHU-INVHIST-ROT                                      
181200         IF SEGMENT-FINNS                                                 
181300           PERFORM IMS-GNP-INVHIST-WDH7                                   
181400           MOVE 1 TO INDX                                                 
181500                                                                          
181600           PERFORM UNTIL NOT ( SEGMENT-FINNS AND INDX < 6 )               
181700             PERFORM S03-KONV-DATUM                                       
181800             IF DAT-KDSVAR-OK                                             
181900               MOVE DAT-TIAAVVD   TO MOD-TIJUSTDA (INDX)                  
182000             ELSE                                                         
182100               MOVE ZERO          TO MOD-TIJUSTDA (INDX)                  
182200             END-IF                                                       
182300             MOVE INVH-KVJUSTKV TO MOD-KVJUSTKV-UT (INDX)                 
182400             MOVE INVH-KDJUSTYP TO MOD-KDJUSTYP (INDX)                    
182500             ADD 1 TO INDX                                                
182600             PERFORM IMS-GNP-INVHIST-WDH7                                 
182700           END-PERFORM                                                    
182800         END-IF                                                           
182900       END-IF                                                             
183000                                                                          
183100       MOVE +0 TO INLEV-DATUM                                             
183200       MOVE +0 TO INLEV-ANTAL                                             
183300                                                                          
183400       IF DCS-CDC                                                         
183500         PERFORM IMS-GET-INLEVROT                                         
183600                                                                          
183700         IF SEGMENT-SAKNAS                                                
183800           CONTINUE                                                       
183900         ELSE                                                             
184000           PERFORM IMS-GET-INLEVNR                                        
184100                                                                          
184200           PERFORM UNTIL NOT (SEGMENT-FINNS AND                           
184300                         INLEV-DATUM = +0 )                               
184400                                                                          
184500             MOVE INL-INL-DAINLEV TO W-DAINLEV                            
184600             PERFORM IMS-GET-INLEVTRANS3X                                 
184700                                                                          
184800             PERFORM UNTIL NOT (SEGMENT-FINNS AND                         
184900                           INLEV-DATUM = +0 )                             
185000                                                                          
185100               IF INL-MOT-IDDC NOT = WC-CDC-SE                            
185200                 CONTINUE                                                 
185300               ELSE                                                       
185400                 IF INL-MOT-IDPTYP = 'R30'                                
185500                   CONTINUE                                               
185600                 ELSE                                                     
185700                                                                          
185800                   EVALUATE TRUE                                          
185900                                                                          
186000                   WHEN INL-MOT-IDPTYP = 'R31' OR '310'                   
186100                       IF INL-MOT-KDRT NOT = 88 AND 99                    
186200                         PERFORM IMS-GET-P32TRANS                         
186300                         IF SEGMENT-FINNS                                 
186400                           MOVE INL-DEL-TIREGDAT TO INLEV-DATUM           
186500                           MOVE INL-DEL-KVRAPP TO INLEV-ANTAL             
186600                         END-IF                                           
186700                       END-IF                                             
186800                                                                          
186900                   WHEN INL-MOT-IDPTYP = 'R32'                            
187000                       IF INL-MOT-KDRT NOT = 88 AND 99                    
187100                         MOVE INL-MOT-TIUPPDAT TO INLEV-DATUM             
187200                         MOVE INL-MOT-KVAVIS TO INLEV-ANTAL               
187300                       END-IF                                             
187400                                                                          
187500                   WHEN OTHER                                             
187600                       CONTINUE                                           
187700                                                                          
187800                   END-EVALUATE                                           
187900                                                                          
188000                 END-IF                                                   
188100               END-IF                                                     
188200                                                                          
188300               IF INLEV-DATUM = +0                                        
188400                 PERFORM IMS-GET-INLEVTRANS3X                             
188500               END-IF                                                     
188600                                                                          
188700             END-PERFORM                                                  
188800                                                                          
188900             PERFORM IMS-GET-INLEVTRANSR34                                
189000                                                                          
189100             PERFORM UNTIL SEGMENT-SAKNAS OR                              
189200                     INL-DIR-IDDC NOT = WC-CDC-SE                         
189300                                                                          
189400               PERFORM IMS-GET-INLEVTRANSR34                              
189500             END-PERFORM                                                  
189600                                                                          
189700             IF SEGMENT-FINNS                                             
189800               IF INL-DIR-TIAVSDAT > INLEV-DATUM                          
189900                 MOVE INL-DIR-TIAVSDAT TO INLEV-DATUM                     
190000                 MOVE INL-DIR-KVAVIS TO INLEV-ANTAL                       
190100               END-IF                                                     
190200             END-IF                                                       
190300                                                                          
190400             IF INLEV-DATUM = +0                                          
190500               PERFORM IMS-GET-INLEVNR                                    
190600             END-IF                                                       
190700                                                                          
190800           END-PERFORM                                                    
190900                                                                          
191000         END-IF                                                           
191100       ELSE                                                               
191200         PERFORM IMS-GET-WDL601                                           
191300         IF SEGMENT-FINNS                                                 
191400           PERFORM UNTIL NOT (SEGMENT-FINNS AND INLEV-DATUM = +0)         
191500             PERFORM IMS-GNP-WDL611                                       
191600             IF INLC-INL-IDDC = W-IDDC                                    
191700               IF INLC-INL-IDPTYP = 'R32'                                 
191800                 MOVE INLC-INL-TIINLINL TO INLEV-DATUM                    
191900                 MOVE INLC-INL-KVANTMOT TO INLEV-ANTAL                    
192000                                           MOD-KVAVIS                     
192100               END-IF                                                     
192200             END-IF                                                       
192300           END-PERFORM                                                    
192400         END-IF                                                           
192500       END-IF                                                             
192600                                                                          
192700       IF INLEV-DATUM NOT = +0                                            
192800         PERFORM IA-KONV-DATUM                                            
192900         IF DAT-KDSVAR-OK                                                 
193000           MOVE DAT-TIAAVVD TO MOD-TIAAVVD                                
193100         ELSE                                                             
193200           MOVE ZERO        TO MOD-TIAAVVD                                
193300         END-IF                                                           
193400         MOVE INLEV-ANTAL   TO MOD-KVAVIS                                 
193500       END-IF                                                             
193600                                                                          
193700       IF DCS-CDC                                                         
193800         PERFORM IMS-GET-SALDOREG                                         
193900         IF SEGMENT-SAKNAS                                                
194000           MOVE +0                TO MOD-SALDO-KVBUFF-OF                  
194100           MOVE +0                TO MOD-SALDO-KVBUFF-F                   
194200         ELSE                                                             
194300           MOVE ART-SALDO-KVBUFF-F TO MOD-SALDO-KVBUFF-F                  
194400           MOVE ART-SALDO-KVBUFF-OF TO MOD-SALDO-KVBUFF-OF                
194500         END-IF                                                           
194600       ELSE                                                               
194700         MOVE MFS-RENSA-FAELT      TO MOD-SALDO-KVBUFF-F                  
194800                                      MOD-SALDO-KVBUFF-OF                 
194900       END-IF                                                             
195000                                                                          
195100       PERFORM IMS-LAES-INV-ROT                                           
195200                                                                          
195300       IF SEGMENT-FINNS                                                   
195400         MOVE DCS-IDDC TO W-IDDC-WDH1-MIN                                 
195500                          W-IDDC-WDH1-MAX                                 
195600                          W1-IDDC-WDH1                                    
195700                          W2-IDDC-WDH1                                    
195800         MOVE +9       TO W-KDINVKAT-MIN                                  
195900                          W-KDINVKAT-MAX                                  
196000                                                                          
196100         PERFORM IMS-LAES-INV                                             
196200         IF SEGMENT-FINNS                                                 
196300           MOVE INVB-INV-KDINVKAT  TO MOD-KDINVKAT                        
196400           MOVE INVB-INV-TISEGKEY  TO WS-FIX-TISEGKEY                     
196500           MOVE WS-TISEGKEY-3-8    TO MOD-TIM-INV                         
196600         ELSE                                                             
196700           PERFORM IMS-LAES-INV-ROT                                       
196800                                                                          
196900           MOVE +1           TO W1-KDINVKAT                               
197000           MOVE +12          TO W2-KDINVKAT                               
197100                                                                          
197200           PERFORM IMS-LAES-INV-SEG                                       
197300                                                                          
197400           MOVE NEJ            TO INV-FINNS                               
197500           PERFORM UNTIL SEGMENT-SAKNAS OR INV-FINNS = JA                 
197600             IF (INVB-INV-KDINVKAT = 1 OR 2 OR 3 OR 4 OR 5)               
197700               AND  INVB-INV-FLINVBEH = NEJ                               
197800               MOVE JA                TO INV-FINNS                        
197900               MOVE INVB-INV-KDINVKAT TO MOD-KDINVKAT                     
198000               MOVE INVB-INV-TISEGKEY TO WS-FIX-TISEGKEY                  
198100               MOVE WS-TISEGKEY-3-8   TO MOD-TIM-INV                      
198200             ELSE                                                         
198300               PERFORM IMS-LAES-INV-SEG                                   
198400             END-IF                                                       
198500           END-PERFORM                                                    
198600         END-IF                                                           
198700       END-IF                                                             
198800                                                                          
198900       IF W-KDSPRAK = +1                                                  
199000         MOVE 'S  '            TO W-IDSKYLT                               
199100         PERFORM IMS-GU-BEN                                               
199200         IF SEGMENT-FINNS                                                 
199300           MOVE BEN-TEXT-BEART TO MOD-BEART-ENG                           
199400         ELSE                                                             
199500           MOVE SPACE          TO MOD-BEART-ENG                           
199600         END-IF                                                           
199700                                                                          
199800       ELSE                                                               
199900         MOVE 'GB '            TO W-IDSKYLT                               
200000         PERFORM IMS-GU-BEN                                               
200100         IF SEGMENT-FINNS                                                 
200200           MOVE BEN-TEXT-BEART TO MOD-BEART-ENG                           
200300         ELSE                                                             
200400           MOVE SPACE          TO MOD-BEART-ENG                           
200500         END-IF                                                           
200600       END-IF                                                             
200700                                                                          
200800       IF EGEN-BILD = JA AND NOT MFS-UPDATE                               
200900         MOVE MFS-ADD-SAETT-CURSOR                                        
201000                               TO MOD-KVJUSTKV-IN-ATTR                    
201100       END-IF                                                             
201200       MOVE MFS-RENSA-FAELT    TO MOD-KVJUSTKV-IN                         
201300                                  MOD-KDAVVTYP                            
201400                                  MOD-FLANTAL                             
201500                                  MOD-FLSLACK                             
201600                                  MOD-FLFLYTTN                            
201700     END-IF                                                               
201800     .                                                                    
201900     EJECT                                                                
202000 IA-KONV-DATUM  SECTION.                                                  
202100     MOVE 'IA      ' TO WS-SECTION                                        
202200     MOVE INLEV-DATUM     TO DAT-I-TIDATUM                                
202300     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
202400                                                                          
202500     CALL  WDATKONV   USING  DAT-KDDATFORM                                
202600                             DAT-I-TIDATUM                                
202700                             DAT-O-TIDATUM                                
202800                             DAT-KDSVAR                                   
202900     .                                                                    
203000                                                                          
203100     EJECT                                                                
203200******************************************************************        
203300*    UPPDATERINGSUPPGIFTER FRÅN RAD 17 KONTROLLERAS                       
203400*                                                                         
203500                                                                          
203600 J-UPPDATERINGSRAD-GODKEND SECTION.                                       
203700     MOVE 'J-UPPDAT' TO WS-SECTION                                        
203800     MOVE JA                          TO UPPDATERINGSRAD-GODKEND          
203900                                                                          
204000     MOVE MID-KVJUSTKV-IN             TO KVJUSTKV-WS                      
204100     INSPECT KVJUSTKV-WS REPLACING LEADING SPACE BY ZERO                  
204200                                                                          
204300     IF KVJUSTKV-WS NOT NUMERIC                                           
204400       MOVE MEDDELANDE-1(W-KDSPRAK)   TO MOD-TEMFSFEL                     
204500       MOVE MFS-NUM-FAELT-FEL         TO MOD-KVJUSTKV-IN-ATTR             
204600       MOVE NEJ                       TO UPPDATERINGSRAD-GODKEND          
204700     ELSE                                                                 
204800       MOVE MFS-NUM-FAELT-RAETT       TO MOD-KVJUSTKV-IN-ATTR             
204900     END-IF                                                               
205000                                                                          
205100     IF MID-KDAVVTYP = '+' OR '-'                                         
205200       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-KDAVVTYP-ATTR                
205300     ELSE                                                                 
205400       IF MID-KDAVVTYP = ' ' AND KVJUSTKV-WS = ZERO                       
205500         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-KDAVVTYP-ATTR                
205600       ELSE                                                               
205700         MOVE MEDDELANDE-1(W-KDSPRAK) TO MOD-TEMFSFEL                     
205800         MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDAVVTYP-ATTR                
205900         MOVE NEJ                     TO UPPDATERINGSRAD-GODKEND          
206000       END-IF                                                             
206100     END-IF                                                               
206200                                                                          
206300     IF UPPDATERINGSRAD-GODKEND = JA                                      
206400       IF MID-FLANTAL = SPACE AND                                         
206500          MID-FLSLACK = SPACE AND                                         
206600          MID-FLFLYTTN = SPACE                                            
206700                                                                          
206800         IF MID-KVJUSTKV-IN NOT = SPACE AND                               
206900           (MID-KDAVVTYP = '+' OR '-' OR ' ')                             
207000                                                                          
207100            MOVE MID-KVJUSTKV-IN TO WS-KVJUSTKV-X                         
207200            INSPECT WS-KVJUSTKV-X REPLACING LEADING SPACE BY ZERO         
207300            MOVE WS-KVJUSTKV-N   TO WS-KVJUSTKV-COMP                      
207400                                                                          
207500            IF MID-KDAVVTYP = '+' OR '-'                                  
207600              IF DCS-CDC                                                  
207700***   FIX FÖR ÖKA GRÄNSEN PÅ CDC TILL 500 000 KR                          
207800                COMPUTE W-SUMMA-STD =                                     
207900                       (WS-KVJUSTKV-COMP * SPAR-PRARTSTD) / 5             
208000              ELSE                                                        
208100                COMPUTE W-SUMMA-STD =                                     
208200                        WS-KVJUSTKV-COMP * SPAR-PRARTSTD                  
208300              END-IF                                                      
208400              IF W-SUMMA-STD < 100000                                     
208500                MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAVVTYP-ATTR            
208600                MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVJUSTKV-IN-ATTR         
208700                MOVE JA  TO TOTAL-STD                                     
208800              ELSE                                                        
208900***   NDC:ERNA SKALL INTE HA NÅGON LIMIT                                  
209000                IF DCS-NDC                                                
209100                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAVVTYP-ATTR          
209200                  MOVE MFS-NUM-FAELT-RAETT  TO                            
209300                                     MOD-KVJUSTKV-IN-ATTR                 
209400                  MOVE JA  TO TOTAL-STD                                   
209500                ELSE                                                      
209600                  MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDAVVTYP-ATTR          
209700                  MOVE MFS-NUM-FAELT-FEL    TO                            
209800                                     MOD-KVJUSTKV-IN-ATTR                 
209900                  MOVE NEJ TO UPPDATERINGSRAD-GODKEND                     
210000                  MOVE NEJ TO TOTAL-STD                                   
210100                END-IF                                                    
210200              END-IF                                                      
210300            END-IF                                                        
210400         ELSE                                                             
210500           MOVE NEJ                  TO UPPDATERINGSRAD-GODKEND           
210600         END-IF                                                           
210700                                                                          
210800       ELSE                                                               
210900         IF MID-FLANTAL = '1'                                             
211000           IF MID-KVJUSTKV-IN NOT = SPACE AND                             
211100             (MID-KDAVVTYP = '+' OR '-') AND                              
211200              MID-FLSLACK = SPACE AND                                     
211300              MID-FLFLYTTN = SPACE                                        
211400             MOVE MFS-NUM-FAELT-RAETT  TO MOD-FLANTAL-ATTR                
211500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAVVTYP-ATTR               
211600           ELSE                                                           
211700             MOVE NEJ                  TO UPPDATERINGSRAD-GODKEND         
211800           END-IF                                                         
211900         ELSE                                                             
212000           IF MID-FLFLYTTN = '1'                                          
212100             IF MID-KVJUSTKV-IN NOT = SPACE AND                           
212200               (MID-KDAVVTYP = '+' OR '-') AND                            
212300               MID-FLANTAL = SPACE AND                                    
212400               MID-FLSLACK = SPACE                                        
212500                                                                          
212600               MOVE MFS-NUM-FAELT-RAETT  TO MOD-FLFLYTTN-ATTR             
212700               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAVVTYP-ATTR             
212800             ELSE                                                         
212900               MOVE NEJ TO UPPDATERINGSRAD-GODKEND                        
213000             END-IF                                                       
213100           ELSE                                                           
213200             IF MID-FLSLACK = '1'                                         
213300               IF MID-KVJUSTKV-IN = SPACE AND                             
213400                  MID-KDAVVTYP = SPACE AND                                
213500                  MID-FLANTAL = SPACE AND                                 
213600                  MID-FLFLYTTN = SPACE                                    
213700                                                                          
213800                 IF MOD-KDINVKAT = 9                                      
213900                 OR MOD-KDINVKAT = 8                                      
214000                 OR MOD-KDINVKAT = 7                                      
214100                 OR MOD-KDINVKAT = 6                                      
214200                 OR MOD-KDINVKAT = 5                                      
214300                 OR MOD-KDINVKAT = 4                                      
214400                 OR MOD-KDINVKAT = 3                                      
214500                 OR MOD-KDINVKAT = 2                                      
214600                 OR MOD-KDINVKAT = 11                                     
214700                 OR MOD-KDINVKAT = 12                                     
214800                   MOVE MEDDELANDE-5 (W-KDSPRAK) TO MOD-TEMFSINF          
214900                   MOVE NEJ TO UPPDATERINGSRAD-GODKEND                    
215000                 ELSE                                                     
215100                   MOVE MFS-NUM-FAELT-RAETT TO MOD-FLSLACK-ATTR           
215200                                                MOD-FLANTAL-ATTR          
215300                                                MOD-FLFLYTTN-ATTR         
215400                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAVVTYP-ATTR         
215500                 END-IF                                                   
215600               ELSE                                                       
215700                 MOVE NEJ TO UPPDATERINGSRAD-GODKEND                      
215800               END-IF                                                     
215900                                                                          
216000             ELSE                                                         
216100               MOVE NEJ TO UPPDATERINGSRAD-GODKEND                        
216200             END-IF                                                       
216300           END-IF                                                         
216400         END-IF                                                           
216500       END-IF                                                             
216600     END-IF                                                               
216700                                                                          
216800     IF UPPDATERINGSRAD-GODKEND = JA                                      
216900       IF MID-FLANTAL = '1'                                               
217000         IF  WDB6-SAKNAS                                                  
217100         OR  DCS-FLTYP6JU = NEJ                                           
217200           MOVE NEJ               TO UPPDATERINGSRAD-GODKEND              
217300           MOVE MFS-NUM-FAELT-FEL TO MOD-FLANTAL-ATTR                     
217400         END-IF                                                           
217500       END-IF                                                             
217600     END-IF                                                               
217700                                                                          
217800     IF MID-IDARTNR-IN = ALL '+'                                          
217900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                           
218000                                 MOD-IDDC-UT                              
218100                                 MOD-BEART-ENG                            
218200                                 MOD-PRARTSTD                             
218300                                 MOD-TIAAVVD                              
218400                                 MOD-KVAVIS                               
218500                                 MOD-KDERS                                
218600                                 MOD-KDINVKAT                             
218700                                 MOD-TIM-INV                              
218800                                 MOD-SALDO-KVBUFF-OF                      
218900                                 MOD-SALDO-KVBUFF-F                       
219000                                 MOD-KVAKS                                
219100                                 MOD-KVLS                                 
219200                                 MOD-KVEFRS                               
219300                                 MOD-KVROS                                
219400                                 MOD-KVUTRS                               
219500                                 MOD-KVJUSTKV-IN                          
219600                                 MOD-KDAVVTYP                             
219700                                 MOD-FLANTAL                              
219800                                 MOD-FLSLACK                              
219900                                 MOD-FLFLYTTN                             
220000                                 MOD-KVLS-LAGR                            
220100                                 MOD-KVUTRS-LAGR                          
220200     END-IF                                                               
220300                                                                          
220400     IF UPPDATERINGSRAD-GODKEND = NEJ                                     
220500       MOVE MFS-NUM-FAELT-FEL         TO MOD-KVJUSTKV-IN-ATTR             
220600                                         MOD-FLANTAL-ATTR                 
220700                                         MOD-FLSLACK-ATTR                 
220800                                         MOD-FLFLYTTN-ATTR                
220900                                                                          
221000       MOVE MFS-ALFA-FAELT-FEL        TO MOD-KDAVVTYP-ATTR                
221100                                                                          
221200       MOVE MEDDELANDE-1 (W-KDSPRAK)  TO MOD-TEMFSFEL                     
221300                                                                          
221400       IF TOTAL-STD = NEJ                                                 
221500         MOVE MEDDELANDE-9 (W-KDSPRAK) TO MOD-TEMFSINF                    
221600       END-IF                                                             
221700     END-IF                                                               
221800     .                                                                    
221900     EJECT                                                                
222000*****************************************************************         
222100*    REDIGERAR BILD MED RESULTAT AV TÄNKT UPPDATERING                     
222200                                                                          
222300                                                                          
222400 K-UPPDATERINGSBILD SECTION.                                              
222500     MOVE 'K-UPP   ' TO WS-SECTION                                        
222600     MOVE JA TO MOD-KOLLA-PF11-DOLD                                       
222700     MOVE IDARTNR-WS       TO W-IDARTNR                                   
222800     MOVE DCS-IDDC         TO W-IDDC                                      
222900                              W-IDDC-WDD8                                 
223000                                                                          
223100     MOVE MID-KVJUSTKV-IN TO WS-KVJUSTKV-X                                
223200     INSPECT WS-KVJUSTKV-X REPLACING LEADING SPACE BY ZERO                
223300     MOVE WS-KVJUSTKV-N    TO WS-KVJUSTKV-COMP                            
223400                                                                          
223500                                                                          
223600     IF MID-FLSLACK = '1'                                                 
223700       PERFORM KA-SLAEK-INVENTERING                                       
223800     ELSE                                                                 
223900       IF MID-FLFLYTTN = '1'                                              
224000         PERFORM KB-FLYTTNING                                             
224100       ELSE                                                               
224200         IF MID-FLANTAL = '1'                                             
224300           PERFORM KC-JUST-ENDAST-ANTAL                                   
224400         ELSE                                                             
224500                                                                          
224600           IF MID-KDAVVTYP = '+' OR '-' OR ' '                            
224700             IF DCS-CDC                                                   
224800               PERFORM KD-BEHANDLA-CDC                                    
224900             ELSE                                                         
225000               PERFORM KE-BEHANDLA-SDC-NDC                                
225100             END-IF                                                       
225200           END-IF                                                         
225300         END-IF                                                           
225400       END-IF                                                             
225500     END-IF                                                               
225600                                                                          
225700                                                                          
225800     MOVE MEDDELANDE-3 (W-KDSPRAK) TO MOD-TEMFSINF                        
225900                                                                          
226000     IF MID-IDARTNR-IN = ALL '+'                                          
226100       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDARTNR-UT                       
226200                                     MOD-IDDC-UT                          
226300                                     MOD-BEART-ENG                        
226400                                     MOD-PRARTSTD                         
226500                                     MOD-TIAAVVD                          
226600                                     MOD-KVAVIS                           
226700                                     MOD-KDERS                            
226800                                     MOD-KDINVKAT                         
226900                                     MOD-TIM-INV                          
227000                                     MOD-SALDO-KVBUFF-OF                  
227100                                     MOD-SALDO-KVBUFF-F                   
227200                                     MOD-KVAKS                            
227300                                     MOD-KVEFRS                           
227400                                     MOD-KVROS                            
227500                                     MOD-KVJUSTKV-IN                      
227600                                     MOD-KDAVVTYP                         
227700                                     MOD-FLANTAL                          
227800                                     MOD-FLSLACK                          
227900                                     MOD-FLFLYTTN                         
228000                                     MOD-KVLS-LAGR                        
228100                                     MOD-KVUTRS-LAGR                      
228200     ELSE                                                                 
228300       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVJUSTKV-IN                      
228400                                     MOD-KDAVVTYP                         
228500                                     MOD-FLANTAL                          
228600                                     MOD-FLSLACK                          
228700                                     MOD-FLFLYTTN                         
228800     END-IF                                                               
228900                                                                          
229000     MOVE MFS-STAENG-FAELT       TO MOD-KVJUSTKV-IN-ATTR                  
229100                                    MOD-KDAVVTYP-ATTR                     
229200                                    MOD-FLANTAL-ATTR                      
229300                                    MOD-FLSLACK-ATTR                      
229400                                    MOD-FLFLYTTN-ATTR                     
229500     .                                                                    
229600     EJECT                                                                
229700 KA-SLAEK-INVENTERING SECTION.                                            
229800     MOVE 'KA-SLACK' TO WS-SECTION                                        
229900     PERFORM IMS-LAES-INV-ROT                                             
230000                                                                          
230100     IF SEGMENT-FINNS                                                     
230200       MOVE DCS-IDDC       TO W1-IDDC-WDH1                                
230300                              W2-IDDC-WDH1                                
230400       MOVE 01             TO W1-KDINVKAT                                 
230500       MOVE 11             TO W2-KDINVKAT                                 
230600                                                                          
230700       MOVE NEJ                           TO INV-KAT-FINNS                
230800       PERFORM IMS-LAES-ARTIKEL-INV                                       
230900       PERFORM UNTIL INVKOE-ART-FINNS = NEJ                               
231000         IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                
231100           MOVE JA                        TO INV-KAT-FINNS                
231200         END-IF                                                           
231300         PERFORM IMS-LAES-ARTIKEL-INV                                     
231400       END-PERFORM                                                        
231500                                                                          
231600       IF INV-KAT-FINNS = JA                                              
231700         MOVE ZERO                        TO MOD-KVUTRS                   
231800         MOVE MFS-ADD-LYS-UPP-FAELT       TO MOD-KVUTRS-ATTR              
231900       ELSE                                                               
232000                                                                          
232100         PERFORM IMS-LAES-INV-ROT                                         
232200                                                                          
232300         MOVE DCS-IDDC                    TO W-IDDC-WDH1-MIN              
232400                                             W-IDDC-WDH1-MAX              
232500         MOVE +2                          TO W-KDINVKAT-MIN               
232600                                             W-KDINVKAT-MAX               
232700                                                                          
232800         PERFORM IMS-LAES-INV                                             
232900         IF SEGMENT-FINNS                                                 
233000           MOVE ZERO                      TO MOD-KVUTRS                   
233100           MOVE MFS-ADD-LYS-UPP-FAELT     TO MOD-KVUTRS-ATTR              
233200         END-IF                                                           
233300       END-IF                                                             
233400     END-IF                                                               
233500     .                                                                    
233600     EJECT                                                                
233700 KB-FLYTTNING SECTION.                                                    
233800     MOVE 'KB      ' TO WS-SECTION                                        
233900     IF DCS-CDC                                                           
234000       PERFORM IMS-LAES-ARTIKEL-6                                         
234100       PERFORM IMS-LAES-EKONOMISEG                                        
234200                                                                          
234300       IF MID-KDAVVTYP = '+' OR '-'                                       
234400         IF MID-KDAVVTYP = '+'                                            
234500           COMPUTE W-SUMMA = CLAG-KVUTRS + WS-KVJUSTKV-COMP               
234600         ELSE                                                             
234700           COMPUTE W-SUMMA = CLAG-KVUTRS - WS-KVJUSTKV-COMP               
234800         END-IF                                                           
234900       ELSE                                                               
235000         IF MID-KDAVVTYP = ' ' AND MID-KVJUSTKV-IN = ZERO                 
235100           COMPUTE W-SUMMA = CLAG-KVUTRS + WS-KVJUSTKV-COMP               
235200         END-IF                                                           
235300       END-IF                                                             
235400                                                                          
235500       MOVE W-SUMMA               TO MOD-KVUTRS                           
235600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                      
235700                                                                          
235800     ELSE                                                                 
235900       PERFORM IMS-GHU-WDK711                                             
236000                                                                          
236100       IF MID-KDAVVTYP = '+' OR '-'                                       
236200         IF MID-KDAVVTYP = '+'                                            
236300           COMPUTE W-SUMMA = SLAG-KVUTRS + WS-KVJUSTKV-COMP               
236400         ELSE                                                             
236500           COMPUTE W-SUMMA = SLAG-KVUTRS - WS-KVJUSTKV-COMP               
236600         END-IF                                                           
236700       ELSE                                                               
236800         IF MID-KDAVVTYP = ' ' AND MID-KVJUSTKV-IN = ZERO                 
236900           COMPUTE W-SUMMA = CLAG-KVUTRS + WS-KVJUSTKV-COMP               
237000         END-IF                                                           
237100       END-IF                                                             
237200                                                                          
237300       MOVE W-SUMMA               TO MOD-KVUTRS                           
237400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                      
237500     END-IF                                                               
237600     .                                                                    
237700     EJECT                                                                
237800 KC-JUST-ENDAST-ANTAL SECTION.                                            
237900     MOVE 'KC      ' TO WS-SECTION                                        
238000     IF DCS-CDC                                                           
238100       PERFORM IMS-LAES-ARTIKEL-6                                         
238200       PERFORM IMS-LAES-EKONOMISEG                                        
238300       IF MID-KDAVVTYP = '+' OR '-'                                       
238400         IF MID-KDAVVTYP = '+'                                            
238500           COMPUTE W-SUMMA = CLAG-KVLS + WS-KVJUSTKV-COMP                 
238600         ELSE                                                             
238700           COMPUTE W-SUMMA = CLAG-KVLS - WS-KVJUSTKV-COMP                 
238800         END-IF                                                           
238900       ELSE                                                               
239000         IF MID-KDAVVTYP = ' ' AND MID-KVJUSTKV-IN = ZERO                 
239100           COMPUTE W-SUMMA = CLAG-KVUTRS + WS-KVJUSTKV-COMP               
239200         END-IF                                                           
239300       END-IF                                                             
239400       MOVE W-SUMMA               TO MOD-KVLS                             
239500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVLS-ATTR                        
239600       PERFORM S01-FLYTTA-SKAPA-INVINFO                                   
239700       PERFORM S10-DATUM                                                  
239800       MOVE 6                     TO MOD-KDJUSTYP (1)                     
239900     END-IF                                                               
240000     .                                                                    
240100     EJECT                                                                
240200 KD-BEHANDLA-CDC SECTION.                                                 
240300     SKIP2                                                                
240400     MOVE 'KD      ' TO WS-SECTION                                        
240500     PERFORM IMS-LAES-ARTIKEL-6                                           
240600     PERFORM IMS-LAES-EKONOMISEG                                          
240700                                                                          
240800     IF MID-KDAVVTYP = '+' OR '-'                                         
240900       IF MID-KDAVVTYP = '+'                                              
241000         COMPUTE W-SUMMA = CLAG-KVLS + WS-KVJUSTKV-COMP                   
241100       ELSE                                                               
241200         COMPUTE W-SUMMA = CLAG-KVLS - WS-KVJUSTKV-COMP                   
241300       END-IF                                                             
241400     ELSE                                                                 
241500       IF MID-KDAVVTYP = ' ' AND MID-KVJUSTKV-IN = ZERO                   
241600         COMPUTE W-SUMMA = CLAG-KVUTRS + WS-KVJUSTKV-COMP                 
241700       END-IF                                                             
241800     END-IF                                                               
241900                                                                          
242000     MOVE W-SUMMA               TO MOD-KVLS                               
242100     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVLS-ATTR                          
242200                                                                          
242300     MOVE +0                    TO MOD-KVUTRS                             
242400     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                        
242500                                                                          
242600     PERFORM S01-FLYTTA-SKAPA-INVINFO                                     
242700     PERFORM S10-DATUM                                                    
242800                                                                          
242900     PERFORM IMS-LAES-INV-ROT                                             
243000                                                                          
243100     MOVE NEJ                     TO ART-INV-FINNS                        
243200     IF SEGMENT-FINNS                                                     
243300       MOVE DCS-IDDC              TO W1-IDDC-WDH1                         
243400                                     W2-IDDC-WDH1                         
243500       MOVE 01                    TO W1-KDINVKAT                          
243600       MOVE 11                    TO W2-KDINVKAT                          
243700                                                                          
243800       MOVE NEJ                   TO INV-KAT-FINNS                        
243900       PERFORM IMS-LAES-ARTIKEL-INV                                       
244000       PERFORM UNTIL INVKOE-ART-FINNS = NEJ                               
244100         IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                
244200           MOVE JA                TO INV-KAT-FINNS                        
244300                                     ART-INV-FINNS                        
244400         END-IF                                                           
244500         PERFORM IMS-LAES-ARTIKEL-INV                                     
244600       END-PERFORM                                                        
244700                                                                          
244800       IF INV-KAT-FINNS = NEJ                                             
244900         PERFORM IMS-LAES-INV-ROT                                         
245000                                                                          
245100         MOVE DCS-IDDC TO W-IDDC-WDH1-MIN                                 
245200                          W-IDDC-WDH1-MAX                                 
245300         MOVE +2       TO W-KDINVKAT-MIN                                  
245400                          W-KDINVKAT-MAX                                  
245500                                                                          
245600         PERFORM IMS-LAES-INV                                             
245700         IF SEGMENT-FINNS                                                 
245800           MOVE JA                TO ART-INV-FINNS                        
245900         END-IF                                                           
246000       END-IF                                                             
246100     END-IF                                                               
246200                                                                          
246300     PERFORM S011-EVALUERA-INVENT-KATEGORI                                
246400     .                                                                    
246500     EJECT                                                                
246600 KE-BEHANDLA-SDC-NDC SECTION.                                             
246700     MOVE 'KE      ' TO WS-SECTION                                        
246800     PERFORM IMS-GHU-WDK711                                               
246900     IF MID-KDAVVTYP = '+' OR '-'                                         
247000       IF MID-KDAVVTYP = '+'                                              
247100         COMPUTE W-SUMMA = SLAG-KVLS                                      
247200         + WS-KVJUSTKV-COMP                                               
247300       ELSE                                                               
247400         COMPUTE W-SUMMA = SLAG-KVLS                                      
247500         - WS-KVJUSTKV-COMP                                               
247600       END-IF                                                             
247700     ELSE                                                                 
247800       IF MID-KDAVVTYP = ' ' AND WS-KVJUSTKV-COMP = ZERO                  
247900         COMPUTE W-SUMMA = SLAG-KVUTRS + WS-KVJUSTKV-COMP                 
248000       END-IF                                                             
248100     END-IF                                                               
248200                                                                          
248300     MOVE W-SUMMA               TO MOD-KVLS                               
248400     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVLS-ATTR                          
248500                                                                          
248600     MOVE +0                    TO MOD-KVUTRS                             
248700     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVUTRS-ATTR                        
248800                                                                          
248900     PERFORM S01-FLYTTA-SKAPA-INVINFO                                     
249000     PERFORM S10-DATUM                                                    
249100                                                                          
249200     PERFORM IMS-LAES-INV-ROT                                             
249300                                                                          
249400     MOVE NEJ                     TO ART-INV-FINNS                        
249500     IF SEGMENT-FINNS                                                     
249600       MOVE DCS-IDDC              TO W1-IDDC-WDH1                         
249700                                     W2-IDDC-WDH1                         
249800       MOVE 01                    TO W1-KDINVKAT                          
249900       MOVE 11                    TO W2-KDINVKAT                          
250000                                                                          
250100       MOVE NEJ                   TO INV-KAT-FINNS                        
250200       PERFORM IMS-LAES-ARTIKEL-INV                                       
250300       PERFORM UNTIL INVKOE-ART-FINNS = NEJ                               
250400         IF INVB-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                
250500           MOVE JA                TO INV-KAT-FINNS                        
250600                                     ART-INV-FINNS                        
250700         END-IF                                                           
250800         PERFORM IMS-LAES-ARTIKEL-INV                                     
250900       END-PERFORM                                                        
251000                                                                          
251100       IF INV-KAT-FINNS = NEJ                                             
251200         PERFORM IMS-LAES-INV-ROT                                         
251300                                                                          
251400         MOVE DCS-IDDC            TO W-IDDC-WDH1-MIN                      
251500                                     W-IDDC-WDH1-MAX                      
251600         MOVE +2                  TO W-KDINVKAT-MIN                       
251700                                     W-KDINVKAT-MAX                       
251800         PERFORM IMS-LAES-INV                                             
251900         IF SEGMENT-FINNS                                                 
252000           MOVE JA               TO ART-INV-FINNS                         
252100         END-IF                                                           
252200       END-IF                                                             
252300     END-IF                                                               
252400                                                                          
252500     PERFORM S011-EVALUERA-INVENT-KATEGORI                                
252600     .                                                                    
252700     EJECT                                                                
252800******************************************************************        
252900*    FLYTTAR OM INFORMATION FÖR ATT KUNNA VISA NYTT                       
253000*    ADJUSTM.DATE(TIJUSTDA), ADJUSTED QUANTITY(KVJUSTKV),                 
253100*    ADJUSTM.TYPE(KDJUSTYP)                                               
253200*                                                                         
253300*                                                                         
253400 S01-FLYTTA-SKAPA-INVINFO SECTION.                                        
253500     MOVE 'S01     ' TO WS-SECTION                                        
253600     MOVE 5 TO INDX                                                       
253700     MOVE 6 TO INDY                                                       
253800                                                                          
253900     PERFORM UNTIL INDX < 1                                               
254000                                                                          
254100       MOVE MOD-TIJUSTDA      (INDX) TO MOD-TIJUSTDA-X    (INDY)          
254200       MOVE MOD-KVJUSTKV-UT   (INDX) TO MOD-KVJUSTKV-UT-X (INDY)          
254300       MOVE MOD-KDJUSTYP      (INDX) TO MOD-KDJUSTYP-X    (INDY)          
254400                                                                          
254500       SUBTRACT 1 FROM INDX INDY                                          
254600                                                                          
254700     END-PERFORM                                                          
254800                                                                          
254900     IF MID-KDAVVTYP = '-'                                                
255000       MOVE KVJUSTKV-WS         TO WS-KVJUSTKV-X                          
255100       MULTIPLY WS-KVJUSTKV-N BY -1 GIVING MOD-KVJUSTKV-UT (1)            
255200     ELSE                                                                 
255300       MOVE KVJUSTKV-WS         TO MOD-KVJUSTKV-UT (1)                    
255400     END-IF                                                               
255500                                                                          
255600     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIJUSTDA-ATTR (1)                  
255700                                   MOD-KVJUSTKV-UT-ATTR (1)               
255800                                   MOD-KDJUSTYP-ATTR (1)                  
255900     .                                                                    
256000     EJECT                                                                
256100 S011-EVALUERA-INVENT-KATEGORI SECTION.                                   
256200     MOVE 'S011    ' TO WS-SECTION                                        
256300     IF ART-INV-FINNS = NEJ                                               
256400       MOVE 7 TO MOD-KDJUSTYP (1)                                         
256500     ELSE                                                                 
256600                                                                          
256700       EVALUATE TRUE                                                      
256800                                                                          
256900       WHEN INVB-INV-KDINVKAT = 1                                         
257000         MOVE 1 TO MOD-KDJUSTYP (1)                                       
257100                                                                          
257200       WHEN INVB-INV-KDINVKAT = 2                                         
257300         MOVE 2 TO MOD-KDJUSTYP (1)                                       
257400                                                                          
257500       WHEN INVB-INV-KDINVKAT = 3                                         
257600         MOVE 3 TO MOD-KDJUSTYP (1)                                       
257700                                                                          
257800       WHEN INVB-INV-KDINVKAT = 4                                         
257900         MOVE 4 TO MOD-KDJUSTYP (1)                                       
258000                                                                          
258100       WHEN INVB-INV-KDINVKAT = 5                                         
258200         MOVE 5 TO MOD-KDJUSTYP (1)                                       
258300                                                                          
258400       WHEN INVB-INV-KDINVKAT = 8                                         
258500         MOVE 8 TO MOD-KDJUSTYP (1)                                       
258600                                                                          
258700       WHEN INVB-INV-KDINVKAT = 9                                         
258800         MOVE 9 TO MOD-KDJUSTYP (1)                                       
258900                                                                          
259000        WHEN OTHER                                                        
259100          MOVE 'FELAKTIG KATEGORI = KDINVKAT-WS'                          
259200                TO FELTEXT                                                
259300                CALL FELLOG                                               
259400       END-EVALUATE                                                       
259500                                                                          
259600     END-IF                                                               
259700     .                                                                    
259800     EJECT                                                                
259900 S02-SKAPA-TISEGKEY SECTION.                                              
260000                                                                          
260100     MOVE 20          TO WS-SEKEL                                         
260200     MOVE 9           TO WS-LOPNR                                         
260300                                                                          
260400     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
260500                                                                          
260600     MOVE WS-TISEGKEY TO INVH-TISEGKEY                                    
260700     .                                                                    
260800     EJECT                                                                
260900 S03-KONV-DATUM SECTION.                                                  
261000                                                                          
261100     MOVE INVH-DAREGDAT-CLO(3:6) TO DAT-I-TIDATUM                         
261200     MOVE 'AAMMDD'                    TO DAT-KDDATFORM                    
261300                                                                          
261400     CALL WDATKONV USING DAT-KDDATFORM                                    
261500                         DAT-I-TIDATUM                                    
261600                         DAT-O-TIDATUM                                    
261700                         DAT-KDSVAR                                       
261800     .                                                                    
261900     EJECT                                                                
262000 S10-DATUM SECTION.                                                       
262100                                                                          
262200     MOVE 'IDAG  '      TO DAT-KDDATFORM                                  
262300     CALL  WDATKONV  USING DAT-KDDATFORM                                  
262400                           DAT-I-TIDATUM                                  
262500                           DAT-O-TIDATUM                                  
262600                           DAT-KDSVAR                                     
262700                                                                          
262800     IF DAT-KDSVAR-OK                                                     
262900         CONTINUE                                                         
263000     ELSE                                                                 
263100         CALL  FELLOG                                                     
263200     END-IF                                                               
263300                                                                          
263400     MOVE DAT-TIAAVVD  TO MOD-TIJUSTDA (1)                                
263500     MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                                     
263600                          DAGENS-DATUM(3:6)                               
263700     MOVE DAT-TISEKEL  TO DAGENS-DATUM(1:2)                               
263800     .                                                                    
263900     EJECT                                                                
264000* IMS SEKTIONER                                                           
264100                                                                          
264200 IMS-GET-MSG SECTION.                                                     
264300                                                                          
264400     MOVE '  QC' TO GODK-STATUSKODER                                      
264500     CALL  CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                          
264600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
264700     PERFORM IMS-STATUSKONTROLL                                           
264800     .                                                                    
264900     SKIP3                                                                
265000 IMS-INSERT-MSG SECTION.                                                  
265100                                                                          
265200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
265300       MOVE '0' TO MFS-KDHUVOMR                                           
265400     END-IF                                                               
265500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
265600     MOVE SPACE TO GODK-STATUSKODER                                       
265700     CALL  CBLTDLI  USING ISRT MSG-PCB                                    
265800                               MSG-IO-AREA MFS-IDMOD                      
265900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
266000     PERFORM IMS-STATUSKONTROLL                                           
266100     .                                                                    
266200     EJECT                                                                
266300************************ WDH1 INVENTERINGSBAS ********************        
266400                                                                          
266500 IMS-LAES-ARTIKEL-INV SECTION.                                            
266600     MOVE 'IMS-LAES-ARTIKEL-INV' TO WS-IMS                                
266700     STRING 'WDH111  (WDH111KY >' W1-WDH1KEY-X                            
266800                    '&WDH111KY <' W2-WDH1KEY-X                            
266900                    '&FLINVBEH =' NEJ ')'                                 
267000            DELIMITED BY SIZE INTO SSA1                                   
267100     MOVE '  GE' TO GODK-STATUSKODER                                      
267200     CALL  CBLTDLI  USING GNP INVREG-PCB INVB-WDH111   SSA1               
267300     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
267400     PERFORM IMS-STATUSKONTROLL                                           
267500     IF SEGMENT-FINNS AND INVB-INV-FLINVBEH = NEJ                         
267600       MOVE JA    TO INVKOE-ART-FINNS                                     
267700     ELSE                                                                 
267800       MOVE NEJ   TO INVKOE-ART-FINNS                                     
267900     END-IF                                                               
268000     .                                                                    
268100     SKIP2                                                                
268200 IMS-LAES-WDH111-UNIK SECTION.                                            
268300     MOVE 'IMS-LAES-WDH11-UNIK ' TO WS-IMS                                
268400     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
268500            DELIMITED BY SIZE INTO SSA1                                   
268600     STRING 'WDH111  (WDH111KY =' W-WDH1KEY-UNIK-X ')'                    
268700            DELIMITED BY SIZE INTO SSA2                                   
268800     MOVE '  GE' TO GODK-STATUSKODER                                      
268900     CALL  CBLTDLI  USING GHU INVREG-PCB INVB-WDH111 SSA1 SSA2            
269000     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
269100     PERFORM IMS-STATUSKONTROLL                                           
269200                                                                          
269300     .                                                                    
269400     SKIP2                                                                
269500 IMS-LAES-WDH121             SECTION.                                     
269600     MOVE 'IMS-LAES-WDH121     ' TO WS-IMS                                
269700     MOVE 'WDH121   '  TO SSA1                                            
269800     MOVE '  GE' TO GODK-STATUSKODER                                      
269900     CALL  CBLTDLI  USING GNP INVREG-PCB INVB-WDH121 SSA1                 
270000     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
270100     PERFORM IMS-STATUSKONTROLL                                           
270200     .                                                                    
270300     SKIP2                                                                
270400 IMS-LAES-INV  SECTION.                                                   
270500     MOVE 'IMS-LAES-INV        ' TO WS-IMS                                
270600     STRING 'WDH111  (WDH111KY >' W-WDH1KEY-MIN-X                         
270700                    '&WDH111KY <' W-WDH1KEY-MAX-X                         
270800                    '&FLINVBEH =' NEJ ')'                                 
270900            DELIMITED BY SIZE INTO SSA1                                   
271000     MOVE '  GE' TO GODK-STATUSKODER                                      
271100     CALL CBLTDLI USING GNP INVREG-PCB INVB-WDH111   SSA1                 
271200     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
271300     PERFORM IMS-STATUSKONTROLL                                           
271400     .                                                                    
271500     EJECT                                                                
271600 IMS-LAES-INV-ROT  SECTION.                                               
271700     MOVE 'IMS-LAES-INV-ROT    ' TO WS-IMS                                
271800     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
271900            DELIMITED BY SIZE INTO SSA1                                   
272000     MOVE '  GE' TO GODK-STATUSKODER                                      
272100     CALL  CBLTDLI  USING GU INVREG-PCB INVB-WDH101   SSA1                
272200     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
272300     PERFORM IMS-STATUSKONTROLL                                           
272400     .                                                                    
272500     SKIP2                                                                
272600 IMS-ISRT-INV-ROT  SECTION.                                               
272700     MOVE 'IMS-ISRT-INV-ROT    ' TO WS-IMS                                
272800     MOVE 'WDH101   '  TO SSA1                                            
272900     MOVE '  II' TO GODK-STATUSKODER                                      
273000     CALL  CBLTDLI  USING ISRT INVREG-PCB INVB-WDH101   SSA1              
273100     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
273200     PERFORM IMS-STATUSKONTROLL                                           
273300     .                                                                    
273400     SKIP2                                                                
273500 IMS-LAES-INV-SEG  SECTION.                                               
273600     MOVE 'IMS-LAES-SEG        ' TO WS-IMS                                
273700     STRING 'WDH111  (WDH111KY >' W1-WDH1KEY-X '&WDH111KY <'              
273800       W2-WDH1KEY-X ')'                                                   
273900            DELIMITED BY SIZE INTO SSA1                                   
274000     MOVE '  GE' TO GODK-STATUSKODER                                      
274100     CALL  CBLTDLI  USING GHNP INVREG-PCB INVB-WDH111   SSA1              
274200     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
274300     PERFORM IMS-STATUSKONTROLL                                           
274400     .                                                                    
274500     EJECT                                                                
274600 IMS-ISRT-INV-SEG  SECTION.                                               
274700     MOVE 'IMS-ISRT-INV-SEG    ' TO WS-IMS                                
274800     MOVE 'WDH111 ' TO SSA1                                               
274900     MOVE '  IINI' TO GODK-STATUSKODER                                    
275000     CALL  CBLTDLI  USING ISRT INVREG-PCB INVB-WDH111   SSA1              
275100     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
275200     PERFORM IMS-STATUSKONTROLL                                           
275300     .                                                                    
275400     SKIP2                                                                
275500 IMS-INSERT-WDH121      SECTION.                                          
275600     MOVE 'IMS-INSERT-WDH121   ' TO WS-IMS                                
275700     MOVE 'WDH121 ' TO SSA1                                               
275800     MOVE '  II' TO GODK-STATUSKODER                                      
275900     CALL CBLTDLI USING ISRT INVREG-PCB  INVB-WDH121   SSA1               
276000     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
276100     PERFORM IMS-STATUSKONTROLL                                           
276200     .                                                                    
276300     EJECT                                                                
276400 IMS-REPL-INV-SEG  SECTION.                                               
276500     MOVE 'IMS-REPL-INV-SEG    ' TO WS-IMS                                
276600     MOVE '    ' TO GODK-STATUSKODER                                      
276700     CALL  CBLTDLI  USING REPL INVREG-PCB INVB-WDH111                     
276800     MOVE INVREG-STATUS-CODE TO STATUS-WS                                 
276900     PERFORM IMS-STATUSKONTROLL                                           
277000     .                                                                    
277100     EJECT                                                                
277200********************** WDK6 ARTIKELREGISTER CDC ******************        
277300                                                                          
277400 IMS-LAES-ARTIKEL-6 SECTION.                                              
277500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
277600            DELIMITED BY SIZE INTO SSA1                                   
277700     MOVE '  GE' TO GODK-STATUSKODER                                      
277800     CALL  CBLTDLI  USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                 
277900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
278000     PERFORM IMS-STATUSKONTROLL                                           
278100     .                                                                    
278200     SKIP3                                                                
278300 IMS-LAES-EKONOMISEG SECTION.                                             
278400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
278500            DELIMITED BY SIZE INTO SSA1                                   
278600     MOVE '  GE' TO GODK-STATUSKODER                                      
278700     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                
278800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
278900     PERFORM IMS-STATUSKONTROLL                                           
279000     .                                                                    
279100                                                                          
279200 IMS-GU-ARTIKEL-6 SECTION.                                                
279300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
279400            DELIMITED BY SIZE INTO SSA1                                   
279500     MOVE '  ' TO GODK-STATUSKODER                                        
279600     CALL  CBLTDLI  USING GU WDK6-PCB DLI-IO-WDK601 SSA1                  
279700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
279800     PERFORM IMS-STATUSKONTROLL                                           
279900     .                                                                    
280000     SKIP3                                                                
280100 IMS-GET-ART-ROT  SECTION.                                                
280200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X                             
280300                    '&KDERS    =' W-KDERS-0-X ')'                         
280400            DELIMITED BY SIZE INTO SSA1                                   
280500     MOVE '  ' TO GODK-STATUSKODER                                        
280600     CALL  CBLTDLI  USING GU WDK6-PCB DLI-IO-WDK601 SSA1                  
280700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
280800     PERFORM IMS-STATUSKONTROLL                                           
280900     .                                                                    
281000     SKIP3                                                                
281100 IMS-GNP-EKONOMISEG SECTION.                                              
281200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
281300            DELIMITED BY SIZE INTO SSA1                                   
281400     MOVE '  ' TO GODK-STATUSKODER                                        
281500     CALL  CBLTDLI  USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                 
281600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
281700     PERFORM IMS-STATUSKONTROLL                                           
281800     .                                                                    
281900     SKIP2                                                                
282000 IMS-REPL-CLAGER-SEGM SECTION.                                            
282100                                                                          
282200     MOVE '  ' TO GODK-STATUSKODER                                        
282300     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-WDK611                     
282400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
282500     PERFORM IMS-STATUSKONTROLL                                           
282600     .                                                                    
282700     EJECT                                                                
282800                                                                          
282900 IMS-GHU-WDK629  SECTION.                                                 
283000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
283100          DELIMITED BY SIZE INTO SSA1                                     
283200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
283300          DELIMITED BY SIZE INTO SSA2                                     
283400     MOVE   'WDK629  '        TO SSA3                                     
283500     MOVE '  GE' TO GODK-STATUSKODER                                      
283600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
283700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
283800     PERFORM IMS-STATUSKONTROLL                                           
283900     .                                                                    
284000     SKIP3                                                                
284100 IMS-REPL-WDK629 SECTION.                                                 
284200     MOVE '  ' TO GODK-STATUSKODER                                        
284300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
284400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
284500     PERFORM IMS-STATUSKONTROLL                                           
284600     .                                                                    
284700     EJECT                                                                
284800************* WDR4 4505 HÄNDELSEBAS ( TÄCKNING ORDERADSREG.) ****         
284900                                                                          
285000 IMS-ISRT-450511 SECTION.                                                 
285100                                                                          
285200     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
285300            DELIMITED BY SIZE INTO SSA1                                   
285400     MOVE 'WL450511 '                   TO SSA2                           
285500     MOVE '  ' TO GODK-STATUSKODER                                        
285600     CALL  CBLTDLI  USING ISRT 4505-PCB WL450511 SSA1 SSA2                
285700     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
285800     PERFORM IMS-STATUSKONTROLL                                           
285900     .                                                                    
286000     EJECT                                                                
286100                                                                          
286200************ WDG2 XXEF HÄNDELSEBAS (UTREDNINGSSALDO) *************        
286300                                                                          
286400 IMS-GHU-G2-UTREDNSALDO SECTION.                                          
286500                                                                          
286600     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
286700           DELIMITED BY SIZE INTO SSA1                                    
286800     STRING 'WLXXEF11(WDGXKEY  =' W-IDARTNR-UTR-X ')'                     
286900           DELIMITED BY SIZE INTO SSA2                                    
287000     MOVE '  GE' TO GODK-STATUSKODER                                      
287100     CALL  CBLTDLI  USING GHU XXEF-PCB WLXXEF11 SSA1 SSA2                 
287200     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
287300     PERFORM IMS-STATUSKONTROLL                                           
287400     .                                                                    
287500     SKIP2                                                                
287600 IMS-DELETE-G2-UTREDNSALDO SECTION.                                       
287700                                                                          
287800     MOVE '  ' TO GODK-STATUSKODER                                        
287900     CALL  CBLTDLI  USING DLET XXEF-PCB WLXXEF11                          
288000     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
288100     PERFORM IMS-STATUSKONTROLL                                           
288200     .                                                                    
288300     EJECT                                                                
288400************ WDK7 ARTIKELREGISTER SDC  ***************************        
288500                                                                          
288600 IMS-GHU-WDK711 SECTION.                                                  
288700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
288800            DELIMITED BY SIZE INTO SSA1                                   
288900     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
289000            DELIMITED BY SIZE INTO SSA2                                   
289100     MOVE '  GE' TO GODK-STATUSKODER                                      
289200     CALL  CBLTDLI  USING GHU ARTS-PCB WLARTS11 SSA1 SSA2                 
289300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
289400     PERFORM IMS-STATUSKONTROLL                                           
289500     .                                                                    
289600     SKIP2                                                                
289700                                                                          
289800 IMS-GU-WDK711 SECTION.                                                   
289900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
290000            DELIMITED BY SIZE INTO SSA1                                   
290100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
290200            DELIMITED BY SIZE INTO SSA2                                   
290300     MOVE '    ' TO GODK-STATUSKODER                                      
290400     CALL  CBLTDLI  USING GU ARTS-PCB WLARTS11 SSA1 SSA2                  
290500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
290600     PERFORM IMS-STATUSKONTROLL                                           
290700     .                                                                    
290800     SKIP2                                                                
290900 IMS-REPL-SLAGER-SEGM SECTION.                                            
291000                                                                          
291100     MOVE '  ' TO GODK-STATUSKODER                                        
291200     CALL  CBLTDLI  USING REPL ARTS-PCB WLARTS11                          
291300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
291400     PERFORM IMS-STATUSKONTROLL                                           
291500     .                                                                    
291600     EJECT                                                                
291700************ WDH7 INVENTERINGSHISTORIK  **************************        
291800                                                                          
291900 IMS-GHU-INVHIST-ROT SECTION.                                             
292000     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
292100            DELIMITED BY SIZE INTO SSA1                                   
292200     MOVE '  GE' TO GODK-STATUSKODER                                      
292300     CALL  CBLTDLI  USING GHU INVC-PCB WLINVC01 SSA1                      
292400     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
292500     PERFORM IMS-STATUSKONTROLL                                           
292600     .                                                                    
292700     SKIP2                                                                
292800 IMS-GNP-INVHIST-WDH7 SECTION.                                            
292900     STRING 'WLINVC11(TISEGKEY<=' W-TISEGKEY-X                            
293000                    '&IDDC     =' W-IDDC-X ')'                            
293100            DELIMITED BY SIZE INTO SSA1                                   
293200     MOVE '  GE' TO GODK-STATUSKODER                                      
293300     CALL  CBLTDLI  USING GHNP INVC-PCB WLINVC11 SSA1                     
293400     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
293500     PERFORM IMS-STATUSKONTROLL                                           
293600     .                                                                    
293700     SKIP2                                                                
293800 IMS-ISRT-INVHIST-ROT SECTION.                                            
293900                                                                          
294000     MOVE 'WLINVC01 ' TO SSA1                                             
294100     MOVE '  ' TO GODK-STATUSKODER                                        
294200     CALL  CBLTDLI  USING ISRT INVC-PCB WLINVC01 SSA1                     
294300     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
294400     PERFORM IMS-STATUSKONTROLL                                           
294500     .                                                                    
294600     EJECT                                                                
294700 IMS-ISRT-INVHIST-SEGM SECTION.                                           
294800                                                                          
294900     MOVE 'WLINVC11 ' TO SSA1                                             
295000     MOVE '  II' TO GODK-STATUSKODER                                      
295100     CALL  CBLTDLI  USING ISRT INVC-PCB WLINVC11 SSA1                     
295200     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
295300     PERFORM IMS-STATUSKONTROLL                                           
295400     .                                                                    
295500     EJECT                                                                
295600*********  WDL2  INLEVERANS HISTORIK CDC *************************        
295700                                                                          
295800 IMS-GET-INLEVROT    SECTION.                                             
295900                                                                          
296000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X  ') '                       
296100            DELIMITED BY SIZE INTO SSA1                                   
296200     MOVE '  GE' TO GODK-STATUSKODER                                      
296300     CALL  CBLTDLI  USING GU INLE-PCB INL-WLINLE01 SSA1                   
296400     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
296500     PERFORM IMS-STATUSKONTROLL                                           
296600     .                                                                    
296700     SKIP2                                                                
296800 IMS-GET-INLEVNR     SECTION.                                             
296900                                                                          
297000     MOVE 'WLINLE11 '   TO SSA1                                           
297100     MOVE '  GE' TO GODK-STATUSKODER                                      
297200     CALL  CBLTDLI  USING GNP INLE-PCB INL-WLINLE11 SSA1                  
297300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
297400     PERFORM IMS-STATUSKONTROLL                                           
297500     .                                                                    
297600     SKIP2                                                                
297700 IMS-GET-INLEVTRANS3X  SECTION.                                           
297800                                                                          
297900     STRING 'WLINLE11(DAINLEV  =' W-DAINLEVNYCK-X ')'                     
298000            DELIMITED BY SIZE INTO SSA1                                   
298100     MOVE 'WLINLE21 ' TO SSA2                                             
298200     MOVE '  GE' TO GODK-STATUSKODER                                      
298300     CALL  CBLTDLI  USING GNP INLE-PCB INL-WLINLE21 SSA1 SSA2             
298400     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
298500     PERFORM IMS-STATUSKONTROLL                                           
298600     .                                                                    
298700     EJECT                                                                
298800 IMS-GET-INLEVTRANSR34 SECTION.                                           
298900                                                                          
299000     STRING 'WLINLE11(DAINLEV  =' W-DAINLEVNYCK-X ')'                     
299100            DELIMITED BY SIZE INTO SSA1                                   
299200     STRING 'WLINLE22(IDPTYP   =' W-IDLEVNYCK-X ')'                       
299300            DELIMITED BY SIZE INTO SSA2                                   
299400     MOVE '  GE' TO GODK-STATUSKODER                                      
299500     CALL  CBLTDLI  USING GNP INLE-PCB INL-WLINLE22 SSA1 SSA2             
299600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
299700     PERFORM IMS-STATUSKONTROLL                                           
299800     .                                                                    
299900     SKIP2                                                                
300000 IMS-GET-P32TRANS      SECTION.                                           
300100                                                                          
300200     STRING 'WLINLE11(DAINLEV  =' W-DAINLEVNYCK-X ')'                     
300300            DELIMITED BY SIZE INTO SSA1                                   
300400     MOVE 'WLINLE21 ' TO SSA2                                             
300500     MOVE 'WLINLE31 ' TO SSA3                                             
300600     MOVE '  GE' TO GODK-STATUSKODER                                      
300700     CALL  CBLTDLI  USING GNP INLE-PCB INL-WLINLE31                       
300800                          SSA1 SSA2 SSA3                                  
300900     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
301000     PERFORM IMS-STATUSKONTROLL                                           
301100     .                                                                    
301200     EJECT                                                                
301300*********** WDL6 INLEVERANSHISTORIK SDC *************************         
301400                                                                          
301500 IMS-GET-WDL601      SECTION.                                             
301600                                                                          
301700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X  ') '                       
301800            DELIMITED BY SIZE INTO SSA1                                   
301900     MOVE '  GE' TO GODK-STATUSKODER                                      
302000     CALL  CBLTDLI  USING GU INLC-PCB INLC-WLINLC01 SSA1                  
302100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
302200     PERFORM IMS-STATUSKONTROLL                                           
302300     .                                                                    
302400     SKIP2                                                                
302500 IMS-GNP-WDL611      SECTION.                                             
302600                                                                          
302700     MOVE 'WLINLC11 '   TO SSA1                                           
302800     MOVE '  GE' TO GODK-STATUSKODER                                      
302900     CALL  CBLTDLI  USING GNP INLC-PCB INLC-WLINLC11 SSA1                 
303000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
303100     PERFORM IMS-STATUSKONTROLL                                           
303200     .                                                                    
303300     EJECT                                                                
303400********** WDL9 SALDOLOGG*****************************************        
303500 IMS-ISRT-WDL901 SECTION.                                                 
303600     SKIP2                                                                
303700     MOVE 'WLLOGA01 ' TO SSA1                                             
303800     MOVE '  II' TO GODK-STATUSKODER                                      
303900     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
304000     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
304100     PERFORM IMS-STATUSKONTROLL                                           
304200     .                                                                    
304300     EJECT                                                                
304400********** WDR9 PEDAL*********************************************        
304500 IMS-ISRT-WDR901 SECTION.                                                 
304600     SKIP2                                                                
304700     MOVE 'WLSAPA01 ' TO SSA1                                             
304800     MOVE '  II' TO GODK-STATUSKODER                                      
304900     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
305000     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
305100     PERFORM IMS-STATUSKONTROLL                                           
305200     .                                                                    
305300     EJECT                                                                
305400********** WDD8 SALDOREGISTER BUFFERTLAGER ***********************        
305500                                                                          
305600 IMS-GET-SALDOREG    SECTION.                                             
305700     SKIP2                                                                
305800     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X  ') '                       
305900            DELIMITED BY SIZE INTO SSA1                                   
306000     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
306100            DELIMITED BY SIZE INTO SSA2                                   
306200     MOVE '  GE' TO GODK-STATUSKODER                                      
306300     CALL  CBLTDLI  USING GU ARTD-PCB ART-WLARTD11 SSA1 SSA2              
306400     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
306500     PERFORM IMS-STATUSKONTROLL                                           
306600     .                                                                    
306700     SKIP3                                                                
306800*********** WDD3 BENÄMNINGSREGISTER   ****************************        
306900                                                                          
307000 IMS-GU-BEN SECTION.                                                      
307100                                                                          
307200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X  ') '                       
307300            DELIMITED BY SIZE INTO SSA1                                   
307400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
307500            DELIMITED BY SIZE INTO SSA2                                   
307600     MOVE '  GE' TO GODK-STATUSKODER                                      
307700     CALL  CBLTDLI  USING GU BEN-PCB BEN-WLBENA11 SSA1 SSA2               
307800     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
307900     PERFORM IMS-STATUSKONTROLL                                           
308000     .                                                                    
308100     EJECT                                                                
308200 IMS-ISRT-WDR801   SECTION.                                               
308300                                                                          
308400     MOVE 'WLFILB01 '   TO SSA1                                           
308500     MOVE '  II' TO GODK-STATUSKODER                                      
308600     CALL CBLTDLI USING ISRT WFILB-PCB A08-WFILB01 SSA1                   
308700     MOVE WFILB-STATUS-CODE TO STATUS-WS                                  
308800     PERFORM IMS-STATUSKONTROLL                                           
308900     .                                                                    
309000     EJECT                                                                
310400 IMS-GU-WDB601    SECTION.                                                
310500     MOVE 'IMS-GU-WDB601       ' TO WS-IMS                                
310600                                                                          
310700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
310800          DELIMITED BY SIZE INTO SSA1                                     
310900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
311000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
311100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
311200     PERFORM IMS-STATUSKONTROLL                                           
311300     .                                                                    
311400     EJECT                                                                
311500 IMS-STATUSKONTROLL SECTION.                                              
311600                                                                          
311700     SET STATUS-IX TO 1                                                   
311800     SEARCH GODK-STATUS                                                   
311900       AT END                                                             
312000         CALL  FELLOG                                                     
312100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
312200         CONTINUE                                                         
312300     END-SEARCH                                                           
312400     .                                                                    
