000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4183300.                                                
000300 AUTHOR.         SUSANNE OLSSON.                                          
000400 DATE-WRITTEN.   02/04/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET UPPDATERAR OLIKA BASER FÖR DE RADER SOM ÄR            
001000*        KREDITERADE VIA BILL-IT. MOTSVARAR PGM W41835 FÖR                
001100*        PULSKREDITERINGAR.                                               
001200*        SKAPAR INTERN DOKUMENT MED CREDIT FÖR KOD 74 (N-FAKTURA).        
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WDA2  LEV.ANMÄRKNINGAR                     
001500*        PROGRAMMET UPPDATERAR WDK6  ARTIKELREGISTER CDC                  
001600*        PROGRAMMET UPPDATERAR WDK7  ARTIKELREGISTER SDC                  
001700*        PROGRAMMET UPPDATERAR WDH1  INVENTERINGSREGISTER                 
001800*        PROGRAMMET UPPDATERAR WDL9  SALDOFÖRÄNDRINGAR                    
001900*        PROGRAMMET UPPDATERAR WDR8  EKONOMITRANSAR VCCN                  
002000*        PROGRAMMET UPPDATERAR WDR9  EKONOMITRANSAR VCCS                  
002100*        PROGRAMMET UPPDATERAR WDR4  ÅTERSTARTSREGISTER                   
002200*                                    HTYP=4579, SEGMENT=WDGX4580          
002300*        PROGRAMMET UPPDATERAR WDR5  ATTESTANSVARIGA KREDITNOTOR          
002400*                                    HTYP=4103, SEGMENT=WDGX4103          
002500*                                                                         
002600*        E-TRACKER: 1572353  DATE 2005-05-19                              
002700*        E-TRACKER: 1658417  DATE 2006-03-17                              
002800*        E-TRACKER: 850114   DATE 2007-04-03                              
002900*        E-TRACKER: 10143271 DATE 2011-12-09                              
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     SKIP2                                                                
003900*          --- INFIL MED UPPDATERINGSPOSTER FRÅN W418AL                   
004000     SELECT W418AL                     ASSIGN TO W41833D1.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W418AL                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W41834A     -L.                                                
005100                                                                          
005200*01  -COPY W41834B     -L.                                                
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W4183300'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  IX                          PIC 9(2)    VALUE ZERO.                  
006000 77  IX2                         PIC 9(2)    VALUE ZERO.                  
006100 77  INDX                        PIC 9(3)    VALUE ZERO.                  
006200 77  INDX1                       PIC 9(3)    VALUE ZERO.                  
006300 77  WS-KDANMORS                 PIC 9(3)    VALUE ZERO.                  
006400 77  WS-TIKNOTA                  PIC 9(6).                                
006500 77  SPAR-KDANMORS               PIC X(2)    VALUE SPACE.                 
006600 77  SPAR-KDEKSHT-CHINA          PIC X(3)    VALUE SPACE.                 
006700 77  W-KVPOST-IN                 PIC S9(9)   VALUE +0   COMP-3.           
006800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3)   VALUE +16  COMP-3.           
006900 77  W418AL-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  END-OF-W418AL                       VALUE 'J'.                   
007100 77  IDTRACK-QTY-SW              PIC X       VALUE 'N'.                   
007200     88  IDTRACK-QTY-DONE                    VALUE 'J'.                   
007300     88  IDTRACK-QTY-NOT-DONE                VALUE 'N'.                   
007310 77  SW-LYNK-NON-API              PIC X(1)   VALUE 'N'.                   
007320     88 LYNK-NON-API                         VALUE 'J'.                   
007400                                                                          
007500                                                                          
007600 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
007700     88 EVENT-YES                            VALUE 'LYNK'                 
007800                                                   'POLE'                 
007900                                                   'ECOM'                 
008000                                                   'ACC '                 
008100                                                   'APA '                 
008200                                                   'APB '                 
008300                                                   'APC '                 
008400                                                   'APD '                 
008500                                                   'APE '                 
008600                                                   'APF '                 
008700                                                   'APG '                 
008800                                                   'APH '                 
008900                                                   'API '                 
009000                                                   'APJ '                 
009100                                                   'TAD '.                
009200     88 EVENT-NO                             VALUE '    '.                
009300                                                                          
009400 77  WS-EVENT-KUND               PIC X(1)    VALUE SPACE.                 
009500 77  TEST-IDFKNGRP               PIC S9(5)   COMP-3.                      
009600     88  FKNGRP-VSA-IMP                      VALUE 1788.                  
009700     88  FKNGRP-EXT-WARRANTY                 VALUE 1728.                  
009800                                                                          
009900 01  WS-IDAPIDISCREF.                                                     
010000     03 WS-IDDISTR-EVENT         PIC 9(4).                                
010100     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
010200     03 WS-IDRAPPNR-EVENT        PIC 9(7).                                
010300                                                                          
010400     EJECT                                                                
010500 01  FILLER                 PIC X(16) VALUE 'TEST-IDDISTRIKT '.           
010600 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
010700     EJECT                                                                
010800*01  FILLER  -COPY WWDIST35 -RED TEST-IDDISTR.                            
010900     EJECT                                                                
011000*01  FILLER  -COPY WWDIST18 -RED TEST-IDDISTR.                            
011100     EJECT                                                                
011200                                                                          
011300 77  W-TEST-IDKUNDNR             PIC 9(6)    VALUE ZERO.                  
011400     88 NDC-CN-KUND                          VALUE 000071                 
011500                                                   000072                 
011600                                                   000073                 
011700                                                   000074                 
011800                                                   007000                 
011900                                                   007001                 
012000                                                   007002                 
012100                                                   007003                 
012200                                                   007004                 
012300                                                   007005                 
012400                                                   007006                 
012500                                                   007007.                
012600     88 NDC-IN-KUND                          VALUE 000067.                
012700     88 NDC-NON-VCC-CUST                     VALUE 000052                 
012800                                                   000053                 
012900                                                   000063                 
013000                                                   000064                 
013100                                                   000065                 
013200                                                   000066                 
013300                                                   000067                 
013400                                                   000071                 
013500                                                   000072                 
013600                                                   000073                 
013700                                                   000074                 
013800                                                   000085                 
013900                                                   000086                 
014000                                                   000087                 
014100                                                   000093                 
014200                                                   007000                 
014300                                                   007001                 
014400                                                   007002                 
014500                                                   007003                 
014600                                                   007004                 
014700                                                   007005                 
014800                                                   007006                 
014900                                                   007007.                
015000                                                                          
015100                                                                          
015200                                                                          
015300     EJECT                                                                
015400******************************************************************        
015500 01  TABELL.                                                              
015600     03  FILLER             PIC X(11)   VALUE '000 301 371'.              
015700     03  FILLER             PIC X(11)   VALUE '001 000 000'.              
015800     03  FILLER             PIC X(11)   VALUE '002 000 000'.              
015900     03  FILLER             PIC X(11)   VALUE '003 000 000'.              
016000     03  FILLER             PIC X(11)   VALUE '004 000 000'.              
016100     03  FILLER             PIC X(11)   VALUE '005 000 000'.              
016200     03  FILLER             PIC X(11)   VALUE '006 000 000'.              
016300     03  FILLER             PIC X(11)   VALUE '007 000 000'.              
016400     03  FILLER             PIC X(11)   VALUE '008 000 000'.              
016500     03  FILLER             PIC X(11)   VALUE '009 000 000'.              
016600     03  FILLER             PIC X(11)   VALUE '010 000 000'.              
016700     03  FILLER             PIC X(11)   VALUE '011 ??? ???'.              
016800     03  FILLER             PIC X(11)   VALUE '012 311 311'.              
016900     03  FILLER             PIC X(11)   VALUE '013 310 310'.              
017000     03  FILLER             PIC X(11)   VALUE '014 000 000'.              
017100     03  FILLER             PIC X(11)   VALUE '015 000 000'.              
017200     03  FILLER             PIC X(11)   VALUE '016 000 000'.              
017300     03  FILLER             PIC X(11)   VALUE '017 000 000'.              
017400     03  FILLER             PIC X(11)   VALUE '018 000 000'.              
017500     03  FILLER             PIC X(11)   VALUE '019 000 000'.              
017600     03  FILLER             PIC X(11)   VALUE '020 301 371'.              
017700     03  FILLER             PIC X(11)   VALUE '021 ??? ???'.              
017800     03  FILLER             PIC X(11)   VALUE '022 311 311'.              
017900     03  FILLER             PIC X(11)   VALUE '023 310 310'.              
018000     03  FILLER             PIC X(11)   VALUE '024 000 000'.              
018100     03  FILLER             PIC X(11)   VALUE '025 315 365'.              
018200     03  FILLER             PIC X(11)   VALUE '026 000 000'.              
018300     03  FILLER             PIC X(11)   VALUE '027 316 366'.              
018400     03  FILLER             PIC X(11)   VALUE '028 318 368'.              
018500     03  FILLER             PIC X(11)   VALUE '029 000 000'.              
018600     03  FILLER             PIC X(11)   VALUE '030 303 353'.              
018700     03  FILLER             PIC X(11)   VALUE '031 000 000'.              
018800     03  FILLER             PIC X(11)   VALUE '032 000 000'.              
018900     03  FILLER             PIC X(11)   VALUE '033 000 000'.              
019000     03  FILLER             PIC X(11)   VALUE '034 000 000'.              
019100     03  FILLER             PIC X(11)   VALUE '035 000 000'.              
019200     03  FILLER             PIC X(11)   VALUE '036 000 000'.              
019300     03  FILLER             PIC X(11)   VALUE '037 000 000'.              
019400     03  FILLER             PIC X(11)   VALUE '038 000 000'.              
019500     03  FILLER             PIC X(11)   VALUE '039 000 000'.              
019600     03  FILLER             PIC X(11)   VALUE '040 304 354'.              
019700     03  FILLER             PIC X(11)   VALUE '041 000 000'.              
019800     03  FILLER             PIC X(11)   VALUE '042 312 362'.              
019900     03  FILLER             PIC X(11)   VALUE '043 307 371'.              
020000     03  FILLER             PIC X(11)   VALUE '044 000 000'.              
020100     03  FILLER             PIC X(11)   VALUE '045 000 000'.              
020200     03  FILLER             PIC X(11)   VALUE '046 000 000'.              
020300     03  FILLER             PIC X(11)   VALUE '047 000 000'.              
020400     03  FILLER             PIC X(11)   VALUE '048 000 000'.              
020500     03  FILLER             PIC X(11)   VALUE '049 000 000'.              
020600     03  FILLER             PIC X(11)   VALUE '050 000 000'.              
020700     03  FILLER             PIC X(11)   VALUE '051 000 000'.              
020800     03  FILLER             PIC X(11)   VALUE '052 312 362'.              
020900     03  FILLER             PIC X(11)   VALUE '053 308 358'.              
021000     03  FILLER             PIC X(11)   VALUE '054 312 362'.              
021100     03  FILLER             PIC X(11)   VALUE '055 308 358'.              
021200     03  FILLER             PIC X(11)   VALUE '056 000 000'.              
021300     03  FILLER             PIC X(11)   VALUE '057 000 000'.              
021400     03  FILLER             PIC X(11)   VALUE '058 000 000'.              
021500     03  FILLER             PIC X(11)   VALUE '059 000 000'.              
021600     03  FILLER             PIC X(11)   VALUE '060 305 371'.              
021700     03  FILLER             PIC X(11)   VALUE '061 000 000'.              
021800     03  FILLER             PIC X(11)   VALUE '062 312 362'.              
021900     03  FILLER             PIC X(11)   VALUE '063 309 371'.              
022000     03  FILLER             PIC X(11)   VALUE '064 000 000'.              
022100     03  FILLER             PIC X(11)   VALUE '065 000 000'.              
022200     03  FILLER             PIC X(11)   VALUE '066 000 000'.              
022300     03  FILLER             PIC X(11)   VALUE '067 000 000'.              
022400     03  FILLER             PIC X(11)   VALUE '068 000 000'.              
022500     03  FILLER             PIC X(11)   VALUE '069 000 000'.              
022600     03  FILLER             PIC X(11)   VALUE '070 306 356'.              
022700     03  FILLER             PIC X(11)   VALUE '071 000 000'.              
022800     03  FILLER             PIC X(11)   VALUE '072 312 362'.              
022900     03  FILLER             PIC X(11)   VALUE '073 307 357'.              
023000     03  FILLER             PIC X(11)   VALUE '074 000 000'.              
023100     03  FILLER             PIC X(11)   VALUE '075 312 362'.              
023200     03  FILLER             PIC X(11)   VALUE '076 000 000'.              
023300     03  FILLER             PIC X(11)   VALUE '077 000 000'.              
023400     03  FILLER             PIC X(11)   VALUE '078 000 000'.              
023500     03  FILLER             PIC X(11)   VALUE '079 000 000'.              
023600     03  FILLER             PIC X(11)   VALUE '080 302 352'.              
023700     03  FILLER             PIC X(11)   VALUE '081 000 000'.              
023800     03  FILLER             PIC X(11)   VALUE '082 312 362'.              
023900     03  FILLER             PIC X(11)   VALUE '083 307 357'.              
024000     03  FILLER             PIC X(11)   VALUE '084 ??? ???'.              
024100     03  FILLER             PIC X(11)   VALUE '085 000 000'.              
024200     03  FILLER             PIC X(11)   VALUE '086 000 000'.              
024300     03  FILLER             PIC X(11)   VALUE '087 000 000'.              
024400     03  FILLER             PIC X(11)   VALUE '088 000 000'.              
024500     03  FILLER             PIC X(11)   VALUE '089 000 000'.              
024600     03  FILLER             PIC X(11)   VALUE '090 301 371'.              
024700     03  FILLER             PIC X(11)   VALUE '091 000 000'.              
024800     03  FILLER             PIC X(11)   VALUE '092 312 362'.              
024900     03  FILLER             PIC X(11)   VALUE '093 307 357'.              
025000     03  FILLER             PIC X(11)   VALUE '094 312 362'.              
025100     03  FILLER             PIC X(11)   VALUE '095 000 000'.              
025200     03  FILLER             PIC X(11)   VALUE '096 312 362'.              
025300     03  FILLER             PIC X(11)   VALUE '097 312 362'.              
025400     03  FILLER             PIC X(11)   VALUE '098 312 362'.              
025500     03  FILLER             PIC X(11)   VALUE '099 313 313'.              
025600     03  FILLER             PIC X(11)   VALUE '100 000 000'.              
025700     SKIP2                                                                
025800 01  FILLER REDEFINES TABELL.                                             
025900     03  WHTYP-TABELL  OCCURS 101.                                        
026000         05  TAB-KDANMORS        PIC X(3).                                
026100         05  FILLER              PIC X(1).                                
026200         05  TAB-KDEKSHT         PIC 9(3).                                
026300         05  FILLER              PIC X(1).                                
026400         05  TAB-KDEKSHT2        PIC 9(3).                                
026500                                                                          
026600******************************************************************        
026700     EJECT                                                                
026800*      --- VALID IDDC CODES                                               
026900*                                                                         
027000*01    -COPY WWDC99                                                       
027100*01    -COPY WWDCKONS                                                     
027200       EJECT                                                              
027300 01  FELTEXT.                                                             
027400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
027500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
027600                                                                          
027700 01  CHKP-VAR.                                                            
027800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
027900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
028000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
028100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
028200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
028300     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
028400                                                                          
028500     SKIP2                                                                
028600                                                                          
028700 01  WS-KOMMENTAR.                                                        
028800     03  INV-K-KDANMORS          PIC X(2).                                
028900     03  FILLER                  PIC X(2).                                
029000     03  INV-K-KVLEVANM          PIC 9(7).                                
029100     03  FILLER                  PIC X.                                   
029200     03  INV-K-IDDISTR           PIC 9(5).                                
029300     03  FILLER                  PIC X.                                   
029400     03  INV-K-IDKUNDNR          PIC 9(7).                                
029500                                                                          
029600     EJECT                                                                
029700*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
029800 01  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
029900 01  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
030000 01  WS-KVKREANT-MXC             PIC S9(7)   VALUE +0 COMP-3.             
030100 01  WS-TEMP-RETMXC              PIC S9(7)   VALUE +0 COMP-3.             
030200 01  WS-TEMP-RETMXCB             PIC S9(7)   VALUE +0 COMP-3.             
030210 01  WS-TEMP-RETKVAR             PIC S9(7)   VALUE +0 COMP-3.             
030300                                                                          
030400 01  WS-KLOCKAN                  PIC 9(9)    VALUE ZERO.                  
030500 01  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
030600 01  WS-FAKTURA-DATUM            PIC 9(8)    VALUE ZERO.                  
030700 01  WS-INV-DAREGDAT-AREA.                                                
030800     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
030900     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
031000       05  WS-INV-NOLL         PIC 9(1).                                  
031100       05  WS-INV-SEKEL        PIC 9(2).                                  
031200       05  WS-INV-AAMMDD       PIC 9(6).                                  
031300                                                                          
031400 01  WS-TISEGKEYAREA.                                                     
031500     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
031600     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
031700         05  WS-AAR          PIC 9(2).                                    
031800         05  WS-TIAAMMDD     PIC 9(6).                                    
031900         05  WS-LOPNR        PIC 9(1).                                    
032000     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
032100                                                                          
032200     EJECT                                                                
032300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
032400 01  FILLER REDEFINES DAGENS-DATUM.                                       
032500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
032600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
032700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
032800                                                                          
032900 01  WS-DATUM.                                                            
033000     03  WS-SEKEL            PIC 9(2).                                    
033100     03  WS-DAT.                                                          
033200      05 WS-AARTAL           PIC 9(2).                                    
033300      05 FILLER              PIC 9(4).                                    
033400 01  WS-DATUM-N  REDEFINES WS-DATUM   PIC 9(8).                           
033500                                                                          
033600     EJECT                                                                
033700 01  DYNAMISKA-SUBPROGRAM.                                                
033800*                                                                         
033900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
034000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
034100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
034200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
034300     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
034400     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
034500     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
034600     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
034700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
034800*                                                                         
034900     EJECT                                                                
035000*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
035100 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
035200*   -COPY W005WDK7                                                        
035300     EJECT                                                                
035400 01 FILLER                       PIC X(8)    VALUE 'W005WDL7'.            
035500*   -COPY W005WDL7                                                        
035600     EJECT                                                                
035700*    --- PARAMETRAR TILL POSTSUM                                          
035800*                                                                         
035900*01  -COPY W0005   -PRE  POSTSUM-                                         
036000                                                                          
036100     EJECT                                                                
036200*    --- PARAMETRAR TILL W009CIA                                          
036300*01  -COPY W009CIA                                                        
036400                                                                          
036500     EJECT                                                                
036600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
036700*01  -COPY WDATAREA                                                       
036800     EJECT                                                                
036900 01  IN-AREA-START               PIC X(24)   VALUE                        
037000                                             'IN-AREA-START'.             
037100     SKIP2                                                                
037200 01  IN-AREA.                                                             
037300     03  IN-AREA-0.                                                       
037400       05  IN-IDPTYP             PIC X(3).                                
037500       05  FILLER                PIC X(200).                              
037600*   03  FILLER -COPY W41834A   -PRE IN34A-   -RED  IN-AREA-0.             
037700*   03  FILLER -COPY W41834B   -PRE IN34B-   -RED  IN-AREA-0.             
037800                                                                          
037900     EJECT                                                                
038000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
038100     SKIP3                                                                
038200 01  NYCKLAR-TILL-DLI.                                                    
038300     03  W-IDLEVANM-X.                                                    
038400         05 W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.           
038500         05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.           
038600         05 W-IDRAPPNR           PIC  9(7)   VALUE ZERO.                  
038700                                                                          
038800     03  W-WDA211KY-X.                                                    
038900         05  W-IDARTNR-WDA2      PIC S9(9)   VALUE ZERO COMP-3.           
039000         05  W-IDRADNR-WDA2      PIC S9(5)   VALUE ZERO COMP-3.           
039100                                                                          
039200     03  W-IDARTNR-X.                                                     
039300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
039400                                                                          
039500     03  W-KDSEGKEY-X.                                                    
039600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
039700                                                                          
039800     03  W-IDDC-X.                                                        
039900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
040000                                                                          
040100     03  W-WDGXKEY-X.                                                     
040200         05  W-IDHTYP            PIC X(4)    VALUE '4579'.                
040300         05  W-IDPGM             PIC X(8)    VALUE 'W4183300'.            
040400         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
040500                                                                          
040600     03  W-WDH111KY-X.                                                    
040700         05  W-WDH111KY          PIC X(9)    VALUE SPACE.                 
040800                                                                          
040900     03  W-IDGMT-X.                                                       
041000         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
041100         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
041200                                                                          
041300     03  W-WDB101KY-X.                                                    
041400         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
041500         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
041600     03  W-DAINLEV-X.                                                     
041700         05  W-DAINLEV           PIC 9(16).                               
041800                                                                          
041900 01  W-WDH111KY-MIN.                                                      
042000     03  IDDC-SEARCH-MIN       PIC X(2).                                  
042100     03  KDINVKAT-SEARCH-MIN   PIC S9(3) VALUE ZERO       COMP-3.         
042200     03  TISEGKEY-SEARCH-MIN   PIC S9(9) VALUE ZERO       COMP-3.         
042300     03  DAREGDAT-SORT-SEARCH-MIN PIC 9(8) VALUE ZERO.                    
042400                                                                          
042500 01  W-WDH111KY-MAX.                                                      
042600     03  IDDC-SEARCH-MAX       PIC X(2).                                  
042700     03  KDINVKAT-SEARCH-MAX   PIC S9(3) VALUE +999       COMP-3.         
042800     03  TISEGKEY-SEARCH-MAX   PIC S9(9) VALUE +999999999 COMP-3.         
042900     03  DAREGDAT-SORT-SEARCH-MAX PIC 9(8) VALUE 99999999.                
043000                                                                          
043100 01  W-WDGXKEY-4103-X.                                                    
043200     03  W-IDHTYP-4103         PIC X(4)    VALUE '4103'.                  
043300     03  W-IDDISTR-4103        PIC S9(5)   VALUE ZERO COMP-3.             
043400     03  W-IDKUNDNR-4103       PIC S9(7)   VALUE ZERO COMP-3.             
043500     03  W-IDRAPPNR-4103       PIC  9(7)   VALUE ZERO.                    
043600     03  FILLER                PIC X(12)   VALUE LOW-VALUE.               
043700                                                                          
043800     SKIP2                                                                
043900*    --- STATUS-KOD FRÅN IMS                                              
044000 01  STATUS-WS                   PIC XX.                                  
044100     88  SEGMENT-FINNS                       VALUE '  '.                  
044200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
044300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
044400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
044500     88  IMS-EJ-OK                           VALUE 'XD'.                  
044600     SKIP2                                                                
044700 01  GODK-STATUSKODER.                                                    
044800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
044900     SKIP3                                                                
045000 01  SSA1                        PIC X(128).                              
045100 01  SSA2                        PIC X(128).                              
045200 01  SSA3                        PIC X(128).                              
045300     EJECT                                                                
045400*    --- IMS FUNKTIONSKODER                                               
045500*01  -COPY W0003                                                          
045600     EJECT                                                                
045700 01  FILLER                    PIC X(16)  VALUE 'MSG-KOM-WMSGKOM'.        
045800*01  -COPY WMSGKOM  -PRE MSG1-                                            
045900                                                                          
046000 01  FILLER                    PIC X(16)   VALUE 'MSG-IO-AREA'.           
046100*01  -COPY WMSGAREA                                                       
046200                                                                          
046300 01  FILLER                    PIC X(16)   VALUE 'Z430-REQU-AREA'.        
046400*01  -COPY WZ0430I1  -PRE Z430-                                           
046500*    03  -COPY WAPIDISC -RED Z430-REQU-EVENT-DATA -PRE Z430-              
046600                                                                          
046700*    ---  DLI INPUT-OUTPUT AREA                                           
046800                                                                          
046900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
047000 01   DLI-IO-WDB601.                                                      
047100*     03  -COPY WDB601                                                    
047200     EJECT                                                                
047300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
047400 01  DLI-IO-WDA201.                                                       
047500*    03  -COPY WDA201                                                     
047600     EJECT                                                                
047700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
047800 01  DLI-IO-WDA211.                                                       
047900*    03  -COPY WDA211                                                     
048000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA222'.                      
048100 01  DLI-IO-WDA222.                                                       
048200*    03  -COPY WDA222                                                     
048300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
048400 01  DLI-IO-WDK601.                                                       
048500*    03  -COPY WDK601                                                     
048600     EJECT                                                                
048700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
048800 01  DLI-IO-WDK611.                                                       
048900*    03  -COPY WDK611                                                     
049000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
049100 01  DLI-IO-WDK711.                                                       
049200*    03  -COPY WDK711                                                     
049300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK728'.                      
049400 01  DLI-IO-WDK728.                                                       
049500*    03   -COPY WDK728                                                    
049600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH101'.                      
049700 01  DLI-IO-WDH101.                                                       
049800*    03  -COPY WDH101  -PRE INV-                                          
049900     EJECT                                                                
050000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH111'.                      
050100 01  DLI-IO-WDH111.                                                       
050200*    03  -COPY WDH111                                                     
050300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH121'.                      
050400 01  DLI-IO-WDH121.                                                       
050500*    03  -COPY WDH121                                                     
050600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL901'.                      
050700 01  DLI-IO-WDL901.                                                       
050800*    03  -COPY WDL901                                                     
050900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR901'.                      
051000 01  DLI-IO-WDR901.                                                       
051100*    03  -COPY WDR901                                                     
051200*      05  -COPY W510EKHA -RED FIL-WDR901-DATA                            
051300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR801'.                      
051400 01  DLI-IO-WDR801.                                                       
051500*    03  -COPY WDR801 -PRE EKO-                                           
051600*      05  -COPY W510EKHA -RED EKO-FIL-WDR801-DATA -PRE EKO-              
051700                                                                          
051800*-ÅTERSTARTSREGISTER WDR4                                                 
051900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
052000 01  DLI-IO-WDGX4580.                                                     
052100*    03  -COPY WDGX4580                                                   
052200     EJECT                                                                
052300                                                                          
052400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
052500 01  DLI-IO-WDGX4103.                                                     
052600*    03  -COPY WDGX4103                                                   
052700                                                                          
052800 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
052900 01  DLI-IO-OIGA11.                                                       
053000*    03  -COPY WDL711                                                     
053100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB101'.         
053200 01  DLI-IO-WDB101.                                                       
053300*    03  -COPY WDB101                                                     
053400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
053500 01  DLI-IO-WDB201.                                                       
053600*    03  -COPY WDB201                                                     
053700     EJECT                                                                
053800 LINKAGE SECTION.                                                         
053900                                                                          
054000*01  -COPY W0009   -PRE MSG-                                              
054100                                                                          
054200*01  -COPY W0009  -PRE 0693-                                              
054300     05  FILLER                  PIC X.                                   
054400     EJECT                                                                
054500*01  -COPY W0008  -PRE WDA2-                                              
054600     05  FILLER                  PIC X.                                   
054700                                                                          
054800*01  -COPY W0008  -PRE WDK6-                                              
054900     05  FILLER                  PIC X.                                   
055000                                                                          
055100*01  -COPY W0008  -PRE WDK7-                                              
055200     05  FILLER                  PIC X.                                   
055300                                                                          
055400*01  -COPY W0008  -PRE WDH1-                                              
055500     05  FILLER                  PIC X.                                   
055600                                                                          
055700*01  -COPY W0008  -PRE WDL9-                                              
055800     05  FILLER                  PIC X.                                   
055900                                                                          
056000*01  -COPY W0008  -PRE WDR9-                                              
056100     05  FILLER                  PIC X.                                   
056200     EJECT                                                                
056300*01  -COPY W0008  -PRE 4579-                                              
056400     05  FILLER                  PIC X.                                   
056500     EJECT                                                                
056600*01  -COPY W0008  -PRE 4103-                                              
056700     05  FILLER                  PIC X.                                   
056800     EJECT                                                                
056900*01  -COPY W0008  -PRE OIGA-                                              
057000     05  FILLER                  PIC X.                                   
057100     EJECT                                                                
057200*01  -COPY W0008  -PRE WDR8-                                              
057300     05  FILLER                  PIC X.                                   
057400     EJECT                                                                
057500*01  -COPY W0008  -PRE WDB6-                                              
057600     05  FILLER                  PIC X.                                   
057700     EJECT                                                                
057800*01  -COPY W0008  -PRE WDB2-                                              
057900     05  FILLER                  PIC X.                                   
058000     EJECT                                                                
058100*01  -COPY W0008  -PRE WDB1-                                              
058200     05  FILLER                  PIC X.                                   
058300     EJECT                                                                
058400 01  WDP8-PCB                    PIC X.                                   
058500     EJECT                                                                
058600 PROCEDURE DIVISION  USING MSG-PCB 0693-PCB WDA2-PCB WDK6-PCB             
058700     WDK7-PCB WDH1-PCB WDL9-PCB WDR9-PCB 4579-PCB 4103-PCB                
058800     OIGA-PCB WDR8-PCB WDB6-PCB WDB2-PCB WDB1-PCB WDP8-PCB.               
058900 MAIN SECTION.                                                            
059000     ENTRY 'DLITCBL' USING MSG-PCB WDA2-PCB WDK6-PCB                      
059100     WDK7-PCB WDH1-PCB WDL9-PCB WDR9-PCB 4579-PCB 4103-PCB                
059200     OIGA-PCB WDR8-PCB WDB6-PCB WDB2-PCB WDB1-PCB WDP8-PCB.               
059300                                                                          
059400     SKIP2                                                                
059500     PERFORM A-INIT                                                       
059600                                                                          
059700     PERFORM IMS-LAS-ATERSTART                                            
059800     IF 4580-KVPOST > +0                                                  
059900       PERFORM S11-LAS-FRAM-TILL-CHKPOINT                                 
060000     ELSE                                                                 
060100       PERFORM S01-LAES-W418AL                                            
060200       MOVE +1      TO CHKP-ANT                                           
060300     END-IF                                                               
060400                                                                          
060500     PERFORM UNTIL END-OF-W418AL                                          
060600       IF CHKP-ANT > CHKP-MAX                                             
060700         PERFORM X-TAG-CHECKPOINT                                         
060800       END-IF                                                             
060900                                                                          
061000       IF IN-IDPTYP = '34B'                                               
061010         MOVE 'N' TO IDTRACK-QTY-SW                                       
061020         MOVE ZERO TO WS-TEMP-RETMXC                                      
061030         MOVE ZERO TO WS-TEMP-RETMXCB                                     
061040         MOVE ZERO TO WS-TEMP-RETKVAR                                     
061100         PERFORM B-UPPDATATERA-WDA2                                       
061200         MOVE LEV-KDANMORS TO SPAR-KDANMORS                               
061300         IF LEV-KDANMORS = '74'                                           
061400           MOVE LEV-KDANMORS TO SPAR-KDANMORS                             
061500         ELSE                                                             
061600           PERFORM C-UPPDAT-KVLS-WDK6-K7                                  
061700           PERFORM D-UPPDAT-EKO-RAD-WDR8-WDR9                             
061800           IF LEV-FLINVUPD = JA                                           
061900             PERFORM E-INVENTERING-UPD-ROT                                
062000             PERFORM F-INVENTERING-UPD-RAD                                
062100           END-IF                                                         
062200         END-IF                                                           
062300       ELSE                                                               
062400         IF IN-IDPTYP = '34A'                                             
062500           IF SPAR-KDANMORS = '74'                                        
062600             CONTINUE                                                     
062700           ELSE                                                           
062800             PERFORM G-FLYTTA-TILLAEGGSKOSTNADER                          
062900             PERFORM H-UPPDAT-EKO-HUV-WDR9                                
063000           END-IF                                                         
063100           IF IN34A-FLSLUT = 'J'                                          
063200             PERFORM I-SAETT-STATUS-WDA2                                  
063300           END-IF                                                         
063400           MOVE SPACE        TO SPAR-KDANMORS                             
063500         END-IF                                                           
063600       END-IF                                                             
063700                                                                          
063800       PERFORM S01-LAES-W418AL                                            
063900       ADD +1 TO CHKP-ANT                                                 
064000     END-PERFORM                                                          
064100                                                                          
064200                                                                          
064300     PERFORM Z-FINIT                                                      
064400                                                                          
064500     MOVE ZERO TO RETURN-CODE                                             
064600     GOBACK                                                               
064700     .                                                                    
064800     EJECT                                                                
064900 A-INIT SECTION.                                                          
065000     SKIP2                                                                
065100                                                                          
065200     PERFORM IMS-RESTART                                                  
065300                                                                          
065400     OPEN INPUT W418AL                                                    
065500                                                                          
065600                                                                          
065700     MOVE +0                           TO CHKP-ANT                        
065800                                          W-KVPOST-IN                     
065900     MOVE IDPGM                        TO POSTSUM-PROGNAMN                
066000                                                                          
066100     ACCEPT DAGENS-DATUM FROM DATE                                        
066200     ACCEPT WS-KLOCKAN   FROM TIME                                        
066300                                                                          
066400     MOVE DAGENS-DATUM   TO WS-DAT                                        
066500     IF WS-AARTAL < 50                                                    
066600       MOVE 20           TO WS-SEKEL                                      
066700     ELSE                                                                 
066800       MOVE 19           TO WS-SEKEL                                      
066900     END-IF                                                               
067000     MOVE DAGENS-DATUM   TO WS-INV-AAMMDD                                 
067100     MOVE WS-SEKEL       TO WS-INV-SEKEL                                  
067200                                                                          
067300*    -- INITIALIZE W006KOM                                                
067400     MOVE SPACE                      TO MSG1-MSG-KOM-WMSGKOM              
067500     MOVE SPACE                      TO MSG1-MSG-KOM-KDTRANS              
067600     MOVE SPACE                      TO MSG1-MSG-KOM-IDMFSMED             
067700     MOVE LENGTH OF MSG1-MSG-KOM-WMSGKOM TO MSG1-MSG-KOM-KVLL             
067800     MOVE LOW-VALUE                  TO MSG1-MSG-KOM-KDZ1                 
067900     MOVE LOW-VALUE                  TO MSG1-MSG-KOM-KDZ2                 
068000     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG1-MSG-KOM-TIREGDAT             
068100     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG1-MSG-KOM-TIKLOCK              
068200     MOVE LOW-VALUE                  TO MSG-KDZ1                          
068300     MOVE LOW-VALUE                  TO MSG-KDZ2                          
068310                                                                          
068320     MOVE NEJ                        TO SW-LYNK-NON-API                   
068400*                                                                         
068500     .                                                                    
068600     EJECT                                                                
068700 B-UPPDATATERA-WDA2 SECTION.                                              
068800                                                                          
068900     MOVE IN34B-IDDISTR                TO W-IDDISTR                       
069000     MOVE IN34B-IDKUNDNR               TO W-IDKUNDNR                      
069100     MOVE IN34B-IDRAPPNR               TO W-IDRAPPNR                      
069200     MOVE IN34B-IDARTNR                TO W-IDARTNR-WDA2                  
069300     MOVE IN34B-IDRADNR                TO W-IDRADNR-WDA2                  
069400                                                                          
069500     PERFORM IMS-GHU-WDA211                                               
069600     MOVE IN34B-IDKNOTNR               TO LEV-IDKNOTNR                    
069700     IF LEV-KDANMORS = '74'                                               
069800       MOVE 'I'                        TO LEV-KDFAKTYP-KNOT               
069900     ELSE                                                                 
070000       MOVE 'C'                        TO LEV-KDFAKTYP-KNOT               
070100     END-IF                                                               
070200     MOVE IN34B-TIKNOTA                TO LEV-TIKNOTA                     
070300                                                                          
070400*- 021009 - ENLIGT BOSSE H KAN BILL-IT ÄNDRA VATKODEN BEROENDE PÅ         
070500*- OM DET SKALL VARA MOMS ELLER EJ. FRÅN VIPS FÅR MAN ALLTID DEN          
070600*- VATKOD SOM GÄLLER FÖR MOMSBELAGDA LEVERANSER.                          
070700                                                                          
070800     IF IN34B-KDVAT = LEV-KDVAT                                           
070900       CONTINUE                                                           
071000     ELSE                                                                 
071100       MOVE IN34B-KDVAT                TO LEV-KDVAT                       
071200     END-IF                                                               
071300                                                                          
071400     PERFORM IMS-REPL-WDA211                                              
071500     .                                                                    
071600     EJECT                                                                
071700 C-UPPDAT-KVLS-WDK6-K7 SECTION.                                           
071800                                                                          
071900     IF LEV-KDAVVTYP = +0                                                 
072000       CONTINUE                                                           
072100     ELSE                                                                 
072200       MOVE IN34B-IDARTNR                TO W-IDARTNR                     
072300       MOVE IN34B-IDDC                   TO W-IDDC                        
072400                                            WS-IDDC                       
072500                                                                          
072600       IF CDC-SE                                                          
072700          PERFORM IMS-GU-WDK601                                           
072800          PERFORM IMS-GHNP-WDK611                                         
072900          IF LEV-KDAVVTYP = +1                                            
073000             COMPUTE CLAG-KVLS = CLAG-KVLS + IN34B-KVKREANT               
073100             PERFORM IMS-REPL-WDK611                                      
073200                                                                          
073300             MOVE '+'              TO LOGG-IDTECKEN-KVLS                  
073400          END-IF                                                          
073500                                                                          
073600          IF LEV-KDAVVTYP = +2                                            
073700             COMPUTE CLAG-KVLS = CLAG-KVLS - IN34B-KVKREANT               
073800             PERFORM IMS-REPL-WDK611                                      
073900                                                                          
074000             MOVE '-'              TO LOGG-IDTECKEN-KVLS                  
074100          END-IF                                                          
074200                                                                          
074300          MOVE WC-CDC-SE           TO LOGG-IDDC                           
074400          MOVE CLAG-KVLS           TO LOGG-KVLS                           
074500          MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                      
074600          MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                         
074700          COMPUTE LOGG-KVAKS  =  CLAG-KVAKS-CDC                           
074800                              +  CLAG-KVAKS-T                             
074900          PERFORM S13-SKAPA-SALDOLOGG-WDL9                                
075000       ELSE                                                               
075100          PERFORM IMS-GET-HOLD-SALDON-WDK711                              
075200          IF LEV-KDAVVTYP = +1                                            
075300             IF SEGMENT-SAKNAS                                            
075400                PERFORM S02-LAEGG-UPP-NY-WDK7                             
075500                COMPUTE SLAG-KVLS = SLAG-KVLS + IN34B-KVKREANT            
075600             ELSE                                                         
075700                COMPUTE SLAG-KVLS = SLAG-KVLS + IN34B-KVKREANT            
075800                PERFORM IMS-REPL-WDK711                                   
075900                IF NDC-MX AND SLAG-IDDC-REF > SPACE                       
076000                 PERFORM IMS-GU-WDK711                                    
076100                 MOVE 9999999999999999    TO W-DAINLEV                    
076200                 MOVE IN34B-KVKREANT      TO WS-KVKREANT-MXC              
076300                 PERFORM IMS-GHNP-WDK728-LAST                             
076310                 COMPUTE WS-TEMP-RETKVAR = TRCK-KVANTMOT -                
076320                         TRCK-KVTRACK-KVAR                                
076400                 PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE         
076500                  IF TRCK-KVTRACK-KVAR <= TRCK-KVANTMOT AND               
076510                     WS-TEMP-RETKVAR NOT = ZERO                           
076600                   COMPUTE WS-TEMP-RETMXCB = WS-TEMP-RETMXCB +            
076700                                             WS-TEMP-RETMXC               
076800                   COMPUTE WS-TEMP-RETMXC = WS-TEMP-RETMXC +              
076900                            (TRCK-KVANTMOT - TRCK-KVTRACK-KVAR)           
077000                   IF WS-TEMP-RETMXC <= WS-KVKREANT-MXC                   
077100                    MOVE TRCK-KVANTMOT TO TRCK-KVTRACK-KVAR               
077200                    MOVE WS-TEMP-RETMXC TO TRK-KVANTMOT                   
077300                    PERFORM IMS-REPL-WDK728                               
077400                   ELSE                                                   
077500                    COMPUTE WS-TEMP-RETMXCB = WS-KVKREANT-MXC -           
077600                                              WS-TEMP-RETMXCB             
077700                    ADD WS-TEMP-RETMXCB TO TRCK-KVTRACK-KVAR              
077800                    MOVE WS-TEMP-RETMXCB TO TRK-KVANTMOT                  
077900                    PERFORM IMS-REPL-WDK728                               
078000                    MOVE 'J' TO IDTRACK-QTY-SW                            
078100                   END-IF                                                 
078200                   MOVE TRCK-IDTRACK TO TRK-IDTRACK                       
078300                   MOVE TRCK-DAINLEV(1:8) TO TRK-DADATUM                  
078400                   PERFORM IMS-ISRT-WDA222                                
078500                  END-IF                                                  
078600                  IF WS-TEMP-RETMXC = WS-KVKREANT-MXC                     
078700                   MOVE 'J' TO IDTRACK-QTY-SW                             
078800                  END-IF                                                  
078900                  IF IDTRACK-QTY-NOT-DONE                                 
079000                   PERFORM IMS-GU-WDK711                                  
079100                   MOVE  TRCK-DAINLEV TO W-DAINLEV                        
079200                   PERFORM IMS-GHNP-WDK728-LAST                           
079210                   COMPUTE WS-TEMP-RETKVAR = TRCK-KVANTMOT -              
079220                           TRCK-KVTRACK-KVAR                              
079300                  END-IF                                                  
079400                END-PERFORM                                               
079500               END-IF                                                     
079600             END-IF                                                       
079700                                                                          
079800             MOVE '+'              TO LOGG-IDTECKEN-KVLS                  
079900          ELSE                                                            
080000             IF SEGMENT-SAKNAS                                            
080100                PERFORM S02-LAEGG-UPP-NY-WDK7                             
080200                COMPUTE SLAG-KVLS = SLAG-KVLS - IN34B-KVKREANT            
080300             ELSE                                                         
080400                COMPUTE SLAG-KVLS = SLAG-KVLS - IN34B-KVKREANT            
080500                PERFORM IMS-REPL-WDK711                                   
080600                IF NDC-MX AND SLAG-IDDC-REF > SPACE                       
080700                 PERFORM IMS-GU-WDK711                                    
080800                 MOVE 9999999999999999    TO W-DAINLEV                    
080900                 MOVE IN34B-KVKREANT      TO WS-KVKREANT-MXC              
081000                 PERFORM IMS-GHNP-WDK728-LAST                             
081100                 PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE         
081200                  IF TRCK-KVTRACK-KVAR <= TRCK-KVANTMOT AND               
081300                     TRCK-KVTRACK-KVAR NOT = ZERO                         
081400                     COMPUTE WS-KVKREANT-MXC = WS-KVKREANT-MXC -          
081500                                              TRCK-KVTRACK-KVAR           
081600                     IF WS-KVKREANT-MXC >= ZERO                           
081700                      MOVE ZERO TO TRCK-KVTRACK-KVAR                      
081800                      PERFORM IMS-REPL-WDK728                             
081900                     ELSE                                                 
082000                      COMPUTE TRCK-KVTRACK-KVAR =                         
082100                              TRCK-KVTRACK-KVAR - WS-KVKREANT-MXC         
082200                      PERFORM IMS-REPL-WDK728                             
082300                      MOVE 'J' TO IDTRACK-QTY-SW                          
082400                     END-IF                                               
082500                  END-IF                                                  
082600                  IF WS-KVKREANT-MXC = ZERO                               
082700                   MOVE 'J' TO IDTRACK-QTY-SW                             
082800                  END-IF                                                  
082900                  IF IDTRACK-QTY-NOT-DONE                                 
083000                   PERFORM IMS-GU-WDK711                                  
083100                   MOVE  TRCK-DAINLEV TO W-DAINLEV                        
083200                   PERFORM IMS-GHNP-WDK728-LAST                           
083300                  END-IF                                                  
083400                 END-PERFORM                                              
083500                END-IF                                                    
083600             END-IF                                                       
083700                                                                          
083800             MOVE '-'              TO LOGG-IDTECKEN-KVLS                  
083900          END-IF                                                          
084000                                                                          
084100          MOVE IN34B-IDDC          TO LOGG-IDDC                           
084200          MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                      
084300          MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                         
084400          MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                          
084500          MOVE SLAG-KVLS           TO LOGG-KVLS                           
084600          PERFORM S13-SKAPA-SALDOLOGG-WDL9                                
084700       END-IF                                                             
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 D-UPPDAT-EKO-RAD-WDR8-WDR9 SECTION.                                      
085200                                                                          
085300     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
085400     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
085500     MOVE IN34B-IDDISTR               TO TEST-IDDISTR                     
085600     MOVE IN34B-IDKUNDNR              TO W-TEST-IDKUNDNR                  
085700     MOVE IN34B-IDDC                  TO WS-IDDC                          
085800                                                                          
085900     IF (CDC-SE OR GOOD-DDC)                                              
086000     AND (DIST35-CDC-NONVCC-REFILL                                        
086100     OR DIST35-CDC-RETURNS-NON-VCC                                        
086200     OR DIST35-NONVCC-NONVCC-REFILL                                       
086300     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
086400     OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST))                     
086500     OR (NDC                                                              
086600     AND DIST35-VCC-NONVCC-REFILL)                                        
086700     OR (NDC                                                              
086800     AND DIST35-VCC-NONVCC-TRANSFER)                                      
086900                                                                          
087000*       LEV-KDANMORS = '00' OR '60' OR '63' OR '43' OR                    
087100*                        '94' OR '54' '55'                                
087200         MOVE 'W4183300'           TO EKO-FIL-IDPGM                       
087300         MOVE WS-AAAAMMDD          TO EKO-FIL-TIREGDAT                    
087400         MOVE WS-TTMMSSTH          TO EKO-FIL-TIKLOCK                     
087500         MOVE 1                    TO EKO-FIL-IDSEKVNR                    
087600         IF DIST35-CDC-NONVCC-REFILL                                      
087700         OR DIST35-VCC-NONVCC-REFILL                                      
087800         OR DIST35-VCC-NONVCC-TRANSFER                                    
087900         OR DIST35-CDC-RETURNS-NON-VCC                                    
088000         OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)                  
088100         OR DIST35-NONVCC-NONVCC-REFILL                                   
088200         OR DIST35-NONVCC-NONVCC-TRANSFER                                 
088300           IF (DIST18-SCRAP-NDC-QUAL AND NDC-CN-KUND)                     
088400           OR DIST35-REFILL-CN                                            
088500           OR DIST35-JP-NDC71-REFILL                                      
088600**** here we need to add transfer from JP/AU to CN                        
088700           OR DIST35-CN-CDC-RETURNS                                       
088800           OR IN34B-KDTRADP = 'CN05'                                      
088900             MOVE 'W570'             TO EKO-FIL-IDCPYTXT(1:4)             
089000           ELSE                                                           
089100             IF (DIST18-SCRAP-NDC-QUAL AND NDC-IN-KUND)                   
089200             OR DIST35-CDC-IN-REFILL                                      
089300             OR DIST35-JP-NDC67-REFILL                                    
089400**** here we need to add transfer from JP/AU to in                        
089500             OR DIST35-IN-CDC-RETURNS                                     
089600             OR IN34B-KDTRADP = 'IN07'                                    
089700               MOVE 'W515'           TO EKO-FIL-IDCPYTXT(1:4)             
089800             ELSE                                                         
089900               IF DIST35-NDCCN-NDCUS-REFILL                               
090000**** here we need to add transfer from JP/AU to us                        
090100                  MOVE 'W561EKHA'    TO EKO-FIL-IDCPYTXT                  
090200                  MOVE 'US01'        TO EKO-EKH-KDTRADP                   
090300               ELSE                                                       
090400                 IF (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)          
090500                 OR DIST35-CDC-NONVCC-REFILL                              
090600                 OR DIST35-VCC-NONVCC-REFILL                              
090700                 OR DIST35-VCC-NONVCC-TRANSFER                            
090800                 OR DIST35-CDC-RETURNS-NON-VCC                            
090900                 OR DIST35-NONVCC-NONVCC-REFILL                           
091000                 OR DIST35-NONVCC-NONVCC-TRANSFER                         
091100                   MOVE IN34B-KDTRADP    TO EKO-FIL-IDCPYTXT(1:4)         
091200                 END-IF                                                   
091300               END-IF                                                     
091400             END-IF                                                       
091500           END-IF                                                         
091600           MOVE 'EKHA'               TO EKO-FIL-IDCPYTXT(5:4)             
091700           MOVE IN34B-KDTRADP        TO EKO-EKH-KDTRADP                   
091800         END-IF                                                           
091900         PERFORM DA-UPPDAT-EKO-RAD-WDR8                                   
092000         MOVE 'W4183300'             TO FIL-IDPGM                         
092100         MOVE WS-AAAAMMDD            TO FIL-DAREGDAT                      
092200         MOVE WS-TTMMSSTH            TO FIL-TIKLOCK                       
092300         MOVE 1                      TO FIL-IDSEKVNR                      
092400         MOVE 'W510EKHA'             TO FIL-IDCPYTXT                      
092500         MOVE 'W4183300'             TO FIL-IDUSER                        
092600         PERFORM DB-UPPDAT-EKO-RAD-WDR9                                   
092700     ELSE                                                                 
092800       MOVE LEV-IDDC                 TO WS-IDDC                           
092900       IF XDC-NON-VCC-OWNED                                               
093000       OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                          
093100                                                                          
093200         MOVE 'W4183300'             TO EKO-FIL-IDPGM                     
093300         MOVE WS-AAAAMMDD            TO EKO-FIL-TIREGDAT                  
093400         MOVE WS-TTMMSSTH            TO EKO-FIL-TIKLOCK                   
093500         MOVE 1                      TO EKO-FIL-IDSEKVNR                  
093600                                                                          
093700         EVALUATE TRUE                                                    
093800           WHEN NDC-CN                                                    
093900             MOVE 'W570'             TO EKO-FIL-IDCPYTXT(1:4)             
094000           WHEN NDC-IN                                                    
094100             MOVE 'W515'             TO EKO-FIL-IDCPYTXT(1:4)             
094200           WHEN NDC-US                                                    
094300             MOVE 'W561'             TO EKO-FIL-IDCPYTXT(1:4)             
094400           WHEN OTHER                                                     
094500             MOVE IN34B-KDTRADP      TO EKO-FIL-IDCPYTXT(1:4)             
094600         END-EVALUATE                                                     
094700         MOVE 'EKHA'                 TO EKO-FIL-IDCPYTXT(5:4)             
094800         MOVE IN34B-KDTRADP        TO EKO-EKH-KDTRADP                     
094900                                                                          
095000         IF LEV-KDANMORS = '00' OR '20' OR '90' OR '97'                   
095100           PERFORM DA-UPPDAT-EKO-RAD-WDR8                                 
095200         END-IF                                                           
095300       ELSE                                                               
095400         MOVE 'W4183300'             TO FIL-IDPGM                         
095500         MOVE WS-AAAAMMDD            TO FIL-DAREGDAT                      
095600         MOVE WS-TTMMSSTH            TO FIL-TIKLOCK                       
095700         MOVE 1                      TO FIL-IDSEKVNR                      
095800         MOVE 'W510EKHA'             TO FIL-IDCPYTXT                      
095900         MOVE 'W4183300'             TO FIL-IDUSER                        
096000         PERFORM DB-UPPDAT-EKO-RAD-WDR9                                   
096100       END-IF                                                             
096200     END-IF                                                               
096300     .                                                                    
096400     EJECT                                                                
096500                                                                          
096600 DA-UPPDAT-EKO-RAD-WDR8 SECTION.                                          
096700*** SKALL FLYTTA KDVAT HÄR ENLIGT BOSSE HAMMARIN 2004-03-01,(DDI)         
096800     MOVE LEV-KDVAT              TO EKO-EKH-BEVAT                         
096900                                                                          
097000     MOVE IN34B-TIKNOTA          TO WS-TIKNOTA                            
097100     IF WS-TIKNOTA(1:2) < 50                                              
097200       MOVE 20                   TO EKO-EKH-DAVERDAT (1:2)                
097300     ELSE                                                                 
097400       MOVE 19                   TO EKO-EKH-DAVERDAT (1:2)                
097500     END-IF                                                               
097600     MOVE WS-TIKNOTA             TO EKO-EKH-DAVERDAT (3:6)                
097700                                                                          
097800     MOVE LEV-FLLSBOK            TO EKO-EKH-FLLSBOK                       
097900     MOVE LEV-IDANALYS           TO EKO-EKH-IDANALYS                      
098000     MOVE IN34B-IDARTNR          TO EKO-EKH-IDARTNR                       
098100     MOVE LEV-IDDC               TO EKO-EKH-IDDC-SEND                     
098200     MOVE ' '                    TO EKO-EKH-IDDC-REC                      
098300     MOVE IN34B-IDDISTR          TO EKO-EKH-IDDISTR                       
098400                                    TEST-IDDISTR                          
098500     MOVE LEV-IDKONTO            TO EKO-EKH-IDKONTO                       
098600     MOVE LEV-IDKST              TO EKO-EKH-IDKST                         
098700     MOVE IN34B-IDKUNDNR         TO EKO-EKH-IDKUNDNR                      
098800     MOVE ' '                    TO EKO-EKH-IDTRANS                       
098900                                                                          
099000     MOVE 'VO'                    TO CIA-IDARTPRE-IN                      
099100     MOVE IN34B-IDKNOTNR          TO CIA-IDARTBET-IN                      
099200     CALL W009CIA USING  CIA-W009CIA                                      
099300     MOVE CIA-IDARTBET-UT         TO EKO-EKH-IDVERGL                      
099400                                                                          
099500     MOVE LEV-KDANMORS           TO EKO-EKH-KDANMORS                      
099600                                    INDX                                  
099700     MOVE '303'                  TO EKO-EKH-KDEKHHT                       
099800                                                                          
099900     ADD 1                       TO INDX                                  
100000     MOVE TAB-KDANMORS (INDX)    TO WS-KDANMORS                           
100100     IF WS-KDANMORS  = (INDX - 1)                                         
100200       IF TAB-KDEKSHT (INDX) = '000'                                      
100300         MOVE WS-KDANMORS        TO EKO-EKH-KDEKSHT                       
100400       ELSE                                                               
100500         MOVE TAB-KDEKSHT (INDX) TO EKO-EKH-KDEKSHT                       
100600       END-IF                                                             
100700     ELSE                                                                 
100800       MOVE WS-KDANMORS          TO EKO-EKH-KDEKSHT                       
100900     END-IF                                                               
101000                                                                          
101100*--- 2012-02-29                                                           
101200*--- FÖR KINA SÅ SKALL VISSA KODER BOKAS PÅ ANNAN SUB-HTYP ÄN VAD         
101300*--- SOM GÄLLER I TABELLEN. BESLUT AV SUSSI, FREDRIK B, BOSSE H           
101400*---                                                                      
101500*--- KINA DEALERS SKALL HA SUB-TYP 391 ISTÄLLET FÖR 301.                  
101600*--- (PRAVCOST OCH CNY). REFILLEN HAR 301 (STDPRIS OCH SEK).              
101700*---                                                                      
101800*--- KINA RETURER KOD 54 O 94 HAR SUB-TYP 307 ISTÄLLET FÖR 312.           
101900*--- SKALL HA STDPRIS OCH SEK.RETUR TILL CDC.                             
102000*---                                                                      
102100     MOVE IN34B-IDDC            TO WS-IDDC                                
102200     IF XDC-NON-VCC-OWNED                                                 
102300       IF EKO-EKH-KDEKSHT = '301'                                         
102400         MOVE '391'              TO EKO-EKH-KDEKSHT                       
102500       END-IF                                                             
102600                                                                          
102700       IF LEV-KDANMORS = '97'                                             
102800         MOVE '391'              TO EKO-EKH-KDEKSHT                       
102900       END-IF                                                             
103000     END-IF                                                               
103100                                                                          
103200     IF DIST35-CDC-RETURNS-NON-VCC                                        
103300       IF LEV-KDANMORS = '54' OR '94'                                     
103400         MOVE '307'              TO EKO-EKH-KDEKSHT                       
103500       END-IF                                                             
103600     END-IF                                                               
103700                                                                          
103800     IF DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST                        
103900       IF LEV-KDANMORS = '55'                                             
104000         MOVE '307'              TO EKO-EKH-KDEKSHT                       
104100       END-IF                                                             
104200     END-IF                                                               
104300                                                                          
104400     IF DIST35-CDC-NONVCC-REFILL                                          
104500     OR DIST35-VCC-NONVCC-REFILL                                          
104600     OR DIST35-VCC-NONVCC-TRANSFER                                        
104700       IF LEV-KDANMORS = '60' OR '63' OR '43'                             
104800         MOVE '307'              TO EKO-EKH-KDEKSHT                       
104900       END-IF                                                             
105000     END-IF                                                               
105100                                                                          
105200     IF DIST35-NONVCC-NONVCC-REFILL                                       
105300     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
105400       IF LEV-KDANMORS = '00' OR '20' OR '90'                             
105500         MOVE '371'              TO EKO-EKH-KDEKSHT                       
105600       END-IF                                                             
105700       IF LEV-KDANMORS = '60' OR '63' OR '43'                             
105800         MOVE '371'              TO EKO-EKH-KDEKSHT                       
105900       END-IF                                                             
106000     END-IF                                                               
106100                                                                          
106200     MOVE EKO-EKH-KDEKSHT        TO SPAR-KDEKSHT-CHINA                    
106300                                                                          
106400     MOVE 'DET'                  TO EKO-EKH-KDEKNIVA                      
106500     MOVE ZERO                   TO EKO-EKH-KDFRAKT                       
106600                                                                          
106700                                                                          
106800                                                                          
106900     MOVE IN34B-IDARTNR          TO W-IDARTNR                             
107000     PERFORM IMS-GU-WDK601                                                
107100     PERFORM IMS-GNP-WDK611                                               
107200                                                                          
107300     MOVE ART-KDPRODSL           TO EKO-EKH-KDPRODSL                      
107400     MOVE CLAG-KDPSLLOC          TO EKO-EKH-KDPSLLOC                      
107500                                                                          
107600     IF DIST35-CDC-NONVCC-REFILL                                          
107700     OR DIST35-VCC-NONVCC-REFILL                                          
107800     OR DIST35-VCC-NONVCC-TRANSFER                                        
107900     OR DIST35-CDC-RETURNS-NON-VCC                                        
108000     OR DIST35-NONVCC-NONVCC-REFILL                                       
108100     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
108200     OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)                      
108300                                                                          
108400       MOVE IN34B-KDVALISO       TO EKO-EKH-KDVALISO                      
108500     ELSE                                                                 
108600       IF XDC-NON-VCC-OWNED                                               
108700         PERFORM S03-GET-KDVALISO                                         
108800         MOVE BET-KDVALISO       TO EKO-EKH-KDVALISO                      
108900       END-IF                                                             
109000       IF (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                          
109100         MOVE 'USD'              TO EKO-EKH-KDVALISO                      
109200       END-IF                                                             
109300     END-IF                                                               
109400                                                                          
109500     MOVE IN34B-KVKREANT         TO EKO-EKH-KVANTAL                       
109600     MOVE IN34B-PRARTNTO         TO EKO-EKH-PRARTNTO                      
109700     MOVE ZERO                   TO EKO-EKH-PRARTSJK                      
109800                                                                          
109900     IF DIST35-CDC-NONVCC-REFILL                                          
110000     OR DIST35-VCC-NONVCC-REFILL                                          
110100     OR DIST35-VCC-NONVCC-TRANSFER                                        
110200     OR DIST35-CDC-RETURNS-NON-VCC                                        
110300     OR DIST35-NONVCC-NONVCC-REFILL                                       
110400     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
110500     OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)                      
110600       MOVE CLAG-PRARTSTD        TO EKO-EKH-PRARTSTD                      
110700     ELSE                                                                 
110800       MOVE IN34B-IDDC           TO W-IDDC                                
110900       PERFORM IMS-GU-WDK711                                              
111000       IF SEGMENT-FINNS                                                   
111100         MOVE SLAG-PRAVCOST      TO EKO-EKH-PRARTSTD                      
111200       ELSE                                                               
111300         MOVE ZERO               TO EKO-EKH-PRARTSTD                      
111400       END-IF                                                             
111500     END-IF                                                               
111600                                                                          
111700     MOVE ZERO                   TO EKO-EKH-PRDIRLON                      
111800     MOVE ZERO                   TO EKO-EKH-PRDMTRL                       
111900     MOVE ZERO                   TO EKO-EKH-PRINK                         
112000     MOVE IN34B-PRKURS           TO EKO-EKH-PRKURS                        
112100     MOVE ZERO                   TO EKO-EKH-PRLANDCO                      
112200                                    EKO-EKH-PROVRPAL                      
112300                                    EKO-EKH-SUBEL                         
112400                                    EKO-EKH-SUVAT                         
112500                                    EKO-EKH-IDAVINR                       
112600     MOVE SPACE                  TO EKO-EKH-IDLEVNR                       
112700     MOVE ZERO                   TO EKO-EKH-KDAVVTYP                      
112800                                    EKO-EKH-KDRT                          
112900                                    EKO-EKH-KVANTMOT                      
113000                                    EKO-EKH-KVAVIS                        
113100     MOVE ART-KDSORT             TO EKO-EKH-KDSORT                        
113200     MOVE SPACE                  TO EKO-EKH-FLOVRLEV                      
113300     MOVE ZERO                   TO EKO-EKH-IDORDNR5                      
113400                                    EKO-EKH-PRHEMTAG                      
113500     MOVE SPACE                  TO EKO-EKH-IDUSER                        
113600     MOVE LEV-IDFAKT             TO EKO-EKH-IDFAKT-EXP                    
113700     IF LEV-TIFAKT > ZERO                                                 
113800       MOVE LEV-TIFAKT      TO WS-FAKTURA-DATUM                           
113900                                                                          
114000       IF WS-FAKTURA-DATUM < 500000                                       
114100         MOVE 20                     TO WS-FAKTURA-DATUM(1:2)             
114200       ELSE                                                               
114300         IF WS-FAKTURA-DATUM < 999999                                     
114400           MOVE 19                   TO WS-FAKTURA-DATUM(1:2)             
114500         ELSE                                                             
114600           MOVE 99999999             TO WS-FAKTURA-DATUM                  
114700         END-IF                                                           
114800       END-IF                                                             
114900                                                                          
115000       MOVE WS-FAKTURA-DATUM(3:4)        TO EKO-EKH-DAAVIDAT              
115100     ELSE                                                                 
115200       MOVE ZERO                         TO EKO-EKH-DAAVIDAT              
115300     END-IF                                                               
115400                                                                          
115500     IF DIST35-CDC-NONVCC-REFILL                                          
115600     OR DIST35-VCC-NONVCC-REFILL                                          
115700     OR DIST35-VCC-NONVCC-TRANSFER                                        
115800     OR DIST35-CDC-RETURNS-NON-VCC                                        
115900     OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)                      
116000       IF (DIST18-SCRAP-NDC-QUAL AND NDC-CN-KUND)                         
116100       OR DIST35-REFILL-CN                                                
116200       OR DIST35-JP-NDC71-REFILL                                          
116300       OR DIST35-CN-CDC-RETURNS                                           
116400**** here we need to add transfer from JP/AU to CN                        
116500         MOVE 'W570'             TO EKO-FIL-IDCPYTXT(1:4)                 
116600       ELSE                                                               
116700         IF (DIST18-SCRAP-NDC-QUAL AND NDC-IN-KUND)                       
116800         OR DIST35-CDC-IN-REFILL                                          
116900         OR DIST35-JP-NDC67-REFILL                                        
117000**** here we need to add transfer from JP/AU to iN                        
117100         OR DIST35-IN-CDC-RETURNS                                         
117200           MOVE 'W515'           TO EKO-FIL-IDCPYTXT(1:4)                 
117300         ELSE                                                             
117400           IF (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)                
117500           OR DIST35-CDC-NONVCC-REFILL                                    
117600           OR DIST35-VCC-NONVCC-REFILL                                    
117700           OR DIST35-VCC-NONVCC-TRANSFER                                  
117800           OR DIST35-CDC-RETURNS-NON-VCC                                  
117900             MOVE IN34B-KDTRADP  TO EKO-FIL-IDCPYTXT(1:4)                 
118000           END-IF                                                         
118100         END-IF                                                           
118200       END-IF                                                             
118300       MOVE 'EKHA'               TO EKO-FIL-IDCPYTXT(5:4)                 
118400       MOVE IN34B-KDTRADP        TO EKO-EKH-KDTRADP                       
118500     END-IF                                                               
118600                                                                          
118700     IF DIST35-NDCCN-NDCUS-REFILL                                         
118800       MOVE 'US01'               TO EKO-EKH-KDTRADP                       
118900       MOVE 'W561EKHA'           TO EKO-FIL-IDCPYTXT                      
119000     END-IF                                                               
119100                                                                          
119200     PERFORM IMS-ISRT-WDR801                                              
119300     PERFORM UNTIL SEGMENT-FINNS                                          
119400       ADD +1  TO EKO-FIL-IDSEKVNR                                        
119500       PERFORM IMS-ISRT-WDR801                                            
119600     END-PERFORM                                                          
119700     .                                                                    
119800     EJECT                                                                
119900                                                                          
120000 DB-UPPDAT-EKO-RAD-WDR9 SECTION.                                          
120100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
120200     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
120300                                                                          
120400     MOVE 'W4183300'             TO FIL-IDPGM                             
120500     MOVE WS-AAAAMMDD            TO FIL-DAREGDAT                          
120600     MOVE WS-TTMMSSTH            TO FIL-TIKLOCK                           
120700     MOVE 1                      TO FIL-IDSEKVNR                          
120800     MOVE 'W510EKHA'             TO FIL-IDCPYTXT                          
120900     MOVE 'W4183300'             TO FIL-IDUSER                            
121000                                                                          
121100*** SKALL FLYTTA KDVAT HÄR ENLIGT BOSSE HAMMARIN 2004-03-01,(DDI)         
121200     MOVE LEV-KDVAT              TO EKH-BEVAT                             
121300                                                                          
121400     MOVE IN34B-TIKNOTA          TO WS-TIKNOTA                            
121500     IF WS-TIKNOTA(1:2) < 50                                              
121600       MOVE 20                   TO EKH-DAVERDAT (1:2)                    
121700     ELSE                                                                 
121800       MOVE 19                   TO EKH-DAVERDAT (1:2)                    
121900     END-IF                                                               
122000     MOVE WS-TIKNOTA             TO EKH-DAVERDAT (3:6)                    
122100                                                                          
122200     MOVE LEV-FLLSBOK            TO EKH-FLLSBOK                           
122300     MOVE LEV-IDANALYS           TO EKH-IDANALYS                          
122400     MOVE IN34B-IDARTNR          TO EKH-IDARTNR                           
122500     MOVE LEV-IDDC               TO EKH-IDDC-SEND                         
122600     MOVE ' '                    TO EKH-IDDC-REC                          
122700     MOVE IN34B-IDDISTR          TO EKH-IDDISTR                           
122800                                    TEST-IDDISTR                          
122900     MOVE LEV-IDKONTO            TO EKH-IDKONTO                           
123000     MOVE LEV-IDKST              TO EKH-IDKST                             
123100     MOVE IN34B-IDKUNDNR         TO EKH-IDKUNDNR                          
123200     MOVE ' '                    TO EKH-IDTRANS                           
123300                                                                          
123400     MOVE 'VO'                    TO CIA-IDARTPRE-IN                      
123500     MOVE IN34B-IDKNOTNR          TO CIA-IDARTBET-IN                      
123600     CALL W009CIA USING  CIA-W009CIA                                      
123700     MOVE CIA-IDARTBET-UT         TO EKH-IDVERGL                          
123800                                                                          
123900     MOVE LEV-KDANMORS           TO EKH-KDANMORS                          
124000                                    INDX                                  
124100     MOVE 303                    TO EKH-KDEKHHT                           
124200                                                                          
124300     ADD 1                       TO INDX                                  
124400     MOVE TAB-KDANMORS (INDX)    TO WS-KDANMORS                           
124500     IF WS-KDANMORS  = (INDX - 1)                                         
124600       IF TAB-KDEKSHT (INDX) = '000'                                      
124700         MOVE WS-KDANMORS        TO EKH-KDEKSHT                           
124800       ELSE                                                               
124900         MOVE TAB-KDEKSHT (INDX) TO EKH-KDEKSHT                           
125000       END-IF                                                             
125100     ELSE                                                                 
125200       MOVE WS-KDANMORS          TO EKH-KDEKSHT                           
125300     END-IF                                                               
125400                                                                          
125500* FÖR CDC OCH KOD 97 SKALL HÄNDELSE 303 312 GÄLLA, FÖR ÖVRIGA DC          
125600* OCH KOD 97 GÄLLER HÄNDELSE 303 301.                                     
125700     IF LEV-KDANMORS = '97'                                               
125800       MOVE IN34B-IDDC            TO WS-IDDC                              
125900       IF CDC-SE                                                          
126000         CONTINUE                                                         
126100       ELSE                                                               
126200         MOVE '301'              TO EKH-KDEKSHT                           
126300       END-IF                                                             
126400     END-IF                                                               
126500                                                                          
126600* FÖR REFILL-NA DISTRIKT (8141-8143 + 8151) OCH KOD 60 SKALL              
126700* HÄNDELSE 303 314 GÄLLA, ENLIGT TUULA 030605.                            
126800* FÖR ÖVRIGA DISTRIKT KOD 60 GÄLLER HÄNDELSE 303 305.                     
126900     IF LEV-KDANMORS = '60'                                               
127000       IF DIST35-REFILL-NA                                                
127100         MOVE '314'               TO EKH-KDEKSHT                          
127200       ELSE                                                               
127300         CONTINUE                                                         
127400       END-IF                                                             
127500     END-IF                                                               
127600                                                                          
127700**** IF BOUNCE FLOW 303-381 IS THE EVENT                                  
127800     IF DIST35-NONVCC-NONVCC-REFILL                                       
127900     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
128000       MOVE '381'                TO EKH-KDEKSHT                           
128100     END-IF                                                               
128200                                                                          
128300     MOVE 'DET'                  TO EKH-KDEKNIVA                          
128400     MOVE ZERO                   TO EKH-KDFRAKT                           
128500                                                                          
128600     MOVE IN34B-IDARTNR          TO W-IDARTNR                             
128700     PERFORM IMS-GU-WDK601                                                
128800     PERFORM IMS-GNP-WDK611                                               
128900                                                                          
129000***** EXTENDED WARRANTY SHOULD BE OWN EVENT                               
129100     MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                                   
129200     IF FKNGRP-EXT-WARRANTY                                               
129300       MOVE '386'                TO EKH-KDEKSHT                           
129400     end-if                                                               
129500                                                                          
129600     MOVE ART-KDPRODSL           TO EKH-KDPRODSL                          
129700     MOVE CLAG-KDPSLLOC          TO EKH-KDPSLLOC                          
129800     MOVE IN34B-KDVALISO         TO EKH-KDVALISO                          
129900     MOVE IN34B-KVKREANT         TO EKH-KVANTAL                           
130000     MOVE IN34B-PRARTNTO         TO EKH-PRARTNTO                          
130100     MOVE CLAG-PRARTSJK          TO EKH-PRARTSJK                          
130200     MOVE CLAG-PRARTSTD          TO EKH-PRARTSTD                          
130300     MOVE 0                      TO EKH-PRDIRLON                          
130400     MOVE 0                      TO EKH-PRDMTRL                           
130500     MOVE CLAG-PRINK             TO EKH-PRINK                             
130600     MOVE IN34B-PRKURS           TO EKH-PRKURS                            
130700     MOVE IN34B-PRLANDCO-RAD     TO EKH-PRLANDCO                          
130800     MOVE 0                      TO EKH-PROVRPAL                          
130900     MOVE 0                      TO EKH-SUBEL                             
131000     MOVE ZERO                   TO EKH-SUVAT                             
131100     MOVE ZERO                   TO EKH-IDAVINR                           
131200     MOVE SPACE                  TO EKH-IDLEVNR                           
131300     MOVE ZERO                   TO EKH-KDAVVTYP                          
131400                                    EKH-KDRT                              
131500                                    EKH-KVANTMOT                          
131600                                    EKH-KVAVIS                            
131700     MOVE ART-KDSORT             TO EKH-KDSORT                            
131800     MOVE 'SEPV'                 TO EKH-KDTRADP                           
131900     MOVE SPACE                  TO EKH-FLOVRLEV                          
132000     MOVE ZERO                   TO EKH-IDORDNR5                          
132100     MOVE CLAG-PRHEMTAG          TO EKH-PRHEMTAG                          
132200     MOVE SPACE                  TO EKH-IDUSER                            
132300                                    EKH-IDKUNDRF                          
132400     MOVE LEV-IDFAKT             TO EKH-IDFAKT-EXP                        
132500     IF LEV-TIFAKT > ZERO                                                 
132600       MOVE LEV-TIFAKT      TO WS-FAKTURA-DATUM                           
132700                                                                          
132800       IF WS-FAKTURA-DATUM < 500000                                       
132900         MOVE 20                     TO WS-FAKTURA-DATUM(1:2)             
133000       ELSE                                                               
133100         IF WS-FAKTURA-DATUM < 999999                                     
133200           MOVE 19                   TO WS-FAKTURA-DATUM(1:2)             
133300         ELSE                                                             
133400           MOVE 99999999             TO WS-FAKTURA-DATUM                  
133500         END-IF                                                           
133600       END-IF                                                             
133700                                                                          
133800       MOVE WS-FAKTURA-DATUM(3:4)        TO EKH-DAAVIDAT                  
133900     ELSE                                                                 
134000       MOVE ZERO                         TO EKH-DAAVIDAT                  
134100     END-IF                                                               
134200                                                                          
134300     PERFORM IMS-ISRT-WDR901                                              
134400     PERFORM UNTIL SEGMENT-FINNS                                          
134500       ADD +1  TO FIL-IDSEKVNR                                            
134600       PERFORM IMS-ISRT-WDR901                                            
134700     END-PERFORM                                                          
134800     .                                                                    
134900     EJECT                                                                
135000                                                                          
135100 E-INVENTERING-UPD-ROT SECTION.                                           
135200     MOVE IN34B-IDARTNR                TO INV-ART-IDARTNR                 
135300                                                                          
135400*** SOFTWARE ARTIKLAR SKALL INTE INVENTERAS                               
135500     PERFORM IMS-GU-WDK601                                                
135600     IF ART-KDSORT = 'SW'                                                 
135700       CONTINUE                                                           
135800     ELSE                                                                 
135900       PERFORM IMS-ISRT-WDH101                                            
136000     END-IF                                                               
136100     .                                                                    
136200     EJECT                                                                
136300                                                                          
136400 F-INVENTERING-UPD-RAD SECTION.                                           
136500     MOVE IN34B-IDARTNR                TO W-IDARTNR                       
136600     MOVE IN34B-IDDC                   TO IDDC-SEARCH-MAX                 
136700                                          IDDC-SEARCH-MIN                 
136800     PERFORM IMS-GU-WDH111                                                
136900     IF SEGMENT-SAKNAS                                                    
137000                                                                          
137100       PERFORM IMS-GU-WDK601                                              
137200       MOVE ART-IDFKNGRP                 TO INV-IDFKNGRP                  
137300       MOVE ART-KDPRODSL                 TO INV-KDPRODSL                  
137400                                                                          
137500       PERFORM IMS-GHNP-WDK611                                            
137600       MOVE CLAG-KDPSLLOC                TO INV-KDPSLLOC                  
137700       MOVE CLAG-KDVVKL                  TO INV-KDVVKL                    
137800                                                                          
137900       MOVE IN34B-IDDC                   TO WS-IDDC                       
138000       IF CDC-SE                                                          
138100         MOVE CLAG-ADLAGOMR              TO INV-ADLAGOMR                  
138200         MOVE CLAG-ADGANG                TO INV-ADGANG                    
138300         MOVE CLAG-ADPLATS               TO INV-ADPLATS                   
138400       ELSE                                                               
138500         MOVE IN34B-IDDC                 TO W-IDDC                        
138600         PERFORM IMS-GET-HOLD-SALDON-WDK711                               
138700         IF SEGMENT-SAKNAS                                                
138800           MOVE ZERO                     TO INV-ADLAGOMR                  
138900                                            INV-ADGANG                    
139000                                            INV-ADPLATS                   
139100         ELSE                                                             
139200           MOVE SLAG-ADLAGOMR            TO INV-ADLAGOMR                  
139300           MOVE SLAG-ADGANG              TO INV-ADGANG                    
139400           MOVE SLAG-ADPLATS             TO INV-ADPLATS                   
139500         END-IF                                                           
139600       END-IF                                                             
139700       MOVE NEJ                          TO INV-FLINVBEH                  
139800       MOVE NEJ                          TO INV-FLINVSKR                  
139900       MOVE NEJ                          TO INV-FLINV2B                   
140000       MOVE NEJ                          TO INV-FLINV2C                   
140100       MOVE NEJ                          TO INV-FLINV2D                   
140200       MOVE NEJ                          TO INV-FLINV3E                   
140300       MOVE NEJ                          TO INV-FLINV4N                   
140400       MOVE NEJ                          TO INV-FLINV4P                   
140500       MOVE NEJ                          TO INV-FLINV4R                   
140600       MOVE 0                            TO INV-KDINVKAT-OLD              
140700       MOVE NEJ                          TO INV-FLINV85                   
140800       MOVE SPACE                        TO INV-FILLER1                   
140900                                            INV-FILLER2                   
141000       MOVE LEV-IDDC                     TO INV-IDDC                      
141100       MOVE +2                           TO INV-KDINVPRIO                 
141200       MOVE +4                           TO INV-KDINVKAT                  
141300       MOVE +0                           TO INV-KVJUSTKV                  
141400                                                                          
141500       MOVE SPACE                        TO INV-TEINVANM                  
141600       MOVE SPACE                        TO WS-KOMMENTAR                  
141700       MOVE LEV-KDANMORS                 TO INV-K-KDANMORS                
141800       MOVE LEV-KVLEVANM                 TO INV-K-KVLEVANM                
141900       MOVE IN34B-IDDISTR                TO INV-K-IDDISTR                 
142000       MOVE IN34B-IDKUNDNR               TO INV-K-IDKUNDNR                
142100       MOVE WS-KOMMENTAR                 TO INV-TEINVANM                  
142200                                                                          
142300       MOVE ZERO                         TO INV-IDPRTOMG                  
142400                                            INV-IDLOPNR                   
142500                                            INV-KVAKS-OLD                 
142600                                            INV-KVEFRS-OLD                
142700                                            INV-KVLS-OLD                  
142800                                            INV-DAREGDAT-PR1              
142900                                            INV-DAREGDAT-PR2              
143000                                            INV-DAREGDAT-PR3              
143100                                                                          
143200       MOVE WS-SEKEL                     TO WS-AAR                        
143300       MOVE DAGENS-DATUM                 TO WS-TIAAMMDD                   
143400       MOVE 0                            TO WS-LOPNR                      
143500       MOVE WS-TIAAAAMMDDL               TO INV-TISEGKEY                  
143600       MOVE WS-INV-DAREGDAT              TO INV-DAREGDAT-CRE              
143700       MOVE WS-INV-DAREGDAT              TO INV-DAREGDAT                  
143800       MOVE 99999999                     TO INV-DAREGDAT-SORT             
143900                                                                          
144000*** SOFTWARE ARTIKLAR SKALL INTE INVENTERAS                               
144100       IF ART-KDSORT = 'SW'                                               
144200         CONTINUE                                                         
144300       ELSE                                                               
144400         PERFORM IMS-ISRT-WDH111                                          
144500                                                                          
144600         PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                            
144700           IF SEGMENT-FINNS-REDAN                                         
144800             ADD 1 TO INV-TISEGKEY                                        
144900             PERFORM IMS-ISRT-WDH111                                      
145000           END-IF                                                         
145100         END-PERFORM                                                      
145200                                                                          
145300***   INSERT PÅ WDH121 SEGMENTET ***                                      
145400         MOVE 'W4183300'       TO INVL-IDUSER                             
145500         MOVE '0'              TO INVL-KDSEGKEY                           
145600         PERFORM IMS-ISRT-WDH121                                          
145700         MOVE SPACE            TO INVL-IDUSER                             
145800         MOVE '1'              TO INVL-KDSEGKEY                           
145900         PERFORM IMS-ISRT-WDH121                                          
146000         MOVE SPACE            TO INVL-IDUSER                             
146100         MOVE '2'              TO INVL-KDSEGKEY                           
146200         PERFORM IMS-ISRT-WDH121                                          
146300         MOVE SPACE            TO INVL-IDUSER                             
146400         MOVE '3'              TO INVL-KDSEGKEY                           
146500         PERFORM IMS-ISRT-WDH121                                          
146600****   SLUT PÅ INSERT PÅ WDH121 SEGMENT                                   
146700       END-IF                                                             
146800     END-IF                                                               
146900     .                                                                    
147000     EJECT                                                                
147100                                                                          
147200 G-FLYTTA-TILLAEGGSKOSTNADER SECTION.                                     
147300     IF IN34A-SUVAT-FAKT > ZERO                                           
147400       MOVE 'MOMS'                  TO EKH-KDEKNIVA                       
147500       MOVE IN34A-SUVAT-FAKT        TO EKH-SUBEL                          
147600       PERFORM GA-FLYTTA-SKRIV-EKO-TILLRAD                                
147700     END-IF                                                               
147800     IF IN34A-PRLANDCO > ZERO                                             
147900       MOVE 'LAND'                  TO EKH-KDEKNIVA                       
148000       MOVE IN34A-PRLANDCO          TO EKH-SUBEL                          
148100       PERFORM GA-FLYTTA-SKRIV-EKO-TILLRAD                                
148200     END-IF                                                               
148300     IF IN34A-PRFRAKT   > ZERO                                            
148400       MOVE 'FRAKT'                 TO EKH-KDEKNIVA                       
148500       MOVE IN34A-PRFRAKT           TO EKH-SUBEL                          
148600       PERFORM GA-FLYTTA-SKRIV-EKO-TILLRAD                                
148700     END-IF                                                               
148800     IF IN34A-PRFOERS   > ZERO                                            
148900       MOVE 'FÖRS'                  TO EKH-KDEKNIVA                       
149000       MOVE IN34A-PRFOERS           TO EKH-SUBEL                          
149100       PERFORM GA-FLYTTA-SKRIV-EKO-TILLRAD                                
149200     END-IF                                                               
149300     IF IN34A-PRLEGKST > ZERO                                             
149400       MOVE 'LEG'                   TO EKH-KDEKNIVA                       
149500       MOVE IN34A-PRLEGKST          TO EKH-SUBEL                          
149600       PERFORM GA-FLYTTA-SKRIV-EKO-TILLRAD                                
149700     END-IF                                                               
149800     .                                                                    
149900     EJECT                                                                
150000                                                                          
150100 GA-FLYTTA-SKRIV-EKO-TILLRAD SECTION.                                     
150200     MOVE IN34A-IDDC   TO WS-IDDC                                         
150300     IF XDC-NON-VCC-OWNED                                                 
150400       CONTINUE                                                           
150500     ELSE                                                                 
150600       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                    
150700       MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                    
150800       MOVE IN34A-IDDISTR           TO TEST-IDDISTR                       
150900       MOVE IN34A-IDKUNDNR          TO W-TEST-IDKUNDNR                    
151000                                                                          
151100       MOVE 'W4183300'              TO FIL-IDPGM                          
151200       MOVE WS-AAAAMMDD             TO FIL-DAREGDAT                       
151300       MOVE WS-TTMMSSTH             TO FIL-TIKLOCK                        
151400       MOVE 1                       TO FIL-IDSEKVNR                       
151500       MOVE 'W510EKHA'              TO FIL-IDCPYTXT                       
151600       MOVE 'W4183300'              TO FIL-IDUSER                         
151700                                                                          
151800*** SKALL FLYTTA KDVAT HÄR ENLIGT BOSSE HAMMARIN 2004-03-01,(DDI)         
151900       MOVE LEV-KDVAT               TO EKH-BEVAT                          
152000                                                                          
152100       MOVE IN34A-TIKNOTA          TO WS-TIKNOTA                          
152200       IF WS-TIKNOTA(1:2) < 50                                            
152300         MOVE 20                   TO EKH-DAVERDAT (1:2)                  
152400       ELSE                                                               
152500         MOVE 19                   TO EKH-DAVERDAT (1:2)                  
152600       END-IF                                                             
152700       MOVE WS-TIKNOTA             TO EKH-DAVERDAT (3:6)                  
152800                                                                          
152900       MOVE ' '                     TO EKH-FLLSBOK                        
153000       MOVE SPACE                   TO EKH-IDANALYS                       
153100       MOVE 0                       TO EKH-IDARTNR                        
153200       MOVE IN34A-IDDC              TO EKH-IDDC-SEND                      
153300       MOVE ' '                     TO EKH-IDDC-REC                       
153400       MOVE IN34A-IDDISTR           TO EKH-IDDISTR                        
153500       MOVE ZERO                    TO EKH-IDKONTO                        
153600       MOVE SPACE                   TO EKH-IDKST                          
153700       MOVE IN34A-IDKUNDNR          TO EKH-IDKUNDNR                       
153800       MOVE ' '                     TO EKH-IDTRANS                        
153900                                                                          
154000       MOVE 'VO'                    TO CIA-IDARTPRE-IN                    
154100       MOVE IN34A-IDKNOTNR          TO CIA-IDARTBET-IN                    
154200       CALL W009CIA USING  CIA-W009CIA                                    
154300       MOVE CIA-IDARTBET-UT         TO EKH-IDVERGL                        
154400                                                                          
154500       MOVE SPACE                   TO EKH-KDANMORS                       
154600       MOVE '303'                   TO EKH-KDEKHHT                        
154700       IF DIST35-NONVCC-NONVCC-REFILL                                     
154800       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
154900         MOVE '381'                 TO EKH-KDEKSHT                        
155000       ELSE                                                               
155100         MOVE '3XX'                 TO EKH-KDEKSHT                        
155200       END-IF                                                             
155300       MOVE 0                       TO EKH-KDFRAKT                        
155400       MOVE 0                       TO EKH-KDPRODSL                       
155500       MOVE 0                       TO EKH-KDPSLLOC                       
155600       MOVE IN34A-KDVALISO          TO EKH-KDVALISO                       
155700       MOVE 0                       TO EKH-KVANTAL                        
155800       MOVE 0                       TO EKH-PRARTNTO                       
155900       MOVE 0                       TO EKH-PRARTSJK                       
156000       MOVE 0                       TO EKH-PRARTSTD                       
156100       MOVE 0                       TO EKH-PRDIRLON                       
156200       MOVE 0                       TO EKH-PRDMTRL                        
156300       MOVE 0                       TO EKH-PRINK                          
156400       MOVE IN34A-PRKURS            TO EKH-PRKURS                         
156500       MOVE 0                       TO EKH-PRLANDCO                       
156600       MOVE 0                       TO EKH-PROVRPAL                       
156700       MOVE ZERO                    TO EKH-SUVAT                          
156800                                       EKH-IDAVINR                        
156900       MOVE SPACE                   TO EKH-IDLEVNR                        
157000       MOVE ZERO                    TO EKH-KDAVVTYP                       
157100                                       EKH-KDRT                           
157200                                       EKH-KVANTMOT                       
157300                                       EKH-KVAVIS                         
157400       MOVE SPACE                   TO EKH-KDSORT                         
157500       MOVE 'SEPV'                  TO EKH-KDTRADP                        
157600       MOVE SPACE                   TO EKH-FLOVRLEV                       
157700       MOVE ZERO                    TO EKH-IDORDNR5                       
157800       MOVE ZERO                    TO EKH-PRHEMTAG                       
157900       MOVE SPACE                   TO EKH-IDUSER                         
158000                                       EKH-IDKUNDRF                       
158100       MOVE LEV-IDFAKT              TO EKH-IDFAKT-EXP                     
158200       IF LEV-TIFAKT > ZERO                                               
158300         MOVE LEV-TIFAKT    TO WS-FAKTURA-DATUM                           
158400                                                                          
158500         IF WS-FAKTURA-DATUM < 500000                                     
158600           MOVE 20                   TO WS-FAKTURA-DATUM(1:2)             
158700         ELSE                                                             
158800           IF WS-FAKTURA-DATUM < 999999                                   
158900             MOVE 19                 TO WS-FAKTURA-DATUM(1:2)             
159000           ELSE                                                           
159100             MOVE 99999999           TO WS-FAKTURA-DATUM                  
159200           END-IF                                                         
159300         END-IF                                                           
159400                                                                          
159500         MOVE WS-FAKTURA-DATUM(3:4)      TO EKH-DAAVIDAT                  
159600       ELSE                                                               
159700         MOVE ZERO                       TO EKH-DAAVIDAT                  
159800       END-IF                                                             
159900                                                                          
160000       PERFORM IMS-ISRT-WDR901                                            
160100       PERFORM UNTIL SEGMENT-FINNS                                        
160200         ADD +1  TO FIL-IDSEKVNR                                          
160300         PERFORM IMS-ISRT-WDR901                                          
160400       END-PERFORM                                                        
160500     END-IF                                                               
160600                                                                          
160700     MOVE IN34A-IDDC             TO WS-IDDC                               
160800     IF (CDC-SE OR GOOD-DDC)                                              
160900     AND (DIST35-CDC-NONVCC-REFILL                                        
161000     OR DIST35-CDC-RETURNS-NON-VCC                                        
161100     OR DIST35-NONVCC-NONVCC-REFILL                                       
161200     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
161300     OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST))                     
161400     OR (NDC                                                              
161500     AND (DIST35-VCC-NONVCC-REFILL OR DIST35-VCC-NONVCC-TRANSFER))        
161600*      LEV-KDANMORS = '00' OR '43' OR '60' OR '63' OR '94' OR '54'        
161700*                  OR '55'                                                
161800                                                                          
161900       MOVE FIL-IDPGM TO EKO-FIL-IDPGM                                    
162000       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
162100       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
162200       MOVE FIL-IDSEKVNR         TO EKO-FIL-IDSEKVNR                      
162300       MOVE FIL-CT-IDSYSTEM      TO EKO-FIL-CT-IDSYSTEM                   
162400       MOVE FIL-CT-IDPTYP        TO EKO-FIL-CT-IDPTYP                     
162500       MOVE FIL-CT-IDVTYP        TO EKO-FIL-CT-IDVTYP                     
162600       MOVE EKH-BEVAT            TO EKO-EKH-BEVAT                         
162700       MOVE EKH-DAVERDAT         TO EKO-EKH-DAVERDAT                      
162800       MOVE EKH-FLLSBOK          TO EKO-EKH-FLLSBOK                       
162900       MOVE EKH-IDANALYS         TO EKO-EKH-IDANALYS                      
163000       MOVE EKH-IDKONTO          TO EKO-EKH-IDKONTO                       
163100       MOVE EKH-IDKST            TO EKO-EKH-IDKST                         
163200       MOVE EKH-IDARTNR          TO EKO-EKH-IDARTNR                       
163300       MOVE EKH-IDDC-SEND        TO EKO-EKH-IDDC-SEND                     
163400       MOVE EKH-IDDC-REC         TO EKO-EKH-IDDC-REC                      
163500       MOVE EKH-IDDISTR          TO EKO-EKH-IDDISTR                       
163600       MOVE EKH-IDKUNDNR         TO EKO-EKH-IDKUNDNR                      
163700       MOVE EKH-IDTRANS          TO EKO-EKH-IDTRANS                       
163800       MOVE EKH-IDVERGL          TO EKO-EKH-IDVERGL                       
163900       MOVE EKH-KDANMORS         TO EKO-EKH-KDANMORS                      
164000       MOVE EKH-KDEKHHT          TO EKO-EKH-KDEKHHT                       
164100       MOVE EKH-KDEKSHT          TO EKO-EKH-KDEKSHT                       
164200       MOVE EKH-KDEKNIVA         TO EKO-EKH-KDEKNIVA                      
164300       IF DIST35-NONVCC-NONVCC-REFILL                                     
164400       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
164500         IF SPAR-KDANMORS = '00' OR '20' OR '90'                          
164600           MOVE '371'            TO EKO-EKH-KDEKSHT                       
164700         END-IF                                                           
164800         IF SPAR-KDANMORS = '60' OR '63' OR '43'                          
164900           MOVE '371'            TO EKO-EKH-KDEKSHT                       
165000         END-IF                                                           
165100       END-IF                                                             
165200       MOVE EKH-KDFRAKT          TO EKO-EKH-KDFRAKT                       
165300       MOVE EKH-KDPRODSL         TO EKO-EKH-KDPRODSL                      
165400       MOVE EKH-KDPSLLOC         TO EKO-EKH-KDPSLLOC                      
165500                                                                          
165600*--- KINA DEALERS FTG = 60 HAR PRAVCOST OCH 'CNY' MEN                     
165700*--- KINA REFILL DISTR. OCH RETURER TILL CDC KOD 54/94                    
165800*--- HAR STDPRIS OCH 'SEK'.                                               
165900                                                                          
166000       MOVE IN34A-KDVALISO       TO EKO-EKH-KDVALISO                      
166100                                                                          
166200       MOVE EKH-KVANTAL          TO EKO-EKH-KVANTAL                       
166300       MOVE EKH-PRARTNTO         TO EKO-EKH-PRARTNTO                      
166400       MOVE EKH-PRARTSJK         TO EKO-EKH-PRARTSJK                      
166500       MOVE EKH-PRARTSTD         TO EKO-EKH-PRARTSTD                      
166600       MOVE EKH-PRDIRLON         TO EKO-EKH-PRDIRLON                      
166700       MOVE EKH-PRDMTRL          TO EKO-EKH-PRDMTRL                       
166800       MOVE EKH-PRINK            TO EKO-EKH-PRINK                         
166900       MOVE EKH-PRKURS           TO EKO-EKH-PRKURS                        
167000       MOVE EKH-PRLANDCO         TO EKO-EKH-PRLANDCO                      
167100       MOVE EKH-PROVRPAL         TO EKO-EKH-PROVRPAL                      
167200       MOVE EKH-SUBEL            TO EKO-EKH-SUBEL                         
167300       MOVE EKH-SUVAT            TO EKO-EKH-SUVAT                         
167400       MOVE EKH-DAAVIDAT         TO EKO-EKH-DAAVIDAT                      
167500       MOVE EKH-IDAVINR          TO EKO-EKH-IDAVINR                       
167600       MOVE EKH-IDLEVNR          TO EKO-EKH-IDLEVNR                       
167700       MOVE EKH-KDAVVTYP         TO EKO-EKH-KDAVVTYP                      
167800       MOVE EKH-KDRT             TO EKO-EKH-KDRT                          
167900       MOVE EKH-KVANTMOT         TO EKO-EKH-KVANTMOT                      
168000       MOVE EKH-KVAVIS           TO EKO-EKH-KVAVIS                        
168100       MOVE EKH-KDSORT           TO EKO-EKH-KDSORT                        
168200       MOVE EKH-FLOVRLEV         TO EKO-EKH-FLOVRLEV                      
168300       MOVE EKH-IDORDNR5         TO EKO-EKH-IDORDNR5                      
168400       MOVE EKH-PRHEMTAG         TO EKO-EKH-PRHEMTAG                      
168500       MOVE EKH-FLDCET           TO EKO-EKH-FLDCET                        
168600       MOVE SPACE                TO EKO-EKH-IDKUNDRF                      
168700       MOVE LEV-IDFAKT           TO EKO-EKH-IDFAKT-EXP                    
168800                                                                          
168900       IF DIST35-CDC-NONVCC-REFILL                                        
169000       OR DIST35-VCC-NONVCC-REFILL                                        
169100       OR DIST35-VCC-NONVCC-TRANSFER                                      
169200       OR DIST35-CDC-RETURNS-NON-VCC                                      
169300       OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)                    
169400       OR DIST35-NONVCC-NONVCC-REFILL                                     
169500       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
169600         IF (DIST18-SCRAP-NDC-QUAL AND NDC-CN-KUND)                       
169700         OR DIST35-REFILL-CN                                              
169800         OR DIST35-JP-NDC71-REFILL                                        
169900**** here we need to add transfer from JP/AU to CN                        
170000         OR DIST35-CN-CDC-RETURNS                                         
170100         OR IN34A-KDTRADP = 'CN05'                                        
170200           MOVE 'W570'           TO EKO-FIL-IDCPYTXT(1:4)                 
170300         ELSE                                                             
170400           IF (DIST18-SCRAP-NDC-QUAL AND NDC-IN-KUND)                     
170500           OR DIST35-CDC-IN-REFILL                                        
170600           OR DIST35-JP-NDC67-REFILL                                      
170700**** here we need to add transfer from JP/AU to in                        
170800           OR DIST35-IN-CDC-RETURNS                                       
170900           OR IN34A-KDTRADP = 'IN07'                                      
171000             MOVE 'W515'         TO EKO-FIL-IDCPYTXT(1:4)                 
171100           ELSE                                                           
171200             IF DIST35-NDCCN-NDCUS-REFILL                                 
171300**** here we need to add transfer from JP/AU to in                        
171400                MOVE 'US01'             TO EKO-EKH-KDTRADP                
171500                MOVE 'W561EKHA'         TO EKO-FIL-IDCPYTXT               
171600             ELSE                                                         
171700               IF (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)            
171800               OR DIST35-CDC-NONVCC-REFILL                                
171900               OR DIST35-VCC-NONVCC-REFILL                                
172000               OR DIST35-VCC-NONVCC-TRANSFER                              
172100               OR DIST35-CDC-RETURNS-NON-VCC                              
172200               OR DIST35-NONVCC-NONVCC-REFILL                             
172300               OR DIST35-NONVCC-NONVCC-TRANSFER                           
172400                 MOVE IN34A-KDTRADP     TO EKO-FIL-IDCPYTXT(1:4)          
172500               END-IF                                                     
172600             END-IF                                                       
172700           END-IF                                                         
172800         END-IF                                                           
172900         MOVE 'EKHA'             TO EKO-FIL-IDCPYTXT(5:4)                 
173000         MOVE IN34A-KDTRADP      TO EKO-EKH-KDTRADP                       
173100       END-IF                                                             
173200                                                                          
173300       PERFORM IMS-ISRT-WDR801                                            
173400       PERFORM UNTIL SEGMENT-FINNS                                        
173500         ADD +1  TO EKO-FIL-IDSEKVNR                                      
173600         PERFORM IMS-ISRT-WDR801                                          
173700       END-PERFORM                                                        
173800     END-IF                                                               
173900     .                                                                    
174000     EJECT                                                                
174100                                                                          
174200 I-SAETT-STATUS-WDA2 SECTION.                                             
174300     MOVE IN34A-IDDISTR                TO W-IDDISTR                       
174400                                          W-IDDISTR-4103                  
174410                                          W-IDDISTR-B2                    
174500     MOVE IN34A-IDKUNDNR               TO W-IDKUNDNR                      
174600                                          W-IDKUNDNR-4103                 
174610                                          W-IDKUNDNR-B2                   
174700     MOVE IN34A-IDRAPPNR               TO W-IDRAPPNR                      
174800                                          W-IDRAPPNR-4103                 
174801     MOVE NEJ                          TO SW-LYNK-NON-API                 
174802                                                                          
174810     PERFORM IMS-GU-WDB201                                                
174820     IF SEGMENT-FINNS                                                     
174830        IF GMT-KDKUNDKAT = 03                                             
174840            MOVE JA                    TO SW-LYNK-NON-API                 
174850        END-IF                                                            
174860     END-IF                                                               
174900     PERFORM IMS-GHU-WDA201                                               
175000     IF ANM-KDLEVANM = '9'                                                
175100       MOVE ANM-KDLEVANM-UPD           TO ANM-KDLEVANM                    
175200                                                                          
175300       MOVE ANM-IDSYSTEM               TO EVENT-SW                        
175400                                          WS-EVENT-KUND                   
175500                                          Z430-REQU-IDEVENTREC            
175600       PERFORM IMS-REPL-WDA201                                            
175700       IF ANM-KDLEVANM-UPD  = '8'                                         
175800         MOVE ANM-IDDISTR              TO WS-IDDISTR-EVENT                
175900         MOVE ANM-IDKUNDNR             TO WS-IDKUNDNR-EVENT               
176000         MOVE ANM-IDRAPPNR             TO WS-IDRAPPNR-EVENT               
176100                                                                          
176200         IF EVENT-YES OR LYNK-NON-API                                     
176300                                                                          
176310           IF LYNK-NON-API                                                
176320              MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC             
176330           ELSE                                                           
176340              MOVE ANM-IDSYSTEM       TO Z430-REQU-IDEVENTREC             
176350           END-IF                                                         
176360                                                                          
176400           IF (ANM-IDSYSTEM = 'LYNK') OR LYNK-NON-API                     
176500             MOVE 'L'              TO WS-EVENT-KUND                       
176600           END-IF                                                         
176700           IF ANM-IDSYSTEM = 'POLE'                                       
176800             MOVE 'P'              TO WS-EVENT-KUND                       
176900           END-IF                                                         
177000           IF ANM-IDSYSTEM = 'ECOM'                                       
177100             MOVE 'E'              TO WS-EVENT-KUND                       
177200           END-IF                                                         
177300           IF ANM-IDSYSTEM = 'TAD '                                       
177400             MOVE 'T'              TO WS-EVENT-KUND                       
177500           END-IF                                                         
177600           IF ANM-IDSYSTEM = 'ACC '                                       
177700             MOVE 'A'              TO WS-EVENT-KUND                       
177800           END-IF                                                         
177900           IF ANM-IDSYSTEM = 'APA '                                       
178000             MOVE 'K'              TO WS-EVENT-KUND                       
178100           END-IF                                                         
178200           IF ANM-IDSYSTEM = 'APB '                                       
178300             MOVE 'B'              TO WS-EVENT-KUND                       
178400           END-IF                                                         
178500           IF ANM-IDSYSTEM = 'APC '                                       
178600             MOVE 'C'              TO WS-EVENT-KUND                       
178700           END-IF                                                         
178800           IF ANM-IDSYSTEM = 'APD '                                       
178900             MOVE 'D'              TO WS-EVENT-KUND                       
179000           END-IF                                                         
179100           IF ANM-IDSYSTEM = 'APE '                                       
179200             MOVE 'M'              TO WS-EVENT-KUND                       
179300           END-IF                                                         
179400           IF ANM-IDSYSTEM = 'APF '                                       
179500             MOVE 'F'              TO WS-EVENT-KUND                       
179600           END-IF                                                         
179700           IF ANM-IDSYSTEM = 'APG '                                       
179800             MOVE 'G'              TO WS-EVENT-KUND                       
179900           END-IF                                                         
180000           IF ANM-IDSYSTEM = 'APH '                                       
180100             MOVE 'H'              TO WS-EVENT-KUND                       
180200           END-IF                                                         
180300           IF ANM-IDSYSTEM = 'API '                                       
180400             MOVE 'I'              TO WS-EVENT-KUND                       
180500           END-IF                                                         
180600           IF ANM-IDSYSTEM = 'APJ '                                       
180700             MOVE 'J'              TO WS-EVENT-KUND                       
180800           END-IF                                                         
180900            PERFORM J-CREATE-EVENT                                        
181000         END-IF                                                           
181100                                                                          
181200         PERFORM IMS-GHU-WDGX4103                                         
181300         IF SEGMENT-FINNS                                                 
181400           PERFORM IMS-DLET-WDGX4103                                      
181500         END-IF                                                           
181600       END-IF                                                             
181700     END-IF                                                               
181800     .                                                                    
181900     EJECT                                                                
182000                                                                          
182100 H-UPPDAT-EKO-HUV-WDR9  SECTION.                                          
182200     MOVE IN34A-IDDC   TO WS-IDDC                                         
182300     IF XDC-NON-VCC-OWNED                                                 
182400       CONTINUE                                                           
182500     ELSE                                                                 
182600       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                    
182700       MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                    
182800       MOVE IN34A-IDDISTR           TO TEST-IDDISTR                       
182900       MOVE IN34A-IDKUNDNR          TO W-TEST-IDKUNDNR                    
183000                                                                          
183100       MOVE 'W4183300'              TO FIL-IDPGM                          
183200       MOVE WS-AAAAMMDD             TO FIL-DAREGDAT                       
183300       MOVE WS-TTMMSSTH             TO FIL-TIKLOCK                        
183400       MOVE 1                       TO FIL-IDSEKVNR                       
183500       MOVE 'W510EKHA'              TO FIL-IDCPYTXT                       
183600       MOVE 'W4183300'              TO FIL-IDUSER                         
183700                                                                          
183800*** SKALL FLYTTA KDVAT HÄR ENLIGT BOSSE HAMMARIN 2004-03-01,(DDI)         
183900       MOVE LEV-KDVAT               TO EKH-BEVAT                          
184000                                                                          
184100       MOVE IN34A-TIKNOTA          TO WS-TIKNOTA                          
184200       IF WS-TIKNOTA(1:2) < 50                                            
184300         MOVE 20                   TO EKH-DAVERDAT (1:2)                  
184400       ELSE                                                               
184500         MOVE 19                   TO EKH-DAVERDAT (1:2)                  
184600       END-IF                                                             
184700       MOVE WS-TIKNOTA             TO EKH-DAVERDAT (3:6)                  
184800                                                                          
184900       MOVE ' '                     TO EKH-FLLSBOK                        
185000       MOVE SPACE                   TO EKH-IDANALYS                       
185100       MOVE 0                       TO EKH-IDARTNR                        
185200       MOVE IN34A-IDDC              TO EKH-IDDC-SEND                      
185300       MOVE ' '                     TO EKH-IDDC-REC                       
185400       MOVE IN34A-IDDISTR           TO EKH-IDDISTR                        
185500       MOVE ZERO                    TO EKH-IDKONTO                        
185600       MOVE SPACE                   TO EKH-IDKST                          
185700       MOVE IN34A-IDKUNDNR          TO EKH-IDKUNDNR                       
185800       MOVE ' '                     TO EKH-IDTRANS                        
185900                                                                          
186000       MOVE 'VO'                    TO CIA-IDARTPRE-IN                    
186100       MOVE IN34A-IDKNOTNR          TO CIA-IDARTBET-IN                    
186200       CALL W009CIA USING  CIA-W009CIA                                    
186300       MOVE CIA-IDARTBET-UT         TO EKH-IDVERGL                        
186400                                                                          
186500       MOVE SPACE                   TO EKH-KDANMORS                       
186600       MOVE '303'                   TO EKH-KDEKHHT                        
186700       IF DIST35-NONVCC-NONVCC-REFILL                                     
186800       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
186900         MOVE '381'                 TO EKH-KDEKSHT                        
187000       ELSE                                                               
187100         MOVE '3XX'                 TO EKH-KDEKSHT                        
187200       END-IF                                                             
187300       MOVE 'SUM'                   TO EKH-KDEKNIVA                       
187400       MOVE ZERO                    TO EKH-KDFRAKT                        
187500       MOVE 0                       TO EKH-KDPRODSL                       
187600       MOVE 0                       TO EKH-KDPSLLOC                       
187700       MOVE IN34A-KDVALISO          TO EKH-KDVALISO                       
187800       MOVE 0                       TO EKH-KVANTAL                        
187900       MOVE 0                       TO EKH-PRARTNTO                       
188000       MOVE 0                       TO EKH-PRARTSJK                       
188100       MOVE 0                       TO EKH-PRARTSTD                       
188200       MOVE 0                       TO EKH-PRDIRLON                       
188300       MOVE 0                       TO EKH-PRDMTRL                        
188400       MOVE 0                       TO EKH-PRINK                          
188500       MOVE IN34A-PRKURS            TO EKH-PRKURS                         
188600       MOVE 0                       TO EKH-PRLANDCO                       
188700       MOVE 0                       TO EKH-PROVRPAL                       
188800       MOVE IN34A-SUKRETOT          TO EKH-SUBEL                          
188900       MOVE IN34A-SUVAT-FAKT        TO EKH-SUVAT                          
189000       MOVE ZERO                    TO EKH-IDAVINR                        
189100       MOVE SPACE                   TO EKH-IDLEVNR                        
189200       MOVE ZERO                    TO EKH-KDAVVTYP                       
189300                                       EKH-KDRT                           
189400                                       EKH-KVANTMOT                       
189500                                       EKH-KVAVIS                         
189600       MOVE SPACE                   TO EKH-KDSORT                         
189700       MOVE 'SEPV'                  TO EKH-KDTRADP                        
189800       MOVE SPACE                   TO EKH-FLOVRLEV                       
189900       MOVE ZERO                    TO EKH-IDORDNR5                       
190000       MOVE ZERO                    TO EKH-PRHEMTAG                       
190100       MOVE SPACE                   TO EKH-IDUSER                         
190200                                       EKH-IDKUNDRF                       
190300       MOVE LEV-IDFAKT              TO EKH-IDFAKT-EXP                     
190400       IF LEV-TIFAKT > ZERO                                               
190500         MOVE LEV-TIFAKT    TO WS-FAKTURA-DATUM                           
190600                                                                          
190700         IF WS-FAKTURA-DATUM < 500000                                     
190800           MOVE 20                   TO WS-FAKTURA-DATUM(1:2)             
190900         ELSE                                                             
191000           IF WS-FAKTURA-DATUM < 999999                                   
191100             MOVE 19                 TO WS-FAKTURA-DATUM(1:2)             
191200           ELSE                                                           
191300             MOVE 99999999           TO WS-FAKTURA-DATUM                  
191400           END-IF                                                         
191500         END-IF                                                           
191600                                                                          
191700         MOVE WS-FAKTURA-DATUM(3:4)      TO EKH-DAAVIDAT                  
191800       ELSE                                                               
191900         MOVE ZERO                       TO EKH-DAAVIDAT                  
192000       END-IF                                                             
192100                                                                          
192200                                                                          
192300       PERFORM IMS-ISRT-WDR901                                            
192400       PERFORM UNTIL SEGMENT-FINNS                                        
192500         ADD +1  TO FIL-IDSEKVNR                                          
192600         PERFORM IMS-ISRT-WDR901                                          
192700       END-PERFORM                                                        
192800     END-IF                                                               
192900                                                                          
193000     MOVE IN34A-IDDC              TO WS-IDDC                              
193100     IF (CDC-SE OR GOOD-DDC)                                              
193200     AND (DIST35-CDC-NONVCC-REFILL                                        
193300     OR DIST35-CDC-RETURNS-NON-VCC                                        
193400     OR DIST35-NONVCC-NONVCC-REFILL                                       
193500     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
193600     OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST))                     
193700     OR (NDC                                                              
193800     AND (DIST35-VCC-NONVCC-REFILL OR DIST35-VCC-NONVCC-TRANSFER))        
193900*      LEV-KDANMORS = '00' OR '43' OR '60' OR '63' OR '94' OR '54'        
194000*                  OR '55'                                                
194100                                                                          
194200       MOVE FIL-IDPGM TO EKO-FIL-IDPGM                                    
194300       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
194400       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
194500       MOVE FIL-IDSEKVNR         TO EKO-FIL-IDSEKVNR                      
194600       MOVE FIL-CT-IDSYSTEM      TO EKO-FIL-CT-IDSYSTEM                   
194700       MOVE FIL-CT-IDPTYP        TO EKO-FIL-CT-IDPTYP                     
194800       MOVE FIL-CT-IDVTYP        TO EKO-FIL-CT-IDVTYP                     
194900       MOVE EKH-BEVAT            TO EKO-EKH-BEVAT                         
195000       MOVE EKH-DAVERDAT         TO EKO-EKH-DAVERDAT                      
195100       MOVE EKH-FLLSBOK          TO EKO-EKH-FLLSBOK                       
195200       MOVE EKH-IDANALYS         TO EKO-EKH-IDANALYS                      
195300       MOVE EKH-IDKONTO          TO EKO-EKH-IDKONTO                       
195400       MOVE EKH-IDKST            TO EKO-EKH-IDKST                         
195500       MOVE EKH-IDARTNR          TO EKO-EKH-IDARTNR                       
195600       MOVE EKH-IDDC-SEND        TO EKO-EKH-IDDC-SEND                     
195700       MOVE EKH-IDDC-REC         TO EKO-EKH-IDDC-REC                      
195800       MOVE EKH-IDDISTR          TO EKO-EKH-IDDISTR                       
195900       MOVE EKH-IDKUNDNR         TO EKO-EKH-IDKUNDNR                      
196000       MOVE EKH-IDTRANS          TO EKO-EKH-IDTRANS                       
196100       MOVE EKH-IDVERGL          TO EKO-EKH-IDVERGL                       
196200       MOVE EKH-KDANMORS         TO EKO-EKH-KDANMORS                      
196300       MOVE EKH-KDEKHHT          TO EKO-EKH-KDEKHHT                       
196400       MOVE EKH-KDEKSHT          TO EKO-EKH-KDEKSHT                       
196500       IF DIST35-NONVCC-NONVCC-REFILL                                     
196600       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
196700         IF SPAR-KDANMORS = '00' OR '20' OR '90'                          
196800           MOVE '371'              TO EKO-EKH-KDEKSHT                     
196900         END-IF                                                           
197000         IF SPAR-KDANMORS = '60' OR '63' OR '43'                          
197100           MOVE '371'              TO EKO-EKH-KDEKSHT                     
197200         END-IF                                                           
197300       END-IF                                                             
197400       MOVE EKH-KDEKNIVA         TO EKO-EKH-KDEKNIVA                      
197500       MOVE EKH-KDFRAKT          TO EKO-EKH-KDFRAKT                       
197600       MOVE EKH-KDPRODSL         TO EKO-EKH-KDPRODSL                      
197700       MOVE EKH-KDPSLLOC         TO EKO-EKH-KDPSLLOC                      
197800                                                                          
197900*--- KINA DEALERS FTG = 60 HAR PRAVCOST OCH 'CNY' MEN                     
198000*--- KINA REFILL DISTR. OCH RETURER TILL CDC KOD 54/94                    
198100*--- HAR STDPRIS OCH 'SEK'.                                               
198200                                                                          
198300       MOVE IN34A-KDVALISO       TO EKO-EKH-KDVALISO                      
198400                                                                          
198500       MOVE EKH-KVANTAL          TO EKO-EKH-KVANTAL                       
198600       MOVE EKH-PRARTNTO         TO EKO-EKH-PRARTNTO                      
198700       MOVE EKH-PRARTSJK         TO EKO-EKH-PRARTSJK                      
198800       MOVE EKH-PRARTSTD         TO EKO-EKH-PRARTSTD                      
198900       MOVE EKH-PRDIRLON         TO EKO-EKH-PRDIRLON                      
199000       MOVE EKH-PRDMTRL          TO EKO-EKH-PRDMTRL                       
199100       MOVE EKH-PRINK            TO EKO-EKH-PRINK                         
199200       MOVE EKH-PRKURS           TO EKO-EKH-PRKURS                        
199300       MOVE EKH-PRLANDCO         TO EKO-EKH-PRLANDCO                      
199400       MOVE EKH-PROVRPAL         TO EKO-EKH-PROVRPAL                      
199500       MOVE EKH-SUBEL            TO EKO-EKH-SUBEL                         
199600       MOVE EKH-SUVAT            TO EKO-EKH-SUVAT                         
199700       MOVE EKH-DAAVIDAT         TO EKO-EKH-DAAVIDAT                      
199800       MOVE EKH-IDAVINR          TO EKO-EKH-IDAVINR                       
199900       MOVE '1441'               TO EKO-EKH-IDLEVNR                       
200000       MOVE EKH-KDAVVTYP         TO EKO-EKH-KDAVVTYP                      
200100       MOVE EKH-KDRT             TO EKO-EKH-KDRT                          
200200       MOVE EKH-KVANTMOT         TO EKO-EKH-KVANTMOT                      
200300       MOVE EKH-KVAVIS           TO EKO-EKH-KVAVIS                        
200400       MOVE EKH-KDSORT           TO EKO-EKH-KDSORT                        
200500       MOVE EKH-FLOVRLEV         TO EKO-EKH-FLOVRLEV                      
200600       MOVE EKH-IDORDNR5         TO EKO-EKH-IDORDNR5                      
200700       MOVE EKH-PRHEMTAG         TO EKO-EKH-PRHEMTAG                      
200800       MOVE EKH-FLDCET           TO EKO-EKH-FLDCET                        
200900       MOVE SPACE                TO EKO-EKH-IDKUNDRF                      
201000       MOVE LEV-IDFAKT           TO EKO-EKH-IDFAKT-EXP                    
201100                                                                          
201200       IF DIST35-CDC-NONVCC-REFILL                                        
201300       OR DIST35-VCC-NONVCC-REFILL                                        
201400       OR DIST35-VCC-NONVCC-TRANSFER                                      
201500       OR DIST35-CDC-RETURNS-NON-VCC                                      
201600       OR (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)                    
201700       OR DIST35-NONVCC-NONVCC-REFILL                                     
201800       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
201900         IF (DIST18-SCRAP-NDC-QUAL AND NDC-CN-KUND)                       
202000         OR DIST35-REFILL-CN                                              
202100         OR DIST35-JP-NDC71-REFILL                                        
202200**** here we need to add transfer from JP/AU to CN                        
202300         OR DIST35-CN-CDC-RETURNS                                         
202400         OR IN34A-KDTRADP = 'CN05'                                        
202500           MOVE 'W570'             TO EKO-FIL-IDCPYTXT(1:4)               
202600         ELSE                                                             
202700           IF (DIST18-SCRAP-NDC-QUAL AND NDC-IN-KUND)                     
202800           OR DIST35-CDC-IN-REFILL                                        
202900           OR DIST35-JP-NDC67-REFILL                                      
203000**** here we need to add transfer from JP/AU to iN                        
203100           OR DIST35-IN-CDC-RETURNS                                       
203200           OR IN34A-KDTRADP = 'IN07'                                      
203300             MOVE 'W515'           TO EKO-FIL-IDCPYTXT(1:4)               
203400           ELSE                                                           
203500             IF DIST35-NDCCN-NDCUS-REFILL                                 
203600**** here we need to add transfer from JP/AU to us                        
203700                MOVE 'US01'               TO EKO-EKH-KDTRADP              
203800                MOVE 'W561EKHA'           TO EKO-FIL-IDCPYTXT             
203900             ELSE                                                         
204000               IF (DIST18-SCRAP-NDC-QUAL AND NDC-NON-VCC-CUST)            
204100               OR DIST35-CDC-NONVCC-REFILL                                
204200               OR DIST35-VCC-NONVCC-REFILL                                
204300               OR DIST35-VCC-NONVCC-TRANSFER                              
204400               OR DIST35-CDC-RETURNS-NON-VCC                              
204500               OR DIST35-NONVCC-NONVCC-REFILL                             
204600               OR DIST35-NONVCC-NONVCC-TRANSFER                           
204700                 MOVE IN34A-KDTRADP  TO EKO-FIL-IDCPYTXT(1:4)             
204800               END-IF                                                     
204900             END-IF                                                       
205000           END-IF                                                         
205100         END-IF                                                           
205200         MOVE 'EKHA'               TO EKO-FIL-IDCPYTXT(5:4)               
205300         MOVE IN34A-KDTRADP        TO EKO-EKH-KDTRADP                     
205400       END-IF                                                             
205500                                                                          
205600       PERFORM IMS-ISRT-WDR801                                            
205700       PERFORM UNTIL SEGMENT-FINNS                                        
205800         ADD +1  TO EKO-FIL-IDSEKVNR                                      
205900         PERFORM IMS-ISRT-WDR801                                          
206000       END-PERFORM                                                        
206100     END-IF                                                               
206200     .                                                                    
206300     EJECT                                                                
206400                                                                          
206500 J-CREATE-EVENT SECTION.                                                  
206600                                                                          
206700     MOVE '001'                      TO Z430-REQU-IDMSGVER                
206800     MOVE 'Discrepancy'              TO Z430-REQU-IDEVENT                 
206900     MOVE 'UPDATE'                   TO Z430-REQU-IDEVENTTYP              
207000     MOVE FUNCTION CURRENT-DATE      TO Z430-REQU-TIMESTAMP               
207100     MOVE 'WAPIDISC'                 TO Z430-REQU-IDCPYTXT                
207200     MOVE WS-IDAPIDISCREF            TO Z430-IDAPIDISCREF                 
207300     MOVE '159'                      TO Z430-IDMSG                        
207400     MOVE 'Return goods is Binned'   TO Z430-TEMFSINF                     
207500                                                                          
207600     MOVE 'WZ0430X '                 TO MSG-KDTRANS-1                     
207700     MOVE 'Z430'                     TO MSG-IDTRANS-1                     
207800     MOVE '1'                        TO MSG-KDMFSFOR-1                    
207900     MOVE 'W4183300'                 TO MSG1-MSG-KOM-IDSNDJOB             
208000     MOVE 'WZ0430I1'                 TO MSG1-MSG-KOM-IDCPYTXT             
208100                                                                          
208200*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
208300*    -- IDSNDNOD REFER AS EVE-XXXX (XXX -CUSTOMER DETAILS)                
208400*    -- IDCPYTXT REFER AS RETURN COPYBOOK                                 
208500**   MOVE 'WAPIDISC'          TO MSG1-MSG-KOM-IDCPYTXT                    
208600     STRING 'EVE' WS-EVENT-KUND WS-IDDISTR-EVENT                          
208700          DELIMITED BY SIZE INTO MSG1-MSG-KOM-IDSNDNOD                    
208800                                                                          
208900     ADD  1                    TO MSG1-MSG-KOM-TIKLOCK                    
209000     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
209100     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
209200                                                                          
209300     CALL W006KOM USING MSG-PCB                                           
209400                        0693-PCB                                          
209500                        WDP8-PCB                                          
209600                        MSG1-MSG-KOM-WMSGKOM                              
209700                        MSG-IO-AREA                                       
209800     IF MSG1-MSG-KOM-IDMFSMED NOT = SPACE                                 
209900        MOVE                                                              
210000        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
210100                                     TO FELTEXT                           
210200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
210300     END-IF                                                               
210400     .                                                                    
210500     EJECT                                                                
210600 Z-FINIT SECTION.                                                         
210700                                                                          
210800     CLOSE W418AL                                                         
210900                                                                          
211000     PERFORM S12-NOLLA-ATERSTART                                          
211100                                                                          
211200     MOVE 'S'                          TO POSTSUM-OPKOD                   
211300     CALL POSTSUM USING POSTSUM-PARM                                      
211400     .                                                                    
211500     EJECT                                                                
211600 S01-LAES-W418AL  SECTION.                                                
211700     SKIP2                                                                
211800     READ W418AL INTO IN-AREA                                             
211900     AT END                                                               
212000        SET END-OF-W418AL              TO TRUE                            
212100                                                                          
212200     NOT AT END                                                           
212300        MOVE 'W41833'                  TO POSTSUM-FDNAMN                  
212400        MOVE 'W41833D1'                TO POSTSUM-DDNAMN2                 
212500        MOVE IN-IDPTYP                 TO POSTSUM-TRANSTYP                
212600        CALL POSTSUM USING POSTSUM-PARM                                   
212700                                                                          
212800        ADD 1                          TO W-KVPOST-IN                     
212900     END-READ                                                             
213000     .                                                                    
213100     EJECT                                                                
213200 S02-LAEGG-UPP-NY-WDK7 SECTION.                                           
213300                                                                          
213400     MOVE ALL '+'      TO WDK7-W005WDK7                                   
213500     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
213600     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
213700     MOVE IN34B-IDDC   TO WDK7-IDDC-KFB                                   
213800                          WDK7-IDDC                                       
213900                                                                          
214000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                  
214100                                       WDK7-PCB                           
214200     MOVE WDK7-WDK711   TO SLAG-WDK711                                    
214300     .                                                                    
214400     EJECT                                                                
214500 S03-GET-KDVALISO SECTION.                                                
214600                                                                          
214700     MOVE IN34B-IDDISTR   TO W-IDDISTR-B2                                 
214800     MOVE IN34B-IDKUNDNR  TO W-IDKUNDNR-B2                                
214900     PERFORM IMS-GU-WDB201                                                
215000     IF SEGMENT-FINNS                                                     
215100       MOVE GMT-IDPARTNR              TO W-IDPARTNR                       
215200       MOVE GMT-IDFTG                 TO W-IDFTG                          
215300       PERFORM IMS-GU-WDB101                                              
215400     END-IF                                                               
215500     .                                                                    
215600     EJECT                                                                
215700 S11-LAS-FRAM-TILL-CHKPOINT SECTION.                                      
215800                                                                          
215900     PERFORM S01-LAES-W418AL                                              
216000                                                                          
216100     PERFORM UNTIL END-OF-W418AL OR                                       
216200                    W-KVPOST-IN = 4580-KVPOST                             
216300        PERFORM S01-LAES-W418AL                                           
216400     END-PERFORM                                                          
216500                                                                          
216600     IF END-OF-W418AL                                                     
216700        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
216800                                       TO FELTEXT-STR                     
216900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
217000     ELSE                                                                 
217100       MOVE +1 TO CHKP-ANT                                                
217200     END-IF                                                               
217300     .                                                                    
217400     EJECT                                                                
217500 S12-NOLLA-ATERSTART SECTION.                                             
217600                                                                          
217700     PERFORM IMS-LAS-ATERSTART                                            
217800                                                                          
217900     MOVE +0                           TO 4580-KVPOST                     
218000     MOVE DAGENS-DATUM                 TO 4580-TIUPPDAT                   
218100     ACCEPT 4580-TIUPPTID FROM TIME                                       
218200                                                                          
218300     PERFORM IMS-REPL-ATERSTART                                           
218400                                                                          
218500     .                                                                    
218600     EJECT                                                                
218700 S13-SKAPA-SALDOLOGG-WDL9 SECTION.                                        
218800                                                                          
218900     MOVE IN34B-IDARTNR       TO LOGG-IDARTNR                             
219000     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
219100     ACCEPT WS-KLOCKAN               FROM TIME                            
219200     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
219300     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-KLOCKAN                
219400     MOVE 9                   TO LOGG-IDSEKVNR                            
219500     MOVE 'DISC'              TO LOGG-IDHUVTYP                            
219600     MOVE 'DIS'               TO LOGG-IDSUBTYP                            
219700     MOVE 'W4183300'          TO LOGG-IDPGM                               
219800     MOVE SPACE               TO LOGG-IDTRANS                             
219900     MOVE 'W4183300'          TO LOGG-IDUSER                              
220000     MOVE SPACE               TO LOGG-REF                                 
220100     MOVE IN34B-IDDISTR       TO LOGG-IDDISTR                             
220200     MOVE IN34B-IDKUNDNR      TO LOGG-IDKUNDNR                            
220300     MOVE IN34B-IDRAPPNR      TO LOGG-IDRAPPNR                            
220400     MOVE LEV-KVLEVANM-BEKR   TO LOGG-KVART-SALDO                         
220500     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
220600     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
220700     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
220800     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
220900                                                                          
221000     PERFORM IMS-ISRT-WDL901                                              
221100     IF SEGMENT-FINNS-REDAN                                               
221200       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
221300         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
221400         PERFORM IMS-ISRT-WDL901                                          
221500       END-PERFORM                                                        
221600     END-IF                                                               
221700     .                                                                    
221800     EJECT                                                                
221900 X-TAG-CHECKPOINT   SECTION.                                              
222000                                                                          
222100***  UPPDATERA ÅTERSTARTREGISTRET                                         
222200     PERFORM IMS-LAS-ATERSTART                                            
222300                                                                          
222400     MOVE W-KVPOST-IN    TO 4580-KVPOST                                   
222500     ACCEPT 4580-TIUPPDAT FROM DATE                                       
222600     ACCEPT 4580-TIUPPTID FROM TIME                                       
222700                                                                          
222800     PERFORM IMS-REPL-ATERSTART                                           
222900                                                                          
223000     PERFORM IMS-CHECKPOINT                                               
223100     MOVE +1             TO CHKP-ANT                                      
223200     .                                                                    
223300     EJECT                                                                
223400* --- IMS SEKTIONER ---                                                   
223500                                                                          
223600     EJECT                                                                
223700 IMS-GHU-WDA201 SECTION.                                                  
223800                                                                          
223900     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
224000          DELIMITED BY SIZE INTO SSA1                                     
224100     MOVE '  GE' TO GODK-STATUSKODER                                      
224200     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA201 SSA1                   
224300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
224400     PERFORM IMS-STATUSKONTROLL                                           
224500     .                                                                    
224600     SKIP3                                                                
224700 IMS-REPL-WDA201 SECTION.                                                 
224800                                                                          
224900     MOVE '  ' TO GODK-STATUSKODER                                        
225000     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA201                       
225100     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
225200     PERFORM IMS-STATUSKONTROLL                                           
225300     .                                                                    
225400     EJECT                                                                
225500 IMS-GHU-WDA211 SECTION.                                                  
225600                                                                          
225700     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
225800          DELIMITED BY SIZE INTO SSA1                                     
225900     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
226000          DELIMITED BY SIZE INTO SSA2                                     
226100     MOVE '    ' TO GODK-STATUSKODER                                      
226200     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA211 SSA1 SSA2              
226300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
226400     PERFORM IMS-STATUSKONTROLL                                           
226500     .                                                                    
226600     SKIP3                                                                
226700 IMS-ISRT-WDA222 SECTION.                                                 
226800                                                                          
226900     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
227000         DELIMITED BY SIZE INTO SSA1                                      
227100     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
227200         DELIMITED BY SIZE INTO SSA2                                      
227300     MOVE 'WDA222 '            TO SSA3                                    
227400     MOVE '  II'           TO GODK-STATUSKODER                            
227500     CALL CBLTDLI USING ISRT WDA2-PCB DLI-IO-WDA222 SSA1 SSA2 SSA3        
227600     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
227700     PERFORM IMS-STATUSKONTROLL                                           
227800     .                                                                    
227900 IMS-REPL-WDA211 SECTION.                                                 
228000                                                                          
228100     MOVE '  ' TO GODK-STATUSKODER                                        
228200     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA211                       
228300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
228400     PERFORM IMS-STATUSKONTROLL                                           
228500     .                                                                    
228600     EJECT                                                                
228700 IMS-GU-WDK601 SECTION.                                                   
228800                                                                          
228900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
229000          DELIMITED BY SIZE INTO SSA1                                     
229100     MOVE '  GE' TO GODK-STATUSKODER                                      
229200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
229300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
229400     PERFORM IMS-STATUSKONTROLL                                           
229500     .                                                                    
229600     EJECT                                                                
229700 IMS-GNP-WDK611 SECTION.                                                  
229800                                                                          
229900     MOVE 'WDK611   ' TO SSA1                                             
230000     MOVE '    ' TO GODK-STATUSKODER                                      
230100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
230200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
230300     PERFORM IMS-STATUSKONTROLL                                           
230400     .                                                                    
230500     SKIP3                                                                
230600 IMS-GHNP-WDK611 SECTION.                                                 
230700                                                                          
230800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
230900          DELIMITED BY SIZE INTO SSA1                                     
231000     MOVE '  GE' TO GODK-STATUSKODER                                      
231100     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
231200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
231300     PERFORM IMS-STATUSKONTROLL                                           
231400     .                                                                    
231500     SKIP3                                                                
231600 IMS-REPL-WDK611 SECTION.                                                 
231700                                                                          
231800     MOVE '  ' TO GODK-STATUSKODER                                        
231900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
232000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
232100     PERFORM IMS-STATUSKONTROLL                                           
232200     .                                                                    
232300     EJECT                                                                
232400 IMS-GU-WDK711 SECTION.                                                   
232500                                                                          
232600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
232700            DELIMITED BY SIZE INTO SSA1                                   
232800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
232900            DELIMITED BY SIZE INTO SSA2                                   
233000                                                                          
233100     MOVE '  GE' TO GODK-STATUSKODER                                      
233200     CALL CBLTDLI USING                                                   
233300           GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2                            
233400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
233500     PERFORM IMS-STATUSKONTROLL                                           
233600     .                                                                    
233700     EJECT                                                                
233800 IMS-GET-HOLD-SALDON-WDK711 SECTION.                                      
233900                                                                          
234000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
234100            DELIMITED BY SIZE INTO SSA1                                   
234200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
234300            DELIMITED BY SIZE INTO SSA2                                   
234400                                                                          
234500     MOVE '  GE' TO GODK-STATUSKODER                                      
234600     CALL CBLTDLI USING                                                   
234700           GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2                           
234800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
234900     PERFORM IMS-STATUSKONTROLL                                           
235000     .                                                                    
235100     EJECT                                                                
235200 IMS-REPL-WDK711 SECTION.                                                 
235300                                                                          
235400     MOVE '  ' TO GODK-STATUSKODER                                        
235500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
235600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
235700     PERFORM IMS-STATUSKONTROLL                                           
235800     .                                                                    
235900     EJECT                                                                
236000 IMS-GHNP-WDK728-LAST SECTION.                                            
236100                                                                          
236200     STRING 'WDK728  *L(DAINLEV < ' W-DAINLEV ')'                         
236300          DELIMITED BY SIZE INTO SSA1                                     
236400     MOVE '  GE' TO GODK-STATUSKODER                                      
236500     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728 SSA1                  
236600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
236700     PERFORM IMS-STATUSKONTROLL                                           
236800     .                                                                    
236900     EJECT                                                                
237000 IMS-REPL-WDK728 SECTION.                                                 
237100                                                                          
237200     MOVE '  ' TO GODK-STATUSKODER                                        
237300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
237400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
237500     PERFORM IMS-STATUSKONTROLL                                           
237600     .                                                                    
237700     EJECT                                                                
237800 IMS-GU-WDH111 SECTION.                                                   
237900                                                                          
238000     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
238100          DELIMITED BY SIZE INTO SSA1                                     
238200     STRING  'WDH111  (WDH111KY>=' W-WDH111KY-MIN                         
238300                     '&WDH111KY<=' W-WDH111KY-MAX ')'                     
238400              DELIMITED BY SIZE INTO SSA2                                 
238500     MOVE '  GE' TO GODK-STATUSKODER                                      
238600     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-WDH111 SSA1 SSA2               
238700     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
238800     PERFORM IMS-STATUSKONTROLL                                           
238900     .                                                                    
239000     EJECT                                                                
239100 IMS-ISRT-WDH101 SECTION.                                                 
239200                                                                          
239300     MOVE 'WDH101 ' TO SSA1                                               
239400     MOVE '  II' TO GODK-STATUSKODER                                      
239500     CALL CBLTDLI USING ISRT WDH1-PCB DLI-IO-WDH101 SSA1                  
239600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
239700     PERFORM IMS-STATUSKONTROLL                                           
239800     .                                                                    
239900     EJECT                                                                
240000 IMS-ISRT-WDH111 SECTION.                                                 
240100                                                                          
240200     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
240300          DELIMITED BY SIZE INTO SSA1                                     
240400     MOVE 'WDH111 ' TO SSA2                                               
240500     MOVE '  II' TO GODK-STATUSKODER                                      
240600     CALL CBLTDLI USING ISRT WDH1-PCB DLI-IO-WDH111 SSA1 SSA2             
240700     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
240800     PERFORM IMS-STATUSKONTROLL                                           
240900     .                                                                    
241000     SKIP3                                                                
241100 IMS-ISRT-WDH121 SECTION.                                                 
241200                                                                          
241300     MOVE 'WDH121 ' TO SSA1                                               
241400     MOVE '  II' TO GODK-STATUSKODER                                      
241500     CALL CBLTDLI USING ISRT WDH1-PCB DLI-IO-WDH121 SSA1                  
241600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
241700     PERFORM IMS-STATUSKONTROLL                                           
241800     .                                                                    
241900     SKIP3                                                                
242000 IMS-ISRT-WDL901 SECTION.                                                 
242100                                                                          
242200     MOVE 'WDL901 ' TO SSA1                                               
242300     MOVE '  II' TO GODK-STATUSKODER                                      
242400     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
242500     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
242600     PERFORM IMS-STATUSKONTROLL                                           
242700     .                                                                    
242800     EJECT                                                                
242900 IMS-ISRT-WDR901 SECTION.                                                 
243000                                                                          
243100     MOVE 'WDR901 ' TO SSA1                                               
243200     MOVE '  II' TO GODK-STATUSKODER                                      
243300     CALL CBLTDLI USING ISRT WDR9-PCB DLI-IO-WDR901 SSA1                  
243400     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
243500     PERFORM IMS-STATUSKONTROLL                                           
243600     .                                                                    
243700     EJECT                                                                
243800 IMS-ISRT-WDR801 SECTION.                                                 
243900                                                                          
244000     MOVE 'WDR801 ' TO SSA1                                               
244100     MOVE '  II' TO GODK-STATUSKODER                                      
244200     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
244300     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
244400     PERFORM IMS-STATUSKONTROLL                                           
244500     .                                                                    
244600     EJECT                                                                
244700 IMS-GU-WDB101  SECTION.                                                  
244800                                                                          
244900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
245000          DELIMITED BY SIZE INTO SSA1                                     
245100     MOVE '    ' TO GODK-STATUSKODER                                      
245200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
245300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
245400     PERFORM IMS-STATUSKONTROLL                                           
245500     .                                                                    
245600     SKIP3                                                                
245700 IMS-GU-WDB201  SECTION.                                                  
245800                                                                          
245900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
246000          DELIMITED BY SIZE INTO SSA1                                     
246100     MOVE '    ' TO GODK-STATUSKODER                                      
246200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
246300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
246400     PERFORM IMS-STATUSKONTROLL                                           
246500     .                                                                    
246600     SKIP3                                                                
246700 IMS-GHU-WDGX4103 SECTION.                                                
246800                                                                          
246900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
247000          DELIMITED BY SIZE INTO SSA1                                     
247100     MOVE '  GE' TO GODK-STATUSKODER                                      
247200     CALL CBLTDLI USING GHU 4103-PCB DLI-IO-WDGX4103 SSA1                 
247300     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
247400     PERFORM IMS-STATUSKONTROLL                                           
247500     .                                                                    
247600     SKIP3                                                                
247700 IMS-DLET-WDGX4103 SECTION.                                               
247800                                                                          
247900     MOVE '  ' TO GODK-STATUSKODER                                        
248000     CALL CBLTDLI USING DLET 4103-PCB DLI-IO-WDGX4103                     
248100     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
248200     PERFORM IMS-STATUSKONTROLL                                           
248300     .                                                                    
248400     EJECT                                                                
248500 IMS-LAS-ATERSTART SECTION.                                               
248600                                                                          
248700     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
248800                    DELIMITED BY SIZE INTO SSA1                           
248900     MOVE 'WDR470 '      TO SSA2                                          
249000     MOVE '  '           TO GODK-STATUSKODER                              
249100     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
249200     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
249300     PERFORM IMS-STATUSKONTROLL                                           
249400     .                                                                    
249500     SKIP2                                                                
249600 IMS-REPL-ATERSTART SECTION.                                              
249700                                                                          
249800     MOVE '  '             TO GODK-STATUSKODER                            
249900     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
250000     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
250100     PERFORM IMS-STATUSKONTROLL                                           
250200     .                                                                    
250300     EJECT                                                                
250400 IMS-RESTART SECTION.                                                     
250500     SKIP2                                                                
250600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
250700     MOVE '  ' TO GODK-STATUSKODER                                        
250800     CALL CBLTDLI USING XRST MSG-PCB                                      
250900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
251000                        CHKP-AREA-LENGTH CHKP-AREA                        
251100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
251200     PERFORM IMS-STATUSKONTROLL                                           
251300     .                                                                    
251400     SKIP3                                                                
251500 IMS-CHECKPOINT SECTION.                                                  
251600     SKIP2                                                                
251700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
251800     MOVE '  XD' TO GODK-STATUSKODER                                      
251900     CALL CBLTDLI USING CHKP MSG-PCB                                      
252000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
252100                        CHKP-AREA-LENGTH CHKP-AREA                        
252200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
252300     PERFORM IMS-STATUSKONTROLL                                           
252400                                                                          
252500     IF IMS-EJ-OK                                                         
252600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
252700       DISPLAY FELTEXT                                                    
252800       CALL FELLOG                                                        
252900     END-IF                                                               
253000     .                                                                    
253100     EJECT                                                                
253200 IMS-STATUSKONTROLL SECTION.                                              
253300     SKIP2                                                                
253400     SET STATUS-IX TO 1                                                   
253500     SEARCH GODK-STATUS                                                   
253600       AT END                                                             
253700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
253800           DELIMITED BY SIZE INTO FELTEXT-STR                             
253900         DISPLAY FELTEXT                                                  
254000         CALL FELLOG                                                      
254100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
254200         CONTINUE                                                         
254300     END-SEARCH                                                           
254400     .                                                                    
