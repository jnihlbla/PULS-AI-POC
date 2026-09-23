000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL016100.                                                
000300 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000400 DATE-WRITTEN.   2004/10/05.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR DETALJUPPGIFTER FÖR EN LEVERANANSMÄRKNINGSRAD              
000900*        GÖR KONTROLLER.                                                  
001000*        UPPDATERING AV BEHANDLINGSKOD.                                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLKREE                                     
001300*        PROGRAMMET UPPDATERAR WL4109  WDR1 ANALYSNUMMER                  
001400*        PROGRAM 418ANSV LÄSER WL4113  WDR1 ADM-ANSVARIG                  
001500*                              WL4115  WDR1 RET-ANSVARIG                  
001600*                              WL4117  WDR1 REM-ANSVARIG                  
001700*        PROGRAMMET LÄSER      WDL5                                       
001800*        PROGRAMMET LÄSER      WLARTC                                     
001900*        PROGRAMMET LÄSER      WLBENA                                     
002000*                                                                         
002100*        WL016100 PROGRAM IS A REPLICA OF W4072200 PROGRAM                
002200*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002300*                                                                         
002400*    ADDRESS: 'CARPARTS.LDC.DISCRLINEDETAIL'                              
002500*                                                                         
002600*    E-TRACKER 8635407       20091021 RETURN CODES MATRIX                 
002700*    E-TRACKER 10143271      2011-09  CHINA WAREHOUSE PROJECT-1           
002800*    E-TRACKER 10296404      2017-01  RETURNS FROM CA TO US               
002900*    E-TRACKER 10302968      2017-07  GENERIC SOLUTION IDFTG              
003000*                                                                         
003100*                                                                         
003200*    INDATA.                                                              
003300*        TRANSAKTION: WL0161U                                             
003400*        REQUEST:     WZ01REQU                                            
003500*                     WL0161I1                                            
003600*                                                                         
003700*    UTDATA.                                                              
003800*        RESPONSE:    WZ01RESP                                            
003900*                     WL016101                                            
004000*                     WL016110                                            
004100                                                                          
004200                                                                          
004300 ENVIRONMENT DIVISION.                                                    
004400                                                                          
004500 DATA DIVISION.                                                           
004600                                                                          
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900*    -- CHECKED BY WY2000                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(08)   VALUE 'WL016100'.            
005200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005300 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
005400                                                                          
005500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005700                                                                          
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  YES                         PIC X       VALUE 'Y'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
006300 77  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
006400 77  WS-CP-UTF8                  PIC X(4)   VALUE 'UTF8'.                 
006500 77  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
006601 77  WS-PRKURS                   PIC S9(2)V9(5) VALUE +0   COMP-3.        
006701 77  WS-PRARTBTO-CONV            PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006702 77  SW-KDKREBEH-Y               PIC X       VALUE 'N'.                   
006703 77  WS-KDVALISO-SPAR            PIC X(3)   VALUE SPACE.                  
006704                                                                          
006705 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
006800 77  MEAN-IX                     PIC S9(4)   VALUE +0  COMP SYNC.         
006900                                                                          
007000 77  VKTARA                      PIC S9(6)V9 VALUE +0  COMP-3.            
007100 77  WS-VKTARA                   PIC S9(6)V9 VALUE +0  COMP-3.            
007200 77  VKORDBTO-KOLLI              PIC S9(9)V9 VALUE +0  COMP-3.            
007300 77  VKORDNTO-KOLLI              PIC S9(9)V9 VALUE +0  COMP-3.            
007400 77  W-KVPB-TOT                  PIC S9(6)V9 VALUE ZERO COMP-3.           
007701 77  W-DATE-AAMM                 PIC  9(4)   VALUE ZERO.                  
007702 77  WS-VKORDNTO-DIFF            PIC S9(9)V9 VALUE +0  COMP-3.            
007703 77  WS-VKORDNTO-KOLLI-TOT       PIC S9(9)V9 VALUE +0  COMP-3.            
007704 77  WS-VLORDNTO-KOLLI-TOT       PIC S9(8)V9(3) VALUE +0 COMP-3.          
007800 77  WS-VLORDNTO-KOLLI           PIC S9(4)V9(3) VALUE +0 COMP-3.          
007900 77  WS-VKORDBTO                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
008000 77  WS-VKORDNTO                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
008100 77  WS-KVLEVART                 PIC S9(5)   VALUE +0  COMP-3.            
008200 77  WS-KVOKS-TOT-CDC            PIC S9(9)   VALUE +0  COMP-3.            
008300 77  WS-KVDISP                   PIC S9(9)   VALUE +0  COMP-3.            
008400 77  WS-IDFAKT                   PIC S9(7)   COMP-3 VALUE ZERO.           
008500 77  WS-IDKOLLI                  PIC S9(5)   VALUE ZERO COMP-3.           
008600 77  WS-IDKUNDRF                 PIC X(10).                               
008700 77  WS-KVLEVANM-TOT             PIC S9(7)   VALUE ZERO COMP-3.           
008800 77  WS-DATUM                    PIC 9(6).                                
008900                                                                          
009000 77  WS-IDANSK                   PIC  9(3)      VALUE ZERO.               
009100 77  WS-VKART                    PIC S9(7)      VALUE ZERO.               
009200 77  WS-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO.               
009601                                                                          
009701 77  WS-FLAUTREM-FLAG            PIC X.                                   
009801     88  WS-FLAUTREM-Y                       VALUE 'J'.                   
009901     88  WS-FLAUTREM-N                       VALUE 'N'.                   
009902                                                                          
009903 77  OBEH-RADER-FINNS-SW         PIC X.                                   
009904   88  OBEH-RADER-FINNS                     VALUE 'J'.                    
009905                                                                          
009906 77  OBEH-REMISS-FINNS-SW        PIC X.                                   
009907   88  OBEH-REMISS-FINNS                    VALUE 'J'.                    
009908                                                                          
010000 77  BEH-RADER-FINNS-SW          PIC X.                                   
010100   88  BEH-RADER-FINNS                      VALUE 'J'.                    
010200                                                                          
010300 77  AVVISADE-RADER-SW           PIC X.                                   
010400   88  AVVISADE-RADER-FINNS                 VALUE 'J'.                    
010500                                                                          
010600 77  GODKAENDA-RADER-SW          PIC X.                                   
010700   88  GODKAENDA-RADER-FINNS                VALUE 'J'.                    
010800                                                                          
010900 77  WS-KDANMORS                 PIC X(2).                                
011000   88  WS-ANTAL-SKALL-FINNAS                 VALUE '00'                   
011100                                                   '20'                   
011200                                                   '30'                   
011300                                                   '31'                   
011400                                                   '42'                   
011500                                                   '43'                   
011600                                                   '72'                   
011700                                                   '73'                   
011800                                                   '84'.                  
011900                                                                          
012000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012100     88  INDATA-OK                           VALUE 'J'.                   
012200     88  INDATA-FEL                          VALUE 'N'.                   
012300                                                                          
012400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012500     88  NYCKLAR-OK                          VALUE 'J'.                   
012600     88  NYCKLAR-FEL                         VALUE 'N'.                   
012700                                                                          
012800 77  WDB6-SW                     PIC X       VALUE 'J'.                   
012900     88  WDB6-FINNS                          VALUE 'J'.                   
013000     88  WDB6-SAKNAS                         VALUE 'N'.                   
013100                                                                          
013200 01  TEST-KDKREBEH.                                                       
013300     03  KDKREBEH-1              PIC X(1)   VALUE SPACE.                  
013400     03  KDKREBEH-2              PIC X(1)   VALUE SPACE.                  
013500     03  KDKREBEH-3              PIC X(1)   VALUE SPACE.                  
013600                                                                          
013700     EJECT                                                                
013800*                                                                         
013900*01    -COPY WWDCLAND                                                     
014000                                                                          
014100       EJECT                                                              
014200 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
014300*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
014400*                                                                         
014500     EJECT                                                                
014600                                                                          
014700*01  -COPY WWIDFTG                                                        
014800                                                                          
014900     EJECT                                                                
015000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015300     EJECT                                                                
015400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015500 01  GENERELLA-SUBPROGRAM.                                                
015600     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
015700     03  W418MEAN                PIC X(8)    VALUE 'W418MEAN'.            
015800     03  WL016110                PIC X(8)    VALUE 'WL016110'.            
015900     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
016000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
016400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
016500     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
017301     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
017401     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
017402     EJECT                                                                
017403*    --- PARAMETRAR TILL SUBPROGRAM W418ANSV                              
017404 01  FILLER                      PIC X(16)   VALUE 'W418ANSV'.            
017405*01 -COPY W418ANSV                                                        
017406     EJECT                                                                
017407*    --- PARAMETRAR TILL SUBPROGRAM W418MEAN                              
017408 01  FILLER                      PIC X(16)   VALUE 'W418MEAN'.            
017409*01 -COPY W418MEAN                                                        
017410     EJECT                                                                
017500*    --- PARAMETRAR TILL SUBPROGRAM WL016110                              
017600 01  FILLER                      PIC X(16)   VALUE 'WL016110'.            
017700*01 -COPY WL016110                                                        
017800     EJECT                                                                
017900*    ---  LÄNKAREA TILL W418OKOD                                          
018000 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
018100*01 -COPY W418OKOD           -PRE OKOD-.                                  
019101     EJECT                                                                
019201 01  FILLER                      PIC X(16)   VALUE 'W335CURR'.            
019301*01 -COPY W335CURR                                                        
019401     EJECT                                                                
019501*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
019601 01  FILLER                      PIC X(16)   VALUE 'W510CURR'.            
019701*01 -COPY W510CURR                                                        
019702     EJECT                                                                
019703*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
019704*01 -COPY WWOMVAND                                                        
019705     SKIP3                                                                
019706     EJECT                                                                
019707*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019708*                                                                         
019709 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
019710*01  -COPY WTRAUTF8                                                       
019711     EJECT                                                                
019712     EJECT                                                                
019713 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB '.            
019714*01  -COPY WZ01SUB                                                        
019715     EJECT                                                                
019716 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
019717*01  -COPY WZ01SEND                                                       
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
020000 01  REQU-AREA.                                                           
020100*    03  -COPY WZ01REQU                                                   
020200*    03  -COPY WL0161I1                                                   
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
020500 01  RESP-AREA.                                                           
020600*    03  -COPY WZ01RESP                                                   
020700*    03  -COPY WL0161O1                                                   
020800     EJECT                                                                
020900 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
021000                                                                          
021100 01  ALT-MSG-IO-AREA.                                                     
021200   03 ALT-LL                     PIC S9(4)  VALUE +17  COMP SYNC.         
021300   03 ALT-Z1                     PIC X.                                   
021400   03 ALT-Z2                     PIC X.                                   
021500   03 ALT-TRANSKOD               PIC X(8)   VALUE 'W4T721  '.             
021600   03 ALT-IDTRANS                PIC X(4)   VALUE '4722'.                 
021700   03 ALT-SPRAK                  PIC X.                                   
021800     EJECT                                                                
021900                                                                          
022000 01  ERR-MESSAGE-CODES.                                                   
022100   03 ERR-NOT-AUTHORIZED         PIC X(3)   VALUE '00A'.                  
022200                                                                          
022300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022400*                                                                         
022500     EJECT                                                                
022600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022700                                                                          
022800                                                                          
022900 01  NYCKLAR-TILL-DLI.                                                    
023000                                                                          
023100     03  W-IDLEVANM-X.                                                    
023200         05  W-IDDISTR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
023300         05  W-IDKUNDNR-A2       PIC S9(7)   COMP-3 VALUE ZERO.           
023400         05  W-IDRAPPNR-A2       PIC  X(7)          VALUE ZERO.           
023500                                                                          
023600     03  W-WDA211KY-X.                                                    
023700         05  W-IDARTNR-A2        PIC S9(9)   COMP-3 VALUE ZERO.           
023800         05  W-IDRADNR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
023900                                                                          
024000     03  W-WDA2DSEQ-MIN-X.                                                
024100         05  W-IDFTG-DSEQ-FOM    PIC  X(2)   VALUE SPACE.                 
024200         05  W-IDARTNR-DSEQ-FOM  PIC S9(9)   COMP-3 VALUE ZERO.           
024300         05  W-IDDISTR-DSEQ-FOM  PIC S9(5)   COMP-3 VALUE ZERO.           
024400         05  W-IDKUNDNR-DSEQ-FOM PIC S9(7)   COMP-3 VALUE ZERO.           
024500         05  FILLER              PIC X(10)     VALUE LOW-VALUE.           
024600                                                                          
024700     03  W-WDA2DSEQ-MAX-X.                                                
024800         05  W-IDFTG-DSEQ-TOM    PIC  X(2)   VALUE SPACE.                 
024900         05  W-IDARTNR-DSEQ-TOM  PIC S9(9)   COMP-3 VALUE ZERO.           
025000         05  W-IDDISTR-DSEQ-TOM  PIC S9(5)   COMP-3 VALUE ZERO.           
025100         05  W-IDKUNDNR-DSEQ-TOM PIC S9(7)   COMP-3 VALUE ZERO.           
025200         05  FILLER              PIC X(10)    VALUE HIGH-VALUE.           
025300                                                                          
025400     03  W-IDFAKT-X.                                                      
025500         05 W-IDFAKT-L5          PIC S9(7)   COMP-3 VALUE ZERO.           
025600                                                                          
025700     03  W-IDGMTREF-X.                                                    
025800         05 W-IDDISTR-L5         PIC S9(5)   COMP-3 VALUE ZERO.           
025900         05 W-IDKUNDNR-L5        PIC S9(7)   COMP-3 VALUE ZERO.           
026000         05 W-IDKUNDRF-L5        PIC X(10).                               
027000                                                                          
027100     03  W-IDARTNR-L5-X.                                                  
027200         05 W-IDARTNR-L5         PIC S9(9)   VALUE ZERO  COMP-3.          
027300                                                                          
027400     03  W-WDL511KY-X.                                                    
027500         05  W-IDPRODNR-L5       PIC S9(7)   VALUE ZERO  COMP-3.          
027600         05  W-IDKOLLI-L5-X.                                              
027700           07 W-IDKOLLI-L5       PIC S9(5)   VALUE ZERO  COMP-3.          
027800                                                                          
027900     03  W-IDARTNR-X.                                                     
028000         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
028100                                                                          
028200     03  W-IDDC-X.                                                        
028300         05  W-IDDC              PIC X(2).                                
028400                                                                          
028500     03  W-IDLAND-X.                                                      
028600         05  W-IDLAND            PIC X(2).                                
028700                                                                          
028800     03  W-IDDC-B6-X.                                                     
028900         05 W-IDDC-B6            PIC X(2).                                
029000                                                                          
029400     03  W-KDSEGKEY-X.                                                    
029500         05  W-KDSEGKEY          PIC  X(1)          VALUE '1'.            
029600                                                                          
029700     03  W-KDKOLLI-X.                                                     
029800         05  W-KDKOLLI           PIC  X(8).                               
029900                                                                          
030000     03  W-IDSKYLT-X.                                                     
030100         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
030200                                                                          
030300     03  W-KDARBTYP-X.                                                    
030400         05  W-KDARBTYP          PIC  X(8).                               
030500                                                                          
030600     03 W-4109-X.                                                         
030700         05 FILLER               PIC  X(4)   VALUE '4109'.                
030800         05 W-IDFTG              PIC  X(2)   VALUE SPACE.                 
030900         05 FILLER               PIC  X(24)  VALUE LOW-VALUE.             
031000                                                                          
031100     03 W-WDGXKEY-MIN-X.                                                  
031200         05 W-IDARTNR-4110-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
031300         05 W-KDANMORS-4110-MIN  PIC X(2)    VALUE SPACE.                 
031400         05 FILLER               PIC X(8)    VALUE LOW-VALUE.             
031500                                                                          
031600     03 W-WDGXKEY-MAX-X.                                                  
031700         05 W-IDARTNR-4110-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
031800         05 W-KDANMORS-4110-MAX  PIC X(2)    VALUE SPACE.                 
031900         05 FILLER               PIC X(8)    VALUE HIGH-VALUE.            
032000                                                                          
032100     03  W-IDPERSON-X.                                                    
032200         05  W-IDPERSON          PIC S9(3)   COMP-3 VALUE ZERO.           
032300                                                                          
032400*    -NYCKLAR TIL WDB201                                                  
032500     03  W-IDGMT-X.                                                       
032600       05  W-IDDISTR-WDB2        PIC S9(5) VALUE ZERO COMP-3.             
032700       05  W-IDKUNDNR-WDB2       PIC S9(7) VALUE ZERO COMP-3.             
032800     03  W-IDGMT-MIN-X.                                                   
032900       05  W-IDDISTR-WDB2-MIN    PIC S9(5) VALUE ZERO COMP-3.             
033000       05  W-IDKUNDNR-WDB2-MIN   PIC S9(7) VALUE ZERO COMP-3.             
033100     03  W-IDGMT-MAX-X.                                                   
033200       05  W-IDDISTR-WDB2-MAX    PIC S9(5) VALUE ZERO COMP-3.             
033300       05  W-IDKUNDNR-WDB2-MAX   PIC S9(7) VALUE ZERO COMP-3.             
033400                                                                          
033500* TILL WDB101                                                             
033600     03  W-WDB101KY-X.                                                    
033700       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
033800       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
033900*                                                                         
034000                                                                          
034100*    --- STATUS-KOD FRÅN IMS                                              
034200 01  STATUS-WS                   PIC XX.                                  
034300     88  STATUS-OK                           VALUE '  '.                  
034400     88  SEGMENT-FINNS                       VALUE '  '.                  
034500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
034600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
034800                                                                          
034900 01  GODK-STATUSKODER.                                                    
035000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035100                                                                          
035200 01  SSA1                        PIC X(192).                              
035300 01  SSA2                        PIC X(64).                               
035400 01  SSA3                        PIC X(64).                               
035500     EJECT                                                                
035600*    --- IMS FUNKTIONSKODER                                               
035700*01  -COPY W0003                                                          
035800     EJECT                                                                
035900*    ---  DLI INPUT-OUTPUT AREA                                           
036000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
036100                                                                          
036200 01  DLI-IO-AREA.                                                         
036300     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
036400     03  WLKREE01 REDEFINES IO-AREA.                                      
036500*        05  -COPY WDA201                                                 
036600     03  WLKREE11 REDEFINES IO-AREA.                                      
036700*        05  -COPY WDA211                                                 
036800     EJECT                                                                
036900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
037000 01  DLI-IO-AREA2.                                                        
037100     03  IO-AREA2                PIC X(1200) VALUE SPACE.                 
037200     03  WLKREE21 REDEFINES IO-AREA2.                                     
037300*        05  -COPY WDA221                                                 
037400     EJECT                                                                
037500     03  WLKREI01 REDEFINES IO-AREA2.                                     
037600*        05  -COPY WDA2D1                                                 
037700     EJECT                                                                
038100     03  WLARTC01 REDEFINES IO-AREA2.                                     
038200*        05  -COPY WDK601                                                 
038300     EJECT                                                                
038400     03  WLEMBB01 REDEFINES IO-AREA2.                                     
038500*        05  -COPY WDK501                                                 
038600     EJECT                                                                
038700     03  WLARTC11 REDEFINES IO-AREA2.                                     
038800*        05  -COPY WDK611                                                 
038900     EJECT                                                                
039000     03  WLARTM01 REDEFINES IO-AREA2.                                     
039100*        05  -COPY WDK901                                                 
039200     EJECT                                                                
039300     03  WLBENA11 REDEFINES IO-AREA2.                                     
039400*        05  -COPY WDD311                                                 
039500     EJECT                                                                
039600 01  FILLER                      PIC X(20) VALUE 'DLI-IO-WDL501'.         
039700                                                                          
039800 01  DLI-IO-WDL501.                                                       
039900*    03  -COPY WDL501                                                     
040000     EJECT                                                                
040100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL511'.         
040200 01  DLI-IO-WDL511.                                                       
040300*    03  -COPY WDL511                                                     
040400     EJECT                                                                
040500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL521'.         
040600 01  DLI-IO-WDL521.                                                       
040700*    03  -COPY WDL521                                                     
040800     EJECT                                                                
040900 01  FILLER                      PIC X(20) VALUE                          
041000                                          'DLI-IO-AREA-K711'.             
041100 01  DLI-IO-AREA-K711.                                                    
041200*    03  -COPY WDK711                                                     
041300     EJECT                                                                
041400 01  FILLER                      PIC X(20) VALUE                          
041500                                          'DLI-IO-AREA-K712'.             
041600 01  DLI-IO-AREA-K712.                                                    
041700*    03  -COPY WDK712                                                     
041800     EJECT                                                                
041900 01  FILLER                      PIC X(20) VALUE                          
042000                                          'DLI-IO-AREA-K722'.             
042100 01  DLI-IO-AREA-K722.                                                    
042200*    03  -COPY WDK722                                                     
042300     EJECT                                                                
042400 01  FILLER                      PIC X(20) VALUE                          
042500                                          'DLI-IO-AREA-4109'.             
042600 01  DLI-IO-AREA-4109.                                                    
042700     03  WL410901.                                                        
042800*        05  -COPY WDGX4109                                               
042900     EJECT                                                                
043000 01  FILLER                      PIC X(20)  VALUE                         
043100                                          'DLI-IO-AREA-4110'.             
043200 01  DLI-IO-AREA-4110.                                                    
043300     03  WL410911.                                                        
043400*        05  -COPY WDGX4110                                               
043500     EJECT                                                                
043600 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4113'.            
043700                                                                          
043800 01  DLI-IO-AREA-4113.                                                    
043900     03  WL411301.                                                        
044000*        05  -COPY WDGX4113                                               
044100     EJECT                                                                
044200*                                                                         
044300 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4114'.            
044400                                                                          
044500 01  DLI-IO-AREA-4114.                                                    
044600     03  WL411311.                                                        
044700*        05  -COPY WDGX4114                                               
044800     EJECT                                                                
044900*                                                                         
045000 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4115'.            
045100                                                                          
045200 01  DLI-IO-AREA-4115.                                                    
045300     03  WL411501.                                                        
045400*        05  -COPY WDGX4115                                               
045500     EJECT                                                                
045600*                                                                         
045700 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4116'.            
045800                                                                          
045900 01  DLI-IO-AREA-4116.                                                    
046000     03  WL411511.                                                        
046100*        05  -COPY WDGX4116                                               
046200     EJECT                                                                
046300*                                                                         
046400 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-P311'.            
046500                                                                          
046600 01  DLI-IO-AREA-P311.                                                    
046700*    03  -COPY WDP311                                                     
046800                                                                          
046900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
047000 01  DLI-IO-WDB201.                                                       
047100*     03  -COPY WDB201.                                                   
047200     EJECT                                                                
047300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
047400 01  DLI-IO-WDB101.                                                       
047500*     03  -COPY WDB101.                                                   
047600                                                                          
047700 01  FILLER                    PIC X(16)   VALUE 'WDB601 AREA'.           
047800 01   DLI-IO-AREA-B601.                                                   
047900*     03  -COPY WDB601                                                    
048000     EJECT                                                                
048100 LINKAGE SECTION.                                                         
048200*01  -COPY W0009   -PRE MSG-                                              
048300     EJECT                                                                
048400 01  DISTRDOC-PCB                PIC X.                                   
048500     EJECT                                                                
048600*01  -COPY W0009   -PRE MAIL-                                             
048700     EJECT                                                                
048800     EJECT                                                                
048900*01  -COPY W0008   -PRE KREE-                                             
049000     05  FILLER                  PIC X.                                   
049100     EJECT                                                                
049200*01  -COPY W0008   -PRE WDL5-                                             
049300     05  FILLER                  PIC X.                                   
049400     EJECT                                                                
049500*01  -COPY W0008   -PRE ARTC-                                             
049600     05  FILLER                  PIC X.                                   
049700     EJECT                                                                
049800*01  -COPY W0008   -PRE ARTM-                                             
049900     05  FILLER                  PIC X.                                   
050000     EJECT                                                                
050100*01  -COPY W0008   -PRE WDK7-                                             
050200     05  FILLER                  PIC X.                                   
050300     EJECT                                                                
050400*01  -COPY W0008   -PRE BENA-                                             
050500     05  FILLER                  PIC X.                                   
050600     EJECT                                                                
050700*01  -COPY W0008   -PRE 4113-                                             
050800     05  FILLER                  PIC X.                                   
050900     EJECT                                                                
051000*01  -COPY W0008   -PRE WDP3-                                             
051100     05  FILLER                  PIC X.                                   
051200     EJECT                                                                
051300*01  -COPY W0008   -PRE KREI-                                             
051400     05  FILLER                  PIC X.                                   
051500     EJECT                                                                
051600*01  -COPY W0008   -PRE EMBB-                                             
051700     05  FILLER                  PIC X.                                   
051800     EJECT                                                                
051900*01  -COPY W0008   -PRE 4115-                                             
052000     05  FILLER                  PIC X.                                   
052100     EJECT                                                                
052200*01  -COPY W0008   -PRE 4117-                                             
052300     05  FILLER                  PIC X.                                   
052400     EJECT                                                                
052500*01  -COPY W0008      -PRE WDB1-                                          
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800*01  -COPY W0008      -PRE WDB2-                                          
052900     05  FILLER                  PIC X.                                   
053000     EJECT                                                                
053100*01  -COPY W0008      -PRE WDB6-                                          
053200     05  FILLER                  PIC X.                                   
053300     EJECT                                                                
053401*01  -COPY W0008      -PRE WDG2-                                          
053501     05  FILLER                  PIC X.                                   
053601     EJECT                                                                
053602 PROCEDURE DIVISION  USING MSG-PCB  DISTRDOC-PCB MAIL-PCB                 
053603                           KREE-PCB WDL5-PCB ARTC-PCB                     
053604                           ARTM-PCB                                       
053700                           WDK7-PCB                                       
053800                           BENA-PCB                                       
053900                           4113-PCB                                       
054000                           WDP3-PCB                                       
054100                           KREI-PCB                                       
054200                           EMBB-PCB                                       
054300                           4115-PCB                                       
054400                           4117-PCB                                       
054500                           WDB1-PCB                                       
054600                           WDB2-PCB                                       
055001                           WDB6-PCB                                       
055101                           WDG2-PCB.                                      
055102 MAIN SECTION.                                                            
055103     ENTRY 'DLITCBL' USING MSG-PCB  DISTRDOC-PCB MAIL-PCB                 
055104                           KREE-PCB WDL5-PCB ARTC-PCB                     
055105                           ARTM-PCB                                       
055200                           WDK7-PCB                                       
055300                           BENA-PCB                                       
055400                           4113-PCB                                       
055500                           WDP3-PCB                                       
055600                           KREI-PCB                                       
055700                           EMBB-PCB                                       
055800                           4115-PCB                                       
055900                           4117-PCB                                       
056000                           WDB1-PCB                                       
056100                           WDB2-PCB                                       
056601                           WDB6-PCB                                       
056701                           WDG2-PCB.                                      
056702                                                                          
056703     PERFORM S17-FETCH-REQUEST-ARGUMENT                                   
056704     IF SUB-KDRC = 0                                                      
056705                                                                          
056706       PERFORM A-INIT                                                     
056800       PERFORM B-KOLLA-NYCKLAR                                            
056900       IF NYCKLAR-OK                                                      
057000         IF REQU-KDPGMACT = 'E' OR 'S'                                    
057100           IF REQU-KDPGMACT = 'E'                                         
057200             PERFORM G-KOLLA-INPUT                                        
057300             IF INDATA-OK                                                 
057400                PERFORM H-UPPDATERA                                       
057500             END-IF                                                       
057600           END-IF                                                         
057700           PERFORM F-LAES-VISA-INFO                                       
057800         END-IF                                                           
057900         IF REQU-KDPGMACT = 'P'                                           
058000            PERFORM I-SKRIV-DETALJLISTA                                   
058100         END-IF                                                           
058200       END-IF                                                             
058300                                                                          
058400       IF REQU-KDPGMACT = 'E' OR 'S'                                      
058500       OR RESP-IDMSG-ERROR NOT = SPACE                                    
058600         PERFORM S18-RETURN-RESPONSE                                      
058700       END-IF                                                             
058800                                                                          
058900     END-IF                                                               
059000     MOVE ZERO                  TO RETURN-CODE                            
059100     GOBACK                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 A-INIT                         SECTION.                                  
059500                                                                          
059600     MOVE ALL '+'    TO RESP-AREA                                         
059700     MOVE SPACE      TO RESP-IDMSG-INFO                                   
059800                        RESP-IDMSG-ERROR                                  
059900                        RESP-IDELMT-ERROR                                 
060000     MOVE 001        TO RESP-IDMSGVER                                     
060100*    -- BEART SKA VARA SPACE I UNICODE                                    
060200     MOVE ALL X'20'  TO RESP-BEART                                        
060300                                                                          
060400                                                                          
060500     ACCEPT WS-DATUM            FROM DATE                                 
060600                                                                          
060700     MOVE JA                    TO INDATA-SW                              
060800                                                                          
060900                                                                          
061000     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
061100                                                                          
061200     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
061300                                                                          
061400     MOVE +1                    TO INDX                                   
061500     PERFORM UNTIL INDX      >  13                                        
061600        MOVE ZERO               TO MEAN-IDDISTR  (INDX)                   
061700                                   MEAN-IDKUNDNR (INDX)                   
061800                                   MEAN-IDRAPPNR (INDX)                   
061900                                   MEAN-IDARTNR  (INDX)                   
062000                                   MEAN-IDRADNR  (INDX)                   
062100        MOVE SPACE              TO MEAN-KDKREBEH (INDX)                   
062200        MOVE SPACE              TO MEAN-TEANMNOT-ADM-GRP (INDX)           
062300                                   MEAN-TEANMNOT-REM-GRP (INDX)           
062400        ADD +1                  TO INDX                                   
062500     END-PERFORM                                                          
062600                                                                          
062700*    -- TO GET A VALID OUTPUT VALUE IF NOT KDPGMACT=S                     
062800     MOVE REQU-IDDC-KEY         TO RESP-IDDC                              
062900                                   W-IDDC-B6                              
063000     PERFORM IMS-GU-WDB601                                                
063100     .                                                                    
063200     EJECT                                                                
063300 B-KOLLA-NYCKLAR                SECTION.                                  
063400                                                                          
063500                                                                          
063600     MOVE 'GB '                 TO W-IDSKYLT                              
063700     MOVE JA                    TO NYCKLAR-SW                             
063800                                                                          
063900     PERFORM BA-KOLLA-IDDISTR                                             
064000     PERFORM BB-KOLLA-IDKUNDNR                                            
064100     PERFORM BC-KOLLA-IDRAPPNR                                            
064200     PERFORM BD-KOLLA-IDARTNR                                             
064300     PERFORM BE-KOLLA-IDRADNR                                             
064400     PERFORM BF-KOLLA-IDFTG                                               
064500                                                                          
064600     .                                                                    
064700     EJECT                                                                
064800 BA-KOLLA-IDDISTR               SECTION.                                  
064900                                                                          
065000                                                                          
065100     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
065200       MOVE REQU-IDDISTR-KEY    TO W-IDDISTR-A2                           
065300                                   RESP-IDDISTR-KEY                       
065400                                   W-IDDISTR-WDB2                         
065500                                   TEST-IDDISTR                           
065600     ELSE                                                                 
065700       MOVE NEJ                 TO NYCKLAR-SW                             
065800       IF REQU-IDDISTR-KEY = ZERO                                         
065900          MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                   
066000          MOVE '126'               TO RESP-IDMSG-ERROR                    
066100       ELSE                                                               
066200          MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                   
066300          MOVE '024'               TO RESP-IDMSG-ERROR                    
066400       END-IF                                                             
066500     END-IF                                                               
066600                                                                          
066700     .                                                                    
066800     EJECT                                                                
066900                                                                          
067000 BB-KOLLA-IDKUNDNR              SECTION.                                  
067100                                                                          
067200                                                                          
067300     IF REQU-IDKUNDNR-KEY       NUMERIC                                   
067400       MOVE REQU-IDKUNDNR-KEY   TO W-IDKUNDNR-A2                          
067500                                   RESP-IDKUNDNR-KEY                      
067600                                   W-IDKUNDNR-WDB2                        
067700     ELSE                                                                 
067800       MOVE NEJ                 TO NYCKLAR-SW                             
067900       MOVE 'IDKUNDNR'          TO RESP-IDELMT-ERROR                      
068000       MOVE '024'               TO RESP-IDMSG-ERROR                       
068100     END-IF                                                               
068200                                                                          
068300     .                                                                    
068400     EJECT                                                                
068500 BC-KOLLA-IDRAPPNR              SECTION.                                  
068600                                                                          
068700                                                                          
068800     IF REQU-IDRAPPNR-KEY       NUMERIC                                   
068900       MOVE REQU-IDRAPPNR-KEY   TO W-IDRAPPNR-A2                          
069000                                   RESP-IDRAPPNR-KEY                      
069100     ELSE                                                                 
069200       MOVE NEJ                 TO NYCKLAR-SW                             
069300       MOVE 'IDRAPPNR'          TO RESP-IDELMT-ERROR                      
069400       MOVE '024'               TO RESP-IDMSG-ERROR                       
069500     END-IF                                                               
069600                                                                          
069700                                                                          
069800     .                                                                    
069900     EJECT                                                                
070000 BD-KOLLA-IDARTNR               SECTION.                                  
070100                                                                          
070200                                                                          
070300     IF REQU-IDARTNR-KEY   NUMERIC AND REQU-IDARTNR-KEY > ZERO            
070400       MOVE REQU-IDARTNR-KEY    TO W-IDARTNR                              
070500                                   W-IDARTNR-A2                           
070600                                   RESP-IDARTNR-KEY                       
070700     ELSE                                                                 
070800       MOVE NEJ                 TO NYCKLAR-SW                             
070900       IF REQU-IDARTNR-KEY = ZERO                                         
071000          MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                   
071100          MOVE '126'               TO RESP-IDMSG-ERROR                    
071200       ELSE                                                               
071300          MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                   
071400          MOVE '024'               TO RESP-IDMSG-ERROR                    
071500       END-IF                                                             
071600     END-IF                                                               
071700                                                                          
071800     .                                                                    
071900     EJECT                                                                
072000 BE-KOLLA-IDRADNR               SECTION.                                  
072100                                                                          
072200     IF REQU-IDRADNR-KEY        NUMERIC                                   
072300       MOVE REQU-IDRADNR-KEY    TO W-IDRADNR-A2                           
072400                                   RESP-IDRADNR-KEY                       
072500     ELSE                                                                 
072600       MOVE NEJ                 TO NYCKLAR-SW                             
072700       MOVE 'IDRADNR'           TO RESP-IDELMT-ERROR                      
072800       MOVE '024'               TO RESP-IDMSG-ERROR                       
072900     END-IF                                                               
073000                                                                          
073100     .                                                                    
073200     EJECT                                                                
073300 BF-KOLLA-IDFTG                 SECTION.                                  
073400                                                                          
073500*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
073600*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
073700*    IF REQU-IDUSER = 'PHCA4G1'                                           
073800*       AND REQU-IDDC-KEY = '44'                                          
073900*      MOVE '54'            TO REQU-IDFTG-KEY                             
074000*    END-IF                                                               
074100*    END FIX                                                              
074200     IF REQU-IDFTG-KEY NOT NUMERIC                                        
074300       MOVE NEJ             TO NYCKLAR-SW                                 
074400       MOVE 'IDFTG'         TO RESP-IDELMT-ERROR                          
074500       MOVE '023'           TO RESP-IDMSG-ERROR                           
074600     END-IF                                                               
074700     .                                                                    
074800     EJECT                                                                
074900 F-LAES-VISA-INFO               SECTION.                                  
075000                                                                          
075100     MOVE REQU-IDFTG-KEY        TO WS-IDFTG                               
075200                                                                          
075300     IF DIST79-DEALER-PRICE   OR                                          
075400        IDFTG-CN              OR                                          
075500        IDFTG-IN              OR                                          
075600        IDFTG-KR              OR                                          
075700        IDFTG-TR              OR                                          
075800        IDFTG-MY              OR                                          
075900        IDFTG-MX              OR                                          
076000        IDFTG-BR              OR                                          
076010        IDFTG-ZA                                                          
076100       PERFORM S10-HAMTA-KDVALISO                                         
076200     ELSE                                                                 
076300       MOVE 'SEK'               TO RESP-KDVALISO                          
076400     END-IF                                                               
076500                                                                          
076600     PERFORM IMS-GHU-WLKREE11                                             
076700     IF SEGMENT-SAKNAS                                                    
076800        MOVE 'IDLEVANM' TO RESP-IDELMT-ERROR                              
076900        MOVE '025' TO RESP-IDMSG-ERROR                                    
077000     ELSE                                                                 
077100       IF LEV-IDFTG NOT = REQU-IDFTG-KEY                                  
077200        MOVE ERR-NOT-AUTHORIZED TO RESP-IDMSG-ERROR                       
077300       ELSE                                                               
077400        PERFORM FA-REDIGERA-BILD                                          
077500       END-IF                                                             
077600     END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900                                                                          
078000 FA-REDIGERA-BILD               SECTION.                                  
078100                                                                          
078200     PERFORM FAA-REDIGERA-ANM-INFO                                        
078300     PERFORM FAB-REDIGERA-FAKT-INFO                                       
078400     PERFORM FAC-REDIGERA-ART-INFO                                        
078500                                                                          
078600     .                                                                    
078700     EJECT                                                                
078800 FAA-REDIGERA-ANM-INFO          SECTION.                                  
078900                                                                          
079000     MOVE '-'                   TO RESP-STRECK-1                          
079100     IF INDATA-OK                                                         
079200       MOVE LEV-KDKREBEH        TO RESP-KDKREBEH                          
079300       IF RESP-KDKREBEH(1:1)       = 'C'                                  
079400          IF RESP-KDKREBEH(2:1)    = '1'                                  
079500             MOVE ZERO          TO RESP-KDKREBEH(2:1)                     
079600          END-IF                                                          
079700       END-IF                                                             
079800       IF RESP-KDKREBEH(1:1)     = 'J'                                    
079900          MOVE 'Y'              TO RESP-KDKREBEH(1:1)                     
080000       ELSE                                                               
080100          IF RESP-KDKREBEH      = 'ANN'                                   
080200             MOVE 'DEL'         TO RESP-KDKREBEH                          
080300          END-IF                                                          
080400       END-IF                                                             
080500       MOVE LEV-FLSVAR          TO RESP-FLSVAR                            
080600     END-IF                                                               
080700                                                                          
080800     MOVE LEV-IDDC              TO RESP-IDDC                              
080900                                   W-IDDC                                 
081000                                   W-IDDC-B6                              
081100                                                                          
081200     MOVE LEV-IDARTNR           TO RESP-IDARTNR                           
081300                                   W-IDARTNR-DSEQ-FOM                     
081400                                   W-IDARTNR-DSEQ-TOM                     
081500     MOVE LEV-KDANMORS          TO RESP-KDANMORS                          
081600                                   WS-KDANMORS                            
081700                                   OKOD-KDANMORS                          
081800                                                                          
081900     MOVE JA                    TO WDB6-SW                                
082000     PERFORM IMS-GU-WDB601                                                
082100     IF SEGMENT-SAKNAS                                                    
082200       MOVE NEJ                 TO WDB6-SW                                
082300     END-IF                                                               
082400                                                                          
082500     MOVE LEV-KVLEVANM-BEKR     TO RESP-KVLEVANM-BEKR                     
082600     MOVE LEV-IDORDNR7          TO RESP-IDORDNR5                          
082700     MOVE LEV-IDKUNDRF          TO WS-IDKUNDRF                            
082800     MOVE LEV-IDKOLLI           TO RESP-IDKOLLI                           
082900                                   WS-IDKOLLI                             
083000     MOVE LEV-IDFAKT            TO RESP-IDFAKT                            
083100                                   WS-IDFAKT                              
083200     MOVE LEV-TIFAKT            TO RESP-TIFAKT                            
083300                                                                          
083400     MOVE LEV-IDFTG TO WS-IDFTG                                           
083500     IF IDFTG-PV                                                          
083600        MOVE ZERO               TO RESP-KDFRAKT                           
083700                                   RESP-PRFRAKT                           
083800     ELSE                                                                 
083900        IF DCS-CDC OR DCS-DDC                                             
084000           MOVE LEV-KDFRAKT     TO RESP-KDFRAKT                           
084100           MOVE LEV-PRFRAKT     TO RESP-PRFRAKT                           
084200        ELSE                                                              
084300           MOVE LEV-IDFAKT-LOC  TO RESP-IDFAKT                            
084400           MOVE LEV-TIFAKT-LOC  TO RESP-TIFAKT                            
084500           MOVE LEV-KDFRAKT     TO RESP-KDFRAKT                           
084600           MOVE LEV-PRFRAKT     TO RESP-PRFRAKT                           
084700        END-IF                                                            
084800     END-IF                                                               
084900     IF LEV-FLTEXT = JA                                                   
085000       MOVE YES                 TO RESP-FLTEXT                            
085100     ELSE                                                                 
085200       MOVE LEV-FLTEXT          TO RESP-FLTEXT                            
085300     END-IF                                                               
085400                                                                          
085500     CALL W418OKOD USING OKOD-W418OKOD                                    
085600                                                                          
085700     IF WS-ANTAL-SKALL-FINNAS                                             
085800       PERFORM FAAA-KONTROLLERA-KVLEVANM-TOT                              
085900       MOVE WS-KVLEVANM-TOT     TO RESP-SULEVANM                          
086000     END-IF                                                               
086100                                                                          
086200     .                                                                    
086300     EJECT                                                                
086400 FAAA-KONTROLLERA-KVLEVANM-TOT  SECTION.                                  
086500                                                                          
086600     MOVE +0                    TO WS-KVLEVANM-TOT                        
086700     MOVE REQU-IDDISTR-KEY      TO W-IDDISTR-DSEQ-FOM                     
086800                                   W-IDDISTR-DSEQ-TOM                     
086900     MOVE REQU-IDKUNDNR-KEY     TO W-IDKUNDNR-DSEQ-FOM                    
087000                                   W-IDKUNDNR-DSEQ-TOM                    
087100     MOVE REQU-IDFTG-KEY        TO W-IDFTG-DSEQ-FOM                       
087200                                   W-IDFTG-DSEQ-TOM                       
087300                                   W-IDFTG                                
087400                                                                          
087500     PERFORM IMS-GN-WLKREI-SEQ                                            
087600     PERFORM UNTIL SEGMENT-SAKNAS                                         
087700       MOVE SEQD-IDDISTR        TO W-IDDISTR-A2                           
087800       MOVE SEQD-IDKUNDNR       TO W-IDKUNDNR-A2                          
087900       MOVE SEQD-IDRAPPNR       TO W-IDRAPPNR-A2                          
088000       MOVE SEQD-IDRADNR        TO W-IDRADNR-A2                           
088100       PERFORM IMS-GHU-WLKREE11                                           
088200       IF LEV-IDKOLLI = WS-IDKOLLI                                        
088300         IF LEV-IDKUNDRF = WS-IDKUNDRF                                    
088400           IF LEV-KDKREBEH(1:1) = 'N'                                     
088500             CONTINUE                                                     
088600           ELSE                                                           
088700             ADD LEV-KVLEVANM-BEKR TO WS-KVLEVANM-TOT                     
088800           END-IF                                                         
088900         END-IF                                                           
089000       END-IF                                                             
089100       PERFORM IMS-GN-WLKREI-SEQ                                          
089200     END-PERFORM                                                          
089300     .                                                                    
089400     EJECT                                                                
089500                                                                          
089600 FAB-REDIGERA-FAKT-INFO         SECTION.                                  
089700                                                                          
090101     MOVE LEV-IDFAKT            TO W-IDFAKT-L5                            
090400     MOVE REQU-IDDISTR-KEY      TO W-IDDISTR-L5                           
090500     MOVE REQU-IDKUNDNR-KEY     TO W-IDKUNDNR-L5                          
090600     MOVE WS-IDKUNDRF           TO W-IDKUNDRF-L5                          
090800     MOVE WS-IDKOLLI            TO W-IDKOLLI-L5                           
090900     MOVE LEV-IDARTNR           TO W-IDARTNR-L5                           
091000                                                                          
091100     PERFORM IMS-GU-WDL501                                                
091200     IF SEGMENT-FINNS                                                     
091300        PERFORM IMS-GNP-WDL511                                            
091400        IF SEGMENT-FINNS                                                  
091500          MOVE FAKC-IDPRODNR TO W-IDPRODNR-L5                             
091600          PERFORM IMS-GNP-WDL521                                          
091700        END-IF                                                            
091800     END-IF                                                               
092000     IF SEGMENT-FINNS                                                     
092100        MOVE FAKL-IDBORD        TO RESP-IDBORD                            
092200        MOVE FAKL-KVBEART-Q     TO RESP-KVBEART-Q                         
092300        MOVE FAKL-KVLEVART      TO RESP-KVLEVART                          
092400                                                                          
092500        ADD LEV-KVLEVANM-BEKR   TO WS-KVLEVANM-TOT                        
092600        IF REQU-KDMATT = 'U'                                              
092700           COMPUTE WS-VKORDBTO = FAKC-VKORDBTO-KOLLI *                    
092800                                 CONV-KG-TO-LB                            
092900           COMPUTE WS-VKORDNTO = FAKC-VKORDNTO-KOLLI *                    
093000                                 CONV-KG-TO-LB                            
093100           MOVE WS-VKORDBTO     TO RESP-VKORDBTO-KOLLI                    
093200                                   VKORDBTO-KOLLI                         
093300           MOVE WS-VKORDNTO     TO VKORDNTO-KOLLI                         
093400                                                                          
093500        ELSE                                                              
093600           MOVE FAKC-VKORDBTO-KOLLI                                       
093700                                TO RESP-VKORDBTO-KOLLI                    
093800                                   VKORDBTO-KOLLI                         
093900           MOVE FAKC-VKORDNTO-KOLLI                                       
094000                                TO VKORDNTO-KOLLI                         
094100        END-IF                                                            
094200                                                                          
094300        MOVE FAKC-KDORDKL       TO RESP-KDORDKL                           
094400        MOVE FAKC-KDKOLLI       TO W-KDKOLLI                              
094500                                   RESP-KDKOLLI                           
094600        IF RESP-KDKOLLI = '00000000'                                      
094700          MOVE SPACE            TO RESP-KDKOLLI                           
094800        END-IF                                                            
094900        IF FAK-FLDIRLEV          = JA                                     
095000            MOVE YES            TO RESP-FLDIRLEV                          
095100        ELSE                                                              
095200          MOVE FAK-FLDIRLEV     TO RESP-FLDIRLEV                          
095300        END-IF                                                            
095400        MOVE FAKL-IDUSER-PACK   TO RESP-IDUSER-PACK                       
095500        IF RESP-IDUSER-PACK = '00000000'                                  
095600          MOVE SPACE            TO RESP-IDUSER-PACK                       
095700        END-IF                                                            
095800        MOVE FAKC-IDPRODNR      TO RESP-IDPRODNR                          
096300        MOVE FAKL-KVORDRAD      TO RESP-KVORDRAD                          
096400        MOVE FAKL-IDUSER-OREG   TO RESP-IDUSER-OREG                       
096500        IF RESP-IDUSER-OREG = '00000000'                                  
096600          MOVE SPACE            TO RESP-IDUSER-OREG                       
096700        END-IF                                                            
096801        MOVE FAKC-TIFAKT        TO RESP-TIREGDAT                          
096900        PERFORM FABA-BERAEKNA-VKORDNTO-DIFF                               
097000        COMPUTE WS-VKORDNTO-KOLLI-TOT =                                   
097100                WS-VKORDNTO-KOLLI-TOT / 1000                              
097200                                                                          
097300        PERFORM IMS-GET-EMBB01                                            
097400        IF SEGMENT-FINNS                                                  
097500          IF REQU-KDMATT = 'U'                                            
097600            COMPUTE WS-VKTARA = EMB-VKTARA * CONV-KG-TO-LB                
097700            MOVE WS-VKTARA      TO RESP-VKTARA                            
097800                                   VKTARA                                 
097900          ELSE                                                            
098000            MOVE EMB-VKTARA     TO RESP-VKTARA                            
098100                                   VKTARA                                 
098200          END-IF                                                          
098300          COMPUTE VKORDNTO-KOLLI =                                        
098400                  VKORDBTO-KOLLI -                                        
098500                  VKTARA                                                  
098600          END-COMPUTE                                                     
098700        END-IF                                                            
098800                                                                          
098900        COMPUTE WS-VKORDNTO-DIFF =                                        
099000                VKORDNTO-KOLLI   -                                        
099100                WS-VKORDNTO-KOLLI-TOT                                     
099200        END-COMPUTE                                                       
099300        IF REQU-KDMATT = 'U'                                              
099400             COMPUTE WS-VKORDNTO-DIFF = WS-VKORDNTO-DIFF *                
099500                                        CONV-KG-TO-LB                     
099600        END-IF                                                            
099700        MOVE WS-VKORDNTO-DIFF        TO RESP-VKORDBTO-DIFF                
099800        COMPUTE WS-VLORDNTO-KOLLI =                                       
099900                WS-VLORDNTO-KOLLI-TOT / 1000                              
100000        IF REQU-KDMATT = 'U'                                              
100100           COMPUTE WS-VLORDNTO-KOLLI = WS-VLORDNTO-KOLLI *                
100200                                       CONV-M3-TO-YD3                     
100300        END-IF                                                            
100400      MOVE WS-VLORDNTO-KOLLI       TO RESP-VLORDBTO-KOLLI                 
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900 FABA-BERAEKNA-VKORDNTO-DIFF    SECTION.                                  
101000                                                                          
101100     MOVE ZERO                  TO WS-VKORDNTO-KOLLI-TOT                  
101200     PERFORM IMS-GNP-WDL521-KLI-1ST                                       
101300     PERFORM UNTIL SEGMENT-SAKNAS                                         
101400       MOVE FAKL-IDARTNR        TO W-IDARTNR                              
101500       MOVE FAKL-KVLEVART       TO WS-KVLEVART                            
101600       PERFORM IMS-GU-WLARTC01                                            
101700       PERFORM S20-READ-ARTC11-WDK712-22                                  
101800       COMPUTE WS-VKORDNTO-KOLLI-TOT =                                    
101900               WS-VKORDNTO-KOLLI-TOT +                                    
102000               (WS-KVLEVART * WS-VKART)                                   
102100       COMPUTE WS-VLORDNTO-KOLLI-TOT =                                    
102200               WS-VLORDNTO-KOLLI-TOT +                                    
102300               (WS-KVLEVART * WS-VLARTNTO)                                
102400       PERFORM IMS-GNP-WDL521-KLI                                         
102500     END-PERFORM                                                          
102600     .                                                                    
102700     EJECT                                                                
102800 FAC-REDIGERA-ART-INFO          SECTION.                                  
102900                                                                          
103000     MOVE +0                    TO WS-KVOKS-TOT-CDC                       
103100     MOVE REQU-IDARTNR-KEY      TO W-IDARTNR                              
103200                                                                          
103300     PERFORM IMS-GET-ARTM-WDK9                                            
103400     IF SEGMENT-FINNS                                                     
103500       COMPUTE WS-KVOKS-TOT-CDC = ART-KVOKS-BULK +                        
103600                                  ART-KVOKS-DAG  +                        
103700                                  ART-KVOKS-VOR                           
103800     END-IF                                                               
103900                                                                          
104000     PERFORM IMS-GU-WLARTC01                                              
104100                                                                          
104200     MOVE ART-REKSIFFR          TO RESP-REKSIFFR                          
104300     MOVE ART-IDFKNGRP          TO RESP-IDFKNGRP                          
104400     MOVE ART-KDPRODSL          TO RESP-KDPRODSL                          
104500                                                                          
104600     PERFORM S20-READ-ARTC11-WDK712-22                                    
104700                                                                          
104800     COMPUTE W-KVPB-TOT       =  CLAG-KVPB-SEP +                          
104900                                 CLAG-KVPB-SATS +                         
105000                                 CLAG-KVPB-TPO                            
105100                                                                          
105200     MOVE W-KVPB-TOT            TO RESP-KVPB-TOT                          
105300     MOVE CLAG-KDERS            TO RESP-KDERS                             
105400     MOVE WS-VKART              TO RESP-VKART                             
105500     IF DCS-NDC-NA              OR                                        
105600        DCS-NDC-CN              OR                                        
105700       (DCS-NDC-PF AND DCS-INDIA) OR                                      
105800       (DCS-NDC-PF AND DCS-KOREA) OR                                      
105900       (DCS-NDC-PF AND DCS-MALAYSIA)                                      
106000       CONTINUE                                                           
106100     ELSE                                                                 
106200       MOVE WS-IDANSK           TO RESP-IDANSK                            
106300     END-IF                                                               
106400                                                                          
106500     IF DCS-CDC OR DCS-DDC                                                
106600       MOVE CLAG-ADLAGOMR       TO RESP-ADLAGOMR                          
106700       MOVE CLAG-ADGANG         TO RESP-ADGANG                            
106800       MOVE CLAG-ADPLATS        TO RESP-ADPLATS                           
106900       MOVE CLAG-TIINVDAT       TO RESP-TIINVDAT                          
107000       MOVE CLAG-KVINVS         TO RESP-KVINVS                            
107100       COMPUTE WS-KVDISP         = CLAG-KVLS   -                          
107200                                   CLAG-KVRESS -                          
107300                                   CLAG-KVUTRS -                          
107400                                   WS-KVOKS-TOT-CDC                       
107500       MOVE WS-KVDISP           TO RESP-KVLS                              
107600     ELSE                                                                 
107700       PERFORM IMS-GU-WDK711                                              
107800       IF SEGMENT-FINNS                                                   
107900          MOVE SLAG-ADLAGOMR    TO RESP-ADLAGOMR                          
108000          MOVE SLAG-ADGANG      TO RESP-ADGANG                            
108100          MOVE SLAG-ADPLATS     TO RESP-ADPLATS                           
108200          MOVE SLAG-TIINVDAT    TO RESP-TIINVDAT                          
108300          MOVE SLAG-KVINVS      TO RESP-KVINVS                            
108400          COMPUTE WS-KVDISP      = SLAG-KVLS   -                          
108500                                   SLAG-KVUTRS                            
108600          MOVE WS-KVDISP        TO RESP-KVLS                              
108700          IF DCS-NDC-NA             OR                                    
108800             DCS-NDC-CN             OR                                    
108900            (DCS-NDC-PF AND DCS-INDIA) OR                                 
109000            (DCS-NDC-PF AND DCS-KOREA) OR                                 
109100            (DCS-NDC-PF AND DCS-MALAYSIA)                                 
109200            MOVE SLAG-KVPB-REF  TO RESP-KVPB-TOT                          
109300            MOVE SLAG-IDPERSON-BUY                                        
109400                                TO RESP-IDANSK                            
109500          END-IF                                                          
109600       END-IF                                                             
109700     END-IF                                                               
109800                                                                          
109900     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
110000     IF DCS-UNICODE-IDSKYLT                                               
110100        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
110200     ELSE                                                                 
110300        MOVE '278 '             TO TRAUTF8-KDCP                           
110400     END-IF                                                               
110500     PERFORM IMS-GU-WLBENA11                                              
110600     IF SEGMENT-FINNS                                                     
110700       MOVE TEXT-BEART          TO TRAUTF8-TECONV-FROM                    
110800       IF TEXT-BEART = SPACE                                              
110900         MOVE '?'               TO TRAUTF8-TECONV-FROM                    
111000       END-IF                                                             
111100     ELSE                                                                 
111200       MOVE WS-CP-EBCDIC        TO TRAUTF8-KDCP                           
111300       MOVE '?'                 TO TRAUTF8-TECONV-FROM                    
111400     END-IF                                                               
111500                                                                          
111600* -- STRIP SPACE OR CONVERT TO UNICODE                                    
111700     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
111800                                                                          
111900* -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                           
112000     MOVE TRAUTF8-TECONV-TO     TO RESP-BEART                             
112100     .                                                                    
112200     EJECT                                                                
112300                                                                          
112400 G-KOLLA-INPUT                  SECTION.                                  
112500                                                                          
112600     MOVE JA                    TO INDATA-SW                              
112700     PERFORM IMS-GET-WLKREE01-KVAL                                        
112800     IF SEGMENT-FINNS                                                     
112900       IF ANM-KDLEVANM > '0' AND < '4'                                    
113000         PERFORM IMS-GHU-WLKREE11                                         
113100                                                                          
113200         IF SEGMENT-FINNS                                                 
113300           PERFORM GB-KOLLA-FLSVAR                                        
113400           PERFORM GE-KOLLA-PRISSATT                                      
113501           PERFORM GH-KOLLA-FLAUTREM                                      
113502         ELSE                                                             
113600           MOVE NEJ             TO INDATA-SW                              
113700           MOVE 'IDLEVANM' TO RESP-IDELMT-ERROR                           
113800           MOVE '025' TO RESP-IDMSG-ERROR                                 
113900         END-IF                                                           
114000       ELSE                                                               
114100         MOVE NEJ               TO INDATA-SW                              
114200         MOVE '007'             TO RESP-IDMSG-ERROR                       
114300       END-IF                                                             
114400     ELSE                                                                 
114500       MOVE NEJ                 TO INDATA-SW                              
114600       MOVE 'IDLEVANM' TO RESP-IDELMT-ERROR                               
114700       MOVE '025' TO RESP-IDMSG-ERROR                                     
114800     END-IF                                                               
114900                                                                          
115000     IF INDATA-OK                                                         
115100       IF REQU-FLSVAR NOT = 'Y' AND 'J' AND 'N'                           
115200         MOVE NEJ               TO INDATA-SW                              
115300         MOVE '291'             TO RESP-IDMSG-ERROR                       
115400       END-IF                                                             
115500     END-IF                                                               
115600     .                                                                    
115700     EJECT                                                                
115800 GB-KOLLA-FLSVAR                SECTION.                                  
115900                                                                          
116000*    IF LEV-KDKREBEH = 'RR ' OR 'QR ' OR 'PR '                            
116100       IF REQU-FLSVAR = 'Y' OR 'J' OR 'N'                                 
116200                                                                          
116300         MOVE +1                TO ANSV-KDCALL                            
116400         MOVE REQU-IDDISTR-KEY  TO ANSV-IDDISTR                           
116500         MOVE REQU-IDFTG-KEY    TO ANSV-IDFTG                             
116600         MOVE REQU-IDKUNDNR-KEY TO ANSV-IDKUNDNR                          
116700         MOVE LEV-KDANMORS      TO ANSV-KDANMORS                          
116800         MOVE +0                TO ANSV-KDORDKL                           
116900                                   ANSV-ADLAGOMR                          
117000                                                                          
117100         CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                       
117200                                           4115-PCB                       
117300                                           4117-PCB                       
117400                                                                          
117500         IF ANSV-OK                                                       
117600           MOVE ANSV-KDARBTYP TO LEV-KDARBTYP                             
117700           MOVE ANSV-IDPERSON TO LEV-IDPERSON                             
117800         ELSE                                                             
117900           IF ANSV-KDSVAR = 'S'                                           
118000             MOVE HIGH-VALUE           TO ANSV-KDANMORS                   
118100             CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                   
118200             IF ANSV-KDSVAR = SPACE                                       
118300               MOVE ANSV-KDARBTYP      TO LEV-KDARBTYP                    
118400               MOVE ANSV-IDPERSON      TO LEV-IDPERSON                    
118502               SET  WS-FLAUTREM-Y        TO TRUE                          
118503             ELSE                                                         
118600               MOVE NEJ                TO INDATA-SW                       
118700               MOVE '292'              TO RESP-IDMSG-ERROR                
118800             END-IF                                                       
118900           ELSE                                                           
119000             MOVE NEJ                  TO INDATA-SW                       
119100             MOVE '293'                TO RESP-IDMSG-ERROR                
119200           END-IF                                                         
119300         END-IF                                                           
119400       ELSE                                                               
119500         MOVE NEJ                      TO INDATA-SW                       
119600         MOVE '294'                    TO RESP-IDMSG-ERROR                
119700       END-IF                                                             
119800*    ELSE                                                                 
119900*      MOVE NEJ                        TO INDATA-SW                       
120000*      MOVE '295'                      TO RESP-IDMSG-ERROR                
120100*    END-IF                                                               
120200     .                                                                    
120300     EJECT                                                                
120400 GE-KOLLA-PRISSATT              SECTION.                                  
120500                                                                          
120600     IF DIST79-DEALER-PRICE                                               
120700       IF LEV-FLPRQUES = JA                                               
120800         IF LEV-KDVAT = SPACE  OR  LEV-PRARTBTO-LOC = ZERO                
120900           MOVE NEJ            TO INDATA-SW                               
121000           MOVE '260'          TO RESP-IDMSG-ERROR                        
121100         END-IF                                                           
121200       END-IF                                                             
121300     END-IF                                                               
121400     .                                                                    
121500     EJECT                                                                
121701 GH-KOLLA-FLAUTREM              SECTION.                                  
121801                                                                          
121901     MOVE NEJ TO SW-KDKREBEH-Y                                            
122001     IF REQU-FLSVAR NOT = ALL '+'                                         
122101        IF LEV-KDKREBEH = 'RR '                                           
122201           IF REQU-FLSVAR = 'J' OR 'Y'                                    
122301              PERFORM IMS-GU-GMTA-WDB201                                  
122401              IF NOT SEGMENT-FINNS                                        
122501                 PERFORM IMS-GET-WDB201                                   
122601              END-IF                                                      
122701              IF GMT-FLAUTREM = 'J' OR 'Y'                                
122901                    IF NOT (LEV-KDANMORS = '42' OR                        
123001                            LEV-KDANMORS = '62')                          
123101                       MOVE 'Y'  TO LEV-KDKREBEH                          
123201                       MOVE JA   TO SW-KDKREBEH-Y                         
123301                    END-IF                                                
123501              ELSE                                                        
123601                 IF GMT-FLAUTREM = 'N'                                    
123801                       IF NOT (LEV-KDANMORS = '12' OR                     
123901                               LEV-KDANMORS = '22' OR                     
124001                               LEV-KDANMORS = '42' OR                     
124101                               LEV-KDANMORS = '62')                       
124201                          MOVE 'Y'  TO LEV-KDKREBEH                       
124301                          MOVE JA   TO SW-KDKREBEH-Y                      
124401                       END-IF                                             
124601                 END-IF                                                   
124701              END-IF                                                      
124801           ELSE                                                           
124802* RIGHT NOW WE ARE NOT WORKING ON CANCEL REFERRAL IT WILL COME            
124803* IN FUTURE SIMILAR CHANGE TO BE IN W4072200                              
124901              IF WS-FLAUTREM-N                                            
125001                 PERFORM GHA-KOLLA-PRARTBTO                               
125101              END-IF                                                      
125201           END-IF                                                         
125301        ELSE                                                              
125401           IF LEV-KDKREBEH = 'Y' AND REQU-FLSVAR = 'N'                    
125703              PERFORM GHA-KOLLA-PRARTBTO                                  
125704           END-IF                                                         
125705        END-IF                                                            
125706     ELSE                                                                 
125707        IF WS-FLAUTREM-Y                                                  
125801           PERFORM GHA-KOLLA-PRARTBTO                                     
125901        END-IF                                                            
126001     END-IF                                                               
126101     .                                                                    
126201     EJECT                                                                
126301 GHA-KOLLA-PRARTBTO             SECTION.                                  
126401                                                                          
126501     IF (REQU-FLSVAR = 'N' OR LEV-FLSVAR ='N')                            
126701           IF NOT (LEV-KDANMORS = '00'  OR                                
126801                   LEV-KDANMORS = '11'  OR                                
126901                   LEV-KDANMORS = '12'  OR                                
127001                   LEV-KDANMORS = '20'  OR                                
127101                   LEV-KDANMORS = '21'  OR                                
127201                   LEV-KDANMORS = '22'  OR                                
127301                   LEV-KDANMORS = '42'  OR                                
127401                   LEV-KDANMORS = '62')                                   
127501              IF WS-KDVALISO-SPAR = 'SEK' AND                             
127601                 (LEV-PRARTBTO < 500 OR LEV-PRARTBTO-LOC < 500)           
128003                 IF LEV-KDKREBEH = 'RR '                                  
128004                    MOVE 'Y'  TO LEV-KDKREBEH                             
128005                    MOVE JA   TO SW-KDKREBEH-Y                            
128101                 END-IF                                                   
128201              ELSE                                                        
128301                 IF WS-KDVALISO-SPAR NOT = 'SEK'                          
128401                    IF LEV-PRARTBTO  = ZERO                               
128501                       MOVE LEV-PRARTBTO-LOC TO WS-PRARTBTO-CONV          
128601                       PERFORM GHB-KOLLA-PRARTBTO-CONV                    
128701                    ELSE                                                  
128801                       IF LEV-PRARTBTO-LOC = ZERO                         
128901                          MOVE LEV-PRARTBTO  TO WS-PRARTBTO-CONV          
129001                       END-IF                                             
129101                       PERFORM GHB-KOLLA-PRARTBTO-CONV                    
129201                    END-IF                                                
129301                    IF WS-PRARTBTO-CONV < 500                             
129501                       IF LEV-KDKREBEH = 'RR '                            
129601                          MOVE 'Y'  TO LEV-KDKREBEH                       
129701                          MOVE JA   TO SW-KDKREBEH-Y                      
129801                       END-IF                                             
129901                    ELSE                                                  
130001                       MOVE NEJ               TO INDATA-SW                
130101                       MOVE '007'             TO RESP-IDMSG-ERROR         
130401                    END-IF                                                
130501                 END-IF                                                   
130601              END-IF                                                      
131201              IF SW-KDKREBEH-Y = 'N' AND INDATA-SW = 'Y'                  
131203                 MOVE 'R'           TO LEV-KDKREBEH                       
131204                 MOVE  2            TO ANM-KDLEVANM                       
131205                 PERFORM IMS-REPL-KREE                                    
131401              END-IF                                                      
131402           ELSE                                                           
131404              MOVE NEJ               TO INDATA-SW                         
131405              MOVE '007'             TO RESP-IDMSG-ERROR                  
131406           END-IF                                                         
131407     END-IF                                                               
131501     .                                                                    
131601     EJECT                                                                
131701 GHB-KOLLA-PRARTBTO-CONV        SECTION.                                  
131801                                                                          
131901     MOVE W-DATE-AAMM      TO CURR-TIAAMM                                 
132001     MOVE WS-KDVALISO-SPAR TO CURR-KDVALISO-ROW                           
132101                              CURR-KDVALISO-HUV                           
132201     MOVE 'M'              TO CURR-KDVALTYP                               
132301     CALL W510CURR      USING CURR-W510CURR WDG2-PCB                      
132401     IF CURR-KDSVAR = ' '                                                 
132501        MOVE CURR-PRKURS-NEW   TO WS-PRKURS                               
132601     ELSE                                                                 
132701        MOVE 1                 TO WS-PRKURS                               
132801     END-IF                                                               
132901     COMPUTE WS-PRARTBTO-CONV  ROUNDED =                                  
133001             WS-PRARTBTO-CONV * WS-PRKURS * LEV-KVLEVANM-BEKR             
133101     .                                                                    
133201     EJECT                                                                
133202 H-UPPDATERA                    SECTION.                                  
133203                                                                          
133204     PERFORM HA-AENDRA-KDKREBEH                                           
133503                                                                          
133504     MOVE +1                    TO MEAN-IX                                
133505     PERFORM IMS-GET-WLKREE01-KVAL                                        
133506     PERFORM IMS-GET-KREE11-GNP                                           
133507     PERFORM UNTIL SEGMENT-SAKNAS                                         
133508       MOVE LEV-KDKREBEH        TO TEST-KDKREBEH                          
133510       IF KDKREBEH-1 = 'R' OR 'Q' OR 'P'                                  
133512         MOVE JA                TO OBEH-RADER-FINNS-SW                    
133513         IF KDKREBEH-2 = 'R'                                              
133515           MOVE JA              TO OBEH-REMISS-FINNS-SW                   
133516         END-IF                                                           
133517       ELSE                                                               
133518         MOVE JA                TO BEH-RADER-FINNS-SW                     
134903         IF KDKREBEH-1 = 'N'                                              
134905           PERFORM HB-FYLL-MEAN-AREA                                      
134906           MOVE JA              TO AVVISADE-RADER-SW                      
134907         ELSE                                                             
134908           IF LEV-KDKREBEH = 'ANN'                                        
134909             CONTINUE                                                     
134910           ELSE                                                           
134920             IF KDKREBEH-1 = 'Y' OR 'C' OR 'J'                            
135702               MOVE JA          TO GODKAENDA-RADER-SW                     
135703             END-IF                                                       
135704           END-IF                                                         
135705         END-IF                                                           
135706       END-IF                                                             
135707       PERFORM IMS-GET-KREE11-GNP                                         
135708     END-PERFORM                                                          
135709                                                                          
135710     PERFORM IMS-GET-WLKREE01-KVAL                                        
135711     IF OBEH-RADER-FINNS                                                  
135713       IF BEH-RADER-FINNS OR OBEH-REMISS-FINNS                            
135714         MOVE  2                TO ANM-KDLEVANM                           
135716         PERFORM IMS-REPL-KREE                                            
135717       END-IF                                                             
135718     ELSE                                                                 
135719       IF GODKAENDA-RADER-FINNS                                           
135720           IF ANM-IDUSER-ADM = SPACE                                      
135721             MOVE REQU-IDUSER     TO ANM-IDUSER-ADM                       
135722             MOVE REQU-BEANST     TO ANM-BEANST                           
135723           END-IF                                                         
135724         MOVE  3                TO ANM-KDLEVANM                           
135725         PERFORM IMS-REPL-KREE                                            
135726         IF AVVISADE-RADER-FINNS                                          
135727             PERFORM HC-SKRIV-MEAN-AREA                                   
135728         END-IF                                                           
135729       ELSE                                                               
135730         MOVE '7'             TO ANM-KDLEVANM                             
135731         PERFORM IMS-REPL-KREE                                            
135732         IF AVVISADE-RADER-FINNS                                          
135733             PERFORM HC-SKRIV-MEAN-AREA                                   
135734         END-IF                                                           
135735       END-IF                                                             
135736     END-IF                                                               
135737                                                                          
135738     MOVE '001'               TO RESP-IDMSG-INFO                          
135739     .                                                                    
135740     EJECT                                                                
135741                                                                          
135742 HA-AENDRA-KDKREBEH             SECTION.                                  
135743                                                                          
135744     MOVE REQU-FLSVAR         TO LEV-FLSVAR                               
135745     IF LEV-KDKREBEH = 'QR '                                              
139901       MOVE 'Q  '             TO  LEV-KDKREBEH                            
139902     ELSE                                                                 
139903       IF LEV-KDKREBEH = 'PR '                                            
140201         MOVE 'P  '           TO  LEV-KDKREBEH                            
140301       END-IF                                                             
140401*        MOVE 'R  '           TO LEV-KDKREBEH                             
140601       IF LEV-KDKREBEH = 'RR'                                             
140602         MOVE 'R  '           TO  LEV-KDKREBEH                            
140603       END-IF                                                             
140604     END-IF                                                               
140608     MOVE WS-DATUM            TO LEV-TIREMISS-IN                          
140609                                                                          
140610     PERFORM IMS-REPL-KREE                                                
140611     .                                                                    
140612     EJECT                                                                
140613                                                                          
140614 HB-FYLL-MEAN-AREA              SECTION.                                  
140615                                                                          
140616     IF MEAN-IX < +14                                                     
140617       MOVE REQU-IDDISTR-KEY    TO MEAN-IDDISTR  (MEAN-IX)                
140618       MOVE REQU-IDKUNDNR-KEY   TO MEAN-IDKUNDNR (MEAN-IX)                
140619       MOVE REQU-IDRAPPNR-KEY   TO MEAN-IDRAPPNR (MEAN-IX)                
140620                                                                          
140621       MOVE LEV-IDARTNR         TO MEAN-IDARTNR  (MEAN-IX)                
140622                                   W-IDARTNR-A2                           
140623       MOVE LEV-IDRADNR         TO MEAN-IDRADNR  (MEAN-IX)                
140624                                   W-IDRADNR-A2                           
140625       MOVE LEV-KDKREBEH        TO MEAN-KDKREBEH (MEAN-IX)                
140626                                                                          
140627       PERFORM IMS-GNP-WLKREE21                                           
140628                                                                          
140629       IF SEGMENT-FINNS                                                   
140630        MOVE TXT-TEANMNOT-ADM (1) TO                                      
140631             MEAN-TEANMNOT-ADM (MEAN-IX, 1)                               
140632        MOVE TXT-TEANMNOT-ADM (2) TO                                      
140633             MEAN-TEANMNOT-ADM (MEAN-IX, 2)                               
140634        MOVE TXT-TEANMNOT-ADM (3) TO                                      
140635             MEAN-TEANMNOT-ADM (MEAN-IX, 3)                               
140636                                                                          
140637        MOVE TXT-TEANMNOT-REM (1) TO                                      
140638             MEAN-TEANMNOT-REM (MEAN-IX, 1)                               
140639        MOVE TXT-TEANMNOT-REM (2) TO                                      
140640             MEAN-TEANMNOT-REM (MEAN-IX, 2)                               
140641        MOVE TXT-TEANMNOT-REM (3) TO                                      
140642             MEAN-TEANMNOT-REM (MEAN-IX, 3)                               
140643       ELSE                                                               
140644         MOVE SPACE             TO MEAN-TEANMNOT-ADM-GRP (MEAN-IX)        
140645         MOVE SPACE             TO MEAN-TEANMNOT-REM-GRP (MEAN-IX)        
140646       END-IF                                                             
140647                                                                          
140648       ADD +1                   TO MEAN-IX                                
140649     END-IF                                                               
140650     .                                                                    
140651     EJECT                                                                
140652 HC-SKRIV-MEAN-AREA             SECTION.                                  
140653                                                                          
140654     MOVE +1                    TO ANSV-KDCALL                            
140655     MOVE REQU-IDDISTR-KEY      TO ANSV-IDDISTR                           
140656     MOVE REQU-IDFTG-KEY        TO ANSV-IDFTG                             
140657     MOVE REQU-IDKUNDNR-KEY     TO ANSV-IDKUNDNR                          
140658     MOVE LEV-KDANMORS          TO ANSV-KDANMORS                          
140659     MOVE +0                    TO ANSV-KDORDKL                           
140660                                   ANSV-ADLAGOMR                          
140661                                                                          
140662     CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                           
140663                                       4115-PCB                           
140664                                       4117-PCB                           
140665                                                                          
140666     IF ANSV-OK                                                           
140667       MOVE ANSV-KDARBTYP       TO W-KDARBTYP                             
140668       MOVE ANSV-IDPERSON       TO W-IDPERSON                             
140669       PERFORM IMS-GET-WDP311                                             
140670       IF SEGMENT-FINNS                                                   
140671          MOVE PERS-IDMAIL      TO MEAN-IDMAIL                            
140672       END-IF                                                             
140673     ELSE                                                                 
140674       IF ANSV-KDSVAR = 'S'                                               
140675         MOVE HIGH-VALUE            TO ANSV-KDANMORS                      
140676         CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                       
140677         IF ANSV-OK                                                       
140678           MOVE ANSV-KDARBTYP       TO W-KDARBTYP                         
140679           MOVE ANSV-IDPERSON       TO W-IDPERSON                         
140680           PERFORM IMS-GET-WDP311                                         
140681           IF SEGMENT-FINNS                                               
140682              MOVE PERS-IDMAIL      TO MEAN-IDMAIL                        
140683           END-IF                                                         
140684         END-IF                                                           
140685       END-IF                                                             
140686     END-IF                                                               
140687                                                                          
140688     CALL W418MEAN USING MEAN-W418MEAN MAIL-PCB                           
140689     .                                                                    
140690                                                                          
140691     EJECT                                                                
140692 I-SKRIV-DETALJLISTA            SECTION.                                  
140693                                                                          
140694     IF INDATA-OK                                                         
140695        PERFORM IMS-GHU-WLKREE11                                          
140696                                                                          
140697        MOVE LEV-IDDC           TO W-IDDC                                 
140698                                                                          
140699        IF LEV-IDDC NOT = DCS-IDDC                                        
140700           MOVE LEV-IDDC TO W-IDDC-B6                                     
140701           PERFORM IMS-GU-WDB601                                          
140702        END-IF                                                            
140703                                                                          
140704        MOVE LEV-IDARTNR        TO W-IDARTNR                              
140705                                                                          
140706*       -- FOR D&P HEADER (IDOUTDEST)                                     
140707        MOVE REQU-IDDC-KEY       TO L16110-IDDC                           
140708        MOVE REQU-IDUSER         TO L16110-IDUSER                         
140709*       CALL ABEND                                                        
140710        PERFORM IB-FIXA-LEVANM-INFO                                       
140711        PERFORM IC-FIXA-FAKT-INFO                                         
140712        PERFORM ID-FIXA-ART-INFO                                          
140713                                                                          
140714        CALL WL016110 USING L16110-WL016110                               
140715                                                                          
140716     END-IF                                                               
140717                                                                          
140718     .                                                                    
140719     EJECT                                                                
140720                                                                          
140721 IB-FIXA-LEVANM-INFO            SECTION.                                  
140722                                                                          
140800     MOVE REQU-IDDISTR-KEY      TO L16110-IDDISTR                         
140900     MOVE REQU-IDKUNDNR-KEY     TO L16110-IDKUNDNR                        
141000     MOVE REQU-IDRAPPNR-KEY     TO L16110-IDRAPPNR                        
141100                                                                          
141200     MOVE LEV-IDARTNR           TO L16110-IDARTNR                         
141300                                   W-IDARTNR-A2                           
141400     MOVE LEV-IDRADNR           TO L16110-IDRADNR                         
141500                                   W-IDRADNR-A2                           
141600     MOVE LEV-KDANMORS          TO L16110-KDANMORS                        
141700     MOVE LEV-KVLEVANM-BEKR     TO L16110-KVLEVANM-BEKR                   
141800     MOVE LEV-IDORDNR7          TO L16110-IDORDNR5                        
141900     MOVE LEV-IDKOLLI           TO L16110-IDKOLLI                         
142000                                                                          
142100     IF DCS-CDC OR DCS-DDC                                                
142200       IF DIST79-DEALER-PRICE                                             
142300         MOVE LEV-PRARTBTO-LOC  TO L16110-PRARTBTO                        
142400       ELSE                                                               
142500         MOVE LEV-PRARTBTO      TO L16110-PRARTBTO                        
142600       END-IF                                                             
142700       MOVE LEV-KDFAKTYP        TO L16110-KDFAKTYP                        
142800     ELSE                                                                 
142900       MOVE LEV-PRARTBTO-LOCINV TO L16110-PRARTBTO                        
143000       MOVE LEV-KDFAKTYP        TO L16110-KDFAKTYP                        
143100     END-IF                                                               
143200                                                                          
143300     MOVE LEV-IDFAKT            TO L16110-IDFAKT                          
143400     MOVE LEV-TIFAKT            TO L16110-TIFAKT                          
143500                                                                          
143600     MOVE LEV-IDFTG TO WS-IDFTG                                           
143700     IF IDFTG-PV                                                          
143800        CONTINUE                                                          
143900     ELSE                                                                 
144000        IF DCS-CDC OR DCS-DDC                                             
144100           CONTINUE                                                       
144200        ELSE                                                              
144300           MOVE LEV-IDFAKT-LOC  TO L16110-IDFAKT                          
144400           MOVE LEV-TIFAKT-LOC  TO L16110-TIFAKT                          
144500        END-IF                                                            
144600     END-IF                                                               
144700                                                                          
144800     MOVE +1                    TO INDX                                   
144900     PERFORM UNTIL INDX        >  3                                       
145000      MOVE SPACE                TO L16110-TEANMNOT-REG(INDX)              
145100                                   L16110-TEANMNOT-ADM(INDX)              
145200                                   L16110-TEANMNOT-REM(INDX)              
145300                                   L16110-TEANMNOT-RET(INDX)              
145400                                                                          
145500      ADD +1                    TO INDX                                   
145600     END-PERFORM                                                          
145700                                                                          
145800     IF LEV-FLTEXT             =  JA                                      
145900        PERFORM IMS-GNP-WLKREE21                                          
146000        IF SEGMENT-FINNS                                                  
146100           MOVE +1              TO INDX                                   
146200           PERFORM UNTIL INDX           >  3                              
146300            MOVE TXT-TEANMNOT-REG(INDX) TO                                
146400                                         L16110-TEANMNOT-REG(INDX)        
146500            MOVE TXT-TEANMNOT-ADM(INDX) TO                                
146600                                         L16110-TEANMNOT-ADM(INDX)        
146700            MOVE TXT-TEANMNOT-REM(INDX) TO                                
146800                                         L16110-TEANMNOT-REM(INDX)        
146900            MOVE TXT-TEANMNOT-RET(INDX) TO                                
147000                                         L16110-TEANMNOT-RET(INDX)        
147100                                                                          
147200            ADD +1                       TO INDX                          
147300           END-PERFORM                                                    
147400        END-IF                                                            
147500     END-IF                                                               
147600                                                                          
147700     .                                                                    
147800     EJECT                                                                
147900                                                                          
148000 IC-FIXA-FAKT-INFO              SECTION.                                  
148100                                                                          
148500     MOVE LEV-IDFAKT            TO W-IDFAKT-L5                            
148800     MOVE REQU-IDDISTR-KEY      TO W-IDDISTR-L5                           
149000     MOVE REQU-IDKUNDNR-KEY     TO W-IDKUNDNR-L5                          
149100     MOVE LEV-IDKUNDRF          TO W-IDKUNDRF-L5                          
149200     MOVE LEV-IDKOLLI           TO W-IDKOLLI-L5                           
149300     MOVE LEV-IDARTNR           TO W-IDARTNR-L5                           
149400                                                                          
149500     PERFORM IMS-GU-WDL501                                                
149600     IF SEGMENT-FINNS                                                     
149700        PERFORM IMS-GNP-WDL511                                            
149800        IF SEGMENT-FINNS                                                  
149900          MOVE FAKC-IDPRODNR TO W-IDPRODNR-L5                             
150000          PERFORM IMS-GNP-WDL521                                          
150100        END-IF                                                            
150200     END-IF                                                               
150300     IF SEGMENT-FINNS                                                     
150400        MOVE FAKL-KVBEART-Q     TO L16110-KVBEART-Q                       
150500        MOVE FAKL-KVLEVART      TO L16110-KVLEVART                        
150600                                                                          
150700        IF REQU-KDMATT = 'U'                                              
150800          COMPUTE WS-VKORDBTO = FAKC-VKORDBTO-KOLLI *                     
150900                                CONV-KG-TO-LB                             
151000          MOVE WS-VKORDBTO      TO L16110-VKORDBTO-KOLLI                  
151100          COMPUTE WS-VKORDNTO = FAKC-VKORDNTO-KOLLI *                     
151200                                CONV-KG-TO-LB                             
151300          MOVE WS-VKORDNTO      TO L16110-VKORDNTO-KOLLI                  
151400        ELSE                                                              
151500          MOVE FAKC-VKORDBTO-KOLLI TO L16110-VKORDBTO-KOLLI               
151600          MOVE FAKC-VKORDNTO-KOLLI TO L16110-VKORDNTO-KOLLI               
151700        END-IF                                                            
151800                                                                          
151900        MOVE FAKC-KDORDKL       TO L16110-KDORDKL                         
152000        MOVE FAKC-KDKOLLI       TO W-KDKOLLI                              
152100                                                                          
152200        IF FAK-FLDIRLEV          = JA                                     
152300            MOVE YES            TO L16110-FLDIRLEV                        
152400        ELSE                                                              
152500          MOVE FAK-FLDIRLEV       TO L16110-FLDIRLEV                      
152600        END-IF                                                            
152700                                                                          
152800        MOVE FAKL-IDUSER-PACK   TO L16110-IDUSER-PACK                     
152900        MOVE FAKC-IDPRODNR      TO L16110-IDPRODNR                        
153000        MOVE FAKL-KVORDRAD      TO L16110-KVORDRAD                        
153100        MOVE FAKL-IDUSER-OREG   TO L16110-IDUSER-OREG                     
153201        MOVE FAKC-TIFAKT        TO L16110-TIREGDAT                        
153300        MOVE +0                 TO L16110-VKORDNTO-TOT                    
153400     ELSE                                                                 
153500        MOVE ZERO               TO L16110-KVBEART-Q                       
153600                                   L16110-KVLEVART                        
153700                                   L16110-VKORDBTO-KOLLI                  
153800                                   L16110-VKORDNTO-KOLLI                  
153900                                   L16110-VKORDNTO-TOT                    
154000                                   L16110-VKTARA                          
154100                                   L16110-KDORDKL                         
154200                                   L16110-KVORDRAD                        
154300                                   L16110-IDPRODNR                        
154400                                   L16110-TIREGDAT                        
154500        MOVE SPACE              TO L16110-FLDIRLEV                        
154600                                   L16110-IDUSER-PACK                     
154700                                   L16110-IDUSER-OREG                     
154800                                                                          
154900     END-IF                                                               
155000*    CALL ABEND                                                           
155100     PERFORM IMS-GET-EMBB01                                               
155200     IF SEGMENT-FINNS                                                     
155300       IF REQU-KDMATT = 'U'                                               
155400         COMPUTE WS-VKTARA = EMB-VKTARA * CONV-KG-TO-LB                   
155500         END-COMPUTE                                                      
155600         MOVE WS-VKTARA           TO L16110-VKTARA                        
155700       ELSE                                                               
155800         MOVE EMB-VKTARA          TO L16110-VKTARA                        
155900       END-IF                                                             
156000     ELSE                                                                 
156100       MOVE +0                    TO L16110-VKTARA                        
156200     END-IF                                                               
156300     .                                                                    
156400     EJECT                                                                
156500                                                                          
156600 ID-FIXA-ART-INFO               SECTION.                                  
156700                                                                          
156800     MOVE +0                    TO WS-KVOKS-TOT-CDC                       
156900     MOVE REQU-IDARTNR-KEY      TO W-IDARTNR                              
157000                                                                          
157100     PERFORM IMS-GET-ARTM-WDK9                                            
157200     IF SEGMENT-FINNS                                                     
157300       COMPUTE WS-KVOKS-TOT-CDC = ART-KVOKS-BULK +                        
157400                                  ART-KVOKS-DAG  +                        
157500                                  ART-KVOKS-VOR                           
157600     END-IF                                                               
157700                                                                          
157800     PERFORM IMS-GU-WLARTC01                                              
157900                                                                          
158000     MOVE ART-REKSIFFR          TO L16110-REKSIFFR                        
158100     MOVE ART-IDFKNGRP          TO L16110-IDFKNGRP                        
158200     MOVE ART-KDPRODSL          TO L16110-KDPRODSL                        
158300                                                                          
158400     PERFORM S20-READ-ARTC11-WDK712-22                                    
158500                                                                          
158600     COMPUTE W-KVPB-TOT       =  CLAG-KVPB-SEP +                          
158700                                 CLAG-KVPB-SATS +                         
158800                                 CLAG-KVPB-TPO                            
158900                                                                          
159000     MOVE W-KVPB-TOT            TO L16110-KVPB-TOT                        
159100     MOVE CLAG-KDERS            TO L16110-KDERS                           
159200     MOVE WS-VKART              TO L16110-VKART                           
159300     MOVE CLAG-PRARTBTO-EXP     TO L16110-PRARTBTO-EXP                    
159400     IF DCS-NDC-NA              OR                                        
159500        DCS-NDC-CN              OR                                        
159600       (DCS-NDC-PF AND DCS-INDIA) OR                                      
159700       (DCS-NDC-PF AND DCS-KOREA) OR                                      
159800       (DCS-NDC-PF AND DCS-MALAYSIA)                                      
159900       CONTINUE                                                           
160000     ELSE                                                                 
160100       MOVE WS-IDANSK          TO L16110-IDANSK                           
160200     END-IF                                                               
160300                                                                          
160400     IF DCS-CDC OR DCS-DDC                                                
160500       MOVE CLAG-ADLAGOMR       TO L16110-ADLAGOMR                        
160600       MOVE CLAG-ADGANG         TO L16110-ADGANG                          
160700       MOVE CLAG-ADPLATS        TO L16110-ADPLATS                         
160800       MOVE CLAG-PRINK          TO L16110-PRINK                           
160900       MOVE CLAG-TIINVDAT       TO L16110-TIINVDAT                        
161000       MOVE CLAG-KVINVS         TO L16110-KVINVS                          
161100       COMPUTE WS-KVDISP = CLAG-KVLS   -                                  
161200                           CLAG-KVRESS -                                  
161300                           CLAG-KVUTRS -                                  
161400                           WS-KVOKS-TOT-CDC                               
161500       MOVE WS-KVDISP           TO L16110-KVLS                            
161600     ELSE                                                                 
161700       IF (DCS-SDC) OR                                                    
161800           DCS-NDC-PF                                                     
161900         MOVE CLAG-PRINK        TO L16110-PRINK                           
162000       END-IF                                                             
162100       PERFORM IMS-GU-WDK711                                              
162200         IF SEGMENT-FINNS                                                 
162300           MOVE SLAG-ADLAGOMR   TO L16110-ADLAGOMR                        
162400           MOVE SLAG-ADGANG     TO L16110-ADGANG                          
162500           MOVE SLAG-ADPLATS    TO L16110-ADPLATS                         
162600           MOVE SLAG-TIINVDAT   TO L16110-TIINVDAT                        
162700           MOVE SLAG-KVINVS     TO L16110-KVINVS                          
162800           COMPUTE WS-KVDISP     = SLAG-KVLS -                            
162900                                   SLAG-KVUTRS                            
163000           MOVE WS-KVDISP       TO L16110-KVLS                            
163100           IF DCS-NDC-NA             OR                                   
163200              DCS-NDC-CN             OR                                   
163300             (DCS-NDC-PF AND DCS-INDIA) OR                                
163400             (DCS-NDC-PF AND DCS-KOREA) OR                                
163500             (DCS-NDC-PF AND DCS-MALAYSIA)                                
163600             MOVE SLAG-KVPB-REF TO L16110-KVPB-TOT                        
163700             MOVE SLAG-PRAVCOST TO L16110-PRINK                           
163800             MOVE SLAG-IDPERSON-BUY                                       
163900                                TO L16110-IDANSK                          
164000           END-IF                                                         
164100         ELSE                                                             
164200           MOVE +0              TO L16110-ADLAGOMR                        
164300                                   L16110-ADGANG                          
164400                                   L16110-ADPLATS                         
164500                                   L16110-TIINVDAT                        
164600                                   L16110-KVINVS                          
164700                                   L16110-KVLS                            
164800                                   L16110-KVPB-TOT                        
164900                                   L16110-PRINK                           
165000         END-IF                                                           
165100     END-IF                                                               
165200                                                                          
165300     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
165400     IF DCS-UNICODE-IDSKYLT                                               
165500        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
165600     ELSE                                                                 
165700        MOVE '278 '             TO TRAUTF8-KDCP                           
165800     END-IF                                                               
165900     PERFORM IMS-GU-WLBENA11                                              
166000     IF SEGMENT-FINNS                                                     
166100       MOVE TEXT-BEART          TO TRAUTF8-TECONV-FROM                    
166200     ELSE                                                                 
166300       MOVE WS-CP-EBCDIC        TO TRAUTF8-KDCP                           
166400       MOVE SPACE               TO TRAUTF8-TECONV-FROM                    
166500     END-IF                                                               
166600                                                                          
166700* -- STRIP SPACE OR CONVERT TO UNICODE                                    
166800     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
166900                                                                          
167000* -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                           
167100     MOVE TRAUTF8-TECONV-TO     TO L16110-BEART                           
167200                                                                          
167300     .                                                                    
167400     EJECT                                                                
167500 S10-HAMTA-KDVALISO   SECTION.                                            
167600                                                                          
167700     PERFORM IMS-GU-WLKREE01                                              
167800     IF SEGMENT-FINNS                                                     
167900       MOVE ANM-KDVALISO         TO RESP-KDVALISO                         
178710                                    WS-KDVALISO-SPAR                      
178711       IF (ANM-KDVALISO = SPACE) OR IDFTG-CN OR IDFTG-IN OR               
178712          IDFTG-KR OR IDFTG-TR OR IDFTG-MY OR                             
178713          IDFTG-MX OR IDFTG-BR OR IDFTG-ZA                                
178714         PERFORM IMS-GU-GMTA-WDB201                                       
178715         IF SEGMENT-FINNS                                                 
178716           CONTINUE                                                       
178717         ELSE                                                             
178718           PERFORM IMS-GET-WDB201                                         
178719         END-IF                                                           
178720         MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                       
178721         MOVE WS-IDFTG           TO W-WDB1-IDFTG                          
178722         PERFORM IMS-GU-WDB1-WDB101                                       
178723         IF SEGMENT-FINNS                                                 
178724           IF DIST79-DEALER-PRICE OR                                      
178725              IDFTG-CN            OR                                      
178726              IDFTG-IN            OR                                      
178727              IDFTG-KR            OR                                      
178728              IDFTG-TR            OR                                      
178729              IDFTG-MY            OR                                      
178730              IDFTG-MX            OR                                      
178731              IDFTG-BR            OR                                      
178732              IDFTG-ZA                                                    
178733                                                                          
178734**-- HÅRDKODA ENLIGT BOSSE H EFTERSOM BET-KDVALISO = SEK (5131)           
178735**-- FÖR KINA OCH VIPS FAKTURERAR I CNY.                                  
178736**-- INDIEN FAKTURERAR I INR.                                             
178737             IF IDFTG-CN                                                  
178738               MOVE 'CNY'          TO RESP-KDVALISO                       
178739             ELSE                                                         
178740               IF IDFTG-IN                                                
178741                 MOVE 'INR'        TO RESP-KDVALISO                       
178742               ELSE                                                       
178743                 IF IDFTG-KR                                              
178744                   MOVE 'KRW'      TO RESP-KDVALISO                       
178745                 ELSE                                                     
178746                   IF IDFTG-TR                                            
178747                     MOVE 'TRY'      TO RESP-KDVALISO                     
178748                   ELSE                                                   
178749                     IF IDFTG-MY                                          
178750                       MOVE 'MYR'      TO RESP-KDVALISO                   
178751                     ELSE                                                 
178752                       IF IDFTG-MX                                        
178753                         MOVE 'MXN'      TO RESP-KDVALISO                 
178754                       ELSE                                               
178755                         IF IDFTG-BR                                      
178756                           MOVE 'BRL'      TO RESP-KDVALISO               
178757                         ELSE                                             
178758                           IF IDFTG-ZA                                    
178759                             MOVE 'ZAR'      TO RESP-KDVALISO             
178760                           ELSE                                           
178761                              MOVE BET-KDVALISO TO RESP-KDVALISO          
178762                         END-IF                                           
178763                       END-IF                                             
178764                     END-IF                                               
178765                   END-IF                                                 
178766                 END-IF                                                   
178767               END-IF                                                     
178768             END-IF                                                       
178769           ELSE                                                           
178770             MOVE 'SEK'            TO RESP-KDVALISO                       
178771           END-IF                                                         
178772         ELSE                                                             
178773           MOVE SPACE            TO RESP-KDVALISO                         
178774         END-IF                                                           
178775       END-IF                                                             
178776     ELSE                                                                 
178777       MOVE SPACE                TO RESP-KDVALISO                         
178778     END-IF                                                               
178779     .                                                                    
178780     EJECT                                                                
178781*    --- DISPATCHER SECTIONS                                              
178782 S17-FETCH-REQUEST-ARGUMENT SECTION.                                      
178783                                                                          
178784     MOVE 'GETARG'               TO SUB-KDFUNC                            
178785     MOVE  'CARPARTS.LDC.DISCRLINEDETAIL'     TO SUB-ADDISPABS            
178786     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
178787                                                                          
178788     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
178789                                                                          
178790     IF SUB-KDRC > 0                                                      
178791       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
178792       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
178793       DELIMITED BY SIZE INTO ERROR-TEXT                                  
178794       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
178795     END-IF                                                               
178796     .                                                                    
178797     SKIP3                                                                
178798 S18-RETURN-RESPONSE SECTION.                                             
178799                                                                          
178800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
178801     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
178802                                                                          
178803     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
178804                                                                          
178805     IF SUB-KDRC > 0                                                      
178806       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
178807       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
178808       DELIMITED BY SIZE INTO ERROR-TEXT                                  
178809       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
178810     END-IF                                                               
178811     .                                                                    
178812     EJECT                                                                
178813                                                                          
178814 S20-READ-ARTC11-WDK712-22  SECTION.                                      
178815                                                                          
178816     MOVE ZERO TO WS-IDANSK                                               
178817                  WS-VKART                                                
178818                  WS-VLARTNTO                                             
178819                                                                          
178820     PERFORM IMS-GNP-WLARTC11                                             
178821     IF SEGMENT-FINNS                                                     
178822        MOVE CLAG-IDANSK    TO WS-IDANSK                                  
178830        MOVE CLAG-VKART     TO WS-VKART                                   
178900        MOVE CLAG-VLARTNTO  TO WS-VLARTNTO                                
179000     END-IF                                                               
179100                                                                          
179200     PERFORM IMS-GU-WDK722                                                
179300     IF SEGMENT-FINNS                                                     
179400        MOVE XLAG-IDANSK    TO WS-IDANSK                                  
179500     END-IF                                                               
179600                                                                          
179700     SEARCH ALL DC-LAND                                                   
179800        AT END                                                            
179900           MOVE SPACE          TO W-IDLAND                                
180000        WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC                             
180100           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
180200                               TO W-IDLAND                                
180300     END-SEARCH                                                           
180400     PERFORM IMS-GU-WDK712                                                
180500     IF SEGMENT-FINNS                                                     
180600        MOVE LART-VKART     TO WS-VKART                                   
180700        MOVE LART-VLARTNTO  TO WS-VLARTNTO                                
180800     END-IF                                                               
180900                                                                          
181000     .                                                                    
181100     EJECT                                                                
181200                                                                          
181300* --- IMS SEKTIONER ---                                                   
181400                                                                          
181500 IMS-GU-WLKREE01               SECTION.                                   
181600                                                                          
181700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
181800          DELIMITED BY SIZE INTO SSA1                                     
181900     MOVE '  GE'           TO GODK-STATUSKODER                            
182000     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
182100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
182200     PERFORM IMS-STATUSKONTROLL                                           
182300     .                                                                    
182400                                                                          
182500 IMS-GET-WLKREE01-KVAL          SECTION.                                  
182600                                                                          
182700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
182800          DELIMITED BY SIZE INTO SSA1                                     
182900     MOVE '  GE'           TO GODK-STATUSKODER                            
183000     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1                     
183100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
183200     PERFORM IMS-STATUSKONTROLL                                           
183300     .                                                                    
183400                                                                          
183500 IMS-GET-KREE11-GNP             SECTION.                                  
183600                                                                          
183700     MOVE 'WLKREE11 ' TO SSA1                                             
183800     MOVE '  GE' TO GODK-STATUSKODER                                      
183900     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
184000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
184100     PERFORM IMS-STATUSKONTROLL                                           
184200     .                                                                    
184300                                                                          
184400 IMS-GHU-WLKREE11               SECTION.                                  
184500                                                                          
184600     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
184700          DELIMITED BY SIZE INTO SSA1                                     
184800     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
184900          DELIMITED BY SIZE INTO SSA2                                     
185000     MOVE '  GE'           TO GODK-STATUSKODER                            
185100     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1 SSA2                
185200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
185300     PERFORM IMS-STATUSKONTROLL                                           
185400     .                                                                    
185500                                                                          
185600 IMS-REPL-KREE                  SECTION.                                  
185700                                                                          
185800     MOVE '    '           TO GODK-STATUSKODER                            
185900     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
186000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
186100     PERFORM IMS-STATUSKONTROLL                                           
186200     .                                                                    
186300     EJECT                                                                
186400                                                                          
186500                                                                          
186600 IMS-GNP-WLKREE21               SECTION.                                  
186700                                                                          
186800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
186900            DELIMITED BY SIZE INTO SSA1                                   
187000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
187100            DELIMITED BY SIZE INTO SSA2                                   
187200     MOVE 'WLKREE21 ' TO SSA3                                             
187300     MOVE '  GE' TO GODK-STATUSKODER                                      
187400     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3          
187500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
187600     PERFORM IMS-STATUSKONTROLL                                           
187700     .                                                                    
187800     EJECT                                                                
187900 IMS-GN-WLKREI-SEQ              SECTION.                                  
188000                                                                          
188100     STRING 'WLKREI01(WDA2D1KY>=' W-WDA2DSEQ-MIN-X                        
188200                    '&WDA2D1KY<=' W-WDA2DSEQ-MAX-X ')'                    
188300            DELIMITED BY SIZE INTO SSA1                                   
188400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
188500     CALL CBLTDLI USING GN KREI-PCB DLI-IO-AREA2 SSA1                     
188600     MOVE KREI-STATUS-CODE TO STATUS-WS                                   
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900     EJECT                                                                
189000 IMS-GU-WLARTC01                SECTION.                                  
189100                                                                          
189200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
189300          DELIMITED BY SIZE INTO SSA1                                     
189400     MOVE '  GE'           TO GODK-STATUSKODER                            
189500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1                     
189600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
189700     PERFORM IMS-STATUSKONTROLL                                           
189800     .                                                                    
189900                                                                          
190000 IMS-GNP-WLARTC11               SECTION.                                  
190100                                                                          
190200     MOVE 'WLARTC11'       TO SSA1                                        
190300     MOVE '  GE'           TO GODK-STATUSKODER                            
190400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
190500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
190600     PERFORM IMS-STATUSKONTROLL                                           
190700     .                                                                    
190800     EJECT                                                                
190900 IMS-GET-ARTM-WDK9              SECTION.                                  
191000                                                                          
191100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
191200     DELIMITED BY SIZE INTO SSA1                                          
191300     MOVE '  GE' TO GODK-STATUSKODER                                      
191400     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
191500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
191600     PERFORM IMS-STATUSKONTROLL                                           
191700     .                                                                    
191800     EJECT                                                                
191900 IMS-GU-WDK711                 SECTION.                                   
192000                                                                          
192100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
192200     DELIMITED BY SIZE INTO SSA1                                          
192300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
192400     DELIMITED BY SIZE INTO SSA2                                          
192500     MOVE '  GE' TO GODK-STATUSKODER                                      
192600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2            
192700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
192800     PERFORM IMS-STATUSKONTROLL                                           
192900     .                                                                    
193000     EJECT                                                                
193100 IMS-GU-WDK712                 SECTION.                                   
193200                                                                          
193300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
193400     DELIMITED BY SIZE INTO SSA1                                          
193500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
193600     DELIMITED BY SIZE INTO SSA2                                          
193700     MOVE '  GE' TO GODK-STATUSKODER                                      
193800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K712 SSA1 SSA2            
193900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
194000     PERFORM IMS-STATUSKONTROLL                                           
194100     .                                                                    
194200     EJECT                                                                
194300 IMS-GU-WDK722                 SECTION.                                   
194400                                                                          
194500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
194600     DELIMITED BY SIZE INTO SSA1                                          
194700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
194800     DELIMITED BY SIZE INTO SSA2                                          
194900     STRING 'WDK722  (KDSEGKEY =1)'                                       
195000     DELIMITED BY SIZE INTO SSA3                                          
195100     MOVE '  GE' TO GODK-STATUSKODER                                      
195200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K722                      
195300                                    SSA1 SSA2 SSA3                        
195400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSKONTROLL                                           
195600     .                                                                    
195700     EJECT                                                                
195800 IMS-GU-WDL501                  SECTION.                                  
195900                                                                          
196000     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
196100          DELIMITED BY SIZE INTO SSA1                                     
196200     MOVE '  GE'           TO GODK-STATUSKODER                            
196300     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-WDL501 SSA1                    
196400     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700     EJECT                                                                
196800 IMS-GNP-WDL511                 SECTION.                                  
196900                                                                          
197000     STRING 'WDL511  (IDGMTREF =' W-IDGMTREF-X                            
197100                    '&IDKOLLI  =' W-IDKOLLI-L5-X ')'                      
197200          DELIMITED BY SIZE INTO SSA1                                     
197300     MOVE '  GE'           TO GODK-STATUSKODER                            
197400     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL511 SSA1                   
197500     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
197600     PERFORM IMS-STATUSKONTROLL                                           
197700     .                                                                    
197800     EJECT                                                                
197900 IMS-GNP-WDL521                 SECTION.                                  
198000                                                                          
198100     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
198200          DELIMITED BY SIZE INTO SSA1                                     
198301     STRING 'WDL521  (IDARTNR  =' W-IDARTNR-L5-X ')'                      
198400          DELIMITED BY SIZE INTO SSA2                                     
198500     MOVE '  GE'           TO GODK-STATUSKODER                            
198600     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
198700     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
198800     PERFORM IMS-STATUSKONTROLL                                           
198900     .                                                                    
199000     EJECT                                                                
199100 IMS-GNP-WDL521-KLI-1ST         SECTION.                                  
199200                                                                          
199300     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
199400          DELIMITED BY SIZE INTO SSA1                                     
199500     MOVE 'WDL521  *F '       TO SSA2                                     
199600     MOVE '  GE'           TO GODK-STATUSKODER                            
199700     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
199800     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
199900     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
200100     EJECT                                                                
200200 IMS-GNP-WDL521-KLI             SECTION.                                  
200300                                                                          
200400     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
200500          DELIMITED BY SIZE INTO SSA1                                     
200600     MOVE 'WDL521  '       TO SSA2                                        
200700     MOVE '  GE'           TO GODK-STATUSKODER                            
200800     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
200900     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
201000     PERFORM IMS-STATUSKONTROLL                                           
201100     .                                                                    
201200     EJECT                                                                
204700                                                                          
204800 IMS-GU-WLBENA11                SECTION.                                  
204900                                                                          
205000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
205100            DELIMITED BY SIZE INTO SSA1                                   
205200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
205300            DELIMITED BY SIZE INTO SSA2                                   
205400     MOVE '  ' TO GODK-STATUSKODER                                        
205500     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA2 SSA1 SSA2               
205600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
205700     PERFORM IMS-STATUSKONTROLL                                           
205800     .                                                                    
205900     EJECT                                                                
206000                                                                          
206100 IMS-GET-WDP311                 SECTION.                                  
206200                                                                          
206300     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
206400            DELIMITED BY SIZE INTO SSA1                                   
206500     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
206600            DELIMITED BY SIZE INTO SSA2                                   
206700     MOVE '  GE' TO GODK-STATUSKODER                                      
206800     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-AREA-P311 SSA1 SSA2           
206900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
207000     PERFORM IMS-STATUSKONTROLL                                           
207100     .                                                                    
207200     EJECT                                                                
207300 IMS-GET-EMBB01                 SECTION.                                  
207400                                                                          
207500     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-X ')'                         
207600          DELIMITED BY SIZE INTO SSA1                                     
207700     MOVE '  GE'           TO GODK-STATUSKODER                            
207800     CALL CBLTDLI USING GU EMBB-PCB DLI-IO-AREA2 SSA1                     
207900     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
208000     PERFORM IMS-STATUSKONTROLL                                           
208100     .                                                                    
208200     EJECT                                                                
208300 IMS-GU-GMTA-WDB201               SECTION.                                
208400                                                                          
208500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
208600          DELIMITED BY SIZE INTO SSA1                                     
208700     MOVE '  GE'              TO GODK-STATUSKODER                         
208800                                                                          
208900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
209000     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
209100     PERFORM IMS-STATUSKONTROLL                                           
209200     .                                                                    
209300     EJECT                                                                
209400 IMS-GET-WDB201 SECTION.                                                  
209500                                                                          
209600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
209700                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
209800          DELIMITED BY SIZE INTO SSA1                                     
209900     MOVE '    '              TO GODK-STATUSKODER                         
210000                                                                          
210100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
210200     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
210300     PERFORM IMS-STATUSKONTROLL                                           
210400     .                                                                    
210500     EJECT                                                                
210600 IMS-GU-WDB1-WDB101              SECTION.                                 
210700                                                                          
210800     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
210900          DELIMITED BY SIZE INTO SSA1                                     
211000     MOVE '  GE'              TO GODK-STATUSKODER                         
211100                                                                          
211200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
211300     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
211400     PERFORM IMS-STATUSKONTROLL                                           
211500     .                                                                    
211600     EJECT                                                                
211700                                                                          
211800 IMS-GU-WDB601    SECTION.                                                
211900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
212000          DELIMITED BY SIZE INTO SSA1                                     
212100     MOVE '  GE' TO GODK-STATUSKODER                                      
212200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
212300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
212400     PERFORM IMS-STATUSKONTROLL                                           
212500     IF SEGMENT-SAKNAS                                                    
212600        MOVE SPACE TO DCS-KDDC                                            
212700     END-IF                                                               
212800     .                                                                    
212900     EJECT                                                                
213000 IMS-STATUSKONTROLL             SECTION.                                  
213100                                                                          
213200     SET STATUS-IX TO 1                                                   
213300     SEARCH GODK-STATUS                                                   
213400       AT END                                                             
213500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
213600         DELIMITED BY SIZE INTO FELTEXT                                   
213700         CALL FELLOG                                                      
213800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
213900         CONTINUE                                                         
214000     END-SEARCH                                                           
220000     .                                                                    
