000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6020300.                                                
000300 AUTHOR.         ELAINE CURTSSON.                                         
000400 DATE-WRITTEN.   OKTOBER 91.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KONTROLLRAPPORT BEDÖMD                                           
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR W6KVAE (W6H7)                              
001100*        PROGRAMMET UPPDATERAR WLFILC (WDR3)                              
001200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001300*        PROGRAMMET UPPDATERAR WDD8                                       
001400*        PROGRAMMET UPPDATERAR WDJ9                                       
001500*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001600*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
001700*        PROGRAMMET LÄSER      WDP3                                       
001800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001900*        PROGRAMMET LÄSER      W6LEVA (W6F1)                              
002000*        PROGRAMMET LÄSER      WDF5                                       
002100*        PROGRAMMET ANROPAR W426KVIR SOM REDIGERAR OCH SKICKAR            
002200*        TRANSAR TILL VIR.                                                
002300*        PROGRAMMET ANROPAR I VISSA FALL W602KRUP SOM AUTOMAT-            
002400*        FAKTURERAR KR.                                                   
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W6T203                                              
002800*        MID:         W6I20301                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W6O20301                                            
003200*    CHANGE LOG:                                                          
003300*      13/11/13 - REDDY RAHUL     - IR CORRECTIONS                        
003400*                                   SCR 3235165                           
003500*                                   ADDED NEW FIELDS TO ALLOW             
003600*                                   APPROVALS OF IR IF DEVIATION          
003700*                                   IS LESS THAN 150 SEK SO AS TO         
003800*                                   AVOID MANUAL CORRECTION OF            
003900*                                   STOCK BALANCE.                        
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800     SKIP3                                                                
004900 77  IDPGM                       PIC X(08)   VALUE 'W6020300'.            
005000                                                                          
005100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  YES                         PIC X       VALUE 'Y'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  FAX                         PIC X(3)    VALUE 'FAX'.                 
005800 77  TLX                         PIC X(3)    VALUE 'TLX'.                 
005900                                                                          
006000     EJECT                                                                
006100*    --- INDEX                                                            
006200 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
006300 77  IX2                         PIC S9(9)  VALUE +0    COMP SYNC.        
006400 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
006500                                                                          
006600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006700 77  WS-IDKR                     PIC X(5)       VALUE SPACE.              
006800 77  WS-KDKRSTA                  PIC 9(1)       VALUE ZERO.               
006900 77  WS-KVARBTID                 PIC S9(2)V9    VALUE 0 COMP-3.           
007000 77  WS-SUMAT                    PIC S9(7)V9(2) VALUE 0 COMP-3.           
007100 77  WS-PRARTSTD                 PIC S9(7)V9(2) VALUE 0 COMP-3.           
007200 77  WS-FLKROMK                  PIC X          VALUE SPACE.              
007300 77  WS-FLARBDEB                 PIC X          VALUE SPACE.              
007400 77  WS-FLKRGODK                 PIC X          VALUE SPACE.              
007500 77  WS-FLANNULL                 PIC X          VALUE SPACE.              
007600 77  WS-JUSTERING                PIC X(1)       VALUE 'N'.                
007700 77  WS-KDKRJUST                 PIC X(1)       VALUE SPACE.              
007800 77  WS-IDANSK                   PIC 9(3)       VALUE ZERO.               
007900 77  WS-IDINK                    PIC X(4)       VALUE SPACE.              
008000 77  WS-KDMEMO                   PIC 9(1)  VALUE ZERO.                    
008100 77  WS-KDFAX                    PIC 9(1)  VALUE ZERO.                    
008200 77  WS-BEKRANS                  PIC X(25) VALUE SPACE.                   
008300 77  WS-IDKRATLF                 PIC X(20) VALUE SPACE.                   
008400 77  WS-SPAR-KDKRSTA             PIC X(1)  VALUE SPACE.                   
008500 77  WS-SPAR-KDKVASTA            PIC X(1)  VALUE SPACE.                   
008600 77  WS-IDPROVPL-PRI             PIC X(1)  VALUE SPACE.                   
008700 77  WS-IDPROVPL-SEK             PIC X(1)  VALUE SPACE.                   
008800 77  WS-SUM-KVART                PIC S9(7) VALUE +0   COMP-3.             
008900 77  WS-SUM-ANTAL                PIC S9(7) VALUE +0   COMP-3.             
009000 77  WS-KVLS                     PIC S9(7) VALUE +0   COMP-3.             
009100 77  WS-ADATTENT                 PIC X(40) VALUE SPACE.                   
009200 77  WS-MAIL-ANS                 PIC X(60) VALUE SPACE.                   
009300 77  WS-MAIL-LEV                 PIC X(60) VALUE SPACE.                   
009400 77  WS-FLGODK-VIR               PIC X     VALUE SPACE.                   
009500 77  WS-FLEJKNTRL                PIC X     VALUE SPACE.                   
009600 77  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
009700 77  KDDISP-OK                   PIC X     VALUE 'J'.                     
009800 77  KDDISP-RET-OK               PIC X     VALUE 'J'.                     
009900 77  KDDISP-ADJ-OK               PIC X     VALUE 'J'.                     
010000 77  KDDISP-SKR-OK               PIC X     VALUE 'J'.                     
010100 77  KDHANDCO-OK                 PIC X     VALUE 'J'.                     
010200 77  ATT-ANNUL                   PIC X     VALUE 'J'.                     
010300 77  ANSK-INK-FINNS              PIC X     VALUE SPACE.                   
010400 77  WS-IDSKYLT                  PIC X(3)  VALUE SPACE.                   
010500 77  W-R40-ANTAL                 PIC S9(7) VALUE ZERO COMP-3.             
010600 77  WS-ANTAL-OK                 PIC X     VALUE 'J'.                     
010700 77  KR-ANTAL                    PIC S9(7) VALUE ZERO COMP-3.             
010800 77  WS-KVLEVANDE                PIC S9(7) VALUE ZERO COMP-3.             
010900 77  WS-FLSVS                    PIC X     VALUE 'N'.                     
011000 77  W-KVSALDO-REST              PIC S9(7) VALUE ZERO.                    
011100 77  W-ADBUFFPL                  PIC 9(5)  VALUE ZERO.                    
011200 77  WS-CHANGED-QTY              PIC 9(9)  VALUE ZERO.                    
011300 77  WS-FLKRLIM                  PIC X     VALUE SPACE.                   
011400 77  WS-SUKRLIM                  PIC S9(9)V9(2) VALUE 0 COMP-3.           
011500 77  WS-TEXT                     PIC X(24) VALUE                          
011600                             'ANNULERAD PGA LÅGT VÄRDE'.                  
011700                                                                          
011800*01  -COPY WWDCKONS                                                       
011900                                                                          
012000*01  -COPY WWDC99                                                         
012100                                                                          
012200*01  -COPY WWDCLAND                                                       
012300                                                                          
012400 01  W-MFSINF.                                                            
012500    03 FILLER                    PIC X(30).                               
012600    03 W-MFSINF-IDKR             PIC Z(05).                               
012700                                                                          
012800 01  TLXGOT                      PIC X(8) VALUE 'TLXGOT  '.               
012900 01  FAXGOT                      PIC X(8) VALUE 'FAXGOT  '.               
013000                                                                          
013100 77  DATUM                       PIC 9(6)    VALUE ZERO.                  
013200                                                                          
013300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
013400     88  ALLT-OK                             VALUE 'J'.                   
013500                                                                          
013600 77  WS-K722-SW                  PIC X       VALUE 'N'.                   
013700     88  WS-K722-EXISTS                      VALUE 'Y'.                   
013800                                                                          
013900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
014000     88  INDATA-OK                           VALUE 'J'.                   
014100     88  INDATA-FEL                          VALUE 'N'.                   
014200                                                                          
014300 77  MOT-KONTROLL-SW             PIC X       VALUE 'J'.                   
014400     88  MOT-KONTROLL-KLAR                   VALUE 'J'.                   
014500     88  MOT-KONTROLL-EJ-KLAR                VALUE 'N'.                   
014600                                                                          
014700 77  R32-KLAR-SW                 PIC X       VALUE 'J'.                   
014800     88  R32-KLAR                            VALUE 'J'.                   
014900     88  R32-EJ-KLAR                         VALUE 'N'.                   
015000                                                                          
015100 77  KVART-FEL-SW                PIC X       VALUE 'N'.                   
015200     88  KVART-FEL                           VALUE 'J'.                   
015300                                                                          
015400 77  RET-ALLT-FEL-SW             PIC X       VALUE 'N'.                   
015500     88  RET-ALLT-FEL                        VALUE 'J'.                   
015600                                                                          
015700 77  WDF11-SAKNAS-SW             PIC X       VALUE 'N'.                   
015800     88  WDF11-FEL                           VALUE 'J'.                   
015900                                                                          
016000 77  W6F1-SAKNAS-SW              PIC X       VALUE 'N'.                   
016100     88  W6F1-FEL                            VALUE 'J'.                   
016200                                                                          
016300 77  SPLITT-PF23-SW              PIC X       VALUE 'N'.                   
016400     88  PF23-PRESS                          VALUE 'J'.                   
016500                                                                          
016600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
016700     88  NYCKLAR-OK                          VALUE 'J'.                   
016800     88  NYCKLAR-FEL                         VALUE 'N'.                   
016900                                                                          
017000 77  SIFFRA-FUNNEN-SW            PIC X       VALUE 'J'.                   
017100     88  SIFFRA-FUNNEN                       VALUE 'J'.                   
017200                                                                          
017300 77  UNDER-ELLER-OVERLEVERANS-SW PIC X       VALUE 'J'.                   
017400     88  UNDER-ELLER-OVERLEVERANS            VALUE 'J'.                   
017500                                                                          
017600 77  TRYCK-PF11-SW               PIC X       VALUE 'N'.                   
017700     88  TRYCK-PF11                          VALUE 'J'.                   
017800                                                                          
017900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
018000     88  EGEN-MID                            VALUE '6203'.                
018100     88  GODK-MID                            VALUE '6202' '6203'          
018200                                                   '6204' '6205'          
018300                                                   '6206' '6207'          
018400                                                   '6208' '6209'.         
018500     88  HELP-MID                            VALUE '0551'.                
018600     EJECT                                                                
018700                                                                          
018800 01  TEST-IDLEVNR                PIC X(5).                                
018900*01  FILLER -COPY WWLEVHF -RED TEST-IDLEVNR.                              
019000                                                                          
019100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019200 01  GENERELLA-SUBPROGRAM.                                                
019300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
019700     03  W426KVIR                PIC X(8)    VALUE 'W426KVIR'.            
019800     03  W602KRUP                PIC X(8)    VALUE 'W602KRUP'.            
019900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
020000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020100     EJECT                                                                
020200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020300*01 -COPY WMEDAREA                                                        
020400     SKIP3                                                                
020500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
020600*01 -COPY WMSGINIT                                                        
020700     SKIP3                                                                
020800 01  MESSAGE-CODES.                                                       
020900     03  ERR-MOTKONTR-EJ-KLAR    PIC X(3)    VALUE '215'.                 
021000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
021100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021200     03  ERR-ANTAL-FEL           PIC X(3)    VALUE '181'.                 
021300     03  INF-INGET-ANDRAT        PIC X(3)    VALUE '414'.                 
021400     03  INF-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
021500     03  INF-FARLIGT-GODS        PIC X(3)    VALUE '021'.                 
021600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
021700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
021800     03  INF-UPPDAT-OTILLATET    PIC X(3)    VALUE '777'.                 
021900     03  INF-ANT-KR-FINNS        PIC X(3)    VALUE '224'.                 
022000     03  INF-KVAL-KR-FINNS       PIC X(3)    VALUE '225'.                 
022100     03  INF-FEL-KDDISP          PIC X(3)    VALUE '326'.                 
022200     03  INF-HANDCO-NOT-UPDATED  PIC X(3)    VALUE '327'.                 
022300     03  INF-KR-SEND             PIC X(3)    VALUE '264'.                 
022400     03  OTILL-DISP-RET          PIC X(3)    VALUE '328'.                 
022500     03  OTILL-DISP-ADJ          PIC X(3)    VALUE '329'.                 
022600     03  OTILL-DISP-SKR          PIC X(3)    VALUE '330'.                 
022700     03  FEL-ANSK-INK            PIC X(3)    VALUE '331'.                 
022800     03  QUANT-TOO-BIG           PIC X(3)    VALUE '302'.                 
022900     03  NO-ATT-CHANGE           PIC X(3)    VALUE '339'.                 
023000     03  ERR-VERNR-OVERSKRIDEN   PIC X(3)    VALUE '176'.                 
023100     03  VALUTAKOD-SAKNAS        PIC X(3)    VALUE '148'.                 
023200     03  FAX-MAIL-SAKNAS         PIC X(3)    VALUE '340'.                 
023300     03  UPD-NOT-ALLOWED         PIC X(3)    VALUE '007'.                 
023400     03  SPLITT-PRESS-PF23       PIC X(3)    VALUE '284'.                 
023500     EJECT                                                                
023600*01  -COPY WDECAREA                                                       
023700     EJECT                                                                
023800*    ---  COPYTEXT FÖR TRANS TILL W426KVIR                                
023900*01  -COPY W426KVIR                                                       
024000     EJECT                                                                
024100                                                                          
024200*    ---  COPYTEXT FÖR TRANS TILL W602KRUP                                
024300*01  -COPY W602KRUP                                                       
024400     EJECT                                                                
024500                                                                          
024600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024700*                                                                         
024800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024900     SKIP3                                                                
025000*01  MID -COPY W6I20301                                                   
025100     EJECT                                                                
025200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025300     SKIP3                                                                
025400*01  -COPY WMSGAREA                                                       
025500     EJECT                                                                
025600     03  MOD REDEFINES MSG-AREA.                                          
025700*      05  -COPY W6O20301                                                 
025800     EJECT                                                                
025900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026000     SKIP3                                                                
026100*01  -COPY WMFSAREA                                                       
026200     EJECT                                                                
026300 01  FILLER                      PIC X(16)  VALUE 'ALT1-MSG-AREA'.        
026400 01  ALT1-MSG-IO-AREA.                                                    
026500                                                                          
026600  03     ALT1-LL                 PIC S9(4) COMP SYNC.                     
026700  03     ALT1-Z1                 PIC X(1)  VALUE LOW-VALUE.               
026800  03     ALT1-Z2                 PIC X(1)  VALUE LOW-VALUE.               
026900  03     ALT1-TRANSKOD           PIC X(8)  VALUE 'W2T191X '.              
027000  03     ALT1-IDTRANS            PIC X(4)  VALUE '6203'.                  
027100  03     ALT1-SPRAK              PIC X(1).                                
027200* 03     MID -COPY W2I19101   -PRE ALT1-                                  
027300     EJECT                                                                
027400*01  -COPY WMSGKOM                                                        
027500     EJECT                                                                
027600 01  FILLER                      PIC X(16)  VALUE 'MSG/KOM-AREA'.         
027700     SKIP3                                                                
027800*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
027900     EJECT                                                                
028000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028100*                                                                         
028200     EJECT                                                                
028300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028400     SKIP3                                                                
028500 01  NYCKLAR-TILL-DLI.                                                    
028600     03  W-IDDC-X.                                                        
028700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
028800     03  W-IDDC-B6-X.                                                     
028900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
029000     03  W-IDKR-X.                                                        
029100         05  W-IDKR              PIC 9(5)    VALUE ZERO.                  
029200     03  W-IDARTNR-X.                                                     
029300         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
029400     03  W-KDSEGKEY-X.                                                    
029500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
029600     03  W-IDSKYLT-X.                                                     
029700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
029800     03  W-IDLOPNRM-X.                                                    
029900         05  W-IDLOPNRM          PIC S9(9)   COMP-3 VALUE ZERO.           
030000     03  W-W6H7CSEQ-X.                                                    
030100         05  W-IDLOPNRM-CSEQ     PIC S9(9)   COMP-3 VALUE ZERO.           
030200         05  W-DAAVSDAT-CSEQ     PIC 9(8)    VALUE ZERO.                  
030300     03  W-IDLEVNR-X.                                                     
030400         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
030500     03  W-IDLAND-X.                                                      
030600         05  W-IDLAND            PIC  X(2)   VALUE SPACE.                 
030700     03  W-IDLEVG-X.                                                      
030800         05  W-IDLEVG            PIC S9(5)   COMP-3 VALUE ZERO.           
030900     03  W-KDARBTYP-X.                                                    
031000         05  W-KDARBTYP          PIC X(8)    VALUE 'QUAL    '.            
031100     03  W-IDPERSON-X.                                                    
031200         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
031300     03  W-W6GX-6101-KEY-X.                                               
031400         05  W-6101-IDHTYP       PIC X(4)    VALUE '6101'.                
031500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
031600     03  W-W6GX-6102-KEY-X.                                               
031700         05  W-IDPROVPL          PIC 9(1).                                
031800         05  W-KDPROVPL          PIC X(1).                                
031900     SKIP2                                                                
032000*    --- STATUS-KOD FRÅN IMS                                              
032100 01  STATUS-WS                   PIC XX.                                  
032200     88  SEGMENT-FINNS                       VALUE '  '.                  
032300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032500     SKIP2                                                                
032600 01  GODK-STATUSKODER.                                                    
032700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032800     SKIP3                                                                
032900 01  SSA1                        PIC X(64).                               
033000 01  SSA2                        PIC X(64).                               
033100     SKIP3                                                                
033200*    --- IMS FUNKTIONSKODER                                               
033300*01  -COPY W0003                                                          
033400     EJECT                                                                
033500*    ---  DLI INPUT-OUTPUT AREA                                           
033600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
033700     SKIP3                                                                
033800 01  DLI-IO-AREA1.                                                        
033900     03  IO-AREA1                PIC X(395)  VALUE SPACE.                 
034000     03  W6KVAE01 REDEFINES IO-AREA1.                                     
034100*        05  -COPY W6H701                                                 
034200     SKIP3                                                                
034300     03  W6KVAE13 REDEFINES IO-AREA1.                                     
034400*        05  -COPY W6H713                                                 
034500     SKIP3                                                                
034600 01  DLI-IO-W6H714.                                                       
034700     03  W6KVAE14.                                                        
034800*        05  -COPY W6H714                                                 
034900     EJECT                                                                
035000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
035100     SKIP3                                                                
035200 01  DLI-IO-AREA2.                                                        
035300     03  IO-AREA2                PIC X(120)   VALUE SPACE.                
035400     03  WLBENA11 REDEFINES IO-AREA2.                                     
035500*        05  -COPY WDD311                                                 
035600     EJECT                                                                
035700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
035800     SKIP3                                                                
035900 01  DLI-IO-AREA3.                                                        
036000     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
036100     03  WLLEVA01 REDEFINES IO-AREA3.                                     
036200*        05  -COPY WDF101                                                 
036300     SKIP3                                                                
036400     03 WLLEVA11 REDEFINES IO-AREA3.                                      
036500*        05 -COPY WDF102                                                  
036600     EJECT                                                                
036700     03  WLLEVA14 REDEFINES IO-AREA3.                                     
036800*        05  -COPY WDF106                                                 
036900     EJECT                                                                
037000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
037100     SKIP3                                                                
037200 01  DLI-IO-AREA4.                                                        
037300     03  IO-AREA4                PIC X(900)  VALUE SPACE.                 
037400     SKIP3                                                                
037500     03  WLARTC01 REDEFINES IO-AREA4.                                     
037600*        05  -COPY WDK601  -PRE ARTC-                                     
037700     SKIP3                                                                
037800     03  WLARTC11 REDEFINES IO-AREA4.                                     
037900*        05  -COPY WDK611  -PRE ARTC-                                     
038000     EJECT                                                                
038100 01  DLI-IO-AREA-WDK711.                                                  
038200*    03  -COPY WDK711                                                     
038300     EJECT                                                                
038400 01  DLI-IO-AREA-WDK722.                                                  
038500*    03  -COPY WDK722                                                     
038600     EJECT                                                                
038700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD801'.                      
038800     EJECT                                                                
038900 01  DLI-IO-WDD801.                                                       
039000*    03  -COPY WDD801                                                     
039100     EJECT                                                                
039200                                                                          
039300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD811'.                      
039400 01  DLI-IO-WDD811.                                                       
039500*    03  -COPY WDD811                                                     
039600     EJECT                                                                
039700                                                                          
039800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ901'.                      
039900 01  DLI-IO-WDJ901.                                                       
040000*    03  -COPY WDJ901                                                     
040100     EJECT                                                                
040200                                                                          
040300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ911'.                      
040400 01  DLI-IO-WDJ911.                                                       
040500*    03  -COPY WDJ911                                                     
040600     EJECT                                                                
040700                                                                          
040800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P311'.         
040900     SKIP3                                                                
041000 01  DLI-IO-P311.                                                         
041100*    03  -COPY WDP311                                                     
041200     EJECT                                                                
041300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
041400     SKIP3                                                                
041500 01  DLI-IO-AREA8.                                                        
041600     03  IO-AREA8                PIC X(300)  VALUE SPACE.                 
041700     03  W6LEVA01 REDEFINES IO-AREA8.                                     
041800*        05  -COPY W6F101                                                 
041900     EJECT                                                                
042000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA9'.        
042100     SKIP3                                                                
042200 01  DLI-IO-AREA9.                                                        
042300     03  IO-AREA9                PIC X(286)  VALUE SPACE.                 
042400     03  W6UPFA01 REDEFINES IO-AREA9.                                     
042500*        05  -COPY W6L101                                                 
042600     03  W6UPFA11 REDEFINES IO-AREA9.                                     
042700*        05  -COPY W6L111                                                 
042800     03  W6UPFA12 REDEFINES IO-AREA9.                                     
042900*        05  -COPY W6L112                                                 
043000                                                                          
043100     EJECT                                                                
043200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA10'.        
043300     SKIP3                                                                
043400 01  DLI-IO-AREA10.                                                       
043500     03  IO-AREA10               PIC X(27)   VALUE SPACE.                 
043600     03  W6KVAH12 REDEFINES IO-AREA10.                                    
043700*        05  -COPY W6D201                                                 
043800     EJECT                                                                
043900     03  W6KVAH12 REDEFINES IO-AREA10.                                    
044000*        05  -COPY W6D212                                                 
044100     EJECT                                                                
044200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA11'.        
044300     SKIP3                                                                
044400 01  DLI-IO-AREA11.                                                       
044500     03  IO-AREA11               PIC X(150)  VALUE SPACE.                 
044600     03  W6INLA11 REDEFINES IO-AREA11.                                    
044700*        05  -COPY W6D111                                                 
044800     EJECT                                                                
044900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA21'.        
045000     SKIP3                                                                
045100 01  DLI-IO-AREA21.                                                       
045200     03  IO-AREA21               PIC X(150)  VALUE SPACE.                 
045300     03  W6INLA21 REDEFINES IO-AREA21.                                    
045400*        05  -COPY W6D121                                                 
045500     EJECT                                                                
045600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA12'.        
045700     SKIP3                                                                
045800 01  DLI-IO-AREA12.                                                       
045900     03  IO-AREA12               PIC X(64)  VALUE SPACE.                  
046000     03  W6PROA01 REDEFINES IO-AREA12.                                    
046100*        05  -COPY W6GX6102                                               
046200     EJECT                                                                
046300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA13'.        
046400     SKIP3                                                                
046500 01  DLI-IO-AREA13.                                                       
046600     03  IO-AREA13               PIC X(516) VALUE SPACE.                  
046700     03  W6KVAE01 REDEFINES IO-AREA13.                                    
046800*        05  -COPY W6H701  -PRE CSEQ-                                     
046900     EJECT                                                                
047000                                                                          
047100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
047200 01  DLI-IO-WDB601.                                                       
047300*    03  -COPY WDB601                                                     
047400     EJECT                                                                
047500 01  DLI-IO-AREA-WDF502.                                                  
047600     03  WDF502.                                                          
047700*        05  -COPY WDF502                                                 
047800     EJECT                                                                
047900 01  FILLER                      PIC X(16)  VALUE 'MID-6193-AREA'.        
048000 01  MID-6193-AREA.                                                       
048100     03  6193-AREA               PIC X(1500)  VALUE SPACE.                
048200     SKIP3                                                                
048300     03  W6I19301 REDEFINES 6193-AREA.                                    
048400*        05  -COPY W6I19301 -PRE 6193-                                    
048500     EJECT                                                                
048600 01  FILLER                      PIC X(16)  VALUE 'MID-611D-AREA'.        
048700 01  MID-611D-AREA.                                                       
048800*    03  -COPY W6I11D01 -PRE 611D-                                        
048900     EJECT                                                                
049000 LINKAGE SECTION.                                                         
049100                                                                          
049200*01  -COPY W0009  -PRE MSG-                                               
049300     EJECT                                                                
049400*01  -COPY W0009  -PRE ALT1-                                              
049500     EJECT                                                                
049600*01  -COPY W0009  -PRE DISP-                                              
049700     EJECT                                                                
049800*01  -COPY W0009  -PRE ALT2-                                              
049900     EJECT                                                                
050000*01  -COPY W0008  -PRE USEA-                                              
050100     05  FILLER                  PIC X.                                   
050200     EJECT                                                                
050300*01  -COPY W0008  -PRE KVAE-                                              
050400     05  FILLER                  PIC X.                                   
050500     EJECT                                                                
050600*01  -COPY W0008  -PRE BENA-                                              
050700     05  FILLER                  PIC X.                                   
050800     EJECT                                                                
050900*01  -COPY W0008  -PRE LEVA-                                              
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008  -PRE WDP3-                                              
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01  -COPY W0008  -PRE ARTC-                                              
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008  -PRE WDD8-                                              
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100*01  -COPY W0008  -PRE WDJ9-                                              
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400*01  -COPY W0008  -PRE W6F1-                                              
052500     05  FILLER                  PIC X.                                   
052600     EJECT                                                                
052700*01  -COPY W0008  -PRE UPFA-                                              
052800     05  FILLER                  PIC X.                                   
052900     EJECT                                                                
053000*01  -COPY W0008  -PRE KVAH-                                              
053100     05  FILLER                  PIC X.                                   
053200     EJECT                                                                
053300*01  -COPY W0008  -PRE INLA-                                              
053400     05  FILLER                  PIC X.                                   
053500     EJECT                                                                
053600*01  -COPY W0008  -PRE PROA-                                              
053700     05  FILLER                  PIC X.                                   
053800     EJECT                                                                
053900*01  -COPY W0008  -PRE KVAI-                                              
054000     05  FILLER                  PIC X.                                   
054100     EJECT                                                                
054200 01  KOM-KOMA-PCB                PIC X.                                   
054300     EJECT                                                                
054400*01  -COPY W0008  -PRE W6H7-                                              
054500     05  FILLER                  PIC X.                                   
054600     EJECT                                                                
054700*01  -COPY W0008  -PRE WDG2-                                              
054800     05  FILLER                  PIC X.                                   
054900     EJECT                                                                
055000*01  -COPY W0008  -PRE WDK6-                                              
055100     05  FILLER                  PIC X.                                   
055200     EJECT                                                                
055300*01  -COPY W0008  -PRE LOPB-                                              
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600*01  -COPY W0008  -PRE FILC-                                              
055700     05  FILLER                  PIC X.                                   
055800     EJECT                                                                
055900*01  -COPY W0008  -PRE WDK7-                                              
056000     05  FILLER                  PIC X.                                   
056100     EJECT                                                                
056200*01  -COPY W0008  -PRE WDB6-                                              
056300     05  FILLER                  PIC X.                                   
056400     EJECT                                                                
056500*01  -COPY W0008  -PRE WDF5-                                              
056600     05  FILLER                  PIC X.                                   
056700     EJECT                                                                
056800 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB DISP-PCB ALT2-PCB             
056900     USEA-PCB                                                             
057000     KVAE-PCB BENA-PCB LEVA-PCB WDP3-PCB ARTC-PCB                         
057100     WDD8-PCB WDJ9-PCB                                                    
057200     W6F1-PCB UPFA-PCB KVAH-PCB INLA-PCB PROA-PCB KVAI-PCB                
057300     KOM-KOMA-PCB W6H7-PCB WDG2-PCB WDK6-PCB LOPB-PCB FILC-PCB            
057400     WDK7-PCB WDB6-PCB WDF5-PCB.                                          
057500 MAIN SECTION.                                                            
057600                                                                          
057700     PERFORM IMS-GET-MSG                                                  
057800     IF SEGMENT-FINNS                                                     
057900       PERFORM A-INIT                                                     
058000       PERFORM B-KOLLA-NYCKLAR                                            
058100       IF NYCKLAR-OK                                                      
058200          IF MFS-UPDATE OR MFS-UPD-V                                      
058300             PERFORM G-KOLLA-INPUT                                        
058400             IF INDATA-OK                                                 
058500                IF WS-FLGODK-VIR = JA                                     
058600** FÖRHANDSUTSKICK TILL LEVERANTÖR AV ÄNNU EJ GODKÄND KR.                 
058700** MID-FLKRTOVIR = JA. ENDAST UPPDATERING AV ANVARIGS NAMN OCH            
058800** TELEFONNUMMER, SAMT ATTENTION.                                         
058900                   IF NDC-CN OR NDC-US                                    
059000                     CONTINUE                                             
059100                   ELSE                                                   
059200                     PERFORM I-SKICKA-EJ-GODK-KR-TILL-VIR                 
059300                   END-IF                                                 
059400                ELSE                                                      
059500                   PERFORM H-UPPDATERA                                    
059600                END-IF                                                    
059700             END-IF                                                       
059800          ELSE                                                            
059900             IF MFS-FIRST                                                 
060000                CONTINUE                                                  
060100             ELSE                                                         
060200                PERFORM E-SAMMA-SIDA                                      
060300             END-IF                                                       
060400          END-IF                                                          
060500          IF ALLT-OK                                                      
060600             PERFORM F-LAES-VISA-INFO                                     
060700          END-IF                                                          
060800       END-IF                                                             
060900       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O20301 + 4                      
061000       PERFORM IMS-INSERT-MSG                                             
061100     END-IF                                                               
061200     MOVE ZERO TO RETURN-CODE                                             
061300     GOBACK                                                               
061400     .                                                                    
061500     EJECT                                                                
061600 A-INIT SECTION.                                                          
061700                                                                          
061800     IF MSG-DUBBLA-TRANSKODER                                             
061900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I20301                 
062000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
062100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
062200     ELSE                                                                 
062300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I20301                  
062400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
062500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
062600     END-IF                                                               
062700     MOVE '1'              TO ALT1-SPRAK                                  
062800                                                                          
062900     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
063000     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
063100     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
063200                                                                          
063300     MOVE LOW-VALUE        TO MSG-AREA                                    
063400     MOVE 'W6O203N1'       TO MFS-IDMOD                                   
063500     MOVE '6203'           TO MOD-IDTRANS                                 
063600     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
063700                                                                          
063800     IF EGEN-MID OR HELP-MID                                              
063900        CONTINUE                                                          
064000     ELSE                                                                 
064100        MOVE SPACE TO MFS-KDTRTYP                                         
064200        MOVE '7' TO MFS-IDPFK                                             
064300        MOVE ALL '+' TO MID-FAX-TLX                                       
064400     END-IF                                                               
064500                                                                          
064600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
064700     MOVE '013'             TO MSGI-KDCALL                                
064800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
064900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
065000                                                                          
065100     IF MSGI-IDLAND-SPR = 'SE'                                            
065200       MOVE 'S  ' TO MED-IDSKYLT                                          
065300       MOVE 'S  ' TO W-IDSKYLT                                            
065400       MOVE 'S  ' TO WS-IDSKYLT                                           
065500     ELSE                                                                 
065600       MOVE 'B  ' TO MED-IDSKYLT                                          
065700       MOVE 'GB ' TO W-IDSKYLT                                            
065800       MOVE 'GB ' TO WS-IDSKYLT                                           
065900     END-IF                                                               
066000                                                                          
066100     ACCEPT DATUM FROM DATE                                               
066200     PERFORM MFS-FORM-ATTR                                                
066300     .                                                                    
066400     EJECT                                                                
066500 B-KOLLA-NYCKLAR SECTION.                                                 
066600                                                                          
066700     MOVE JA TO NYCKLAR-SW                                                
066800                                                                          
066900*    -- KONTROLL AV IDKR                                                  
067000     MOVE MFS-RENSA-FAELT TO MOD-IDKR-IN                                  
067100                                                                          
067200     IF MID-IDKR-IN = ALL '+'                                             
067300       MOVE MID-IDKR-UT TO WS-IDKR                                        
067400       INSPECT WS-IDKR REPLACING LEADING SPACE BY ZERO                    
067500     ELSE                                                                 
067600       MOVE MID-IDKR-IN TO WS-IDKR                                        
067700       MOVE '7'         TO MFS-IDPFK                                      
067800       MOVE SPACE       TO MFS-KDTRTYP                                    
067900       MOVE ALL '+'     TO MID-KDMEMO                                     
068000                           MID-KDFAX                                      
068100     END-IF                                                               
068200     IF WS-IDKR NUMERIC AND WS-IDKR > ZERO                                
068300        MOVE WS-IDKR TO W-IDKR                                            
068400     ELSE                                                                 
068500        MOVE NEJ TO NYCKLAR-SW                                            
068600     END-IF                                                               
068700                                                                          
068800     IF GODK-MID OR NYCKLAR-OK                                            
068900       MOVE WS-IDKR TO MOD-IDKR-UT                                        
069000       INSPECT MOD-IDKR-UT REPLACING LEADING ZERO BY SPACE                
069100     ELSE                                                                 
069200       MOVE MFS-RENSA-FAELT TO MOD-IDKR-UT                                
069300     END-IF                                                               
069400                                                                          
069500     IF NYCKLAR-FEL                                                       
069600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
069700       CALL WMEDKONV USING MED-WMEDAREA                                   
069800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
069900       PERFORM MFS-RENSA-FAELT-IN                                         
070000       PERFORM MFS-RENSA-FAELT-UT                                         
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 E-SAMMA-SIDA SECTION.                                                    
070500                                                                          
070600     IF EGEN-MID OR HELP-MID                                              
070700                                                                          
070800       IF MID-INPUT = ALL '+'                                             
070900         PERFORM MFS-RENSA-FAELT-IN                                       
071000       ELSE                                                               
071100         MOVE JA             TO TRYCK-PF11-SW                             
071200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
071300         CALL WMEDKONV    USING MED-WMEDAREA                              
071400         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
071500         PERFORM EA-MID-INDATA-TILL-MOD                                   
071600       END-IF                                                             
071700     ELSE                                                                 
071800       PERFORM MFS-RENSA-FAELT-IN                                         
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200 EA-MID-INDATA-TILL-MOD SECTION.                                          
072300                                                                          
072400     IF MID-FLKRLFEL NOT = ALL '+'                                        
072500        MOVE MID-FLKRLFEL          TO MOD-FLKRLFEL                        
072600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRLFEL-ATTR                   
072700     ELSE                                                                 
072800        MOVE MFS-RENSA-FAELT       TO MOD-FLKRLFEL                        
072900     END-IF                                                               
073000                                                                          
073100     IF MID-KDKRATG NOT = ALL '+'                                         
073200        MOVE MID-KDKRATG           TO MOD-KDKRATG                         
073300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKRATG-ATTR                    
073400     ELSE                                                                 
073500        MOVE MFS-RENSA-FAELT       TO MOD-KDKRATG                         
073600     END-IF                                                               
073700                                                                          
073800     IF MID-FLINKANS NOT = ALL '+'                                        
073900        MOVE MID-FLINKANS          TO MOD-FLINKANS                        
074000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLINKANS-ATTR                   
074100     ELSE                                                                 
074200        MOVE MFS-RENSA-FAELT       TO MOD-FLINKANS                        
074300     END-IF                                                               
074400                                                                          
074500     IF MID-ADATTENT NOT = ALL '+'                                        
074600        MOVE MID-ADATTENT          TO MOD-ADATTENT                        
074700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADATTENT-ATTR                   
074800     ELSE                                                                 
074900        MOVE MFS-RENSA-FAELT       TO MOD-ADATTENT                        
075000     END-IF                                                               
075100                                                                          
075200     IF MID-FLKRTOVIR NOT = ALL '+'                                       
075300        MOVE MID-FLKRTOVIR         TO MOD-FLKRTOVIR                       
075400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRTOVIR-ATTR                  
075500     ELSE                                                                 
075600        MOVE MFS-RENSA-FAELT       TO MOD-FLKRTOVIR                       
075700     END-IF                                                               
075800                                                                          
075900     IF MID-FLEJKNTRL NOT = ALL '+'                                       
076000        IF MID-FLEJKNTRL = JA OR YES                                      
076100           MOVE MID-FLEJKNTRL      TO MOD-FLEJKNTRL                       
076200        ELSE                                                              
076300           MOVE MFS-RENSA-FAELT    TO MOD-FLEJKNTRL                       
076400        END-IF                                                            
076500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLEJKNTRL-ATTR                  
076600     ELSE                                                                 
076700        MOVE MFS-RENSA-FAELT       TO MOD-FLEJKNTRL                       
076800     END-IF                                                               
076900                                                                          
077000     IF MID-KVARBTID-IN NOT = ALL '+'                                     
077100        MOVE MID-KVARBTID-IN       TO MOD-KVARBTID-IN                     
077200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVARBTID-IN-ATTR                
077300     ELSE                                                                 
077400        MOVE MFS-RENSA-FAELT       TO MOD-KVARBTID-IN                     
077500     END-IF                                                               
077600                                                                          
077700     IF MID-TEKRSPEC-ATID NOT = ALL '+'                                   
077800        MOVE MID-TEKRSPEC-ATID     TO MOD-TEKRSPEC-ATID                   
077900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKRSPEC-ATID-ATTR              
078000     ELSE                                                                 
078100        MOVE MFS-RENSA-FAELT       TO MOD-TEKRSPEC-ATID                   
078200     END-IF                                                               
078300                                                                          
078400     IF MID-SUOMK-IN NOT = ALL '+'                                        
078500        MOVE MID-SUOMK-IN          TO MOD-SUOMK-IN                        
078600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUOMK-IN-ATTR                   
078700     ELSE                                                                 
078800        MOVE MFS-RENSA-FAELT       TO MOD-SUOMK-IN                        
078900     END-IF                                                               
079000                                                                          
079100     IF MID-TEKRSPEC-OMK NOT = ALL '+'                                    
079200        MOVE MID-TEKRSPEC-OMK      TO MOD-TEKRSPEC-OMK                    
079300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKRSPEC-OMK-ATTR               
079400     ELSE                                                                 
079500        MOVE MFS-RENSA-FAELT       TO MOD-TEKRSPEC-OMK                    
079600     END-IF                                                               
079700                                                                          
079800     IF MID-SUMAT-IN NOT = ALL '+'                                        
079900        MOVE MID-SUMAT-IN          TO MOD-SUMAT-IN                        
080000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUMAT-IN-ATTR                   
080100     ELSE                                                                 
080200        MOVE MFS-RENSA-FAELT       TO MOD-SUMAT-IN                        
080300     END-IF                                                               
080400                                                                          
080500     IF MID-TEKRSPEC-MAT NOT = ALL '+'                                    
080600        MOVE MID-TEKRSPEC-MAT      TO MOD-TEKRSPEC-MAT                    
080700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKRSPEC-MAT-ATTR               
080800     ELSE                                                                 
080900        MOVE MFS-RENSA-FAELT       TO MOD-TEKRSPEC-MAT                    
081000     END-IF                                                               
081100                                                                          
081200     IF MID-FLKROMK NOT = ALL '+'                                         
081300        MOVE MID-FLKROMK           TO MOD-FLKROMK                         
081400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKROMK-ATTR                    
081500     ELSE                                                                 
081600        MOVE MFS-RENSA-FAELT       TO MOD-FLKROMK                         
081700     END-IF                                                               
081800                                                                          
081900     IF MID-FLARBDEB NOT = ALL '+'                                        
082000        MOVE MID-FLARBDEB          TO MOD-FLARBDEB                        
082100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLARBDEB-ATTR                   
082200     ELSE                                                                 
082300        MOVE MFS-RENSA-FAELT       TO MOD-FLARBDEB                        
082400     END-IF                                                               
082500                                                                          
082600     IF MID-FLKRLIM NOT = ALL '+'                                         
082700        MOVE MID-FLKRLIM           TO MOD-FLKRLIM                         
082800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRLIM-ATTR                    
082900     ELSE                                                                 
083000        MOVE MFS-RENSA-FAELT       TO MOD-FLKRLIM                         
083100     END-IF                                                               
083200                                                                          
083300     IF MID-KDPERSON NOT = ALL '+'                                        
083400        INSPECT MID-KDPERSON REPLACING LEADING ZERO BY SPACE              
083500        MOVE MID-KDPERSON          TO MOD-KDPERSON                        
083600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPERSON-ATTR                   
083700     ELSE                                                                 
083800        MOVE MFS-RENSA-FAELT       TO MOD-KDPERSON                        
083900     END-IF                                                               
084000                                                                          
084100     IF MID-IDKRATLF NOT = ALL '+'                                        
084200        MOVE MID-IDKRATLF          TO MOD-IDKRATLF                        
084300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKRATLF-ATTR                   
084400     ELSE                                                                 
084500        MOVE MFS-RENSA-FAELT       TO MOD-IDKRATLF                        
084600     END-IF                                                               
084700                                                                          
084800     IF MID-FLKRGODK NOT = ALL '+'                                        
084900        MOVE MID-FLKRGODK          TO MOD-FLKRGODK                        
085000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRGODK-ATTR                   
085100     ELSE                                                                 
085200        MOVE MFS-RENSA-FAELT       TO MOD-FLKRGODK                        
085300     END-IF                                                               
085400                                                                          
085500     IF MID-KDFAX NOT = ALL '+'                                           
085600        INSPECT MID-KDFAX REPLACING LEADING ZERO BY SPACE                 
085700        MOVE MID-KDFAX             TO MOD-KDFAX                           
085800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFAX-ATTR                      
085900     ELSE                                                                 
086000        MOVE MFS-RENSA-FAELT       TO MOD-KDFAX                           
086100     END-IF                                                               
086200                                                                          
086300     IF MID-KDMEMO NOT = ALL '+'                                          
086400        INSPECT MID-KDMEMO REPLACING LEADING ZERO BY SPACE                
086500        MOVE MID-KDMEMO            TO MOD-KDMEMO                          
086600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDMEMO-ATTR                     
086700     ELSE                                                                 
086800        MOVE MFS-RENSA-FAELT       TO MOD-KDMEMO                          
086900     END-IF                                                               
087000                                                                          
087100     IF MID-FLANN NOT = ALL '+'                                           
087200        MOVE MID-FLANN             TO MOD-FLANN                           
087300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLANN-ATTR                      
087400     ELSE                                                                 
087500        MOVE MFS-RENSA-FAELT       TO MOD-FLANN                           
087600     END-IF                                                               
087700     .                                                                    
087800     EJECT                                                                
087900 F-LAES-VISA-INFO SECTION.                                                
088000                                                                          
088100     PERFORM IMS-GU-W6KVAE01                                              
088200                                                                          
088300     IF SEGMENT-SAKNAS                                                    
088400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
088500       CALL WMEDKONV USING MED-WMEDAREA                                   
088600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
088700       PERFORM MFS-RENSA-FAELT-UT                                         
088800     ELSE                                                                 
088900       MOVE KR-FLKRGODK TO WS-FLKRGODK                                    
089000       MOVE KR-FLKROMK  TO WS-FLKROMK                                     
089100       MOVE KR-FLARBDEB TO WS-FLARBDEB                                    
089200       MOVE KR-FLANNULL TO WS-FLANNULL                                    
089300       MOVE KR-IDDC     TO WS-IDDC                                        
089400       PERFORM FA-VISA-KR-INFO                                            
089500       PERFORM FB-LAES-VISA-LEV-INFO                                      
089600       PERFORM FC-LAES-VISA-BEN-INFO                                      
089700       PERFORM FD-LAES-VISA-ANSK-INFO                                     
089800       PERFORM FE-KOLLA-OM-FARLIGT-GODS                                   
089900       PERFORM FF-LAES-VISA-KVAL-ANTAL-KR                                 
090000       PERFORM FG-LAES-VISA-MEMOID                                        
090100       PERFORM MFS-EV-SPAERRA-FAELT                                       
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 FA-VISA-KR-INFO SECTION.                                                 
090600                                                                          
090700     MOVE KR-IDARTNR           TO MOD-IDARTNR                             
090800     MOVE KR-KDKRSTA           TO MOD-KDKRSTA                             
090900     COMPUTE WS-DAREGDAT = 99999999 - KR-DAREGDAT-9KOMPL                  
091000     MOVE WS-DAREGDAT (3:6)    TO MOD-TIREGDAT                            
091100     MOVE KR-IDLEVNR           TO MOD-IDLEVNR                             
091200     MOVE KR-IDLEVG            TO MOD-IDLEVG                              
091300     MOVE KR-KVARBTID          TO MOD-KVARBTID-UT                         
091400     MOVE KR-SUOMK             TO MOD-SUOMK-UT                            
091500     MOVE KR-SUMAT             TO MOD-SUMAT-UT                            
091600     MOVE KR-FLINKANS          TO MOD-FLINKANS                            
091700     IF MID-FAX-TLX = ALL '+'                                             
091800        MOVE FAX               TO MOD-FAX-TLX                             
091900     ELSE                                                                 
092000        MOVE MFS-ROER-EJ-FAELT TO MOD-FAX-TLX                             
092100     END-IF                                                               
092200                                                                          
092300     IF TRYCK-PF11                                                        
092400        PERFORM FAB-PRESS-PF11                                            
092500     ELSE                                                                 
092600       MOVE  KR-ADATTENT      TO MOD-ADATTENT                             
092700       IF KR-FLEJKNTRL = JA                                               
092800         MOVE KR-FLEJKNTRL    TO MOD-FLEJKNTRL                            
092900       ELSE                                                               
093000         MOVE MFS-RENSA-FAELT TO MOD-FLEJKNTRL                            
093100       END-IF                                                             
093200       MOVE  KR-TEKRSPEC-ATID TO MOD-TEKRSPEC-ATID                        
093300       MOVE  KR-TEKRSPEC-OMK  TO MOD-TEKRSPEC-OMK                         
093400       MOVE  KR-TEKRSPEC-MAT  TO MOD-TEKRSPEC-MAT                         
093500       MOVE  KR-FLKROMK       TO MOD-FLKROMK                              
093600       MOVE  KR-FLARBDEB      TO MOD-FLARBDEB                             
093700       MOVE  KR-BEKRANS       TO MOD-BEKRANS                              
093800       MOVE  KR-IDKRATLF      TO MOD-IDKRATLF                             
093900       MOVE  KR-FLANNULL      TO MOD-FLANN                                
094000       IF KR-FLKRLIM = SPACE                                              
094100         MOVE JA              TO KR-FLKRLIM                               
094200       END-IF                                                             
094300       MOVE KR-FLKRLIM        TO MOD-FLKRLIM                              
094400       IF WS-IDSKYLT = 'GB '                                              
094500         IF MOD-FLANN = JA                                                
094600           MOVE YES TO MOD-FLANN                                          
094700         END-IF                                                           
094800         IF MOD-FLKROMK = JA                                              
094900           MOVE YES TO MOD-FLKROMK                                        
095000         END-IF                                                           
095100         IF MOD-FLARBDEB = JA                                             
095200           MOVE YES TO MOD-FLARBDEB                                       
095300         END-IF                                                           
095400         IF MOD-FLKRLIM = JA                                              
095500           MOVE YES TO MOD-FLKRLIM                                        
095600         END-IF                                                           
095700       END-IF                                                             
095800                                                                          
095900       IF KR-KDKRSTA = '0' OR '1'                                         
096000          MOVE SPACE          TO MOD-FLKRGODK                             
096100          MOVE KR-FLKRLFEL    TO MOD-FLKRLFEL                             
096200          IF WS-IDSKYLT = 'GB '                                           
096300            IF MOD-FLKRLFEL = JA                                          
096400              MOVE YES TO MOD-FLKRLFEL                                    
096500            END-IF                                                        
096600          END-IF                                                          
096700          MOVE SPACE          TO MOD-KDKRATG                              
096800       ELSE                                                               
096900          MOVE  KR-FLKRGODK   TO MOD-FLKRGODK                             
097000          IF WS-IDSKYLT = 'GB '                                           
097100            IF MOD-FLKRGODK = JA                                          
097200              MOVE YES        TO MOD-FLKRGODK                             
097300            END-IF                                                        
097400          END-IF                                                          
097500          MOVE  KR-KDKRATG    TO MOD-KDKRATG                              
097600          MOVE  KR-FLKRLFEL   TO MOD-FLKRLFEL                             
097700          IF WS-IDSKYLT = 'GB '                                           
097800            IF MOD-FLKRLFEL = JA                                          
097900              MOVE YES        TO MOD-FLKRLFEL                             
098000            END-IF                                                        
098100          END-IF                                                          
098200       END-IF                                                             
098300       PERFORM MFS-RENSA-FAELT-IN                                         
098400     END-IF                                                               
098500     .                                                                    
098600     EJECT                                                                
098700 FAB-PRESS-PF11 SECTION.                                                  
098800                                                                          
098900     IF MID-ADATTENT = ALL '+'                                            
099000       MOVE  KR-ADATTENT            TO MOD-ADATTENT                       
099100     ELSE                                                                 
099200       MOVE MID-ADATTENT            TO MOD-ADATTENT                       
099300       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-ADATTENT-ATTR                  
099400     END-IF                                                               
099500     IF MID-FLEJKNTRL = ALL '+'                                           
099600       IF KR-FLEJKNTRL = JA                                               
099700         MOVE KR-FLEJKNTRL          TO MOD-FLEJKNTRL                      
099800       ELSE                                                               
099900         MOVE MFS-RENSA-FAELT       TO MOD-FLEJKNTRL                      
100000       END-IF                                                             
100100     ELSE                                                                 
100200       IF MID-FLEJKNTRL = JA OR YES                                       
100300         MOVE MID-FLEJKNTRL         TO MOD-FLEJKNTRL                      
100400       ELSE                                                               
100500         MOVE MFS-RENSA-FAELT       TO MOD-FLEJKNTRL                      
100600       END-IF                                                             
100700       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLEJKNTRL-ATTR                 
100800     END-IF                                                               
100900     IF MID-TEKRSPEC-ATID = ALL '+'                                       
101000       MOVE  KR-TEKRSPEC-ATID       TO MOD-TEKRSPEC-ATID                  
101100     ELSE                                                                 
101200       MOVE MID-TEKRSPEC-ATID       TO MOD-TEKRSPEC-ATID                  
101300       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TEKRSPEC-ATID-ATTR             
101400     END-IF                                                               
101500     IF MID-TEKRSPEC-OMK  = ALL '+'                                       
101600       MOVE  KR-TEKRSPEC-OMK        TO MOD-TEKRSPEC-OMK                   
101700     ELSE                                                                 
101800       MOVE MID-TEKRSPEC-OMK        TO MOD-TEKRSPEC-OMK                   
101900       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TEKRSPEC-OMK-ATTR              
102000     END-IF                                                               
102100     IF MID-TEKRSPEC-MAT  = ALL '+'                                       
102200       MOVE  KR-TEKRSPEC-MAT        TO MOD-TEKRSPEC-MAT                   
102300     ELSE                                                                 
102400       MOVE MID-TEKRSPEC-MAT        TO MOD-TEKRSPEC-MAT                   
102500       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TEKRSPEC-MAT-ATTR              
102600     END-IF                                                               
102700     IF MID-FLKROMK       = ALL '+'                                       
102800       MOVE  KR-FLKROMK             TO MOD-FLKROMK                        
102900       IF WS-IDSKYLT = 'GB ' AND MOD-FLKROMK = JA                         
103000         MOVE YES                   TO MOD-FLKROMK                        
103100       END-IF                                                             
103200     ELSE                                                                 
103300       MOVE MID-FLKROMK             TO MOD-FLKROMK                        
103400       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLKROMK-ATTR                   
103500     END-IF                                                               
103600     IF MID-FLARBDEB      = ALL '+'                                       
103700       MOVE  KR-FLARBDEB            TO MOD-FLARBDEB                       
103800       IF WS-IDSKYLT = 'GB ' AND MOD-FLARBDEB = JA                        
103900         MOVE YES                   TO MOD-FLARBDEB                       
104000       END-IF                                                             
104100     ELSE                                                                 
104200       MOVE MID-FLARBDEB            TO MOD-FLARBDEB                       
104300       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLARBDEB-ATTR                  
104400     END-IF                                                               
104500     MOVE  KR-BEKRANS               TO MOD-BEKRANS                        
104600     IF MID-IDKRATLF      = ALL '+'                                       
104700       MOVE  KR-IDKRATLF            TO MOD-IDKRATLF                       
104800     ELSE                                                                 
104900       MOVE MID-IDKRATLF            TO MOD-IDKRATLF                       
105000       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDKRATLF-ATTR                  
105100     END-IF                                                               
105200     IF MID-FLANN         = ALL '+'                                       
105300       MOVE  KR-FLANNULL            TO MOD-FLANN                          
105400       IF WS-IDSKYLT = 'GB '                                              
105500         IF MOD-FLANN = JA                                                
105600           MOVE YES                 TO MOD-FLANN                          
105700         END-IF                                                           
105800       END-IF                                                             
105900     ELSE                                                                 
106000       MOVE MID-FLANN               TO MOD-FLANN                          
106100       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLANN-ATTR                     
106200     END-IF                                                               
106300     IF MID-FLKRLIM       = ALL '+'                                       
106400       IF KR-FLKRLIM = SPACE                                              
106500         MOVE JA                    TO KR-FLKRLIM                         
106600       END-IF                                                             
106700       MOVE KR-FLKRLIM              TO MOD-FLKRLIM                        
106800       IF WS-IDSKYLT = 'GB '                                              
106900         IF MOD-FLKRLIM = JA                                              
107000           MOVE YES                 TO MOD-FLKRLIM                        
107100         END-IF                                                           
107200       END-IF                                                             
107300     ELSE                                                                 
107400       MOVE MID-FLKRLIM             TO MOD-FLKRLIM                        
107500       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLKRLIM-ATTR                   
107600     END-IF                                                               
107700                                                                          
107800     IF KR-KDKRSTA = '0' OR '1'                                           
107900        MOVE SPACE                  TO MOD-FLKRGODK                       
108000        MOVE KR-FLKRLFEL            TO MOD-FLKRLFEL                       
108100        IF WS-IDSKYLT = 'GB '                                             
108200          IF MOD-FLKRLFEL = JA                                            
108300            MOVE YES                TO MOD-FLKRLFEL                       
108400          END-IF                                                          
108500        END-IF                                                            
108600        MOVE SPACE                  TO MOD-KDKRATG                        
108700     ELSE                                                                 
108800       IF MID-FLKRGODK      = ALL '+'                                     
108900         MOVE  KR-FLKRGODK          TO MOD-FLKRGODK                       
109000         IF WS-IDSKYLT = 'GB '                                            
109100           IF MOD-FLKRGODK = JA                                           
109200             MOVE YES               TO MOD-FLKRGODK                       
109300           END-IF                                                         
109400         END-IF                                                           
109500       ELSE                                                               
109600         MOVE MID-FLKRGODK          TO MOD-FLKRGODK                       
109700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRGODK-ATTR                  
109800       END-IF                                                             
109900       IF MID-KDKRATG       = ALL '+'                                     
110000         MOVE  KR-KDKRATG           TO MOD-KDKRATG                        
110100       ELSE                                                               
110200         MOVE MID-KDKRATG           TO MOD-KDKRATG                        
110300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKRATG-ATTR                   
110400       END-IF                                                             
110500       IF MID-FLKRLFEL      = ALL '+'                                     
110600         MOVE  KR-FLKRLFEL          TO MOD-FLKRLFEL                       
110700         IF WS-IDSKYLT = 'GB '                                            
110800           IF MOD-FLKRLFEL = JA                                           
110900             MOVE YES               TO MOD-FLKRLFEL                       
111000           END-IF                                                         
111100         END-IF                                                           
111200       ELSE                                                               
111300         MOVE MID-FLKRLFEL          TO MOD-FLKRLFEL                       
111400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRLFEL-ATTR                  
111500       END-IF                                                             
111600     END-IF                                                               
111700     .                                                                    
111800     EJECT                                                                
111900 FB-LAES-VISA-LEV-INFO SECTION.                                           
112000                                                                          
112100     MOVE KR-IDLEVNR     TO W-IDLEVNR                                     
112200     MOVE KR-IDLEVG      TO W-IDLEVG                                      
112300                                                                          
112400     PERFORM IMS-GU-WLLEVA01                                              
112500     PERFORM IMS-GNP-WLLEVA14                                             
112600     MOVE  ADR-BELEV TO MOD-LEVTEXT                                       
112700     .                                                                    
112800     EJECT                                                                
112900 FC-LAES-VISA-BEN-INFO SECTION.                                           
113000                                                                          
113100     MOVE KR-IDARTNR TO W-IDARTNR                                         
113200                                                                          
113300     PERFORM IMS-GU-WLBENA11                                              
113400     IF SEGMENT-FINNS                                                     
113500       MOVE TEXT-BEART TO MOD-ARTIKELTEXT                                 
113600     ELSE                                                                 
113700       MOVE SPACE      TO MOD-ARTIKELTEXT                                 
113800     END-IF                                                               
113900     .                                                                    
114000     EJECT                                                                
114100 FD-LAES-VISA-ANSK-INFO SECTION.                                          
114200                                                                          
114300     MOVE KR-IDARTNR TO W-IDARTNR                                         
114400     MOVE KR-IDDC    TO W-IDDC                                            
114500                                                                          
114600     IF KR-IDDC NOT = DCS-IDDC                                            
114700        MOVE KR-IDDC TO W-IDDC-B6                                         
114800        PERFORM IMS-GU-WDB601                                             
114900        IF SEGMENT-SAKNAS                                                 
115000           MOVE SPACE TO DCS-IDDC                                         
115100                         DCS-KDDC                                         
115200        END-IF                                                            
115300     END-IF                                                               
115400                                                                          
115500     IF DCS-CDC                                                           
115600       PERFORM IMS-GU-WLARTC01                                            
115700       IF SEGMENT-FINNS                                                   
115800         PERFORM IMS-GNP-WLARTC11                                         
115900         IF SEGMENT-FINNS                                                 
116000           MOVE ARTC-CLAG-IDANSK TO MOD-IDANSK                            
116100           MOVE ARTC-CLAG-PRARTSTD TO WS-PRARTSTD                         
116200         END-IF                                                           
116300       ELSE                                                               
116400         MOVE ZERO               TO MOD-IDANSK                            
116500         MOVE INF-ARTIKEL-SAKNAS TO MED-IDMFSINF                          
116600         CALL WMEDKONV        USING MED-WMEDAREA                          
116700         MOVE MED-MFSINF         TO MOD-TEMFSINF                          
116800       END-IF                                                             
116900     ELSE                                                                 
117000** FOR NDC                                                                
117100       PERFORM IMS-GU-WDK711                                              
117200       IF SEGMENT-FINNS                                                   
117300         PERFORM IMS-GNP-WDK722                                           
117400         IF SEGMENT-FINNS                                                 
117500           MOVE XLAG-IDANSK      TO MOD-IDANSK                            
117600         END-IF                                                           
117700       ELSE                                                               
117800         MOVE ZERO               TO MOD-IDANSK                            
117900         MOVE INF-ARTIKEL-SAKNAS TO MED-IDMFSINF                          
118000         CALL WMEDKONV        USING MED-WMEDAREA                          
118100         MOVE MED-MFSINF         TO MOD-TEMFSINF                          
118200       END-IF                                                             
118300     END-IF                                                               
118400                                                                          
118500     IF KR-SUKRLIM NOT NUMERIC                                            
118600       MOVE ZERO               TO KR-SUKRLIM                              
118700     END-IF                                                               
118800                                                                          
118900     IF KR-SUKRLIM NOT = 0                                                
119000       MOVE KR-SUKRLIM         TO MOD-SUKRLIM                             
119100     ELSE                                                                 
119200       IF KR-IDDC = WC-CDC-SE AND                                         
119300          KR-IDKRFEL(1:1) = 'P' AND                                       
119400          KR-KDKRSTA < 5                                                  
119500         COMPUTE WS-SUKRLIM   = KR-KVART-AAVV * WS-PRARTSTD               
119600         IF WS-SUKRLIM < 0                                                
119700           COMPUTE WS-SUKRLIM = WS-SUKRLIM * -1                           
119800         END-IF                                                           
119900         IF WS-SUKRLIM <= 150                                             
120000           MOVE WS-SUKRLIM     TO MOD-SUKRLIM                             
120100         ELSE                                                             
120200           MOVE MFS-ERASE-FIELD                                           
120300                               TO MOD-SUKRLIM                             
120400           MOVE MFS-CLOSE-FIELD                                           
120500                               TO MOD-FLKRLIM-ATTR                        
120600         END-IF                                                           
120700       ELSE                                                               
120800         MOVE MFS-ERASE-FIELD  TO MOD-SUKRLIM                             
120900         MOVE MFS-CLOSE-FIELD  TO MOD-FLKRLIM-ATTR                        
121000       END-IF                                                             
121100     END-IF                                                               
121200                                                                          
121300                                                                          
121400     .                                                                    
121500     EJECT                                                                
121600 FE-KOLLA-OM-FARLIGT-GODS SECTION.                                        
121700                                                                          
121800     MOVE KR-IDARTNR TO W-IDARTNR                                         
121900                                                                          
122000     PERFORM IMS-GU-WLARTC11                                              
122100     IF SEGMENT-FINNS                                                     
122200       IF ARTC-CLAG-KDFARLIG = 4                                          
122300       OR ARTC-CLAG-KDFARLIG = 7                                          
122400          MOVE INF-FARLIGT-GODS TO MED-IDMFSFEL                           
122500          CALL WMEDKONV USING MED-WMEDAREA                                
122600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
122700       END-IF                                                             
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100 FF-LAES-VISA-KVAL-ANTAL-KR SECTION.                                      
123200                                                                          
123300     IF KR-IDLOPNRM > +0                                                  
123400        MOVE KR-IDLOPNRM              TO W-IDLOPNRM-CSEQ                  
123500        MOVE KR-DAAVSDAT              TO W-DAAVSDAT-CSEQ                  
123600        PERFORM IMS-GU-W6KVAI01                                           
123700        IF SEGMENT-FINNS                                                  
123800           IF CSEQ-KR-IDKR NOT = W-IDKR                                   
123900              IF CSEQ-KR-FLANNULL = NEJ                                   
124000                 IF CSEQ-KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                
124100                    MOVE INF-ANT-KR-FINNS  TO MED-IDMFSINF                
124200                 ELSE                                                     
124300                    MOVE INF-KVAL-KR-FINNS TO MED-IDMFSINF                
124400                 END-IF                                                   
124500                 CALL WMEDKONV USING MED-WMEDAREA                         
124600                 MOVE MED-MFSINF           TO W-MFSINF                    
124700                 MOVE CSEQ-KR-IDKR         TO W-MFSINF-IDKR               
124800                 MOVE W-MFSINF             TO MOD-TEMFSINF                
124900              END-IF                                                      
125000           ELSE                                                           
125100             PERFORM IMS-GN-W6KVAI01                                      
125200             IF SEGMENT-FINNS                                             
125300                IF CSEQ-KR-FLANNULL = NEJ                                 
125400                   IF CSEQ-KR-IDKRFEL = 'PA' OR 'PB' OR 'K '              
125500                      MOVE INF-ANT-KR-FINNS  TO MED-IDMFSINF              
125600                   ELSE                                                   
125700                      MOVE INF-KVAL-KR-FINNS TO MED-IDMFSINF              
125800                   END-IF                                                 
125900                   CALL WMEDKONV USING MED-WMEDAREA                       
126000                   MOVE MED-MFSINF           TO W-MFSINF                  
126100                   MOVE CSEQ-KR-IDKR         TO W-MFSINF-IDKR             
126200                   MOVE W-MFSINF             TO MOD-TEMFSINF              
126300                END-IF                                                    
126400             END-IF                                                       
126500          END-IF                                                          
126600        END-IF                                                            
126700     END-IF                                                               
126800     .                                                                    
126900     EJECT                                                                
127000 FG-LAES-VISA-MEMOID SECTION.                                             
127100                                                                          
127200     PERFORM IMS-GU-W6LEVA01                                              
127300     IF KR-KDFAXVAL NOT = SPACE                                           
127400       MOVE KR-KDFAXVAL           TO WS-KDFAX                             
127500                                     MOD-KDFAX                            
127600     ELSE                                                                 
127700       MOVE +1                    TO WS-KDFAX                             
127800                                     MOD-KDFAX                            
127900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFAX-ATTR                       
128000     END-IF                                                               
128100     IF KR-KDMEMVAL NOT = SPACE                                           
128200       MOVE KR-KDMEMVAL           TO WS-KDMEMO                            
128300                                     MOD-KDMEMO                           
128400     ELSE                                                                 
128500       MOVE +1                    TO WS-KDMEMO                            
128600                                     MOD-KDMEMO                           
128700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDMEMO-ATTR                      
128800     END-IF                                                               
128900     MOVE LEV-IDMAIL(WS-KDMEMO)     TO MOD-IDMAIL                         
129000     MOVE LEV-IDLEVFAX(WS-KDFAX)    TO MOD-IDLEVFAX                       
129100     .                                                                    
129200     EJECT                                                                
129300 G-KOLLA-INPUT SECTION.                                                   
129400                                                                          
129500     MOVE JA  TO INDATA-SW                                                
129600     MOVE JA  TO ALLT-SW                                                  
129700                                                                          
129800     IF MID-INPUT = ALL '+'                                               
129900       MOVE INF-INGET-ANDRAT TO MED-IDMFSINF                              
130000       CALL WMEDKONV      USING MED-WMEDAREA                              
130100       MOVE MED-TEMFSINF     TO MOD-TEMFSINF                              
130200       PERFORM MFS-ROER-EJ-FAELT-IN-UT                                    
130300       PERFORM IMS-GU-W6KVAE01                                            
130400       IF SEGMENT-FINNS                                                   
130500          MOVE KR-FLKRGODK TO WS-FLKRGODK                                 
130600          MOVE KR-FLKROMK  TO WS-FLKROMK                                  
130700          MOVE KR-FLARBDEB TO WS-FLARBDEB                                 
130800          MOVE KR-FLANNULL TO WS-FLANNULL                                 
130900          MOVE KR-IDDC     TO WS-IDDC                                     
131000          PERFORM MFS-EV-SPAERRA-FAELT                                    
131100       END-IF                                                             
131200       MOVE NEJ TO ALLT-SW                                                
131300       MOVE NEJ TO INDATA-SW                                              
131400     ELSE                                                                 
131500                                                                          
131600       PERFORM IMS-GU-W6KVAE01                                            
131700                                                                          
131800       IF SEGMENT-SAKNAS                                                  
131900          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
132000          CALL WMEDKONV   USING MED-WMEDAREA                              
132100          MOVE MED-MFSFEL    TO MOD-TEMFSFEL                              
132200          PERFORM MFS-RENSA-FAELT-UT                                      
132300          MOVE NEJ           TO ALLT-SW                                   
132400          MOVE NEJ           TO INDATA-SW                                 
132500       ELSE                                                               
132600         MOVE KR-FLKRGODK    TO WS-FLKRGODK                               
132700         MOVE KR-FLKROMK     TO WS-FLKROMK                                
132800         MOVE KR-FLARBDEB    TO WS-FLARBDEB                               
132900         MOVE KR-FLANNULL    TO WS-FLANNULL                               
133000         MOVE KR-IDLEVNR     TO W-IDLEVNR                                 
133100         MOVE KR-ADATTENT    TO WS-ADATTENT                               
133200         IF KR-FLKRLIM = SPACE                                            
133300           MOVE JA           TO WS-FLKRLIM                                
133400         ELSE                                                             
133500           MOVE KR-FLKRLIM   TO WS-FLKRLIM                                
133600         END-IF                                                           
133700         IF KR-SUKRLIM NOT NUMERIC                                        
133800           MOVE ZERO         TO KR-SUKRLIM                                
133900         END-IF                                                           
134000                                                                          
134100** FÅR BARA GODKÄNNA SPLITTADE PARTIER MED PF23                           
134200         IF MFS-UPDATE                                                    
134300           MOVE KR-IDLOPNRM TO W-IDLOPNRM                                 
134400           MOVE KR-IDARTNR  TO W-IDARTNR                                  
134500           PERFORM IMS-GU-W6INLA11                                        
134600           IF SEGMENT-FINNS                                               
134700             IF ART-FLSPLPART = JA                                        
134800                MOVE NEJ TO INDATA-SW                                     
134900                MOVE JA  TO SPLITT-PF23-SW                                
135000             END-IF                                                       
135100           END-IF                                                         
135200         END-IF                                                           
135300                                                                          
135400         IF ((MID-FLKRGODK = KR-FLKRGODK) OR (MID-FLKRGODK = '+'))        
135500          AND ((MID-FLANN = KR-FLANNULL) OR (MID-FLANN = '+'))            
135600            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRGODK-ATTR                
135700                                         MOD-FLANN-ATTR                   
135800            MOVE NEJ TO INDATA-SW                                         
135900         END-IF                                                           
136000                                                                          
136100                                                                          
136200         IF MID-FLKRLIM NOT = ALL '+'                                     
136300           IF MID-FLKRLIM = JA OR YES OR NEJ                              
136400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKRLIM-ATTR                
136500             MOVE MID-FLKRLIM          TO WS-FLKRLIM                      
136600           ELSE                                                           
136700             MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRLIM-ATTR                
136800             MOVE NEJ                  TO INDATA-SW                       
136900           END-IF                                                         
137000         END-IF                                                           
137100                                                                          
137200**** TILLÄGG 960226, MOTTAGANDES ATTENTION MÅSTE ALLTID                   
137300**** FYLLAS I, DET RÄCKER INTE MED DEFAULT "QUALITY DEPARTMENT"           
137400****                                                                      
137500         IF MID-FLANN = JA OR YES                                         
137600           IF (KR-KDKRSTA = '0' OR '1')                                   
137700             CONTINUE                                                     
137800           ELSE                                                           
137900** FÅR EJ ÄNDRA ATTENTION I SAMBAND MED ANNULERING                        
138000             IF MID-ADATTENT NOT = ALL '+'                                
138100                MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADATTENT-ATTR            
138200                MOVE NEJ                  TO INDATA-SW                    
138300                MOVE NEJ                  TO ATT-ANNUL                    
138400             END-IF                                                       
138500           END-IF                                                         
138600** FÅR EJ SÄTTA JA PÅ BÅDE GODKÄND OCH ANNULERAD                          
138700           IF MID-FLKRGODK = JA OR YES                                    
138800              MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRGODK-ATTR              
138900              MOVE NEJ                  TO INDATA-SW                      
139000           END-IF                                                         
139100         ELSE                                                             
139200           IF MID-ADATTENT NOT = ALL '+'                                  
139300               IF MID-ADATTENT = SPACE                                    
139400                  MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADATTENT-ATTR          
139500                  MOVE NEJ                  TO INDATA-SW                  
139600               ELSE                                                       
139700                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADATTENT-ATTR          
139800               END-IF                                                     
139900           ELSE                                                           
140000               IF WS-ADATTENT = SPACE                                     
140100                  MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADATTENT-ATTR          
140200                  MOVE NEJ                  TO INDATA-SW                  
140300               ELSE                                                       
140400                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADATTENT-ATTR          
140500               END-IF                                                     
140600           END-IF                                                         
140700         END-IF                                                           
140800                                                                          
140900         IF MID-KDPERSON = ALL '+' OR MID-KDPERSON = SPACE                
141000            MOVE MFS-NUM-FAELT-FEL TO MOD-KDPERSON-ATTR                   
141100            MOVE NEJ               TO INDATA-SW                           
141200         ELSE                                                             
141300            INSPECT MID-KDPERSON REPLACING LEADING SPACE BY ZERO          
141400            IF MID-KDPERSON NOT NUMERIC                                   
141500               MOVE MFS-NUM-FAELT-FEL TO MOD-KDPERSON-ATTR                
141600               MOVE NEJ               TO INDATA-SW                        
141700            ELSE                                                          
141800               MOVE MID-KDPERSON      TO W-IDPERSON                       
141900               PERFORM IMS-GU-WDP311                                      
142000               IF SEGMENT-SAKNAS                                          
142100                  MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPERSON-ATTR           
142200                  MOVE NEJ                 TO INDATA-SW                   
142300               ELSE                                                       
142400                  MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPERSON-ATTR           
142500                  MOVE PERS-IDTFN          TO WS-IDKRATLF                 
142600                  MOVE PERS-IDNAMN         TO WS-BEKRANS                  
142700                  MOVE PERS-IDMAIL         TO WS-MAIL-ANS                 
142800               END-IF                                                     
142900            END-IF                                                        
143000         END-IF                                                           
143100                                                                          
143200         IF INDATA-OK                                                     
143300*** --- GODKÄNNER ÄVEN ATT SKICKA EJ GODKÄND KR TILL VIR                  
143400           IF (MID-FLKRTOVIR = ALL '+' OR MID-FLKRTOVIR = SPACE           
143500           OR MID-FLKRTOVIR = 'N')                                        
143600             MOVE MFS-ALFA-FAELT-RAETT       TO MOD-FLKRTOVIR-ATTR        
143700           ELSE                                                           
143800             IF MID-FLKRTOVIR = JA OR YES                                 
143900               IF KR-IDKRFEL(1:1) = 'P' OR 'K'                            
144000                 IF KR-KDDISP = 5                                         
144100                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKRTOVIR-ATTR        
144200                   MOVE JA                   TO WS-FLGODK-VIR             
144300                 ELSE                                                     
144400                   MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRTOVIR-ATTR        
144500                   MOVE NEJ                  TO INDATA-SW                 
144600                   MOVE NEJ                  TO WS-FLGODK-VIR             
144700                 END-IF                                                   
144800               ELSE                                                       
144900                 IF KR-KDDISP = 6                                         
145000                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKRTOVIR-ATTR        
145100                   MOVE JA                   TO WS-FLGODK-VIR             
145200                 ELSE                                                     
145300                   MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRTOVIR-ATTR        
145400                   MOVE NEJ                  TO INDATA-SW                 
145500                   MOVE NEJ                  TO WS-FLGODK-VIR             
145600                 END-IF                                                   
145700               END-IF                                                     
145800             ELSE                                                         
145900               MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLKRTOVIR-ATTR        
146000               MOVE NEJ                      TO INDATA-SW                 
146100             END-IF                                                       
146200           END-IF                                                         
146300         END-IF                                                           
146400                                                                          
146500         IF INDATA-OK                                                     
146600           IF MID-FLEJKNTRL = ALL '+'                                     
146700             MOVE MFS-ALFA-FAELT-RAETT       TO MOD-FLEJKNTRL-ATTR        
146800           ELSE                                                           
146900             IF MID-FLEJKNTRL = JA OR YES OR NEJ                          
147000                MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEJKNTRL-ATTR           
147100                MOVE MID-FLEJKNTRL TO WS-FLEJKNTRL                        
147200                IF MID-FLEJKNTRL = JA OR YES                              
147300                  MOVE MID-FLEJKNTRL TO MOD-FLEJKNTRL                     
147400                ELSE                                                      
147500                  MOVE MFS-RENSA-FAELT TO MOD-FLEJKNTRL                   
147600                END-IF                                                    
147700             ELSE                                                         
147800                MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLEJKNTRL-ATTR           
147900                MOVE NEJ                  TO INDATA-SW                    
148000             END-IF                                                       
148100           END-IF                                                         
148200         END-IF                                                           
148300                                                                          
148400         IF INDATA-OK                                                     
148500           IF MID-KDFAX NOT = ALL '+'                                     
148600             IF MID-KDFAX = '1' OR '2' OR '3' OR '4'                      
148700               MOVE MID-KDFAX TO MOD-KDFAX                                
148800                                  WS-KDFAX                                
148900               PERFORM IMS-GU-W6LEVA01                                    
149000               IF SEGMENT-FINNS                                           
149100                 IF LEV-IDLEVFAX(WS-KDFAX) NOT = SPACE                    
149200                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFAX-ATTR            
149300                 ELSE                                                     
149400                   IF MID-KDFAX = '2' OR '3' OR '4'                       
149500                     MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFAX-ATTR            
149600                     MOVE NEJ                TO INDATA-SW                 
149700                   ELSE                                                   
149800                     MOVE +1                 TO WS-KDFAX                  
149900                   END-IF                                                 
150000                 END-IF                                                   
150100               END-IF                                                     
150200             ELSE                                                         
150300               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFAX-ATTR                  
150400               MOVE NEJ                TO INDATA-SW                       
150500             END-IF                                                       
150600           ELSE                                                           
150700             MOVE +1                   TO WS-KDFAX                        
150800           END-IF                                                         
150900                                                                          
151000           IF MID-KDMEMO NOT = ALL '+'                                    
151100             IF MID-KDMEMO = '1' OR '2' OR '3'                            
151200               MOVE MID-KDMEMO TO MOD-KDMEMO                              
151300                                  WS-KDMEMO                               
151400               PERFORM IMS-GU-W6LEVA01                                    
151500               IF SEGMENT-FINNS                                           
151600                 IF LEV-IDMAIL(WS-KDMEMO)   NOT = SPACE                   
151700                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMEMO-ATTR           
151800                 ELSE                                                     
151900                   IF MID-KDMEMO = '2' OR '3'                             
152000                     MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMEMO-ATTR           
152100                     MOVE NEJ                TO INDATA-SW                 
152200                   ELSE                                                   
152300                     MOVE +1                 TO WS-KDMEMO                 
152400                   END-IF                                                 
152500                 END-IF                                                   
152600               END-IF                                                     
152700             ELSE                                                         
152800               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMEMO-ATTR                 
152900               MOVE NEJ                TO INDATA-SW                       
153000             END-IF                                                       
153100           ELSE                                                           
153200             MOVE +1                   TO WS-KDMEMO                       
153300           END-IF                                                         
153400         END-IF                                                           
153500                                                                          
153600         IF INDATA-OK AND WS-FLGODK-VIR = SPACE                           
153700** SKA INTE BEHÖVA FYLLA I DISPKOD/HANDLINGCOSTKOD FÖR ATT                
153800** ANNULERA EJ IVÄGSÄNDA KR                                               
153900           IF (MID-FLANN = JA OR YES) AND                                 
154000              (KR-KDKRSTA = '0' OR '1')                                   
154100             CONTINUE                                                     
154200           ELSE                                                           
154300             IF KR-IDKRFEL(1:1) = 'K'                                     
154400               IF KR-KDDISP = 6                                           
154500                 CONTINUE                                                 
154600               ELSE                                                       
154700                 MOVE NEJ TO INDATA-SW                                    
154800                 MOVE NEJ TO KDDISP-OK                                    
154900               END-IF                                                     
155000             ELSE                                                         
155100               IF KR-IDKRFEL(1:1) = 'P'                                   
155200                 IF KR-KDDISP = 1 OR 2 OR 3 OR 4                          
155300                   CONTINUE                                               
155400                 ELSE                                                     
155500                   MOVE NEJ TO INDATA-SW                                  
155600                   MOVE NEJ TO KDDISP-OK                                  
155700                 END-IF                                                   
155800               ELSE                                                       
155900                 IF KR-KDDISP = 1 OR 2 OR 3 OR 4 OR 5 OR 77 OR 91         
156000                   CONTINUE                                               
156100                 ELSE                                                     
156200                   MOVE NEJ TO INDATA-SW                                  
156300                   MOVE NEJ TO KDDISP-OK                                  
156400                 END-IF                                                   
156500               END-IF                                                     
156600             END-IF                                                       
156700                                                                          
156800** ÄR DISPOSITIONSKOD = 1 ELLER 77 (GER TEXTEN: "SHIPMENT WILL BE         
156900** RETURNED" PÅ KONTROLLRAPPORTEN) SÅ MÅSTE RETURFÄLTET VARA              
157000** IFYLLT VID GODKÄNNANDE.                                                
157100**                                                                        
157200             IF KR-KDDISP = 1 OR 77                                       
157300               IF KR-KVART-RET > 0                                        
157400                 CONTINUE                                                 
157500               ELSE                                                       
157600                 MOVE NEJ TO INDATA-SW                                    
157700                 MOVE NEJ TO KDDISP-RET-OK                                
157800               END-IF                                                     
157900             END-IF                                                       
158000                                                                          
158100** LIKADANT MÅSTE DISPOSITIONSKOD VARA 1 ELLER 91                         
158200** OM RETURFÄLT ÄR IFYLLT                                                 
158300** VID GODKÄNNANDE.                                                       
158400**                                                                        
158500             IF KR-KVART-RET > 0                                          
158600               IF KR-KDDISP = 1 OR 77 OR 91                               
158700                 CONTINUE                                                 
158800               ELSE                                                       
158900                 MOVE NEJ TO INDATA-SW                                    
159000                 MOVE NEJ TO KDDISP-RET-OK                                
159100               END-IF                                                     
159200             END-IF                                                       
159300** ÄR DISPOSITIONSKOD = 3 (GER TEXTEN: "DEFECTIVE PARTS WILL BE           
159400** ADJUSTED AT YOUR EXPENSE" PÅ KR)SÅ MÅSTE JUSTERAFÄLTET ELLER           
159500** SKROTFÄLTET VARA                                                       
159600** IFYLLT VID GODKÄNNANDE.                                                
159700**                                                                        
159800             IF KR-KDDISP = 3                                             
159900               IF KR-KVART-KJUST > 0 OR KR-KVART-SKROT > 0                
160000                 CONTINUE                                                 
160100               ELSE                                                       
160200                 MOVE NEJ TO INDATA-SW                                    
160300                 MOVE NEJ TO KDDISP-ADJ-OK                                
160400               END-IF                                                     
160500             END-IF                                                       
160600                                                                          
160700** ÄR DISPOSITIONSKOD = 5 (GER TEXTEN: "PARTS WILL BE SCRAPPED            
160800** AT YOUR EXPENSE" PÅ KR)SÅ MÅSTE SKROTFÄLTET VARA                       
160900** IFYLLT VID GODKÄNNANDE.                                                
161000**                                                                        
161100             IF KR-KDDISP = 5                                             
161200               IF KR-KVART-SKROT > 0                                      
161300                 CONTINUE                                                 
161400               ELSE                                                       
161500                 MOVE NEJ TO INDATA-SW                                    
161600                 MOVE NEJ TO KDDISP-SKR-OK                                
161700               END-IF                                                     
161800             END-IF                                                       
161900** LIKADANT MÅSTE DISPOSITIONSKOD VARA 5 ELLER 91                         
162000** OM SKROTFÄLT ÄR IFYLLT                                                 
162100** VID GODKÄNNANDE (ALTERNATIVT DISPKOD=3 I KOMB MED ANTALS-KR)           
162200**                                                                        
162300             IF KR-KVART-SKROT > 0                                        
162400               IF (KR-KDDISP = 5 OR 91) OR (KR-KDDISP = 3 AND             
162500                   KR-IDKRFEL(1:1) = 'P')                                 
162600                 CONTINUE                                                 
162700               ELSE                                                       
162800                 MOVE NEJ TO INDATA-SW                                    
162900                 MOVE NEJ TO KDDISP-SKR-OK                                
163000               END-IF                                                     
163100             END-IF                                                       
163200                                                                          
163300** DISP CODE 77 IS ONLY ALLOWED FOR R40                                   
163400**                                                                        
163500             IF KR-KDDISP = 77 AND KR-IDLOPNRM > 0                        
163600               MOVE NEJ TO INDATA-SW                                      
163700               MOVE NEJ TO KDDISP-RET-OK                                  
163800             END-IF                                                       
163900                                                                          
164000             IF KR-KDHANDCO = 1 OR 2                                      
164100               CONTINUE                                                   
164200             ELSE                                                         
164300               MOVE NEJ TO INDATA-SW                                      
164400               MOVE NEJ TO KDHANDCO-OK                                    
164500             END-IF                                                       
164600           END-IF                                                         
164700                                                                          
164800           IF MID-FLKRLFEL NOT = ALL '+' AND                              
164900              MID-FLKRLFEL NOT = SPACE                                    
165000              IF MID-FLKRLFEL = JA OR NEJ OR YES                          
165100                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKRLFEL-ATTR           
165200              ELSE                                                        
165300                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRLFEL-ATTR           
165400                 MOVE NEJ                  TO INDATA-SW                   
165500              END-IF                                                      
165600           ELSE                                                           
165700              IF KR-KDKRSTA = '0' OR '1'                                  
165800                IF MID-FLKRGODK  = ALL '+'                                
165900                AND MID-FLANN    = ALL '+'                                
166000                  MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRLFEL-ATTR          
166100                  MOVE NEJ                  TO INDATA-SW                  
166200                END-IF                                                    
166300              END-IF                                                      
166400           END-IF                                                         
166500                                                                          
166600           IF MID-KDKRATG NOT = ALL '+'                                   
166700              IF MID-KDKRATG = 'S' OR 'M' OR 'B' OR                       
166800                               'R' OR 'A' OR 'D' OR SPACE                 
166900                                                                          
167000                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKRATG-ATTR            
167100              ELSE                                                        
167200                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDKRATG-ATTR            
167300                 MOVE NEJ                  TO INDATA-SW                   
167400              END-IF                                                      
167500           END-IF                                                         
167600                                                                          
167700           IF MID-FLINKANS NOT = ALL '+' AND                              
167800              MID-FLINKANS NOT = SPACE                                    
167900             IF MID-FLINKANS = JA OR YES OR NEJ                           
168000                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINKANS-ATTR           
168100             ELSE                                                         
168200                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINKANS-ATTR           
168300                 MOVE NEJ                  TO INDATA-SW                   
168400             END-IF                                                       
168500           END-IF                                                         
168600                                                                          
168700           IF MID-KVARBTID-IN NOT = ALL '+'                               
168800              IF KR-KDKRSTA = '5'                                         
168900** FÅR EJ ÄNDRA ANTAL ELLER PRIS EFTER ATT KR GÅTT UPP I STATUS 5.        
169000                MOVE MFS-NUM-FAELT-FEL                                    
169100                                    TO MOD-KVARBTID-IN-ATTR               
169200                MOVE NEJ            TO INDATA-SW                          
169300              ELSE                                                        
169400                MOVE MID-KVARBTID-IN   TO DEC-IDFRIDATA                   
169500                MOVE 2                 TO DEC-KVHELTAL                    
169600                MOVE 1                 TO DEC-KVDECIMAL                   
169700                CALL WDECEDIT USING DEC-WDECAREA                          
169800                IF DEC-KDSVAR-OK                                          
169900                   MOVE DEC-IDEDITDATA TO WS-KVARBTID                     
170000                   MOVE MFS-NUM-FAELT-RAETT                               
170100                                       TO MOD-KVARBTID-IN-ATTR            
170200                ELSE                                                      
170300                   MOVE MFS-NUM-FAELT-FEL                                 
170400                                       TO MOD-KVARBTID-IN-ATTR            
170500                   MOVE NEJ            TO INDATA-SW                       
170600                END-IF                                                    
170700              END-IF                                                      
170800           END-IF                                                         
170900                                                                          
171000           IF MID-SUOMK-IN NOT = ALL '+'                                  
171100             IF KR-KDKRSTA = '5'                                          
171200               MOVE MFS-NUM-FAELT-FEL   TO MOD-SUOMK-IN-ATTR              
171300               MOVE NEJ                 TO INDATA-SW                      
171400             ELSE                                                         
171500               IF MID-SUOMK-IN NUMERIC                                    
171600                  MOVE MFS-NUM-FAELT-RAETT TO MOD-SUOMK-IN-ATTR           
171700               ELSE                                                       
171800                  MOVE MFS-NUM-FAELT-FEL   TO MOD-SUOMK-IN-ATTR           
171900                  MOVE NEJ                 TO INDATA-SW                   
172000               END-IF                                                     
172100             END-IF                                                       
172200           END-IF                                                         
172300                                                                          
172400           IF MID-SUMAT-IN NOT = ALL '+'                                  
172500             IF KR-KDKRSTA = '5'                                          
172600               MOVE MFS-NUM-FAELT-FEL   TO MOD-SUMAT-IN-ATTR              
172700               MOVE NEJ                 TO INDATA-SW                      
172800             ELSE                                                         
172900               MOVE MID-SUMAT-IN  TO DEC-IDFRIDATA                        
173000               MOVE 7             TO DEC-KVHELTAL                         
173100               MOVE 2             TO DEC-KVDECIMAL                        
173200               CALL WDECEDIT USING DEC-WDECAREA                           
173300               IF DEC-KDSVAR-OK                                           
173400                  MOVE DEC-IDEDITDATA      TO WS-SUMAT                    
173500                  MOVE MFS-NUM-FAELT-RAETT TO MOD-SUMAT-IN-ATTR           
173600               ELSE                                                       
173700                  MOVE MFS-NUM-FAELT-FEL   TO MOD-SUMAT-IN-ATTR           
173800                  MOVE NEJ                 TO INDATA-SW                   
173900               END-IF                                                     
174000             END-IF                                                       
174100           END-IF                                                         
174200                                                                          
174300           IF MID-FLKROMK NOT = ALL '+'                                   
174400              IF MID-FLKROMK = JA OR NEJ OR YES                           
174500                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKROMK-ATTR            
174600                 MOVE MID-FLKROMK          TO WS-FLKROMK                  
174700              ELSE                                                        
174800                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKROMK-ATTR            
174900                 MOVE NEJ                  TO INDATA-SW                   
175000              END-IF                                                      
175100           END-IF                                                         
175200                                                                          
175300           IF MID-FLARBDEB NOT = ALL '+'                                  
175400             IF MID-FLARBDEB = JA OR NEJ OR YES                           
175500                MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLARBDEB-ATTR            
175600                MOVE MID-FLARBDEB         TO WS-FLARBDEB                  
175700             ELSE                                                         
175800                MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLARBDEB-ATTR            
175900                MOVE NEJ                  TO INDATA-SW                    
176000             END-IF                                                       
176100           ELSE                                                           
176200             IF KR-FLARBDEB = 'J' OR 'N'                                  
176300                CONTINUE                                                  
176400             ELSE                                                         
176500                MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLARBDEB-ATTR            
176600                MOVE NEJ                  TO INDATA-SW                    
176700             END-IF                                                       
176800           END-IF                                                         
176900                                                                          
177000           IF MID-KVARBTID-IN NOT = ALL '+' OR                            
177100              MID-SUOMK-IN    NOT = ALL '+' OR                            
177200              MID-SUMAT-IN    NOT = ALL '+'                               
177300              IF WS-FLKROMK = SPACE                                       
177400                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKROMK-ATTR              
177500                 MOVE NEJ                TO INDATA-SW                     
177600              END-IF                                                      
177700           END-IF                                                         
177800                                                                          
177900           IF MID-KVARBTID-IN = ALL '+' AND                               
178000              KR-KVARBTID = ZERO        AND                               
178100              MID-TEKRSPEC-ATID NOT = ALL '+'                             
178200              MOVE MFS-ALFA-FAELT-FEL TO MOD-TEKRSPEC-ATID-ATTR           
178300              MOVE NEJ                TO INDATA-SW                        
178400           END-IF                                                         
178500                                                                          
178600           IF MID-SUOMK-IN = ALL '+' AND                                  
178700              KR-SUOMK = ZERO        AND                                  
178800              MID-TEKRSPEC-OMK NOT = ALL '+'                              
178900              MOVE MFS-ALFA-FAELT-FEL TO MOD-TEKRSPEC-OMK-ATTR            
179000              MOVE NEJ                TO INDATA-SW                        
179100           END-IF                                                         
179200                                                                          
179300            IF MID-SUMAT-IN = ALL '+' AND                                 
179400              KR-SUMAT = ZERO         AND                                 
179500              MID-TEKRSPEC-MAT NOT = ALL '+'                              
179600              MOVE MFS-ALFA-FAELT-FEL TO MOD-TEKRSPEC-MAT-ATTR            
179700              MOVE NEJ                TO INDATA-SW                        
179800           END-IF                                                         
179900                                                                          
180000           IF KR-IDKRATLF = SPACE                                         
180100             AND (MID-IDKRATLF   = ALL '+' OR SPACE)                      
180200             AND WS-IDKRATLF = SPACE                                      
180300              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDKRATLF-ATTR              
180400              MOVE NEJ                  TO INDATA-SW                      
180500           END-IF                                                         
180600                                                                          
180700           IF MID-FLKRGODK NOT = ALL '+' OR SPACE                         
180800              IF MID-FLKRGODK = JA OR NEJ OR YES                          
180900                MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKRGODK-ATTR            
181000                MOVE MID-FLKRGODK         TO WS-FLKRGODK                  
181100                IF (MID-FLKRGODK = JA OR YES)                             
181200                   IF (MID-FLKROMK NOT = JA AND YES)                      
181300                      IF KR-FLKROMK NOT = JA                              
181400* MÅSTE SÄTTA OMKOSTNAD-KLAR FÖR ATT KUNNA GODKÄNNA KR                    
181500                        MOVE MFS-ALFA-FAELT-FEL TO                        
181600                                              MOD-FLKROMK-ATTR            
181700                        MOVE NEJ                TO INDATA-SW              
181800                      END-IF                                              
181900                   END-IF                                                 
182000                END-IF                                                    
182100              ELSE                                                        
182200                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRGODK-ATTR           
182300                 MOVE NEJ                  TO INDATA-SW                   
182400              END-IF                                                      
182500           END-IF                                                         
182600                                                                          
182700           IF KR-KDKRSTA = '0' OR '1'                                     
182800              IF  MID-FLKRGODK = ALL '+'                                  
182900              AND MID-FLANN    = ALL '+'                                  
183000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKRGODK-ATTR             
183100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANN-ATTR                
183200                 MOVE NEJ                TO INDATA-SW                     
183300              END-IF                                                      
183400           END-IF                                                         
183500                                                                          
183600           IF (MID-FLKRGODK = JA OR NEJ OR YES)                           
183700           OR (MID-FLANN    = JA OR NEJ OR YES)                           
183800              IF MID-KDPERSON    = ALL '+'                                
183900                 AND KR-BEKRANS  = SPACE                                  
184000                 MOVE MFS-NUM-FAELT-FEL  TO MOD-KDPERSON-ATTR             
184100                 MOVE NEJ                TO INDATA-SW                     
184200              END-IF                                                      
184300           END-IF                                                         
184400                                                                          
184500           IF MID-IDKRATLF NOT = ALL '+' AND                              
184600              MID-IDKRATLF NOT = SPACE                                    
184700              MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDKRATLF-ATTR             
184800           END-IF                                                         
184900                                                                          
185000           IF MID-FLANN NOT = ALL '+'                                     
185100              IF MID-FLANN = JA OR NEJ OR YES                             
185200                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLANN-ATTR              
185300              ELSE                                                        
185400                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLANN-ATTR              
185500                 MOVE NEJ                  TO INDATA-SW                   
185600              END-IF                                                      
185700           END-IF                                                         
185800                                                                          
185900           IF MID-FLANN   = NEJ          AND                              
186000              KR-FLANNULL = JA           AND                              
186100             (KR-KDKRSTA  = '0' OR '1')                                   
186200              IF KR-IDLOPNRM > +0                                         
186300                 MOVE KR-IDLOPNRM TO W-IDLOPNRM                           
186400                 MOVE KR-IDARTNR  TO W-IDARTNR                            
186500                 PERFORM IMS-GHU-W6INLA11                                 
186600                 IF SEGMENT-SAKNAS                                        
186700                    MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANN-ATTR             
186800                    MOVE NEJ                TO R32-KLAR-SW                
186900                    MOVE NEJ                TO INDATA-SW                  
187000                 ELSE                                                     
187100                    IF ART-FLKLAR = JA                                    
187200                       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANN-ATTR          
187300                       MOVE NEJ                TO R32-KLAR-SW             
187400                       MOVE NEJ                TO INDATA-SW               
187500                    END-IF                                                
187600                 END-IF                                                   
187700              END-IF                                                      
187800           END-IF                                                         
187900                                                                          
188000           IF MID-FAX-TLX NOT = ALL '+'                                   
188100              IF MID-FAX-TLX = FAX OR TLX                                 
188200                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FAX-TLX-ATTR            
188300              ELSE                                                        
188400                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FAX-TLX-ATTR            
188500                 MOVE NEJ                  TO INDATA-SW                   
188600              END-IF                                                      
188700           END-IF                                                         
188800                                                                          
188900           IF INDATA-OK                                                   
189000              IF KR-IDLOPNRM > +0                                         
189100                 PERFORM GA-KONTROLL-MOTKONTROLL                          
189200              END-IF                                                      
189300           END-IF                                                         
189400         END-IF                                                           
189500                                                                          
189600         IF INDATA-OK                                                     
189700** KONTROLLERAR ATT ANSKAFFAR-/INKÖPARNAMN FINNS REGISTRERAT              
189800** PÅ RESPEKTIVE NUMMER                                                   
189900           MOVE NEJ             TO ANSK-INK-FINNS                         
190000           MOVE JA              TO WS-ANTAL-OK                            
190100           MOVE KR-IDARTNR      TO W-IDARTNR                              
190200           MOVE KR-IDDC         TO W-IDDC                                 
190300                                                                          
190400                                                                          
190500           IF KR-IDDC NOT = DCS-IDDC                                      
190600              MOVE KR-IDDC TO W-IDDC-B6                                   
190700              PERFORM IMS-GU-WDB601                                       
190800              IF SEGMENT-SAKNAS                                           
190900                 MOVE SPACE TO DCS-IDDC                                   
191000                               DCS-KDDC                                   
191100              END-IF                                                      
191200           END-IF                                                         
191300                                                                          
191400           PERFORM IMS-GU-WLARTC01                                        
191500           IF SEGMENT-FINNS                                               
191600             PERFORM IMS-GNP-WLARTC11                                     
191700             IF ARTC-CLAG-IDINK NOT = SPACE                               
191800               MOVE ARTC-CLAG-IDINK             TO WS-IDINK               
191900               IF ARTC-CLAG-IDINK (1:3) NUMERIC                           
192000                  MOVE ARTC-CLAG-IDINK (1:3)    TO W-IDPERSON             
192100               ELSE                                                       
192200                  IF ARTC-CLAG-IDINK (2:3) NUMERIC                        
192300                     MOVE ARTC-CLAG-IDINK (2:3) TO W-IDPERSON             
192400                  ELSE                                                    
192500                     MOVE ZERO                  TO W-IDPERSON             
192600                  END-IF                                                  
192700               END-IF                                                     
192800             END-IF                                                       
192900**                                                                        
193000             IF DCS-NDC-CN OR DCS-NDC-NA                                  
193100               PERFORM IMS-GU-WDK711                                      
193200               IF SEGMENT-FINNS                                           
193300                 PERFORM IMS-GNP-WDK722                                   
193400                 IF SEGMENT-FINNS                                         
193500                   MOVE YES      TO WS-K722-SW                            
193600                   IF XLAG-IDINK NOT = SPACE                              
193700                     MOVE XLAG-IDINK             TO WS-IDINK              
193800                     IF XLAG-IDINK (1:3) NUMERIC                          
193900                        MOVE XLAG-IDINK (1:3)    TO W-IDPERSON            
194000                     ELSE                                                 
194100                        IF XLAG-IDINK (2:3) NUMERIC                       
194200                           MOVE XLAG-IDINK (2:3) TO W-IDPERSON            
194300                        END-IF                                            
194400                     END-IF                                               
194500                   END-IF                                                 
194600                 END-IF                                                   
194700               END-IF                                                     
194800             END-IF                                                       
194900*                                                                         
195000             MOVE 'INK     '        TO W-KDARBTYP                         
195100             PERFORM IMS-GU-WDP311                                        
195200             IF SEGMENT-FINNS                                             
195300               IF PERS-IDNAMN NOT = SPACE                                 
195400                 MOVE JA  TO ANSK-INK-FINNS                               
195500               END-IF                                                     
195600             END-IF                                                       
195700*                                                                         
195800             MOVE ZERO                TO W-IDPERSON                       
195900             IF ARTC-CLAG-IDANSK NOT = 0                                  
196000               MOVE ARTC-CLAG-IDANSK  TO WS-IDANSK                        
196100                                         W-IDPERSON                       
196200             END-IF                                                       
196300             IF DCS-NDC-CN OR DCS-NDC-NA                                  
196400               IF WS-K722-EXISTS                                          
196500                 IF XLAG-IDANSK NOT = 0                                   
196600                   MOVE XLAG-IDANSK       TO WS-IDANSK                    
196700                                            W-IDPERSON                    
196800                 END-IF                                                   
196900               END-IF                                                     
197000             END-IF                                                       
197100*                                                                         
197200             MOVE 'ANSK    '        TO W-KDARBTYP                         
197300             PERFORM IMS-GU-WDP311                                        
197400             IF SEGMENT-FINNS                                             
197500               IF PERS-IDNAMN NOT = SPACE                                 
197600                 MOVE JA  TO ANSK-INK-FINNS                               
197700               END-IF                                                     
197800             END-IF                                                       
197900*                                                                         
198000             IF KR-IDLOPNRM = ZERO AND                                    
198100               (MID-FLKRGODK = JA OR YES)                                 
198200                COMPUTE W-R40-ANTAL = KR-KVART-RET +                      
198300                KR-KVART-SKROT - KR-KVART-TIDGK -                         
198400                KR-KVART-SKROT-LDC                                        
198500                IF W-R40-ANTAL > ZERO                                     
198600                  MOVE KR-IDDC          TO WS-IDDC                        
198700                  IF NDC-CN OR NDC-US                                     
198800                    MOVE SLAG-KVLS      TO WS-KVLS                        
198900                  ELSE                                                    
199000                    MOVE ARTC-CLAG-KVLS TO WS-KVLS                        
199100                  END-IF                                                  
199200                  IF MFS-UPD-V                                            
199300                     CONTINUE                                             
199400                  ELSE                                                    
199500                     IF W-R40-ANTAL > WS-KVLS                             
199600                        MOVE NEJ TO WS-ANTAL-OK                           
199700                        MOVE NEJ TO INDATA-SW                             
199800                     END-IF                                               
199900                  END-IF                                                  
200000                END-IF                                                    
200100             END-IF                                                       
200200           END-IF                                                         
200300         END-IF                                                           
200400                                                                          
200500         IF INDATA-OK                                                     
200600           IF KR-SUKRLIM > 0                                              
200700             MOVE KR-SUKRLIM         TO WS-SUKRLIM                        
200800           ELSE                                                           
200900             IF KR-IDDC = WC-CDC-SE AND                                   
201000                KR-IDKRFEL(1:1) = 'P' AND                                 
201100                KR-KDKRSTA < 5                                            
201200               COMPUTE WS-SUKRLIM = KR-KVART-AAVV *                       
201300                                    ARTC-CLAG-PRARTSTD                    
201400               IF WS-SUKRLIM < 0                                          
201500                 COMPUTE WS-SUKRLIM = WS-SUKRLIM * -1                     
201600               END-IF                                                     
201700             END-IF                                                       
201800           END-IF                                                         
201900         END-IF                                                           
202000                                                                          
202100         IF INDATA-OK       AND                                           
202200            WS-SUKRLIM > 150 AND                                          
202300            WS-FLKRLIM = NEJ                                              
202400           MOVE NEJ                  TO INDATA-SW                         
202500           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRLIM-ATTR                  
202600         END-IF                                                           
202700                                                                          
202800         IF ANSK-INK-FINNS = NEJ                                          
202900           MOVE NEJ TO INDATA-SW                                          
203000         END-IF                                                           
203100                                                                          
203200         IF KR-KDKRUTF = '4'                                              
203300** EJ BESTÄLLD INLEVERANS - ALLT I RETUR                                  
203400           IF KR-KVAVIS NOT = KR-KVART-RET                                
203500              MOVE NEJ TO INDATA-SW                                       
203600              MOVE JA  TO RET-ALLT-FEL-SW                                 
203700           END-IF                                                         
203800         END-IF                                                           
203900                                                                          
204000         IF INDATA-OK                                                     
204100           PERFORM S06-SEARCH-IDLAND                                      
204200           PERFORM IMS-GU-WLLEVA11                                        
204300** KONTROLLERAR ATT EK-SEGMENT FINNS UPPLAGT PÅ LEVERANTÖREN              
204400           IF SEGMENT-SAKNAS                                              
204500             MOVE NEJ TO INDATA-SW                                        
204600             MOVE JA  TO WDF11-SAKNAS-SW                                  
204700           END-IF                                                         
204800         END-IF                                                           
204900                                                                          
205000         IF INDATA-OK                                                     
205100           IF (MID-FLANN = JA OR YES) AND                                 
205200              (KR-KDKRSTA = '0' OR '1')                                   
205300               CONTINUE                                                   
205400           ELSE                                                           
205500* KOLLAR ATT FAX/MAIL FINNS UPPDATERAT PÅ W6F1                            
205600             PERFORM IMS-GU-W6LEVA01                                      
205700             IF SEGMENT-FINNS                                             
205800               IF LEV-IDMAIL(WS-KDMEMO) NOT = SPACE                       
205900                 MOVE LEV-IDMAIL(WS-KDMEMO)  TO WS-MAIL-LEV               
206000                 MOVE LEV-IDLEVFAX(WS-KDFAX) TO KVIR-IDLEVFAX             
206100               ELSE                                                       
206200                 MOVE NEJ TO INDATA-SW                                    
206300                 MOVE JA  TO W6F1-SAKNAS-SW                               
206400               END-IF                                                     
206500             ELSE                                                         
206600               MOVE NEJ TO INDATA-SW                                      
206700               MOVE JA  TO W6F1-SAKNAS-SW                                 
206800             END-IF                                                       
206900           END-IF                                                         
207000         END-IF                                                           
207100                                                                          
207200         IF INDATA-FEL                                                    
207300            MOVE NEJ                     TO ALLT-SW                       
207400            IF MOT-KONTROLL-EJ-KLAR                                       
207500               MOVE ERR-MOTKONTR-EJ-KLAR TO MED-IDMFSFEL                  
207600            ELSE                                                          
207700               IF KVART-FEL                                               
207800                  MOVE ERR-ANTAL-FEL     TO MED-IDMFSFEL                  
207900               ELSE                                                       
208000                  IF KDDISP-OK = NEJ OR WS-FLGODK-VIR = NEJ               
208100                    MOVE INF-FEL-KDDISP  TO MED-IDMFSFEL                  
208200                  ELSE                                                    
208300                    IF KDHANDCO-OK = NEJ                                  
208400                      MOVE INF-HANDCO-NOT-UPDATED                         
208500                                         TO MED-IDMFSFEL                  
208600                    ELSE                                                  
208700                      IF KDDISP-RET-OK = NEJ                              
208800                        MOVE OTILL-DISP-RET  TO MED-IDMFSFEL              
208900                      ELSE                                                
209000                        IF KDDISP-ADJ-OK = NEJ                            
209100                          MOVE OTILL-DISP-ADJ  TO MED-IDMFSFEL            
209200                        ELSE                                              
209300                          IF KDDISP-SKR-OK = NEJ                          
209400                            MOVE OTILL-DISP-SKR  TO MED-IDMFSFEL          
209500                          ELSE                                            
209600                            IF ANSK-INK-FINNS = NEJ                       
209700                              MOVE FEL-ANSK-INK  TO MED-IDMFSFEL          
209800                            ELSE                                          
209900                              IF WS-ANTAL-OK = NEJ                        
210000                                MOVE QUANT-TOO-BIG TO MED-IDMFSFEL        
210100                              ELSE                                        
210200                                IF ATT-ANNUL = NEJ                        
210300                                  MOVE NO-ATT-CHANGE                      
210400                                                 TO MED-IDMFSFEL          
210500                                ELSE                                      
210600                                  IF RET-ALLT-FEL                         
210700                                    MOVE UPD-NOT-ALLOWED                  
210800                                             TO MED-IDMFSFEL              
210900                                  ELSE                                    
211000                                    IF WDF11-FEL                          
211100                                      MOVE VALUTAKOD-SAKNAS               
211200                                             TO MED-IDMFSFEL              
211300                                    ELSE                                  
211400                                      IF W6F1-FEL                         
211500                                        MOVE FAX-MAIL-SAKNAS              
211600                                             TO MED-IDMFSFEL              
211700                                      ELSE                                
211800                                        IF PF23-PRESS                     
211900                                         MOVE SPLITT-PRESS-PF23           
212000                                             TO MED-IDMFSFEL              
212100                                        ELSE                              
212200                                         MOVE ERR-CORR-HILITE-FLDS        
212300                                             TO MED-IDMFSFEL              
212400                                        END-IF                            
212500                                      END-IF                              
212600                                    END-IF                                
212700                                  END-IF                                  
212800                                END-IF                                    
212900                              END-IF                                      
213000                            END-IF                                        
213100                          END-IF                                          
213200                        END-IF                                            
213300                      END-IF                                              
213400                    END-IF                                                
213500                  END-IF                                                  
213600               END-IF                                                     
213700            END-IF                                                        
213800            IF R32-KLAR                                                   
213900              MOVE INF-UPPDAT-OTILLATET  TO MED-IDMFSINF                  
214000            END-IF                                                        
214100            CALL WMEDKONV USING MED-WMEDAREA                              
214200            MOVE MED-MFSFEL  TO MOD-TEMFSFEL                              
214300            PERFORM MFS-ROER-EJ-FAELT-IN-UT                               
214400            PERFORM MFS-EV-SPAERRA-FAELT                                  
214500            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRTOVIR-ATTR              
214600                                          MOD-FLEJKNTRL-ATTR              
214700                                          MOD-FLKRLFEL-ATTR               
214800                                          MOD-FLKRLIM-ATTR                
214900                                          MOD-FLKRGODK-ATTR               
215000                                          MOD-FLANN-ATTR                  
215100                                          MOD-FLKROMK-ATTR                
215200         END-IF                                                           
215300                                                                          
215400         IF INDATA-OK AND KR-IDKRFEL(1:1) NOT = 'P'                       
215500** FÖR ATT TVINGA FRAM EN BACKNING AV INLAGT ANTAL DÅ INLAGT ANTAL        
215600** ÄR FÖR STORT OCH SVS-SALDO ÄR UPPDATERAT.                              
215700           PERFORM GB-KOLLA-SVS-UPPDATERAD                                
215800           IF WS-FLSVS = JA                                               
215900             COMPUTE KR-ANTAL = KR-KVART-RET + KR-KVART-SKROT +           
216000                                KR-KVART-SJUST                            
216100             IF WS-KVLEVANDE < KR-ANTAL                                   
216200               MOVE NEJ TO INDATA-SW                                      
216300               MOVE QUANT-TOO-BIG TO MED-IDMFSFEL                         
216400               CALL WMEDKONV USING MED-WMEDAREA                           
216500               MOVE MED-MFSFEL  TO MOD-TEMFSFEL                           
216600               PERFORM MFS-ROER-EJ-FAELT-IN-UT                            
216700               PERFORM MFS-EV-SPAERRA-FAELT                               
216800               MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRTOVIR-ATTR           
216900                                             MOD-FLEJKNTRL-ATTR           
217000                                             MOD-FLKRLFEL-ATTR            
217100                                             MOD-FLKRLIM-ATTR             
217200                                             MOD-FLKRGODK-ATTR            
217300                                             MOD-FLANN-ATTR               
217400             END-IF                                                       
217500           END-IF                                                         
217600         END-IF                                                           
217700       END-IF                                                             
217800     END-IF                                                               
217900     .                                                                    
218000     EJECT                                                                
218100 GA-KONTROLL-MOTKONTROLL SECTION.                                         
218200     SKIP2                                                                
218300     IF KR-KDKRSTA = '0' OR '1'                                           
218400*       IF KR-IDKRFEL = 'PA' OR 'PB'                                      
218500** FIX FÖR ATT KUNNA ANNULERA 1 AV 2 ADMINISTRATIVA KR                    
218600        IF (KR-IDKRFEL = 'PA' OR 'PB') OR KR-IDKR = 1347                  
218700           CONTINUE                                                       
218800        ELSE                                                              
218900           IF MID-FLKRGODK = JA OR YES                                    
219000              MOVE KR-IDLOPNRM     TO W-IDLOPNRM                          
219100              PERFORM IMS-GU-UPFA-01                                      
219200              IF SEGMENT-FINNS                                            
219300                 IF UPPF-KDKVASTA-PRI = '1'                               
219400                 OR UPPF-KDKVASTA-SEK = '1'                               
219500                    MOVE NEJ       TO MOT-KONTROLL-SW                     
219600                    MOVE NEJ       TO INDATA-SW                           
219700                 END-IF                                                   
219800                                                                          
219900                 IF KR-IDKRFEL = 'K '                                     
220000                   IF UPPF-KDKVASTA-ADM = '3'                             
220100                      MOVE UPPF-KDKVASTA-ADM TO WS-SPAR-KDKVASTA          
220200                   END-IF                                                 
220300                 END-IF                                                   
220400                                                                          
220500                 IF UPPF-KDKVASTA-PRI = '3'                               
220600                    MOVE UPPF-KDKVASTA-PRI TO WS-SPAR-KDKVASTA            
220700                 END-IF                                                   
220800                                                                          
220900                 IF UPPF-KDKVASTA-SEK = '3'                               
221000                    MOVE UPPF-KDKVASTA-SEK TO WS-SPAR-KDKVASTA            
221100                 END-IF                                                   
221200                                                                          
221300                 PERFORM IMS-GNP-UPFA-11                                  
221400                 PERFORM UNTIL SEGMENT-SAKNAS OR                          
221500                               MOT-KONTROLL-EJ-KLAR                       
221600                   IF RAPP-KDKVASTA-PRI = '1'                             
221700                       MOVE NEJ TO MOT-KONTROLL-SW                        
221800                       MOVE NEJ TO INDATA-SW                              
221900                   END-IF                                                 
222000                   IF RAPP-KDKVASTA-PRI = '3'                             
222100                      MOVE RAPP-KDKVASTA-PRI TO WS-SPAR-KDKVASTA          
222200                   END-IF                                                 
222300                   PERFORM IMS-GNP-UPFA-11                                
222400                 END-PERFORM                                              
222500                                                                          
222600                 PERFORM IMS-GNP-UPFA-12                                  
222700                 PERFORM UNTIL SEGMENT-SAKNAS OR                          
222800                               MOT-KONTROLL-EJ-KLAR                       
222900                    IF SPEC-KDKVASTA-PRI = '1'                            
223000                       MOVE NEJ TO MOT-KONTROLL-SW                        
223100                       MOVE NEJ TO INDATA-SW                              
223200                    END-IF                                                
223300                    IF SPEC-KDKVASTA-PRI = '3'                            
223400                       MOVE SPEC-KDKVASTA-PRI TO WS-SPAR-KDKVASTA         
223500                    END-IF                                                
223600                    PERFORM IMS-GNP-UPFA-12                               
223700                 END-PERFORM                                              
223800                 IF WS-SPAR-KDKVASTA NOT = '3'                            
223900                    MOVE NEJ TO MOT-KONTROLL-SW                           
224000                 END-IF                                                   
224100              END-IF                                                      
224200              IF MOT-KONTROLL-KLAR                                        
224300                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKRGODK-ATTR           
224400                 MOVE MID-FLKRGODK         TO WS-FLKRGODK                 
224500              ELSE                                                        
224600                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRGODK-ATTR           
224700                 MOVE NEJ                  TO INDATA-SW                   
224800              END-IF                                                      
224900           END-IF                                                         
225000                                                                          
225100           IF MID-FLANN = JA OR YES                                       
225200              MOVE KR-IDLOPNRM     TO W-IDLOPNRM                          
225300              PERFORM IMS-GU-UPFA-01                                      
225400              IF SEGMENT-FINNS                                            
225500                 IF KR-IDKRFEL = 'K '                                     
225600                   IF UPPF-KDKVASTA-ADM =  '3'                            
225700                      MOVE UPPF-KDKVASTA-ADM TO WS-SPAR-KDKVASTA          
225800                   END-IF                                                 
225900                 ELSE                                                     
226000                   IF UPPF-KDKVASTA-PRI = '1' OR '3'                      
226100                      MOVE UPPF-KDKVASTA-PRI TO WS-SPAR-KDKVASTA          
226200                   END-IF                                                 
226300                                                                          
226400                   IF UPPF-KDKVASTA-SEK = '1' OR '3'                      
226500                      MOVE UPPF-KDKVASTA-SEK TO WS-SPAR-KDKVASTA          
226600                   END-IF                                                 
226700                 END-IF                                                   
226800                                                                          
226900                 PERFORM IMS-GNP-UPFA-11                                  
227000                 PERFORM UNTIL SEGMENT-SAKNAS                             
227100                   IF RAPP-KDKVASTA-PRI = '1' OR '3'                      
227200                      MOVE RAPP-KDKVASTA-PRI TO WS-SPAR-KDKVASTA          
227300                   END-IF                                                 
227400                   PERFORM IMS-GNP-UPFA-11                                
227500                 END-PERFORM                                              
227600                                                                          
227700                 PERFORM IMS-GNP-UPFA-12                                  
227800                 PERFORM UNTIL SEGMENT-SAKNAS OR                          
227900                               MOT-KONTROLL-EJ-KLAR                       
228000                   IF SPEC-KDKVASTA-PRI = '1' OR '3'                      
228100                      MOVE SPEC-KDKVASTA-PRI TO WS-SPAR-KDKVASTA          
228200                   END-IF                                                 
228300                   PERFORM IMS-GNP-UPFA-12                                
228400                 END-PERFORM                                              
228500              END-IF                                                      
228600              IF WS-SPAR-KDKVASTA = '1' OR '3'                            
228700                 IF MID-FLKRGODK NOT = ALL '+'                            
228800                   MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKRGODK-ATTR         
228900                 END-IF                                                   
229000                 IF MID-FLANN    NOT = ALL '+'                            
229100                   MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLANN-ATTR            
229200                 END-IF                                                   
229300                 MOVE NEJ TO MOT-KONTROLL-SW                              
229400                 MOVE NEJ TO INDATA-SW                                    
229500              ELSE                                                        
229600                 MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLKRGODK-ATTR         
229700                 MOVE MID-FLKRGODK           TO WS-FLKRGODK               
229800              END-IF                                                      
229900           END-IF                                                         
230000        END-IF                                                            
230100     END-IF                                                               
230200                                                                          
230300     IF  INDATA-OK                                                        
230400     AND (MID-FLKRGODK = JA OR YES)                                       
230500     AND KR-IDLOPNRM  > +0                                                
230600        MOVE KR-IDLOPNRM TO W-IDLOPNRM-CSEQ                               
230700        MOVE KR-DAAVSDAT TO W-DAAVSDAT-CSEQ                               
230800        PERFORM IMS-GU-W6KVAI01                                           
230900        IF SEGMENT-FINNS                                                  
231000           IF CSEQ-KR-FLANNULL = JA                                       
231100              CONTINUE                                                    
231200           ELSE                                                           
231300              IF CSEQ-KR-KVANTMOT > +0                                    
231400                 COMPUTE WS-SUM-ANTAL = CSEQ-KR-KVAVIS +                  
231500                                        CSEQ-KR-KVART-SJUST               
231600              ELSE                                                        
231700                 COMPUTE WS-SUM-KVART = CSEQ-KR-KVART-RET   +             
231800                                        CSEQ-KR-KVART-SKROT +             
231900                                        CSEQ-KR-KVART-BEH   +             
232000                                        CSEQ-KR-KVART-SJUST               
232100              END-IF                                                      
232200           END-IF                                                         
232300        END-IF                                                            
232400        PERFORM IMS-GN-W6KVAI01                                           
232500        IF SEGMENT-FINNS                                                  
232600           IF CSEQ-KR-FLANNULL = JA                                       
232700              CONTINUE                                                    
232800           ELSE                                                           
232900              IF CSEQ-KR-KVANTMOT > +0                                    
233000                 COMPUTE WS-SUM-ANTAL = WS-SUM-ANTAL        +             
233100                                        CSEQ-KR-KVAVIS      +             
233200                                        CSEQ-KR-KVART-SJUST               
233300              ELSE                                                        
233400                 COMPUTE WS-SUM-KVART = WS-SUM-KVART        +             
233500                                        CSEQ-KR-KVART-RET   +             
233600                                        CSEQ-KR-KVART-SKROT +             
233700                                        CSEQ-KR-KVART-BEH   +             
233800                                        CSEQ-KR-KVART-SJUST               
233900              END-IF                                                      
234000           END-IF                                                         
234100        END-IF                                                            
234200        IF CSEQ-KR-KVAVIS = +0                                            
234300          CONTINUE                                                        
234400        ELSE                                                              
234500          IF WS-SUM-ANTAL = +0                                            
234600             IF WS-SUM-KVART > CSEQ-KR-KVAVIS                             
234700                MOVE JA              TO KVART-FEL-SW                      
234800                MOVE NEJ             TO INDATA-SW                         
234900             END-IF                                                       
235000          ELSE                                                            
235100             IF WS-SUM-KVART > WS-SUM-ANTAL                               
235200                MOVE JA              TO KVART-FEL-SW                      
235300                MOVE NEJ             TO INDATA-SW                         
235400             END-IF                                                       
235500          END-IF                                                          
235600        END-IF                                                            
235700     END-IF                                                               
235800     .                                                                    
235900     EJECT                                                                
236000 GB-KOLLA-SVS-UPPDATERAD SECTION.                                         
236100     MOVE ZERO TO WS-KVLEVANDE                                            
236200     MOVE NEJ  TO WS-FLSVS                                                
236300     MOVE KR-IDLOPNRM TO W-IDLOPNRM                                       
236400     MOVE KR-IDARTNR  TO W-IDARTNR                                        
236500     PERFORM IMS-GHU-W6INLA11                                             
236600     IF SEGMENT-FINNS                                                     
236700       PERFORM IMS-GNP-W6INLA21                                           
236800       IF SEGMENT-FINNS                                                   
236900         PERFORM UNTIL SEGMENT-SAKNAS                                     
237000           IF RAD-KDINLSTA NOT = 'INL' AND 'VOR' AND 'ANT' AND            
237100                                 'AVV'                                    
237200             ADD RAD-KVINLART TO WS-KVLEVANDE                             
237300           END-IF                                                         
237400           IF RAD-FLSVSLS = JA                                            
237500             MOVE JA TO WS-FLSVS                                          
237600           END-IF                                                         
237700           PERFORM IMS-GNP-W6INLA21                                       
237800         END-PERFORM                                                      
237900       END-IF                                                             
238000     END-IF                                                               
238100     .                                                                    
238200     EJECT                                                                
238300 H-UPPDATERA SECTION.                                                     
238400                                                                          
238500     PERFORM IMS-GHU-W6KVAE01                                             
238600     IF SEGMENT-FINNS                                                     
238700       MOVE KR-IDDC TO WS-IDDC                                            
238800       IF NDC-CN OR NDC-US                                                
238900         CONTINUE                                                         
239000       ELSE                                                               
239100         PERFORM HA-INITIERA-2191-MID                                     
239200       END-IF                                                             
239300*                                                                         
239400       IF KR-KDKRSTA > '1'                                                
239500          IF MID-FLANN = JA OR YES                                        
239600             PERFORM S01-JUSTERING                                        
239700             IF NDC-CN OR NDC-US                                          
239800               CONTINUE                                                   
239900             ELSE                                                         
240000**             LARM TILL ANSKAFFARE                                       
240100               MOVE 302 TO ALT1-MID-KDLARM                                
240200               PERFORM IMS-ISRT-ALT1-MSG                                  
240300             END-IF                                                       
240400          ELSE                                                            
240500             IF KR-FLKRGODK = NEJ                                         
240600                IF  MID-KVARBTID-IN = ALL '+'                             
240700                AND MID-SUMAT-IN    = ALL '+'                             
240800                AND MID-SUOMK-IN    = ALL '+'                             
240900                AND MID-FLKRLFEL    = ALL '+'                             
241000                    CONTINUE                                              
241100                ELSE                                                      
241200                    PERFORM S01-JUSTERING                                 
241300                END-IF                                                    
241400             END-IF                                                       
241500          END-IF                                                          
241600       END-IF                                                             
241700                                                                          
241800       MOVE KR-KDKRSTA TO WS-KDKRSTA                                      
241900       PERFORM HB-FLYTTA-TILL-KR-REG                                      
242000                                                                          
242100** OM INGEN RETURADRESS SPECIFICERATS SÄTTS NR 1 PÅ W6F1.                 
242200       IF KR-KVART-RET > 0                                                
242300         IF KR-IDLEVG = 0                                                 
242400           MOVE +1 TO KR-IDLEVG                                           
242500         END-IF                                                           
242600       END-IF                                                             
242700       MOVE WS-FLEJKNTRL TO KR-FLEJKNTRL                                  
242800       MOVE WS-KDFAX     TO KR-KDFAXVAL                                   
242900       MOVE WS-KDMEMO    TO KR-KDMEMVAL                                   
243000       IF WS-FLKRLIM = JA OR YES                                          
243100         MOVE JA         TO KR-FLKRLIM                                    
243200         MOVE ZERO       TO KR-SUKRLIM                                    
243300       ELSE                                                               
243400         MOVE NEJ        TO KR-FLKRLIM                                    
243500         MOVE WS-SUKRLIM TO KR-SUKRLIM                                    
243600       END-IF                                                             
243700                                                                          
243800       PERFORM IMS-REPL-W6KVAE01                                          
243900                                                                          
244000       IF ((MID-FLKRGODK = JA OR YES) AND WS-KDKRSTA = 1)                 
244100        OR ((WS-FLKRGODK = JA OR YES)                                     
244200             AND KR-KDKRJUST NOT = SPACE)                                 
244300        OR ((MID-FLANN = JA OR YES) AND KR-KDKRSTA > '1')                 
244400          IF MID-FLKRLIM = NEJ AND CDC-SE                                 
244500            PERFORM S04-SKICKA-TRANS-W6T193X                              
244600                                                                          
244700            IF (MID-FLKRGODK = JA OR YES) AND                             
244800               KR-IDKRFEL(1:1) = 'P' AND                                  
244900               CDC-SE                                                     
245000              PERFORM IMS-GHU-W6KVAE14                                    
245100              IF SEGMENT-FINNS                                            
245200                PERFORM VARYING INDX FROM +15 BY -1                       
245300                  UNTIL INDX < +1                                         
245400                     OR TEXT-TEKRFEL (INDX) NOT = SPACE                   
245500                END-PERFORM                                               
245600                COMPUTE INDX = INDX + 1                                   
245700                IF INDX <= +15                                            
245800                  MOVE WS-TEXT     TO TEXT-TEKRFEL (INDX)                 
245900                  PERFORM IMS-REPL-W6KVAE14                               
246000                END-IF                                                    
246100              ELSE                                                        
246200                MOVE 1             TO TEXT-KDSEGKEY                       
246300                MOVE WS-TEXT       TO TEXT-TEKRFEL (1)                    
246400                PERFORM IMS-ISRT-W6KVAE14                                 
246500              END-IF                                                      
246600            ELSE                                                          
246700              IF MID-FLANN = JA OR YES                                    
246800                PERFORM S04-SKICKA-TRANS-W6T193X                          
246900              END-IF                                                      
247000            END-IF                                                        
247100          ELSE                                                            
247200            IF KR-KDDISP = 77                                             
247300              CONTINUE                                                    
247400            ELSE                                                          
247500              PERFORM S03-CALL-W426KVIR                                   
247600            END-IF                                                        
247700          END-IF                                                          
247800**          LARM                                                          
247900          IF (WS-FLKRGODK = JA OR YES) AND                                
248000             KR-KDKRJUST NOT = SPACE AND CDC-SE                           
248100            MOVE 301 TO ALT1-MID-KDLARM                                   
248200            PERFORM IMS-ISRT-ALT1-MSG                                     
248300          END-IF                                                          
248400       END-IF                                                             
248500       IF WS-KDKRSTA > 1 AND MID-FAX-TLX NOT = ALL '+'                    
248600         IF KR-KDDISP = 77                                                
248700            CONTINUE                                                      
248800         ELSE                                                             
248900            PERFORM S03-CALL-W426KVIR                                     
249000         END-IF                                                           
249100       END-IF                                                             
249200                                                                          
249300       IF KR-IDLEVNR = 'AFGE9'                                            
249400         PERFORM IMS-GU-WDF502                                            
249500         IF SEGMENT-FINNS                                                 
249600           PERFORM IMS-GHU-W6KVAE14                                       
249700           MOVE SPACE          TO TEXT-TEKRFEL(15)                        
249800           MOVE 'LYNK&CO NO. ' TO TEXT-TEKRFEL(15)(1:12)                  
249900           MOVE XLEV-BELEVART  TO TEXT-TEKRFEL(15)(13:30)                 
250000           IF SEGMENT-FINNS                                               
250100             PERFORM IMS-REPL-W6KVAE14                                    
250200           ELSE                                                           
250300             MOVE 1 TO TEXT-KDSEGKEY                                      
250400             PERFORM IMS-ISRT-W6KVAE14                                    
250500           END-IF                                                         
250600         END-IF                                                           
250700       END-IF                                                             
250800                                                                          
250900** REGLER FÖR NÄR W602KRUP SKA ANROPAS                                    
251000       MOVE SPACE TO KRUP-IDVERNR-OK                                      
251100       IF MID-FLKRGODK = JA OR YES                                        
251200         IF KR-KDKRSTA < '5'                                              
251300           MOVE NEJ TO KRUP-FLANNULL                                      
251400** KR EJ TIDIGARE AVSLUTAD                                                
251500           IF KR-KDKRSTA = '2'                                            
251600              IF WS-FLKROMK = JA OR SPACE OR YES                          
251700                 IF KR-KVART-RET = ZERO OR KR-BEKRPACK NOT = SPACE        
251800** PARTIET VÄNTAR INTE PÅ ATT PACKRAPPORTERAS                             
251900                    PERFORM S05-CALL-W602KRUP                             
252000                 END-IF                                                   
252100              END-IF                                                      
252200           ELSE                                                           
252300              PERFORM S05-CALL-W602KRUP                                   
252400           END-IF                                                         
252500         END-IF                                                           
252600       END-IF                                                             
252700       IF (MID-FLANN = JA OR YES) AND KR-KDKRSTA = '5'                    
252800          MOVE JA TO KRUP-FLANNULL                                        
252900          PERFORM S05-CALL-W602KRUP                                       
253000       END-IF                                                             
253100       IF KRUP-IDVERNR-OK = 'F'                                           
253200         PERFORM IMS-ROLLBACK                                             
253300         MOVE ERR-VERNR-OVERSKRIDEN TO MED-IDMFSFEL                       
253400         CALL WMEDKONV USING MED-WMEDAREA                                 
253500         MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                 
253600         PERFORM MFS-ROER-EJ-FAELT-IN-UT                                  
253700         PERFORM MFS-EV-SPAERRA-FAELT                                     
253800         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKRTOVIR-ATTR                 
253900                                       MOD-FLEJKNTRL-ATTR                 
254000                                       MOD-FLKRLFEL-ATTR                  
254100                                       MOD-FLKRLIM-ATTR                   
254200                                       MOD-FLKRGODK-ATTR                  
254300                                       MOD-FLANN-ATTR                     
254400       ELSE                                                               
254500         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
254600         CALL WMEDKONV     USING MED-WMEDAREA                             
254700         MOVE MED-MFSINF      TO MOD-TEMFSINF                             
254800         PERFORM MFS-RENSA-FAELT-IN                                       
254900         PERFORM MFS-FORM-ATTR                                            
255000       END-IF                                                             
255100     END-IF                                                               
255200     .                                                                    
255300     EJECT                                                                
255400 HA-INITIERA-2191-MID SECTION.                                            
255500                                                                          
255600     COMPUTE ALT1-LL = LENGTH OF ALT1-MID-W2I19101 + 17                   
255700     MOVE KR-IDDC (2:1)   TO ALT1-MID-KDCLAGER                            
255800     MOVE KR-IDARTNR      TO ALT1-MID-IDARTNR                             
255900     MOVE WS-IDANSK       TO ALT1-MID-IDANSK                              
256000     MOVE ZERO            TO ALT1-MID-TISENBEK-DAG                        
256100                             ALT1-MID-TISENBEK-KL                         
256200                             ALT1-MID-KDLARM                              
256300                             ALT1-MID-IDDISTR                             
256400                             ALT1-MID-IDKUNDNR                            
256500     MOVE SPACE           TO ALT1-MID-IDKUNDRF                            
256600     MOVE KR-IDKR         TO ALT1-MID-IDKR                                
256700     MOVE SPACE           TO ALT1-MID-FLNYLARM                            
256800     MOVE WC-CDC-SE       TO ALT1-MID-IDDC                                
256900     MOVE SPACE           TO ALT1-MID-IDLEVNR                             
257000     .                                                                    
257100     EJECT                                                                
257200 HB-FLYTTA-TILL-KR-REG SECTION.                                           
257300                                                                          
257400     IF MID-FLKRLFEL NOT = ALL '+'                                        
257500        IF MID-FLKRLFEL = JA OR YES                                       
257600          MOVE JA               TO KR-FLKRLFEL                            
257700        ELSE                                                              
257800          MOVE NEJ              TO KR-FLKRLFEL                            
257900        END-IF                                                            
258000     END-IF                                                               
258100                                                                          
258200     IF MID-KDKRATG NOT = ALL '+'                                         
258300        MOVE MID-KDKRATG        TO KR-KDKRATG                             
258400     END-IF                                                               
258500                                                                          
258600     IF MID-ADATTENT NOT = ALL '+'                                        
258700        MOVE MID-ADATTENT       TO KR-ADATTENT                            
258800     END-IF                                                               
258900                                                                          
259000     IF MID-FLKROMK  NOT = ALL '+'                                        
259100       IF MID-FLKROMK = JA OR YES                                         
259200         MOVE JA  TO KR-FLKROMK                                           
259300       ELSE                                                               
259400         MOVE NEJ TO KR-FLKROMK                                           
259500       END-IF                                                             
259600     END-IF                                                               
259700                                                                          
259800     IF MID-FLARBDEB NOT = ALL '+'                                        
259900       IF MID-FLARBDEB = JA OR YES                                        
260000         MOVE JA  TO KR-FLARBDEB                                          
260100       ELSE                                                               
260200         MOVE NEJ TO KR-FLARBDEB                                          
260300       END-IF                                                             
260400     END-IF                                                               
260500                                                                          
260600     IF MID-KVARBTID-IN NOT = ALL '+'                                     
260700        MOVE WS-KVARBTID       TO KR-KVARBTID                             
260800                                  MOD-KVARBTID-UT                         
260900     END-IF                                                               
261000                                                                          
261100     IF MID-TEKRSPEC-ATID NOT = ALL '+'                                   
261200        MOVE MID-TEKRSPEC-ATID TO KR-TEKRSPEC-ATID                        
261300     END-IF                                                               
261400                                                                          
261500     IF MID-SUOMK-IN NOT = ALL '+'                                        
261600        MOVE MID-SUOMK-IN      TO KR-SUOMK                                
261700                                  MOD-SUOMK-UT                            
261800     END-IF                                                               
261900                                                                          
262000     IF MID-TEKRSPEC-OMK NOT = ALL '+'                                    
262100        MOVE MID-TEKRSPEC-OMK  TO KR-TEKRSPEC-OMK                         
262200     END-IF                                                               
262300                                                                          
262400     IF MID-SUMAT-IN NOT = ALL '+'                                        
262500        MOVE WS-SUMAT          TO KR-SUMAT                                
262600                                  MOD-SUMAT-UT                            
262700     END-IF                                                               
262800                                                                          
262900     IF MID-TEKRSPEC-MAT NOT = ALL '+'                                    
263000        MOVE MID-TEKRSPEC-MAT  TO KR-TEKRSPEC-MAT                         
263100     END-IF                                                               
263200                                                                          
263300     IF MID-KDPERSON NOT = ALL '+'                                        
263400        MOVE WS-BEKRANS        TO KR-BEKRANS                              
263500     END-IF                                                               
263600                                                                          
263700     IF MID-IDKRATLF NOT = ALL '+'  AND                                   
263800        MID-IDKRATLF NOT = SPACE                                          
263900        MOVE MID-IDKRATLF      TO KR-IDKRATLF                             
264000     ELSE                                                                 
264100        IF MID-KDPERSON NOT = ALL '+'                                     
264200           MOVE WS-IDKRATLF    TO KR-IDKRATLF                             
264300        END-IF                                                            
264400     END-IF                                                               
264500                                                                          
264600     IF MID-FLKRLIM NOT = ALL '+'                                         
264700        IF MID-FLKRLIM = JA OR YES                                        
264800          MOVE JA     TO KR-FLKRLIM                                       
264900        ELSE                                                              
265000          MOVE NEJ    TO KR-FLKRLIM                                       
265100        END-IF                                                            
265200     END-IF                                                               
265300                                                                          
265400     IF MID-FLKRGODK NOT = ALL '+'                                        
265500        IF  (MID-FLKRGODK = JA OR YES)                                    
265600        AND KR-FLKRGODK  = NEJ                                            
265700        AND KR-KDKRSTA  > '1'                                             
265800          IF KR-IDLOPNRM > +0                                             
265900            MOVE KR-IDLOPNRM TO W-IDLOPNRM                                
266000            PERFORM IMS-GHU-W6INLA11                                      
266100            IF SEGMENT-FINNS                                              
266200              PERFORM S04-SKICKA-TRANS-W6T193X                            
266300            END-IF                                                        
266400          END-IF                                                          
266500        END-IF                                                            
266600        IF MID-FLKRGODK = JA OR YES                                       
266700          MOVE JA     TO KR-FLKRGODK                                      
266800        ELSE                                                              
266900          MOVE NEJ    TO KR-FLKRGODK                                      
267000        END-IF                                                            
267100        IF  KR-FLKRGODK = NEJ                                             
267200        AND KR-KDKRSTA  > '1'                                             
267300           MOVE SPACE TO KR-KDKRJUST                                      
267400        END-IF                                                            
267500     END-IF                                                               
267600                                                                          
267700     IF MID-FLANN NOT = ALL '+'                                           
267800        IF KR-KDKRSTA > '1'                                               
267900           IF MID-FLANN = JA OR YES                                       
268000              MOVE +0           TO KR-KVKRKNTR                            
268100                                                                          
268200              MOVE KR-IDLOPNRM  TO W-IDLOPNRM                             
268300              PERFORM IMS-GHU-UPFA-01                                     
268400              IF SEGMENT-FINNS                                            
268500                 IF MID-FLANN = JA OR YES                                 
268600                   MOVE JA        TO UPPF-FLANNULL                        
268700                 ELSE                                                     
268800                   MOVE NEJ       TO UPPF-FLANNULL                        
268900                 END-IF                                                   
269000                 PERFORM IMS-REPL-UPFA-01                                 
269100              END-IF                                                      
269200           END-IF                                                         
269300        END-IF                                                            
269400        IF  (KR-KDKRSTA  = '0' OR '1')                                    
269500        AND  KR-FLANNULL = NEJ                                            
269600        AND (MID-FLANN   = JA OR YES)                                     
269700            IF KR-IDLOPNRM > +0                                           
269800               MOVE KR-IDLOPNRM TO W-IDLOPNRM                             
269900               PERFORM IMS-GHU-W6INLA11                                   
270000               IF SEGMENT-FINNS                                           
270100                  PERFORM S04-SKICKA-TRANS-W6T193X                        
270200                  MOVE NEJ      TO ART-FLKVAFEL                           
270300                  MOVE NEJ      TO ART-FLKVAKAR                           
270400                  PERFORM IMS-REPL-W6INLA11                               
270500               END-IF                                                     
270600            END-IF                                                        
270700        END-IF                                                            
270800        IF MID-FLANN = JA OR YES                                          
270900          MOVE JA                 TO KR-FLANNULL                          
271000        ELSE                                                              
271100          MOVE NEJ                TO KR-FLANNULL                          
271200        END-IF                                                            
271300     END-IF                                                               
271400                                                                          
271500* R40                                                                     
271600     IF MID-FLKRGODK = JA OR YES                                          
271700        IF  KR-KVART-RET > ZERO OR KR-KVART-SKROT > ZERO                  
271800            IF KR-IDLOPNRM = ZERO                                         
271900               PERFORM HBA-SKRIV-R40-TILL-DISPATCH                        
272000            END-IF                                                        
272100        END-IF                                                            
272200        IF KR-KDKRSTA = '1'                                               
272300           MOVE DATUM TO KR-TIKRANS                                       
272400        END-IF                                                            
272500     END-IF                                                               
272600     IF MID-FLANN = JA OR YES                                             
272700       IF KR-IDLOPNRM = ZERO AND KR-KDKRSTA > '1'                         
272800          PERFORM HBA-SKRIV-R40-TILL-DISPATCH                             
272900       END-IF                                                             
273000       MOVE +0 TO KR-KVART-TIDGK                                          
273100     ELSE                                                                 
273200        COMPUTE KR-KVART-TIDGK = KR-KVART-SKROT + KR-KVART-RET            
273300     END-IF                                                               
273400     IF WS-FLKRGODK = JA OR YES                                           
273500        MOVE KR-KDKRSTA TO WS-SPAR-KDKRSTA                                
273600        IF KR-KDKRSTA = '1'                                               
273700           IF WS-FLKROMK = JA OR SPACE OR YES                             
273800              IF KR-KVART-RET = ZERO                                      
273900                 MOVE '3' TO KR-KDKRSTA                                   
274000              ELSE                                                        
274100                 MOVE '2' TO KR-KDKRSTA                                   
274200              END-IF                                                      
274300           ELSE                                                           
274400              MOVE '2'    TO KR-KDKRSTA                                   
274500           END-IF                                                         
274600           IF NDC-CN OR NDC-US                                            
274700             CONTINUE                                                     
274800           ELSE                                                           
274900**           LARM TILL ANSKAFFARE                                         
275000             MOVE 300 TO ALT1-MID-KDLARM                                  
275100             PERFORM IMS-ISRT-ALT1-MSG                                    
275200           END-IF                                                         
275300        ELSE                                                              
275400           IF KR-KDKRSTA = '2'                                            
275500              IF WS-FLKROMK = JA OR SPACE OR YES                          
275600                 IF KR-KVART-RET = ZERO                                   
275700                    MOVE '3' TO KR-KDKRSTA                                
275800                 ELSE                                                     
275900                    IF KR-BEKRPACK NOT = SPACE                            
276000                       MOVE '3' TO KR-KDKRSTA                             
276100                    END-IF                                                
276200                 END-IF                                                   
276300              END-IF                                                      
276400           END-IF                                                         
276500        END-IF                                                            
276600        IF KR-KDKRSTA = '3'                                               
276700           MOVE KR-IDLEVNR  TO TEST-IDLEVNR                               
276800           IF IDLEVNR-HF                                                  
276900              MOVE '5'   TO KR-KDKRSTA                                    
277000           END-IF                                                         
277100        END-IF                                                            
277200        IF  WS-SPAR-KDKRSTA = '1'                                         
277300        AND KR-KDKRSTA      > '1'                                         
277400* TILLÄG FÖR ADMN KR, FÖR ATT DE INTE EFTERFÖLJNADE PARTIER               
277500* SKA ÅKA IN PÅ KONTROLL LÄGGS +0 I FÄLTET FÖR KR-KVKRKNTR                
277600*****                                                                     
277700          IF KR-FLKRLFEL = JA                                             
277800            IF KR-IDKRFEL(1:1) = 'K'                                      
277900              MOVE +0 TO KR-KVKRKNTR                                      
278000            ELSE                                                          
278100              IF WS-FLEJKNTRL = JA                                        
278200                MOVE +0 TO KR-KVKRKNTR                                    
278300              ELSE                                                        
278400                MOVE +1 TO KR-KVKRKNTR                                    
278500              END-IF                                                      
278600            END-IF                                                        
278700          END-IF                                                          
278800          IF KR-IDLOPNRM > +0                                             
278900            PERFORM S04-SKICKA-TRANS-W6T193X                              
279000          END-IF                                                          
279100          IF KR-IDKRFEL (1:1) = 'P' OR 'K'                                
279200            CONTINUE                                                      
279300          ELSE                                                            
279400** TILLÄGG FÖR ADM KR, DÅ SKA INTE RÄKNARNA FÖR SKIPLOT-FREKVENS          
279500** RÄKNAS UPP.                                                            
279600**                                                                        
279700            MOVE KR-IDARTNR TO W-IDARTNR                                  
279800            MOVE KR-IDLEVNR TO W-IDLEVNR                                  
279900            PERFORM IMS-GU-W6KVAH-01                                      
280000            IF SEGMENT-FINNS                                              
280100              MOVE ART-IDPROVPL-PRI         TO WS-IDPROVPL-PRI            
280200              MOVE ART-IDPROVPL-SEK         TO WS-IDPROVPL-SEK            
280300              PERFORM IMS-GHNP-W6KVAH-12                                  
280400              IF SEGMENT-FINNS                                            
280500                IF LEV-FLSKPSAK = NEJ                                     
280600                  IF LEV-KVSKPLOT-PRI > +0                                
280700                    MOVE WS-IDPROVPL-PRI TO W-IDPROVPL                    
280800                    MOVE 'N'             TO W-KDPROVPL                    
280900                    PERFORM IMS-GU-W6PROA11                               
281000                    IF SEGMENT-FINNS                                      
281100                      COMPUTE LEV-KVSKPLOT-PRI =                          
281200                                  6102-KVSKPLOT + 1                       
281300                    ELSE                                                  
281400                      MOVE +6           TO LEV-KVSKPLOT-PRI               
281500                    END-IF                                                
281600                  END-IF                                                  
281700                  IF LEV-KVSKPLOT-SEK > +0                                
281800                    MOVE WS-IDPROVPL-SEK TO W-IDPROVPL                    
281900                    MOVE 'N'             TO W-KDPROVPL                    
282000                    PERFORM IMS-GU-W6PROA11                               
282100                    IF SEGMENT-FINNS                                      
282200                      COMPUTE LEV-KVSKPLOT-SEK =                          
282300                                  6102-KVSKPLOT + 1                       
282400                    ELSE                                                  
282500                      MOVE +6           TO LEV-KVSKPLOT-SEK               
282600                    END-IF                                                
282700                  END-IF                                                  
282800                  IF LEV-KVSKPLOT-PRI > +0                                
282900                  OR LEV-KVSKPLOT-SEK > +0                                
283000                    PERFORM IMS-REPL-W6KVAH-12                            
283100                  END-IF                                                  
283200                END-IF                                                    
283300              END-IF                                                      
283400            END-IF                                                        
283500                                                                          
283600            IF KR-IDLOPNRM > +0                                           
283700              MOVE KR-IDLOPNRM TO W-IDLOPNRM                              
283800              PERFORM IMS-GHU-W6INLA11                                    
283900              IF SEGMENT-FINNS                                            
284000                MOVE NEJ        TO ART-FLKVAFEL                           
284100                MOVE NEJ        TO ART-FLKVAKAR                           
284200                PERFORM IMS-REPL-W6INLA11                                 
284300              END-IF                                                      
284400            END-IF                                                        
284500          END-IF                                                          
284600        END-IF                                                            
284700        IF KR-IDLOPNRM > +0                                               
284800           PERFORM HBD-UPPDATERA-KVAL-HIST                                
284900        END-IF                                                            
285000     END-IF                                                               
285100     .                                                                    
285200     EJECT                                                                
285300 HBA-SKRIV-R40-TILL-DISPATCH SECTION.                                     
285400*                                                                         
285500* DET SKA SKAPAS R40 OM ARTIKEL DÄR FEL UPPTÄCKTS I LAGRET                
285600* SKA SKROTAS OCH/ELLER RETURNERAS                                        
285700*                                                                         
285800     MOVE SPACE                TO 611D-MID-W6I11D01                       
285900     MOVE 'W6020300'           TO 611D-MID-IDPGM                          
286000     MOVE 1                    TO 611D-MID-KVPOST                         
286100     MOVE ZERO                 TO 611D-MID-KVANTAL(1)                     
286200                                                                          
286300     IF MID-FLANN = JA OR YES                                             
286400       COMPUTE 611D-MID-KVANTAL(1) =                                      
286500             (KR-KVART-TIDGK - KR-KVART-SKROT-LDC) * (-1)                 
286600     ELSE                                                                 
286700       COMPUTE WS-CHANGED-QTY = KR-KVART-SKROT + KR-KVART-RET             
286800                              - KR-KVART-TIDGK                            
286900       IF WS-CHANGED-QTY NOT = 0                                          
287000         COMPUTE 611D-MID-KVANTAL(1)                                      
287100               = KR-KVART-SKROT + KR-KVART-RET - KR-KVART-TIDGK           
287200                 - KR-KVART-SKROT-LDC                                     
287300       END-IF                                                             
287400       IF (KR-FLKVALSP = 'J' OR 'Y')                                      
287500       OR (KR-FLBUFJUS = 'J' OR 'Y')                                      
287600         PERFORM HBAA-UPD-SKROT-SALDO                                     
287700       END-IF                                                             
287800     END-IF                                                               
287900                                                                          
288000     IF 611D-MID-KVANTAL(1) NOT = ZERO                                    
288100       MOVE KR-IDLEVNR           TO 611D-MID-IDLEVNR(1)                   
288200       MOVE KR-IDKR              TO 611D-MID-IDORDNR(1)                   
288300       MOVE KR-IDARTNR           TO 611D-MID-IDARTNR(1)                   
288400       MOVE KR-KVART-SKROT-LDC   TO 611D-MID-KVART-SKROT-LDC              
288500                                                                          
288600       MOVE ZERO                 TO 611D-MID-FLUPPBR(1)                   
288700       MOVE KR-IDDC              TO 611D-MID-IDDC(1)                      
288800                                                                          
288900       MOVE SPACE                TO MSG-KOM-WMSGKOM                       
289000       COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                   
289100       MOVE LOW-VALUE            TO MSG-KOM-KDZ1                          
289200       MOVE LOW-VALUE            TO MSG-KOM-KDZ2                          
289300       MOVE SPACE                TO MSG-KOM-KDTRANS                       
289400       MOVE 'W6I11D01'           TO MSG-KOM-IDCPYTXT                      
289500       MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                      
289600       MOVE 'W6020300'           TO MSG-KOM-IDSNDJOB                      
289700       ACCEPT MSG-KOM-TIREGDAT FROM DATE                                  
289800       ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                  
289900       MOVE SPACE                TO MSG-KOM-IDMFSMED                      
290000                                                                          
290100       MOVE +65                  TO P-TO-P-MSG-KVLL                       
290200**CTEXTLÄNGD + KOM-MSG-AREA => 46 + 17                                    
290300**(OBSERVERA ATT ENDAST 1:A OCCURS-EN I W6I11D01 ANVÄNDS)                 
290400       MOVE 'W6T11DX '           TO P-TO-P-MSG-KDTRANS                    
290500       MOVE '611D'               TO P-TO-P-MSG-IDTRANS                    
290600       MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                   
290700       MOVE MID-611D-AREA        TO P-TO-P-MSG-INDATA                     
290800                                                                          
290900       CALL W006KOM USING MSG-PCB                                         
291000                          DISP-PCB                                        
291100                          KOM-KOMA-PCB                                    
291200                          MSG-KOM-WMSGKOM                                 
291300                          P-TO-P-MSG-IO-AREA-SNUF                         
291400     END-IF                                                               
291500     .                                                                    
291600     EJECT                                                                
291700 HBAA-UPD-SKROT-SALDO SECTION.                                            
291800     IF KR-FLKVALSP = 'J' OR 'Y'                                          
291900       MOVE KR-IDARTNR TO W-IDARTNR                                       
292000       PERFORM IMS-GU-WLARTC01                                            
292100       PERFORM IMS-GHNP-WLARTC11                                          
292200                                                                          
292300*        --- BOKAR NER KVALITETSSPÄRRAT ANTAL                             
292400       COMPUTE ARTC-CLAG-KVSPARR-KVAL = ARTC-CLAG-KVSPARR-KVAL            
292500                                   - KR-KVART-SKROT                       
292600                                   - KR-KVART-RET                         
292700               END-COMPUTE                                                
292800       IF ARTC-CLAG-KVSPARR-KVAL < +0                                     
292900         MOVE +0  TO ARTC-CLAG-KVSPARR-KVAL                               
293000       END-IF                                                             
293100                                                                          
293200                                                                          
293300       MOVE MSGI-IDUSER      TO ARTC-CLAG-IDUSER-SPKVAL                   
293400       MOVE DATUM            TO ARTC-CLAG-TISPARR-KVAL                    
293500       PERFORM IMS-REPL-WLARTC11                                          
293600     END-IF                                                               
293700*      --- BOKAR NER WDD8 VID SKROTNING OCH RETUR                         
293800     COMPUTE W-KVSALDO-REST = KR-KVART-SKROT + KR-KVART-RET               
293900             END-COMPUTE                                                  
294000     PERFORM IMS-GU-WDD801                                                
294100     IF SEGMENT-FINNS                                                     
294200       PERFORM IMS-GHNP-WDD811                                            
294300       PERFORM UNTIL SEGMENT-SAKNAS                                       
294400                  OR W-KVSALDO-REST <= +0                                 
294500         MOVE SALDO-ADBUFFPL TO W-ADBUFFPL                                
294600         IF (KR-FLBUFJUS = 'J' OR 'Y')                                    
294700            AND (SALDO-ADBUFFOMR = 59                                     
294800            AND W-ADBUFFPL (1:2) = 97)                                    
294900           IF W-KVSALDO-REST > SALDO-KVBUFF-F                             
295000             COMPUTE W-KVSALDO-REST = W-KVSALDO-REST -                    
295100                                      SALDO-KVBUFF-F                      
295200                     END-COMPUTE                                          
295300             MOVE ZERO TO SALDO-KVBUFF-F                                  
295400           ELSE                                                           
295500             COMPUTE SALDO-KVBUFF-F = SALDO-KVBUFF-F -                    
295600                                      W-KVSALDO-REST                      
295700                     END-COMPUTE                                          
295800             MOVE ZERO TO W-KVSALDO-REST                                  
295900           END-IF                                                         
296000           IF SALDO-KVBUFF-F <= ZERO                                      
296100             PERFORM IMS-DLET-WDD811                                      
296200             PERFORM IMS-GU-WDJ901                                        
296300             PERFORM UNTIL SEGMENT-SAKNAS OR (HIST-KDLOC = 'B'            
296400                              AND HIST-IDDC = 11                          
296500                              AND SALDO-ADBUFFOMR = HIST-ADLAGOMR         
296600                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
296700                              AND SALDO-ADBUFFPL = HIST-ADPLATS)          
296800               PERFORM IMS-GHNP-WDJ911                                    
296900               IF SEGMENT-FINNS AND HIST-KDLOC = 'B'                      
297000                                AND HIST-IDDC = 11                        
297100                              AND SALDO-ADBUFFOMR = HIST-ADLAGOMR         
297200                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
297300                              AND SALDO-ADBUFFPL = HIST-ADPLATS           
297400                 MOVE FUNCTION CURRENT-DATE(1:8) TO                       
297500                                       HIST-DASTODAT                      
297600                 MOVE MSGI-IDUSER TO HIST-IDUSER-STO                      
297700                 PERFORM IMS-REPL-WDJ911                                  
297800               END-IF                                                     
297900             END-PERFORM                                                  
298000           ELSE                                                           
298100             PERFORM IMS-REPL-WDD811                                      
298200           END-IF                                                         
298300         END-IF                                                           
298400         PERFORM IMS-GHNP-WDD811                                          
298500       END-PERFORM                                                        
298600     END-IF                                                               
298700     .                                                                    
298800     EJECT                                                                
298900                                                                          
299000 HBD-UPPDATERA-KVAL-HIST SECTION.                                         
299100                                                                          
299200     IF KR-IDKRFEL = 'PA' OR 'PB'                                         
299300        MOVE KR-IDLOPNRM            TO W-IDLOPNRM                         
299400        PERFORM IMS-GHU-UPFA-01                                           
299500        IF SEGMENT-FINNS                                                  
299600           IF WS-SPAR-KDKRSTA = '0' OR '1'                                
299700              IF KR-FLKRGODK = JA OR YES                                  
299800                 MOVE '3'           TO UPPF-KDKVASTA-ANT                  
299900              ELSE                                                        
300000                 MOVE '2'           TO UPPF-KDKVASTA-ANT                  
300100              END-IF                                                      
300200              PERFORM IMS-REPL-UPFA-01                                    
300300           END-IF                                                         
300400        ELSE                                                              
300500           IF  KR-FLKRGODK     = JA                                       
300600           AND WS-SPAR-KDKRSTA = '1'                                      
300700           AND KR-KDKRSTA      > '1'                                      
300800              MOVE KR-IDLOPNRM      TO UPPF-IDLOPNRM                      
300900              MOVE SPACE            TO UPPF-KDKVAKTL                      
301000              MOVE SPACE            TO UPPF-ADKVAULG                      
301100              MOVE SPACE            TO UPPF-BEANST                        
301200              MOVE NEJ              TO UPPF-FLANNULL                      
301300              MOVE NEJ              TO UPPF-FLKVARED                      
301400              MOVE NEJ              TO UPPF-FLKVAUTV-ANT                  
301500              MOVE NEJ              TO UPPF-FLKVAUTV-KVAL                 
301600              MOVE KR-IDARTNR       TO UPPF-IDARTNR                       
301700              MOVE KR-IDLEVNR       TO UPPF-IDLEVNR                       
301800              MOVE SPACE            TO UPPF-IDUSER-PRI                    
301900              MOVE SPACE            TO UPPF-IDUSER-SEK                    
302000              MOVE SPACE            TO UPPF-IDUSER-ADM                    
302100              MOVE '3'              TO UPPF-KDKVASTA-ANT                  
302200              MOVE '0'              TO UPPF-KDKVASTA-PRI                  
302300              MOVE '0'              TO UPPF-KDKVASTA-ADM                  
302400              MOVE '0'              TO UPPF-KDKVASTA-SEK                  
302500              MOVE +0               TO UPPF-KVKVAPRIM                     
302600              MOVE +0               TO UPPF-KVKVASEK                      
302700              MOVE DATUM            TO UPPF-TIREGDAT                      
302800              PERFORM IMS-ISRT-UPFA-01                                    
302900           END-IF                                                         
303000        END-IF                                                            
303100     END-IF                                                               
303200     .                                                                    
303300     EJECT                                                                
303400 I-SKICKA-EJ-GODK-KR-TILL-VIR SECTION.                                    
303500                                                                          
303600     PERFORM IMS-GHU-W6KVAE01                                             
303700     IF SEGMENT-FINNS                                                     
303800       IF MID-KDPERSON NOT = ALL '+'                                      
303900          MOVE WS-BEKRANS   TO KR-BEKRANS                                 
304000       END-IF                                                             
304100                                                                          
304200       IF MID-IDKRATLF NOT = ALL '+'  AND                                 
304300          MID-IDKRATLF NOT = SPACE                                        
304400          MOVE MID-IDKRATLF TO KR-IDKRATLF                                
304500       ELSE                                                               
304600          IF MID-KDPERSON NOT = ALL '+'                                   
304700             MOVE WS-IDKRATLF TO KR-IDKRATLF                              
304800          END-IF                                                          
304900       END-IF                                                             
305000                                                                          
305100       IF MID-ADATTENT NOT = ALL '+'                                      
305200          MOVE MID-ADATTENT TO KR-ADATTENT                                
305300       END-IF                                                             
305400                                                                          
305500       PERFORM IMS-REPL-W6KVAE01                                          
305600     END-IF                                                               
305700                                                                          
305800     PERFORM S03-CALL-W426KVIR                                            
305900     MOVE INF-KR-SEND TO MED-IDMFSINF                                     
306000     CALL WMEDKONV USING MED-WMEDAREA                                     
306100     MOVE MED-MFSINF  TO MOD-TEMFSFEL                                     
306200     PERFORM MFS-RENSA-FAELT-IN                                           
306300     PERFORM MFS-FORM-ATTR                                                
306400     .                                                                    
306500     EJECT                                                                
306600 S01-JUSTERING SECTION.                                                   
306700                                                                          
306800     IF MID-FLKRLFEL NOT = ALL '+'                                        
306900        IF ((MID-FLKRLFEL = JA OR YES) AND                                
307000           KR-FLKRLFEL = NEJ)       OR                                    
307100          (MID-FLKRLFEL = NEJ       AND                                   
307200           KR-FLKRLFEL = JA )                                             
307300           IF MID-FLKRLFEL = JA OR YES                                    
307400              MOVE '2'         TO WS-KDKRJUST                             
307500           ELSE                                                           
307600              MOVE '1'         TO WS-KDKRJUST                             
307700           END-IF                                                         
307800        END-IF                                                            
307900     END-IF                                                               
308000     IF WS-KDKRJUST = SPACE                                               
308100        MOVE '3'               TO WS-KDKRJUST                             
308200     END-IF                                                               
308300                                                                          
308400     IF MID-FLANN = JA OR YES                                             
308500        PERFORM S01A-INITIERA-W6H713                                      
308600        IF KR-KDKRSTA > '3'                                               
308700           MOVE '1'            TO JUST-KDSEGKEY                           
308800           MOVE WS-KDKRJUST    TO JUST-KDKRJUST                           
308900           MOVE KR-KDKRSTA     TO JUST-KDKRSTA                            
309000           MOVE KR-KVARBTID    TO JUST-KVARBTID                           
309100           MOVE KR-KVART-RET   TO JUST-KVART-RET                          
309200           MOVE KR-KVART-SJUST TO JUST-KVART-SJUST                        
309300           MOVE KR-KVART-SKROT TO JUST-KVART-SKROT                        
309400           MOVE KR-SUMAT       TO JUST-SUMAT                              
309500           MOVE KR-SUOMK       TO JUST-SUOMK                              
309600           MOVE DATUM          TO JUST-TIREGDAT                           
309700                                                                          
309800           PERFORM IMS-ISRT-W6KVAE13                                      
309900        ELSE                                                              
310000           IF  KR-IDLOPNRM = +0                                           
310100           AND KR-KDKRSTA = '2' OR '3'                                    
310200              MOVE '1'            TO JUST-KDSEGKEY                        
310300              MOVE WS-KDKRJUST    TO JUST-KDKRJUST                        
310400              MOVE KR-KDKRSTA     TO JUST-KDKRSTA                         
310500              MOVE KR-KVART-RET   TO JUST-KVART-RET                       
310600              MOVE KR-KVART-SKROT TO JUST-KVART-SKROT                     
310700              MOVE DATUM          TO JUST-TIREGDAT                        
310800                                                                          
310900              PERFORM IMS-ISRT-W6KVAE13                                   
311000           END-IF                                                         
311100        END-IF                                                            
311200     ELSE                                                                 
311300        IF KR-KDKRSTA = '3'                                               
311400           PERFORM IMS-GNP-W6KVAE12                                       
311500           IF SEGMENT-FINNS                                               
311600              MOVE JA TO WS-JUSTERING                                     
311700              PERFORM IMS-GHU-W6KVAE01                                    
311800           END-IF                                                         
311900        END-IF                                                            
312000        IF KR-KDKRSTA   > '3'                                             
312100        OR WS-JUSTERING = JA                                              
312200           PERFORM S01A-INITIERA-W6H713                                   
312300           IF WS-KVARBTID NOT = ZERO                                      
312400              IF WS-KVARBTID NOT = KR-KVARBTID                            
312500                 COMPUTE JUST-KVARBTID = WS-KVARBTID - KR-KVARBTID        
312600              END-IF                                                      
312700           END-IF                                                         
312800                                                                          
312900           IF WS-SUMAT NOT = ZERO                                         
313000              IF WS-SUMAT NOT = KR-SUMAT                                  
313100                 COMPUTE JUST-SUMAT = WS-SUMAT - KR-SUMAT                 
313200              END-IF                                                      
313300           END-IF                                                         
313400                                                                          
313500           IF MID-SUOMK-IN NOT = ALL '+'                                  
313600              IF MID-SUOMK-IN NOT = KR-SUOMK                              
313700                 COMPUTE JUST-SUOMK = MID-SUOMK-IN - KR-SUOMK             
313800              END-IF                                                      
313900           END-IF                                                         
314000                                                                          
314100           IF JUST-KVARBTID NOT = +0                                      
314200           OR JUST-SUMAT    NOT = +0                                      
314300           OR JUST-SUOMK    NOT = +0                                      
314400              MOVE '1'         TO JUST-KDSEGKEY                           
314500              MOVE WS-KDKRJUST TO JUST-KDKRJUST                           
314600              MOVE KR-KDKRSTA  TO JUST-KDKRSTA                            
314700              MOVE DATUM       TO JUST-TIREGDAT                           
314800                                                                          
314900              PERFORM IMS-ISRT-W6KVAE13                                   
315000           END-IF                                                         
315100        END-IF                                                            
315200     END-IF                                                               
315300                                                                          
315400     PERFORM IMS-GHU-W6KVAE01                                             
315500     MOVE WS-KDKRJUST TO KR-KDKRJUST                                      
315600     .                                                                    
315700     EJECT                                                                
315800 S01A-INITIERA-W6H713 SECTION.                                            
315900     MOVE SPACE TO JUST-KDKRJUST                                          
316000                   JUST-KDKRSTA                                           
316100     MOVE ZERO  TO JUST-KVARBTID                                          
316200                   JUST-KVART-RET                                         
316300                   JUST-KVART-SJUST                                       
316400                   JUST-KVART-SKROT                                       
316500                   JUST-SUMAT                                             
316600                   JUST-SUOMK                                             
316700                   JUST-TIREGDAT                                          
316800     .                                                                    
316900     EJECT                                                                
317000 S03-CALL-W426KVIR SECTION.                                               
317100*    SUBPGM W426KVIR SKAPAR/SKICKAR TRANS TILL VIR                        
317200                                                                          
317300     MOVE KR-IDKR          TO KVIR-IDKR                                   
317400     MOVE WS-IDANSK        TO KVIR-IDANSK                                 
317500     MOVE WS-IDINK         TO KVIR-IDINK                                  
317600     MOVE WS-MAIL-ANS      TO KVIR-IDMAIL-ANSVARIG                        
317700     MOVE WS-MAIL-LEV      TO KVIR-IDMAIL-LEV                             
317800                                                                          
317900     CALL W426KVIR USING KVIR-W426KVIR                                    
318000                         ALT2-PCB                                         
318100                         KVAE-PCB                                         
318200                         BENA-PCB                                         
318300                         W6F1-PCB                                         
318400                         WDP3-PCB                                         
318500                         LEVA-PCB                                         
318600                                                                          
318700     .                                                                    
318800     EJECT                                                                
318900 S04-SKICKA-TRANS-W6T193X SECTION.                                        
319000                                                                          
319100     MOVE KR-IDLOPNRM          TO 6193-MID-IDLOPNRM                       
319200     MOVE ZERO                 TO 6193-MID-IDRADNR                        
319300                                                                          
319400     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
319500     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
319600     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
319700     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
319800     MOVE SPACE                TO MSG-KOM-KDTRANS                         
319900     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
320000     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
320100     MOVE 'W6020300'           TO MSG-KOM-IDSNDJOB                        
320200     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
320300     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
320400     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
320500                                                                          
320600     MOVE +29                  TO P-TO-P-MSG-KVLL                         
320700**CTEXTLÄNGD + KOM-MSG-AREA => 12 + 17                                    
320800     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
320900     MOVE '6203'               TO P-TO-P-MSG-IDTRANS                      
321000     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
321100     MOVE MID-6193-AREA        TO P-TO-P-MSG-INDATA                       
321200                                                                          
321300     CALL W006KOM USING MSG-PCB                                           
321400                        DISP-PCB                                          
321500                        KOM-KOMA-PCB                                      
321600                        MSG-KOM-WMSGKOM                                   
321700                        P-TO-P-MSG-IO-AREA-SNUF                           
321800     .                                                                    
321900     EJECT                                                                
322000 S05-CALL-W602KRUP SECTION.                                               
322100*    SUBPGM W602KRUP AVSLUTAR KR-RAPPORTERING                             
322200                                                                          
322300     MOVE 'W6020300' TO KRUP-IDPGM                                        
322400     MOVE KR-IDKR    TO KRUP-IDKR                                         
322500                                                                          
322600     CALL W602KRUP USING KRUP-W602KRUP                                    
322700                         W6H7-PCB                                         
322800                         WDG2-PCB                                         
322900                         LEVA-PCB                                         
323000                         WDK6-PCB                                         
323100                         LOPB-PCB                                         
323200                         FILC-PCB                                         
323300                         WDK7-PCB                                         
323400                         WDB6-PCB                                         
323500     .                                                                    
323600     EJECT                                                                
323700 MFS-EV-SPAERRA-FAELT SECTION.                                            
323800                                                                          
323900     MOVE NEJ TO UNDER-ELLER-OVERLEVERANS-SW                              
324000     IF KR-IDKRFEL = 'PA' OR 'PB'                                         
324100        MOVE JA TO UNDER-ELLER-OVERLEVERANS-SW                            
324200     END-IF                                                               
324300                                                                          
324400     IF KR-KDKRSTA = '0' OR                                               
324500        KR-FLANNULL = JA                                                  
324600        PERFORM MFS-SPAERRA-ALLA-FAELT                                    
324700     END-IF                                                               
324800                                                                          
324900     IF KR-FLANNULL = JA AND KR-KDKRSTA > '1' AND (NOT MFS-UPDATE         
325000        OR MFS-UPD-V)                                                     
325100        MOVE INF-UPPDAT-OTILLATET TO MED-IDMFSINF                         
325200        CALL WMEDKONV USING MED-WMEDAREA                                  
325300        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
325400     END-IF                                                               
325500                                                                          
325600     IF KR-KDKRSTA = '1'                                                  
325700        IF WS-FLKROMK = JA OR SPACE OR YES                                
325800         IF INDATA-OK                                                     
325900           PERFORM MFS-SPAERRA-OMKOSTNADER                                
326000         END-IF                                                           
326100        END-IF                                                            
326200     END-IF                                                               
326300                                                                          
326400     IF KR-KDKRSTA = '2'                                                  
326500         IF WS-FLKRGODK = JA OR YES                                       
326600            PERFORM MFS-SPAERRA-FAELT-STATUS-2                            
326700         ELSE                                                             
326800            IF WS-FLKROMK = JA OR YES                                     
326900               PERFORM MFS-SPAERRA-FLKROMK                                
327000            END-IF                                                        
327100         END-IF                                                           
327200     END-IF                                                               
327300                                                                          
327400     IF KR-KDKRSTA > '2'                                                  
327500         IF WS-FLKRGODK = JA OR YES                                       
327600            PERFORM MFS-SPAERRA-FAELT-STATUS-ST-2                         
327700         ELSE                                                             
327800            IF WS-FLKROMK = JA OR YES                                     
327900               PERFORM MFS-SPAERRA-FLKROMK                                
328000            END-IF                                                        
328100         END-IF                                                           
328200     END-IF                                                               
328300     .                                                                    
328400     EJECT                                                                
328500 S06-SEARCH-IDLAND SECTION.                                               
328600     SEARCH ALL DC-LAND                                                   
328700       AT END                                                             
328800         MOVE 'NO MATCH FOUND IN WWDCLAND' TO FELTEXT                     
328900         CALL FELLOG                                                      
329000       WHEN DCLAND-IDDC (DCLAND-IX) = KR-IDDC                             
329100         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
329200     END-SEARCH                                                           
329300     .                                                                    
329400     EJECT                                                                
329500                                                                          
329600 MFS-RENSA-FAELT-UT SECTION.                                              
329700                                                                          
329800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR                                  
329900                             MOD-ARTIKELTEXT                              
330000                             MOD-KDKRSTA                                  
330100                             MOD-TIREGDAT                                 
330200                             MOD-IDANSK                                   
330300                             MOD-IDLEVNR                                  
330400                             MOD-IDLEVG                                   
330500                             MOD-LEVTEXT                                  
330600                             MOD-FLKRLFEL                                 
330700                             MOD-KDKRATG                                  
330800                             MOD-FLINKANS                                 
330900                             MOD-ADATTENT                                 
331000                             MOD-FLKRTOVIR                                
331100                             MOD-FLEJKNTRL                                
331200                             MOD-KVARBTID-UT                              
331300                             MOD-TEKRSPEC-ATID                            
331400                             MOD-SUOMK-UT                                 
331500                             MOD-TEKRSPEC-OMK                             
331600                             MOD-SUMAT-UT                                 
331700                             MOD-TEKRSPEC-MAT                             
331800                             MOD-FLKROMK                                  
331900                             MOD-FLARBDEB                                 
332000                             MOD-FLKRLIM                                  
332100                             MOD-SUKRLIM                                  
332200                             MOD-KDPERSON                                 
332300                             MOD-BEKRANS                                  
332400                             MOD-IDKRATLF                                 
332500                             MOD-FLKRGODK                                 
332600                             MOD-FAX-TLX                                  
332700                             MOD-FLANN                                    
332800                                                                          
332900     .                                                                    
333000     EJECT                                                                
333100 MFS-RENSA-FAELT-IN SECTION.                                              
333200                                                                          
333300     MOVE MFS-RENSA-FAELT TO MOD-KVARBTID-IN                              
333400                             MOD-SUOMK-IN                                 
333500                             MOD-SUMAT-IN                                 
333600                             MOD-KDPERSON                                 
333700     .                                                                    
333800     EJECT                                                                
333900 MFS-FORM-ATTR      SECTION.                                              
334000                                                                          
334100     MOVE MFS-FORMATETS-ATTR TO MOD-FLKRLFEL-ATTR                         
334200                                MOD-KDKRATG-ATTR                          
334300                                MOD-ADATTENT-ATTR                         
334400                                MOD-FLINKANS-ATTR                         
334500                                MOD-FLKRTOVIR-ATTR                        
334600                                MOD-FLEJKNTRL-ATTR                        
334700                                MOD-KVARBTID-IN-ATTR                      
334800                                MOD-TEKRSPEC-ATID-ATTR                    
334900                                MOD-SUOMK-IN-ATTR                         
335000                                MOD-TEKRSPEC-OMK-ATTR                     
335100                                MOD-SUMAT-IN-ATTR                         
335200                                MOD-TEKRSPEC-MAT-ATTR                     
335300                                MOD-FLKROMK-ATTR                          
335400                                MOD-FLARBDEB-ATTR                         
335500                                MOD-FLKRLIM-ATTR                          
335600                                MOD-KDPERSON-ATTR                         
335700                                MOD-IDKRATLF-ATTR                         
335800                                MOD-FLKRGODK-ATTR                         
335900                                MOD-FAX-TLX-ATTR                          
336000                                MOD-IDMAIL-ATTR                           
336100                                MOD-FLANN-ATTR                            
336200     .                                                                    
336300     EJECT                                                                
336400 MFS-ROER-EJ-FAELT-IN-UT  SECTION.                                        
336500                                                                          
336600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKR-UT                                
336700                               MOD-IDARTNR                                
336800                               MOD-ARTIKELTEXT                            
336900                               MOD-KDKRSTA                                
337000                               MOD-TIREGDAT                               
337100                               MOD-IDANSK                                 
337200                               MOD-IDLEVNR                                
337300                               MOD-IDLEVG                                 
337400                               MOD-LEVTEXT                                
337500                               MOD-FLKRLFEL                               
337600                               MOD-KDKRATG                                
337700                               MOD-ADATTENT                               
337800                               MOD-FLINKANS                               
337900                               MOD-FLKRTOVIR                              
338000                               MOD-FLEJKNTRL                              
338100                               MOD-TEKRSPEC-ATID                          
338200                               MOD-TEKRSPEC-OMK                           
338300                               MOD-TEKRSPEC-MAT                           
338400                               MOD-FLKROMK                                
338500                               MOD-FLARBDEB                               
338600                               MOD-KDPERSON                               
338700                               MOD-FLKRLIM                                
338800                               MOD-SUKRLIM                                
338900                               MOD-BEKRANS                                
339000                               MOD-IDKRATLF                               
339100                               MOD-FLKRGODK                               
339200                               MOD-FAX-TLX                                
339300                               MOD-IDMAIL                                 
339400                               MOD-FLANN                                  
339500                               MOD-KVARBTID-UT                            
339600                               MOD-SUOMK-UT                               
339700                               MOD-SUMAT-UT                               
339800                               MOD-KVARBTID-IN                            
339900                               MOD-SUOMK-IN                               
340000                               MOD-SUMAT-IN                               
340100     .                                                                    
340200     EJECT                                                                
340300 MFS-SPAERRA-FLKROMK            SECTION.                                  
340400                                                                          
340500* HÄR SPÄRRAS FLAGGA OMKOSTNADER                                          
340600                                                                          
340700     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLKROMK-ATTR                      
340800                                                                          
340900     .                                                                    
341000     EJECT                                                                
341100 MFS-SPAERRA-ALLA-FAELT              SECTION.                             
341200                                                                          
341300* HÄR SPÄRRAS ALLA FÄLT OM KR-FLANNULL = JA                               
341400                                                                          
341500     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLKRLFEL-ATTR                     
341600                                    MOD-ADATTENT-ATTR                     
341700                                    MOD-FLINKANS-ATTR                     
341800                                    MOD-FLKRTOVIR-ATTR                    
341900                                    MOD-FLEJKNTRL-ATTR                    
342000                                    MOD-KVARBTID-IN-ATTR                  
342100                                    MOD-TEKRSPEC-ATID-ATTR                
342200                                    MOD-SUOMK-IN-ATTR                     
342300                                    MOD-TEKRSPEC-OMK-ATTR                 
342400                                    MOD-SUMAT-IN-ATTR                     
342500                                    MOD-TEKRSPEC-MAT-ATTR                 
342600                                    MOD-FLKROMK-ATTR                      
342700                                    MOD-FLARBDEB-ATTR                     
342800                                    MOD-FLKRLIM-ATTR                      
342900                                    MOD-KDKRATG-ATTR                      
343000                                    MOD-FLKRGODK-ATTR                     
343100     IF KR-FLANNULL = JA AND KR-KDKRSTA > '1'                             
343200        MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLANN-ATTR                     
343300                                       MOD-KDPERSON-ATTR                  
343400                                       MOD-IDKRATLF-ATTR                  
343500     END-IF                                                               
343600                                                                          
343700     .                                                                    
343800     SKIP2                                                                
343900 MFS-SPAERRA-FAELT-STATUS-2          SECTION.                             
344000                                                                          
344100*  KDKRSTA = '2'                                                          
344200*  SPÄRRAR ALLA FÄLT UTOM:                                                
344300*  KDKRATG OCH KDPERSON                                                   
344400*  GODKÄND OM ANNULLERAD = NEJ                                            
344500*  OMKOSTNADER OM OMK-KLAR = JA                                           
344600*  ANNULLERAD OM ANNULLERAD = SPACE                                       
344700*  FAX/TLX                                                                
344800                                                                          
344900     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLKRLFEL-ATTR                     
345000                                    MOD-IDKRATLF-ATTR                     
345100                                                                          
345200     IF WS-FLANNULL = JA                                                  
345300        MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLKRGODK-ATTR                  
345400                                       MOD-FLANN-ATTR                     
345500                                       MOD-FLKRLIM-ATTR                   
345600     END-IF                                                               
345700                                                                          
345800     IF WS-FLKROMK = NEJ                                                  
345900        CONTINUE                                                          
346000     ELSE                                                                 
346100        MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KVARBTID-IN-ATTR               
346200                                       MOD-TEKRSPEC-ATID-ATTR             
346300                                       MOD-SUOMK-IN-ATTR                  
346400                                       MOD-TEKRSPEC-OMK-ATTR              
346500                                       MOD-SUMAT-IN-ATTR                  
346600                                       MOD-TEKRSPEC-MAT-ATTR              
346700     END-IF                                                               
346800                                                                          
346900     .                                                                    
347000     SKIP2                                                                
347100 MFS-SPAERRA-FAELT-STATUS-ST-2       SECTION.                             
347200                                                                          
347300*  KDKRSTA > '2'                                                          
347400*  SPÄRRAR ALLA FÄLT UTOM:                                                
347500*  KDKRATG OCH KDPERSON(MÅSTE ALLTID ANGES FÖR ATT KUNNA SÄNDA            
347600*  TILL VIR).                                                             
347700*  GODKÄND OM ANNULLERAD = NEJ                                            
347800*  ANNULLERAD OM ANNULLERAD = NEJ                                         
347900*  FAX/TLX                                                                
348000                                                                          
348100     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLKRLFEL-ATTR                     
348200                                    MOD-ADATTENT-ATTR                     
348300                                    MOD-FLINKANS-ATTR                     
348400                                    MOD-FLKRTOVIR-ATTR                    
348500                                    MOD-FLEJKNTRL-ATTR                    
348600                                    MOD-IDKRATLF-ATTR                     
348700                                    MOD-KVARBTID-IN-ATTR                  
348800                                    MOD-TEKRSPEC-ATID-ATTR                
348900                                    MOD-SUOMK-IN-ATTR                     
349000                                    MOD-TEKRSPEC-OMK-ATTR                 
349100                                    MOD-SUMAT-IN-ATTR                     
349200                                    MOD-TEKRSPEC-MAT-ATTR                 
349300                                    MOD-FLKROMK-ATTR                      
349400                                                                          
349500     IF WS-FLANNULL = JA                                                  
349600        MOVE MFS-STAENG-FAELT-NOMOD TO MOD-FLKRGODK-ATTR                  
349700                                       MOD-FLANN-ATTR                     
349800                                       MOD-FLKRLIM-ATTR                   
349900     END-IF                                                               
350000                                                                          
350100                                                                          
350200     .                                                                    
350300     SKIP2                                                                
350400 MFS-SPAERRA-OMKOSTNADER SECTION.                                         
350500                                                                          
350600     MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KVARBTID-IN-ATTR                 
350700                                     MOD-SUOMK-IN-ATTR                    
350800                                     MOD-SUMAT-IN-ATTR                    
350900                                     MOD-TEKRSPEC-ATID-ATTR               
351000                                     MOD-TEKRSPEC-OMK-ATTR                
351100                                     MOD-TEKRSPEC-MAT-ATTR                
351200     .                                                                    
351300     EJECT                                                                
351400* --- IMS SEKTIONER ---                                                   
351500     SKIP3                                                                
351600 IMS-GET-MSG SECTION.                                                     
351700                                                                          
351800     MOVE '  QC' TO GODK-STATUSKODER                                      
351900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
352000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
352100     PERFORM IMS-STATUSKONTROLL                                           
352200     .                                                                    
352300     SKIP3                                                                
352400 IMS-INSERT-MSG SECTION.                                                  
352500                                                                          
352600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
352700       MOVE '0' TO MFS-KDHUVOMR                                           
352800     END-IF                                                               
352900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
353000     MOVE SPACE TO GODK-STATUSKODER                                       
353100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
353200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
353300     PERFORM IMS-STATUSKONTROLL                                           
353400     .                                                                    
353500     EJECT                                                                
353600 IMS-ISRT-ALT1-MSG SECTION.                                               
353700     MOVE SPACE TO GODK-STATUSKODER                                       
353800     CALL CBLTDLI USING ISRT ALT1-PCB ALT1-MSG-IO-AREA                    
353900     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
354000     PERFORM IMS-STATUSKONTROLL                                           
354100     .                                                                    
354200     EJECT                                                                
354300 IMS-GU-W6KVAE01 SECTION.                                                 
354400     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
354500          DELIMITED BY SIZE INTO SSA1                                     
354600     MOVE '  GE' TO GODK-STATUSKODER                                      
354700     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA1 SSA1                     
354800     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
354900     PERFORM IMS-STATUSKONTROLL                                           
355000     .                                                                    
355100 IMS-GHU-W6KVAE01 SECTION.                                                
355200     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
355300          DELIMITED BY SIZE INTO SSA1                                     
355400     MOVE '  GE' TO GODK-STATUSKODER                                      
355500     CALL CBLTDLI USING GHU KVAE-PCB DLI-IO-AREA1 SSA1                    
355600     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
355700     PERFORM IMS-STATUSKONTROLL                                           
355800     .                                                                    
355900 IMS-GU-W6KVAI01 SECTION.                                                 
356000     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
356100          DELIMITED BY SIZE INTO SSA1                                     
356200     MOVE '  GE' TO GODK-STATUSKODER                                      
356300     CALL CBLTDLI USING GN KVAI-PCB DLI-IO-AREA13 SSA1                    
356400     MOVE KVAI-STATUS-CODE TO STATUS-WS                                   
356500     PERFORM IMS-STATUSKONTROLL                                           
356600     .                                                                    
356700 IMS-GN-W6KVAI01 SECTION.                                                 
356800     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
356900          DELIMITED BY SIZE INTO SSA1                                     
357000     MOVE '  GE' TO GODK-STATUSKODER                                      
357100     CALL CBLTDLI USING GN KVAI-PCB DLI-IO-AREA13 SSA1                    
357200     MOVE KVAI-STATUS-CODE TO STATUS-WS                                   
357300     PERFORM IMS-STATUSKONTROLL                                           
357400     .                                                                    
357500     EJECT                                                                
357600 IMS-REPL-W6KVAE01 SECTION.                                               
357700                                                                          
357800     MOVE '  ' TO GODK-STATUSKODER                                        
357900     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-AREA1                        
358000     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
358100     PERFORM IMS-STATUSKONTROLL                                           
358200     .                                                                    
358300     SKIP2                                                                
358400 IMS-ISRT-W6KVAE13 SECTION.                                               
358500     SKIP2                                                                
358600     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
358700          DELIMITED BY SIZE INTO SSA1                                     
358800     MOVE 'W6KVAE13 '         TO SSA2                                     
358900     MOVE '  ' TO GODK-STATUSKODER                                        
359000     CALL CBLTDLI USING ISRT KVAE-PCB DLI-IO-AREA1 SSA1 SSA2              
359100     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
359200     PERFORM IMS-STATUSKONTROLL                                           
359300     .                                                                    
359400 IMS-GNP-W6KVAE12 SECTION.                                                
359500     MOVE 'W6KVAE12 '       TO SSA1                                       
359600     MOVE '  GE' TO GODK-STATUSKODER                                      
359700     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA1 SSA1                    
359800     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
359900     PERFORM IMS-STATUSKONTROLL                                           
360000     .                                                                    
360100     EJECT                                                                
360200 IMS-GHU-W6KVAE14 SECTION.                                                
360300                                                                          
360400     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
360500          DELIMITED BY SIZE INTO SSA1                                     
360600     STRING 'W6KVAE14(KDSEGKEY =' W-KDSEGKEY-X ')'                        
360700       DELIMITED BY SIZE INTO SSA2                                        
360800     MOVE '  GE'                 TO GODK-STATUSKODER                      
360900     CALL CBLTDLI             USING GHU KVAE-PCB                          
361000                                    DLI-IO-W6H714 SSA1 SSA2               
361100     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
361200     PERFORM IMS-STATUSKONTROLL                                           
361300     .                                                                    
361400     SKIP3                                                                
361500 IMS-REPL-W6KVAE14 SECTION.                                               
361600                                                                          
361700     MOVE '  ' TO GODK-STATUSKODER                                        
361800     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-W6H714                       
361900     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
362000     PERFORM IMS-STATUSKONTROLL                                           
362100     .                                                                    
362200     SKIP2                                                                
362300 IMS-ISRT-W6KVAE14 SECTION.                                               
362400     SKIP2                                                                
362500     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
362600          DELIMITED BY SIZE INTO SSA1                                     
362700     MOVE 'W6KVAE14 '         TO SSA2                                     
362800     MOVE '  ' TO GODK-STATUSKODER                                        
362900     CALL CBLTDLI USING ISRT KVAE-PCB DLI-IO-W6H714 SSA1 SSA2             
363000     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
363100     PERFORM IMS-STATUSKONTROLL                                           
363200     .                                                                    
363300 IMS-GU-WLBENA11 SECTION.                                                 
363400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
363500          DELIMITED BY SIZE INTO SSA1                                     
363600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
363700          DELIMITED BY SIZE INTO SSA2                                     
363800     MOVE '  GE' TO GODK-STATUSKODER                                      
363900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
364000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
364100     PERFORM IMS-STATUSKONTROLL                                           
364200     .                                                                    
364300     EJECT                                                                
364400 IMS-GU-WLLEVA01 SECTION.                                                 
364500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
364600          DELIMITED BY SIZE INTO SSA1                                     
364700     MOVE '  ' TO GODK-STATUSKODER                                        
364800     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA3 SSA1                     
364900     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
365000     PERFORM IMS-STATUSKONTROLL                                           
365100     .                                                                    
365200     EJECT                                                                
365300 IMS-GU-WLLEVA11 SECTION.                                                 
365400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
365500              DELIMITED BY SIZE INTO SSA1                                 
365600     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
365700              DELIMITED BY SIZE INTO SSA2                                 
365800     MOVE '  GE' TO GODK-STATUSKODER                                      
365900     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA3 SSA1 SSA2                
366000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
366100     PERFORM IMS-STATUSKONTROLL                                           
366200     .                                                                    
366300     SKIP2                                                                
366400 IMS-GNP-WLLEVA14 SECTION.                                                
366500     MOVE 'WLLEVA14 '       TO SSA1                                       
366600     MOVE '  ' TO GODK-STATUSKODER                                        
366700     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA3 SSA1                    
366800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
366900     PERFORM IMS-STATUSKONTROLL                                           
367000     .                                                                    
367100     EJECT                                                                
367200 IMS-GU-W6LEVA01 SECTION.                                                 
367300     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
367400          DELIMITED BY SIZE INTO SSA1                                     
367500     MOVE '  GE' TO GODK-STATUSKODER                                      
367600     CALL CBLTDLI USING GU W6F1-PCB DLI-IO-AREA8 SSA1                     
367700     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
367800     PERFORM IMS-STATUSKONTROLL                                           
367900     .                                                                    
368000     EJECT                                                                
368100 IMS-GU-WDP311 SECTION.                                                   
368200     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
368300          DELIMITED BY SIZE INTO SSA1                                     
368400     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
368500          DELIMITED BY SIZE INTO SSA2                                     
368600     MOVE '  GE' TO GODK-STATUSKODER                                      
368700     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
368800     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
368900     PERFORM IMS-STATUSKONTROLL                                           
369000     .                                                                    
369100     EJECT                                                                
369200 IMS-GU-WLARTC01 SECTION.                                                 
369300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
369400          DELIMITED BY SIZE INTO SSA1                                     
369500     MOVE '  GE' TO GODK-STATUSKODER                                      
369600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1                     
369700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
369800     PERFORM IMS-STATUSKONTROLL                                           
369900     .                                                                    
370000     EJECT                                                                
370100 IMS-GNP-WLARTC11 SECTION.                                                
370200     MOVE 'WLARTC11' TO SSA1                                              
370300     MOVE '  GE' TO GODK-STATUSKODER                                      
370400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA4 SSA1                    
370500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
370600     PERFORM IMS-STATUSKONTROLL                                           
370700     .                                                                    
370800     EJECT                                                                
370900 IMS-GHNP-WLARTC11 SECTION.                                               
371000     MOVE 'WLARTC11' TO SSA1                                              
371100     MOVE '  GE' TO GODK-STATUSKODER                                      
371200     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA4 SSA1                   
371300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
371400     PERFORM IMS-STATUSKONTROLL                                           
371500     .                                                                    
371600     EJECT                                                                
371700 IMS-REPL-WLARTC11 SECTION.                                               
371800                                                                          
371900     MOVE '  ' TO GODK-STATUSKODER                                        
372000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA4                        
372100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
372200     PERFORM IMS-STATUSKONTROLL                                           
372300     .                                                                    
372400     EJECT                                                                
372500 IMS-GU-WLARTC11 SECTION.                                                 
372600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
372700          DELIMITED BY SIZE INTO SSA1                                     
372800     MOVE 'WLARTC11 ' TO SSA2                                             
372900     MOVE '  GE' TO GODK-STATUSKODER                                      
373000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
373100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
373200     PERFORM IMS-STATUSKONTROLL                                           
373300     .                                                                    
373400     EJECT                                                                
373500 IMS-GU-WDK711 SECTION.                                                   
373600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
373700       DELIMITED BY SIZE INTO SSA1                                        
373800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
373900       DELIMITED BY SIZE INTO SSA2                                        
374000     MOVE '  GE'                 TO GODK-STATUSKODER                      
374100     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2         
374200     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
374300     PERFORM IMS-STATUSKONTROLL                                           
374400     .                                                                    
374500     SKIP2                                                                
374600*                                                                         
374700 IMS-GNP-WDK722 SECTION.                                                  
374800     MOVE 'WDK722' TO SSA1                                                
374900     MOVE '  GE'                 TO GODK-STATUSKODER                      
375000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK722 SSA1              
375100     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
375200     PERFORM IMS-STATUSKONTROLL                                           
375300     .                                                                    
375400     SKIP2                                                                
375500*                                                                         
375600 IMS-GU-WDD801 SECTION.                                                   
375700                                                                          
375800     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
375900          DELIMITED BY SIZE INTO SSA1                                     
376000     MOVE '  GE' TO GODK-STATUSKODER                                      
376100     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
376200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
376300     PERFORM IMS-STATUSKONTROLL                                           
376400     .                                                                    
376500     EJECT                                                                
376600 IMS-GHNP-WDD811 SECTION.                                                 
376700                                                                          
376800     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
376900          DELIMITED BY SIZE INTO SSA1                                     
377000     STRING 'WDD811   '                                                   
377100          DELIMITED BY SIZE INTO SSA2                                     
377200     MOVE '  GE' TO GODK-STATUSKODER                                      
377300     CALL CBLTDLI USING GHNP WDD8-PCB DLI-IO-WDD811 SSA1 SSA2             
377400     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
377500     PERFORM IMS-STATUSKONTROLL                                           
377600     .                                                                    
377700     SKIP3                                                                
377800 IMS-REPL-WDD811 SECTION.                                                 
377900                                                                          
378000     MOVE '  ' TO GODK-STATUSKODER                                        
378100     CALL CBLTDLI USING REPL WDD8-PCB DLI-IO-WDD811                       
378200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
378300     PERFORM IMS-STATUSKONTROLL                                           
378400     .                                                                    
378500     EJECT                                                                
378600 IMS-DLET-WDD811 SECTION.                                                 
378700                                                                          
378800     MOVE '  ' TO GODK-STATUSKODER                                        
378900     CALL CBLTDLI USING DLET WDD8-PCB DLI-IO-WDD811                       
379000     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
379100     PERFORM IMS-STATUSKONTROLL                                           
379200     .                                                                    
379300     EJECT                                                                
379400 IMS-GU-WDJ901 SECTION.                                                   
379500                                                                          
379600     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
379700          DELIMITED BY SIZE INTO SSA1                                     
379800     MOVE '  GE' TO GODK-STATUSKODER                                      
379900     CALL CBLTDLI USING GU WDJ9-PCB DLI-IO-WDJ901 SSA1                    
380000     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
380100     PERFORM IMS-STATUSKONTROLL                                           
380200     .                                                                    
380300     EJECT                                                                
380400 IMS-GHNP-WDJ911 SECTION.                                                 
380500                                                                          
380600     MOVE '  GE' TO GODK-STATUSKODER                                      
380700     CALL CBLTDLI USING GHNP WDJ9-PCB DLI-IO-WDJ911                       
380800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
380900     PERFORM IMS-STATUSKONTROLL                                           
381000                                                                          
381100     EJECT                                                                
381200     .                                                                    
381300                                                                          
381400 IMS-REPL-WDJ911 SECTION.                                                 
381500                                                                          
381600     MOVE '  ' TO GODK-STATUSKODER                                        
381700     CALL CBLTDLI USING REPL WDJ9-PCB DLI-IO-WDJ911                       
381800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
381900     PERFORM IMS-STATUSKONTROLL                                           
382000     .                                                                    
382100     EJECT                                                                
382200     SKIP3                                                                
382300                                                                          
382400 IMS-GU-UPFA-01  SECTION.                                                 
382500     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
382600          DELIMITED BY SIZE INTO SSA1                                     
382700     MOVE '  GE' TO GODK-STATUSKODER                                      
382800     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA9 SSA1                     
382900     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
383000     PERFORM IMS-STATUSKONTROLL                                           
383100     .                                                                    
383200     SKIP2                                                                
383300 IMS-GNP-UPFA-11  SECTION.                                                
383400     MOVE 'W6UPFA11' TO SSA1                                              
383500     MOVE '  GE' TO GODK-STATUSKODER                                      
383600     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA9 SSA1                    
383700     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
383800     PERFORM IMS-STATUSKONTROLL                                           
383900     .                                                                    
384000     SKIP2                                                                
384100 IMS-GNP-UPFA-12  SECTION.                                                
384200     MOVE 'W6UPFA12' TO SSA1                                              
384300     MOVE '  GE' TO GODK-STATUSKODER                                      
384400     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA9 SSA1                    
384500     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
384600     PERFORM IMS-STATUSKONTROLL                                           
384700     .                                                                    
384800     EJECT                                                                
384900 IMS-GHU-UPFA-01  SECTION.                                                
385000     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
385100          DELIMITED BY SIZE INTO SSA1                                     
385200     MOVE '  GE' TO GODK-STATUSKODER                                      
385300     CALL CBLTDLI USING GHU UPFA-PCB DLI-IO-AREA9 SSA1                    
385400     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
385500     PERFORM IMS-STATUSKONTROLL                                           
385600     .                                                                    
385700     SKIP2                                                                
385800 IMS-REPL-UPFA-01 SECTION.                                                
385900     SKIP2                                                                
386000     MOVE '  ' TO GODK-STATUSKODER                                        
386100     CALL CBLTDLI USING REPL UPFA-PCB DLI-IO-AREA9                        
386200     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
386300     PERFORM IMS-STATUSKONTROLL                                           
386400     .                                                                    
386500     SKIP2                                                                
386600 IMS-ISRT-UPFA-01 SECTION.                                                
386700     SKIP2                                                                
386800     MOVE 'W6UPFA01'       TO SSA1                                        
386900     MOVE '  ' TO GODK-STATUSKODER                                        
387000     CALL CBLTDLI USING ISRT UPFA-PCB DLI-IO-AREA9 SSA1                   
387100     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
387200     PERFORM IMS-STATUSKONTROLL                                           
387300     .                                                                    
387400     EJECT                                                                
387500 IMS-GU-W6KVAH-01 SECTION.                                                
387600     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
387700          DELIMITED BY SIZE INTO SSA1                                     
387800     MOVE '  GE' TO GODK-STATUSKODER                                      
387900     CALL CBLTDLI USING GHU KVAH-PCB DLI-IO-AREA10 SSA1                   
388000     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
388100     PERFORM IMS-STATUSKONTROLL                                           
388200     .                                                                    
388300     SKIP2                                                                
388400 IMS-GHNP-W6KVAH-12 SECTION.                                              
388500     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
388600          DELIMITED BY SIZE INTO SSA1                                     
388700     MOVE '  GE' TO GODK-STATUSKODER                                      
388800     CALL CBLTDLI USING GHNP KVAH-PCB DLI-IO-AREA10 SSA1                  
388900     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
389000     PERFORM IMS-STATUSKONTROLL                                           
389100     .                                                                    
389200     SKIP2                                                                
389300 IMS-REPL-W6KVAH-12 SECTION.                                              
389400     MOVE '  ' TO GODK-STATUSKODER                                        
389500     CALL CBLTDLI USING REPL KVAH-PCB DLI-IO-AREA10                       
389600     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
389700     PERFORM IMS-STATUSKONTROLL                                           
389800     .                                                                    
389900     EJECT                                                                
390000 IMS-GU-W6INLA11 SECTION.                                                 
390100     SKIP2                                                                
390200     STRING 'W6INLA11(W6D1BSEQ =' W-IDLOPNRM-X                            
390300                    '&IDARTNR  =' W-IDARTNR-X ')'                         
390400          DELIMITED BY SIZE INTO SSA1                                     
390500     MOVE '  GE' TO GODK-STATUSKODER                                      
390600     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA11 SSA1                    
390700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
390800     PERFORM IMS-STATUSKONTROLL                                           
390900     .                                                                    
391000     SKIP2                                                                
391100 IMS-GHU-W6INLA11 SECTION.                                                
391200     SKIP2                                                                
391300     STRING 'W6INLA11(W6D1BSEQ =' W-IDLOPNRM-X                            
391400                    '&IDARTNR  =' W-IDARTNR-X ')'                         
391500          DELIMITED BY SIZE INTO SSA1                                     
391600     MOVE '  GE' TO GODK-STATUSKODER                                      
391700     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA11 SSA1                   
391800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
391900     PERFORM IMS-STATUSKONTROLL                                           
392000     .                                                                    
392100     SKIP2                                                                
392200 IMS-GNP-W6INLA21 SECTION.                                                
392300     SKIP2                                                                
392400     MOVE 'W6INLA21' TO SSA1                                              
392500     MOVE '  GE' TO GODK-STATUSKODER                                      
392600     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA21 SSA1                   
392700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
392800     PERFORM IMS-STATUSKONTROLL                                           
392900     .                                                                    
393000     SKIP2                                                                
393100 IMS-REPL-W6INLA11 SECTION.                                               
393200     SKIP2                                                                
393300     MOVE '  ' TO GODK-STATUSKODER                                        
393400     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA11                       
393500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
393600     PERFORM IMS-STATUSKONTROLL                                           
393700     .                                                                    
393800     EJECT                                                                
393900 IMS-GU-W6PROA11 SECTION.                                                 
394000     SKIP2                                                                
394100     STRING 'W6PROA01(W6GXKEY  =' W-W6GX-6101-KEY-X ')'                   
394200          DELIMITED BY SIZE INTO SSA1                                     
394300     STRING 'W6PROA11(W6GXKEY  =' W-W6GX-6102-KEY-X ')'                   
394400          DELIMITED BY SIZE INTO SSA2                                     
394500     MOVE 'GE  ' TO GODK-STATUSKODER                                      
394600     CALL CBLTDLI USING GU PROA-PCB DLI-IO-AREA12 SSA1 SSA2               
394700     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
394800     PERFORM IMS-STATUSKONTROLL                                           
394900     .                                                                    
395000     EJECT                                                                
395100 IMS-GU-WDB601   SECTION.                                                 
395200     SKIP2                                                                
395300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
395400          DELIMITED BY SIZE INTO SSA1                                     
395500     MOVE '  GE' TO GODK-STATUSKODER                                      
395600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
395700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
395800     PERFORM IMS-STATUSKONTROLL                                           
395900     .                                                                    
396000     EJECT                                                                
396100 IMS-GU-WDF502 SECTION.                                                   
396200                                                                          
396300     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
396400            DELIMITED BY SIZE INTO SSA1                                   
396500     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
396600            DELIMITED BY SIZE INTO SSA2                                   
396700     MOVE '  GE' TO GODK-STATUSKODER                                      
396800     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA-WDF502 SSA1   SSA2        
396900     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
397000     PERFORM IMS-STATUSKONTROLL                                           
397100     .                                                                    
397200     EJECT                                                                
397300 IMS-ROLLBACK    SECTION.                                                 
397400     SKIP2                                                                
397500     CALL CBLTDLI USING ROLB    MSG-PCB                                   
397600     .                                                                    
397700     SKIP2                                                                
397800 IMS-STATUSKONTROLL SECTION.                                              
397900                                                                          
398000     SET STATUS-IX TO 1                                                   
398100     SEARCH GODK-STATUS                                                   
398200       AT END                                                             
398300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
398400         DELIMITED BY SIZE INTO FELTEXT                                   
398500         CALL FELLOG                                                      
398600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
398700     END-SEARCH                                                           
398800     .                                                                    
