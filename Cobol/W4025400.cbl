000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4025400.                                                
000400 AUTHOR.         KERSTIN JOHANSSON  GUIDE DATAKONSULT AB                  
000500 DATE-WRITTEN.   90/12/21.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET ÄR EN BAKGRUNDSTRANS FÖR ANNULLERING AV               
001200*        ORDERRADER.                                                      
001300*        INPUTTRANSAKTIONEN ÄR SKAPAD FRÅN EN SEKVENSFIL OCH              
001400*        UPPDATERAD PÅ KOMMUNIKATIONS DB. EN GENERELL MPP-                
001500*        DISPATCHERN HÄMTAR TRANSAKTIONEN PÅ KOMMUNIKATIONS DB            
001600*        OCH STARTAR DENNA BAKGRUNDSTRANS.                                
001700*        DETTA PROGRAM KONTROLLERAR OM ANNULLERING ÄR TILLÅTEN.           
001800*        DÅ SISTA ANNULLERING UPPDATERATS GÖRS ORDERAVSLUT.               
001900*        ETT FEL/KLAR MEDDELANDE SKICKAS TILL DISPATCHERN DÅ              
002000*        TRANSAKTIONEN BEHANDLAD.                                         
002100*                                                                         
002200*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002300*        PROGRAMMET UPPDATERAR WLORQL (WDQ2)                              
002400*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)                              
002500*        PROGRAMMET UPPDATERAR        (WDE4)                              
002600*        PROGRAMMET UPPDATERAR WLARTM (WDK9)                              
002700*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)                              
002800*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
002900*        PROGRAMMET UPPDATERAR        (WDM2)                              
003100*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
003200*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
003300*        PROGRAMMET UPPDATERAR WLORDP (WDA5)                              
003400*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
003500*                                                                         
003600*                                                                         
003700*    INDATA.                                                              
003800*        TRANSAKTION: W4T254U                                             
003900*        MID:         W4I25401                                            
004000*                     WMSGKOM                                             
004100*                                                                         
004200*    UTDATA.                                                              
004300*        MOD:         WMSGKOM                                             
004400* CHANGE LOG:                                                             
004500*                                                                         
004600* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
004700*                                                                         
004800*HÖSTEN 2004 GÖRAN KJELLSON                                               
004900*ETRACKER 887753                                                          
005000*                                                                         
005100*  SEPT 2005 LINDA NILSSON                                                
005200*  ETRACKER 1334295                                                       
005300*                                                                         
005400* E'TRACKER 6070767 2007-12  RÄTTA LÄSNING WDC7-BORRTAG                   
005500* E'TRACKER 7450328 2008-HÖST  VOHF                                       
005600*                                                                         
005700* E'TRACKER 10206836 2014-08 IF TACDIS SEND A CANCELLATION                
005800*                            THAT IS DECLINED IN PULS, SEND               
005900*                            ORDER CONFIRMATION CODE 20.                  
005910* E'TRACKER 10254592 2015    DECOMISSION VOHF                             
005920* E'TRACKER 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2          
006000* STORY 2375089 ADD IDSYSTEM VOUI, ECOM                                   
006010*                                                                         
006100                                                                          
006200     SKIP3                                                                
006300 ENVIRONMENT DIVISION.                                                    
006400     EJECT                                                                
006500 DATA DIVISION.                                                           
006600 WORKING-STORAGE SECTION.                                                 
006700*    -- CHECKED BY WY2000                                                 
006800     SKIP3                                                                
006900 77  IDPGM                       PIC X(08)   VALUE 'W4025400'.            
007000 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
007100                                                                          
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  YES                         PIC X       VALUE 'Y'.                   
007400 77  NEJ                         PIC X       VALUE 'N'.                   
007500                                                                          
007600 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  WS-TIAAAAMMDD               PIC 9(8).                                
007800 01  FILLER REDEFINES WS-TIAAAAMMDD.                                      
007900     03 WS-TIAA                  PIC 9(2).                                
008000     03 WS-TIAAMMDD              PIC 9(6).                                
008100                                                                          
008200 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
008300*01  -COPY WWDCKONS                                                       
008400                                                                          
008410*01  -COPY WWBYT03                                                        
008420                                                                          
008500 77  2109-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008600 77  MAX-2109-INDX               PIC S9(4)  VALUE +18   COMP SYNC.        
008700                                                                          
008800 77  AVSR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008900 77  MAX-AVSR-INDX               PIC S9(4)  VALUE +100  COMP SYNC.        
009000 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
009100 77  WS-TINUDAT                  PIC S9(7)  COMP-3.                       
009200 77  WS-TINUTID                  PIC S9(9)  COMP-3.                       
009300                                                                          
009400 77  KDRC-DISPLAY                PIC Z(5).                                
009500                                                                          
009600     EJECT                                                                
009700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009800 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
009900 01  FILLER REDEFINES WS-TIHHMMSS.                                        
010000     03 WS-TIHHMM                PIC 9(4).                                
010100     03 FILLER                   PIC 9(2).                                
010200                                                                          
010300 01  WS-9KOMPL-DATUM             PIC 9(8).                                
010400 01  FILLER REDEFINES WS-9KOMPL-DATUM.                                    
010500     03 WS-CENTURY               PIC 9(2).                                
010600     03 WS-AAMMDD                PIC 9(6).                                
010700 77  WS-TITIREGD-9KOMPL          PIC 9(9)    VALUE ZERO.                  
010800 77  WS-TITIORDD-9KOMPL          PIC 9(9)    VALUE ZERO.                  
010900 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
011000 77  WS-IDDISTR-NUM              PIC 9(5)    VALUE ZERO.                  
011100 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
011200 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
011300 77  WS-IDKUNDRF                 PIC X(7)    VALUE SPACE.                 
011400 77  WS-IDKUNDRF-NUM             PIC 9(7)    VALUE ZERO.                  
011500 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
011600 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
011700 77  WS-KVBEART-NUM              PIC 9(6)    VALUE ZERO.                  
011800 77  WS-KVANNANT                 PIC S9(7)   VALUE +0   COMP-3.           
011900 77  WS-KVANNANT-NUM             PIC 9(6)    VALUE ZERO.                  
012000 77  WS-KVBEART                  PIC S9(7)   VALUE +0   COMP-3.           
012100 77  WS-KVQPACK-1                PIC S9(5)   VALUE +0   COMP-3.           
012200 77  WS-KDFRAKT                  PIC S9(3)   VALUE ZERO COMP-3.           
012300 77  WS1-IDDC                    PIC X(2)    VALUE SPACE.                 
012400 77  SPAR-KVANNANT               PIC S9(7)   VALUE +0   COMP-3.           
012500 77  SPAR-IDPURAD                PIC S9(5)   VALUE +0   COMP-3.           
012600 77  WS-IDORDNR7                 PIC 9(7).                                
012700                                                                          
012800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012900     88  NYCKLAR-OK                          VALUE 'J'.                   
013000     88  NYCKLAR-FEL                         VALUE 'N'.                   
013100                                                                          
013200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
013300     88  ALLT-OK                             VALUE 'J'.                   
013400                                                                          
013500 77  AVSR-SW                     PIC X       VALUE 'J'.                   
013600     88  AVSR-OK                             VALUE 'J'.                   
013700                                                                          
013800 77  SKRIV-SW                    PIC X       VALUE 'J'.                   
013900     88  SKRIV-OK                            VALUE 'J'.                   
014000                                                                          
014100 77  RAD-HITTAD-SW               PIC X       VALUE 'J'.                   
014200     88  RAD-HITTAD                          VALUE 'J'.                   
014300                                                                          
014400                                                                          
014410 01  FLBORT-SW                   PIC X       VALUE 'N'.                   
014500 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
014600 01  FILLER  REDEFINES  TEST-IDDISTR.                                     
014700*    ----DIST79-DEALER-PRICE-----                                         
014800*    03  -COPY WWDIST79                                                   
014900                                                                          
015000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015100 01  GENERELLA-SUBPROGRAM.                                                
015200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015700     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
015800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015900                                                                          
016000 01  GEMENSAMMA-SUBPROGRAM.                                               
016100     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
016200     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
016300     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
016400*        PRISFRÅGA                                                        
016500     EJECT                                                                
016600*    --- PARAMETRAR TILL SUBPROGRAM ABEND                                 
016700 01  RKOD-ABEND                PIC S9(4) VALUE +33 COMP SYNC.             
016800     SKIP3                                                                
016900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017000*   -COPY WMSGINIT                                                        
017100     EJECT                                                                
017200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
017300*   -COPY WDATAREA                                                        
017400*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
017500 01  FILLER                      PIC X(16)   VALUE '**W009CIA**'.         
017600*01  -COPY W009CIA                                                        
017700 01  FILLER                      PIC X(16)   VALUE '*W402TACD**'.         
017800*01  -COPY W402TACD                                                       
017900                                                                          
018000 01  HDR-AREA.                                                            
018100*    03  -COPY WZ01REQU                                                   
018200*    03  -COPY WZ04HDR                                                    
018300                                                                          
018400 01  FILLER                      PIC X(16)   VALUE '*WZ01SEND**'.         
018500*01  -COPY WZ01SEND                                                       
018600                                                                          
018700     EJECT                                                                
018800 01  MESSAGE-CODES.                                                       
018900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019000     03  ERR-DEL-NOT-POSS        PIC X(3)    VALUE '066'.                 
019100     03  ERR-LINES-WRITTEN       PIC X(3)    VALUE '067'.                 
019200     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
019300     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
019400     EJECT                                                                
019500*    --- AREOR FÖR GEMENSAMMA-SUBPROGRAM                                  
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'LÄNKAREOR'.           
019800*01  -COPY W413AVSR                                                       
019900     EJECT                                                                
020000*01  -COPY W413AVSO                                                       
020100     EJECT                                                                
020200 01  FILLER                      PIC X(8)    VALUE 'W335PRQU'.            
020300*   -COPY W335PRQU                                                        
020400     EJECT                                                                
020500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020600*                                                                         
020700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020800                                                                          
020900 01  MID-AREA.                                                            
021000*03  MID -COPY W4I25401                                                   
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)  VALUE 'MSG-IO-AREA'.          
021300                                                                          
021400*01  -COPY WMSGAREA                                                       
021500     EJECT                                                                
021600     05  -COPY W2I10902 -PRE 2109-  -RED MSG-MID-OUT                      
021700     EJECT                                                                
021800 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
021900                                                                          
022000 01  KOM-IO-AREA.                                                         
022100*03  -COPY WMSGKOM                                                        
022200     EJECT                                                                
022300*    MSG-AREA FÖR HOPP TILL W40680                                        
022400 01  FILLER            PIC X(16)   VALUE '4680-MSG-IO-AREA'.              
022500 01  W-PROG-TO-PROG-SW.                                                   
022600     03  4680-KVLL                 PIC S9(4) COMP SYNC.                   
022700     03  4680-Z1                   PIC X.                                 
022800     03  4680-Z2                   PIC X.                                 
022900     03  4680-TRANSKOD             PIC X(8)  VALUE 'W4T680X '.            
023000     03  4680-IDTRANS              PIC X(4)  VALUE '4254'.                
023100     03  4680-KDMFSFOR             PIC X.                                 
023200*    03  -COPY W4I68001    -PRE 4680-                                     
023300     EJECT                                                                
023400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023500*                                                                         
023600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023700                                                                          
023800 01  NYCKLAR-TILL-DLI.                                                    
023900*    ---------TILL WDQ201 VIA WDQ2C1(SEK. INDX)                           
024000     03  W-WDQ2CSEQ-X.                                                    
024100         05  W-Q2CSEQ-IDDISTR     PIC S9(5)    VALUE ZERO COMP-3.         
024200         05  W-Q2CSEQ-IDKUNDNR    PIC S9(7)    VALUE ZERO COMP-3.         
024300         05  W-Q2CSEQ-IDKUNDRF    PIC  X(10)   VALUE SPACE.               
024400*    ---------TILL WDQ212                                                 
024470                                                                          
024500     03  W-IDDC-X.                                                        
024600         05  W-IDDC               PIC X(2)    VALUE SPACE.                
024700     EJECT                                                                
024800*    ---------TILL WDQ301                                                 
024900     03  W-WDQ301KY-MIN-X.                                                
025000         05  W-ODEL-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
025100         05  W-ODEL-IDDC-MIN      PIC X(2)     VALUE LOW-VALUE.           
025200         05  FILLER               PIC X(6)     VALUE LOW-VALUE.           
025300     03  W-WDQ301KY-MAX-X.                                                
025400         05  W-ODEL-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
025500         05  W-ODEL-IDDC-MAX      PIC X(2)     VALUE HIGH-VALUE.          
025600         05  FILLER               PIC X(6)     VALUE HIGH-VALUE.          
025700     EJECT                                                                
025800*    ---------TILL WDQ401                                                 
025900     03  W-WDQ4A1KY-MIN-X.                                                
026000         05  W-Q4ASEQ-IDORDER-MIN PIC S9(7)    VALUE ZERO COMP-3.         
026100         05  W-Q4ASEQ-IDARTNR-MIN PIC S9(9)    VALUE ZERO COMP-3.         
026200         05  FILLER               PIC X(11)    VALUE LOW-VALUE.           
026300     03  W-WDQ4A1KY-MAX-X.                                                
026400         05  W-Q4ASEQ-IDORDER-MAX PIC S9(7)    VALUE ZERO COMP-3.         
026500         05  W-Q4ASEQ-IDARTNR-MAX PIC S9(9)    VALUE ZERO COMP-3.         
026600         05  FILLER               PIC X(11)    VALUE HIGH-VALUE.          
026700     03  W-WDQ401KY-X.                                                    
026800         05  W-Q401-IDORDER     PIC S9(7)      VALUE ZERO COMP-3.         
026900         05  W-Q401-IDDC        PIC X(2)       VALUE ZERO.                
027000         05  W-Q401-ADLAGOMR    PIC S9(3)      VALUE ZERO COMP-3.         
027100         05  W-Q401-ADGANG      PIC S9(3)      VALUE ZERO COMP-3.         
027200         05  W-Q401-ADPLATS     PIC S9(5)      VALUE ZERO COMP-3.         
027300         05  W-Q401-IDARTNR     PIC S9(9)      VALUE ZERO COMP-3.         
027400         05  W-Q401-IDLOPNR     PIC S9(3)      VALUE ZERO COMP-3.         
027500*                                                                         
027600     03  W-WDE401-X.                                                      
027700         05  W-401-IDDISTR      PIC S9(5)      VALUE ZERO COMP-3.         
027800         05  W-401-IDKUNDNR     PIC S9(7)      VALUE ZERO COMP-3.         
027900         05  W-401-IDKUNDRF.                                              
028000           07  W-401-IDORDNR    PIC 9(5)       VALUE ZERO.                
028100           07  FILLER           PIC X(5)       VALUE SPACE.               
028200         05  W-401-IDPRODNR     PIC S9(7)      VALUE ZERO COMP-3.         
028300         05  W-401-IDPLKLST     PIC S9(3)      VALUE ZERO COMP-3.         
028400*                                                                         
028500     03  W-WDE411-X.                                                      
028600         05  W-411-IDPURAD      PIC S9(5)      VALUE ZERO COMP-3.         
028700*                                                                         
028800     EJECT                                                                
028900*    ---------TILL WDK901,WDK601                                          
029000     03  W-IDARTNR-X.                                                     
029100         05  W-IDARTNR               PIC S9(9)  VALUE ZERO COMP-3.        
029200*    ---------TILL WDA501                                                 
029300     03  W-WDA501KY-X.                                                    
029400         05  W-IDDISTR-A5            PIC S9(5)  VALUE ZERO COMP-3.        
029500         05  W-IDKUNDNR-A5           PIC S9(7)  VALUE ZERO COMP-3.        
029600         05  W-IDKUNDRF-A5           PIC X(10)  VALUE SPACE.              
029700         05  W-IDARTNR-A5            PIC S9(9)  VALUE ZERO COMP-3.        
029800         05  W-IDLOPNR-A5            PIC S9(3)  VALUE ZERO COMP-3.        
029900                                                                          
030020     SKIP2                                                                
030100   03    W-WDA601KY-MIN-X.                                                
030200     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
030300     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
030400     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
030500     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
030600     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
030700     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
030800     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
030900     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
031000     SKIP2                                                                
031100   03    W-WDA601KY-MAX-X.                                                
031200     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
031300     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
031400     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
031500     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
031600     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
031700     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
031800     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
031900     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
031910*    -------- TILL WDM2                                                   
031920     03  W-WDM201-X.                                                      
031930         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
031940         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
031950                                                                          
031960     03  W-WDM211-X.                                                      
031970         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
031980                                                                          
031990     03  W-WDM221-X.                                                      
031991         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
031992         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
031993         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
031994         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
031995*                                                                         
031996*    -------- TILL WDB6                                                   
034300     03  W-IDDC-B6-X.                                                     
034400         05 W-IDDC-B6                  PIC X(2).                          
034500                                                                          
034600     EJECT                                                                
034700*    --- STATUS-KOD FRÅN IMS                                              
034800 01  STATUS-WS                   PIC XX.                                  
034900     88  SEGMENT-FINNS                       VALUE '  '.                  
035000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035200     88  BASEN-SLUT                          VALUE 'GB'.                  
035300     SKIP2                                                                
035400 01  GODK-STATUSKODER.                                                    
035500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035600     SKIP3                                                                
035700 01  SSA1                        PIC X(200).                              
035800 01  SSA2                        PIC X(128).                              
035810 01  SSA3                        PIC X(128).                              
035900     EJECT                                                                
036000******************************************************                    
036100******************************************************                    
036200*    ARBETSAREA FÖR RYC-TRANS TILL WDG6              *                    
036300******************************************************                    
036400 01  FILLER                      PIC X(16)   VALUE 'RYC-POST   '.         
036500                                                                          
036600 01  W-RYCPOST.                                                           
036700*    03  -COPY WDGZRYC    -PRE W-                                         
036800     EJECT                                                                
036900 01  W-RYCSPOST.                                                          
037000*    03  -COPY WDGZRYCS   -PRE W-                                         
037100     EJECT                                                                
037200*    --- IMS FUNKTIONSKODER                                               
037300*01  -COPY W0003                                                          
037400     EJECT                                                                
037500*    ---  DLI INPUT-OUTPUT AREA                                           
037600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
037700 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ201  '.         
037800                                                                          
037900 01  DLI-IO-AREA-OHUV.                                                    
038000     03  WLORQI01.                                                        
038100*        05  -COPY WDQ201                                                 
038200     EJECT                                                                
038300 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ212  '.         
038400                                                                          
038500 01  DLI-IO-AREA-ARB.                                                     
038600     03  WLORQI12.                                                        
038700*        05  -COPY WDQ212                                                 
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ301  '.         
039000                                                                          
039100 01  DLI-IO-AREA-ODEL.                                                    
039200     03  WLORQA01.                                                        
039300*        05  -COPY WDQ301                                                 
039400     EJECT                                                                
039500 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ401  '.         
039600                                                                          
039700 01  DLI-IO-AREA-ORAD.                                                    
039800     03  WLORQF01.                                                        
039900*        05  -COPY WDQ401                                                 
040000     EJECT                                                                
040100 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ4A1  '.         
040200                                                                          
040300 01  DLI-IO-AREA-SEQA.                                                    
040400     03  WLORQG01.                                                        
040500*        05  -COPY WDQ4A1                                                 
040600     EJECT                                                                
040700                                                                          
040800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E401'.             
040900 01  DLI-IO-AREA-E401.                                                    
041000*    03             -COPY WDE401                                          
041100     EJECT                                                                
041200 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
041300 01  DLI-IO-AREA-WDA6.                                                    
041400*  03    -COPY WDA601                                                     
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E411'.             
041800 01  DLI-IO-AREA-E411.                                                    
041900*    03             -COPY WDE411 -PRE E4-                                 
042000     EJECT                                                                
042100                                                                          
042200 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ901  '.         
042300                                                                          
042400 01  DLI-IO-AREA-ARTM.                                                    
042500     03  WLARTM01.                                                        
042600*        05  -COPY WDK901                                                 
042700     EJECT                                                                
042800                                                                          
042900 01  FILLER                      PIC X(16)   VALUE 'IO-WDK711  '.         
043000 01  DLI-IO-AREA-ARTS.                                                    
043100     03  WLARTS11.                                                        
043200*        05  -COPY WDK711                                                 
043300     EJECT                                                                
043510 01  FILLER                      PIC X(16)   VALUE 'IO-WDK611  '.         
043520                                                                          
043600 01  DLI-IO-AREA-ARTC.                                                    
043700     03  WLARTC11.                                                        
043800*        05  -COPY WDK611                                                 
043900     EJECT                                                                
044000 01  FILLER                      PIC X(16)   VALUE 'IO-WDA501  '.         
044100 01  DLI-IO-AREA-ORDP.                                                    
044200     03  WLORDP01.                                                        
044300*        05  -COPY WDA501                                                 
044400     EJECT                                                                
044500 01  FILLER                      PIC X(16)   VALUE 'IO-WDGZ01  '.         
044600                                                                          
044700 01  DLI-IO-AREA-ZZAC.                                                    
044800     03  WLZZAC01.                                                        
044900*        05  -COPY WDGZ01                                                 
045000     EJECT                                                                
045100 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ101  '.         
045200                                                                          
045300 01  DLI-IO-AREA-OBKR.                                                    
045400     03  WLORQM01.                                                        
045500*        05  -COPY WDQ101                                                 
045600     EJECT                                                                
045700                                                                          
046810 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
046820 01  DLI-IO-WDM211.                                                       
046830*    03 -COPY WDM211                                                      
046840                                                                          
046850     EJECT                                                                
046860 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
046870 01  DLI-IO-WDM221.                                                       
046880*    03 -COPY WDM221                                                      
046890                                                                          
046891     EJECT                                                                
046900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
047000 01   DLI-IO-AREA-B601.                                                   
047100*     03  -COPY WDB601                                                    
047200                                                                          
047300     EJECT                                                                
047460 LINKAGE SECTION.                                                         
047500                                                                          
047600*01  -COPY W0009   -PRE MSG-                                              
047700     EJECT                                                                
047800*01  -COPY W0009   -PRE DISTRDOC-                                         
047900     EJECT                                                                
048000*01  -COPY W0009   -PRE 4680-                                             
048100     EJECT                                                                
048200*01  -COPY W0009   -PRE DISP-                                             
048300     EJECT                                                                
048400*01  -COPY W0009   -PRE 2109-                                             
048500     EJECT                                                                
048600*01  -COPY W0008   -PRE USEA-                                             
048700     05  FILLER                  PIC X.                                   
048800     EJECT                                                                
048900*01  -COPY W0008   -PRE ORQA-                                             
049000     05  FILLER                  PIC X.                                   
049100     EJECT                                                                
049200*01  -COPY W0008   -PRE ORQF-                                             
049300     05  FILLER                  PIC X.                                   
049400     EJECT                                                                
049500*01  -COPY W0008   -PRE ORQG-                                             
049600     05  FILLER                  PIC X.                                   
049700     EJECT                                                                
049800*01  -COPY W0008   -PRE ORQI-                                             
049900     05  FILLER                  PIC X.                                   
050000     EJECT                                                                
050100*01  -COPY W0008   -PRE ORQM-                                             
050200     05  FILLER                  PIC X.                                   
050300     EJECT                                                                
050400*01  -COPY W0008   -PRE ARTC-                                             
050500     05  FILLER                  PIC X.                                   
050600     EJECT                                                                
050700*01  -COPY W0008   -PRE ARTS-                                             
050800     05  FILLER                  PIC X.                                   
050900     EJECT                                                                
051000*01  -COPY W0008   -PRE ARTM-                                             
051100     05  FILLER                  PIC X.                                   
051200     EJECT                                                                
051300*01  -COPY W0008   -PRE ORDP-                                             
051400     05  FILLER                  PIC X.                                   
051500     EJECT                                                                
051600*01  -COPY W0008   -PRE ZZAC-                                             
051700     05  FILLER                  PIC X.                                   
051800     EJECT                                                                
052200*01  -COPY W0008   -PRE WDM2-                                             
052300     05  FILLER                  PIC X.                                   
052400     EJECT                                                                
052500*01    -COPY W0008 -PRE WDE4-                                             
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800*01    -COPY W0008 -PRE WDA6B-                                            
052900     05  FILLER                  PIC X.                                   
053000     EJECT                                                                
053100*01    -COPY W0008 -PRE WDB6-                                             
053200     05  FILLER                  PIC X.                                   
053300     EJECT                                                                
053400*----> SUBPROGRAM W413AVSR.                                               
053500 01  AVSR-LIST-PCB               PIC X.                                   
053600 01  AVSR-ORQI-PCB               PIC X.                                   
053700 01  AVSR-GMTB-PCB               PIC X.                                   
053800 01  AVSR-GMTC-PCB               PIC X.                                   
053900 01  AVSR-WDB2-PCB               PIC X.                                   
054000 01  AVSR-WDB6-PCB               PIC X.                                   
054100                                                                          
054200 01  TRAN-XXKB-PCB               PIC X.                                   
054300     EJECT                                                                
054400*----> SUBPROGRAM W413AVSO.                                               
054500 01  AVSO-WDE6-PCB               PIC X.                                   
054600 01  AVSO-ORQA-PCB               PIC X.                                   
054700 01  AVSO-WDQ2-PCB               PIC X.                                   
054800 01  AVSO-GMTB-PCB               PIC X.                                   
054900 01  AVSO-XXKA-PCB               PIC X.                                   
055000 01  AVSO-4437-PCB               PIC X.                                   
055100 01  AVSO-XXKE-PCB               PIC X.                                   
055200 01  AVSO-XXKF-PCB               PIC X.                                   
055300 01  AVSO-XXKG-PCB               PIC X.                                   
055400 01  AVSO-XXKH-PCB               PIC X.                                   
055500 01  AVSO-XXKI-PCB               PIC X.                                   
055600 01  AVSO-XXKP-PCB               PIC X.                                   
055610 01  AVSO-WDB2-PCB               PIC X.                                   
055620 01  AVSO-WDB6-PCB               PIC X.                                   
055700 01  ORDN-ORQL-PCB               PIC X.                                   
055800 01  ORDN-PROC-PCB               PIC X.                                   
055900 01  ORDN-ORQI-PCB               PIC X.                                   
056000 01  ORDN-WDQ3-PCB               PIC X.                                   
056100     EJECT                                                                
056200 01  PRQU-WDG2-PCB               PIC X.                                   
056300 01  PRQU-WDC7-PCB               PIC X.                                   
056400 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
056500                                                                          
056600 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB                           
056700                           4680-PCB DISP-PCB 2109-PCB                     
056800                           AVSR-LIST-PCB                                  
056900                           USEA-PCB ORQA-PCB ORQF-PCB ORQG-PCB            
057000                           ORQI-PCB ORQM-PCB ARTC-PCB                     
057100                           ARTS-PCB ARTM-PCB ORDP-PCB ZZAC-PCB            
057200                           WDM2-PCB WDE4-PCB WDA6B-PCB                    
057300                           WDB6-PCB                                       
057400                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
057500                           AVSR-GMTC-PCB                                  
057600                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
057700                           TRAN-XXKB-PCB                                  
057800                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
057900                           AVSO-WDQ2-PCB                                  
058000                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
058100                           AVSO-4437-PCB AVSO-XXKE-PCB                    
058200                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
058300                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
058400                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
058410                           AVSO-WDB6-PCB                                  
058500                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
058600                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
058700                           PRQU-WDG2-PCB                                  
058800                           PRQU-WDC7-PCB                                  
058900                           PRQU-SJKO-WDK6-PCB.                            
059000     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB                           
059100                           4680-PCB DISP-PCB 2109-PCB                     
059200                           AVSR-LIST-PCB                                  
059300                           USEA-PCB ORQA-PCB ORQF-PCB ORQG-PCB            
059400                           ORQI-PCB ORQM-PCB ARTC-PCB                     
059500                           ARTS-PCB ARTM-PCB ORDP-PCB ZZAC-PCB            
059600                           WDM2-PCB WDE4-PCB WDA6B-PCB                    
059700                           WDB6-PCB                                       
059800                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
059900                           AVSR-GMTC-PCB                                  
060000                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
060100                           TRAN-XXKB-PCB                                  
060200                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
060300                           AVSO-WDQ2-PCB                                  
060400                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
060500                           AVSO-4437-PCB AVSO-XXKE-PCB                    
060600                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
060700                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
060800                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
060810                           AVSO-WDB6-PCB                                  
060900                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
061000                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
061100                           PRQU-WDG2-PCB                                  
061200                           PRQU-WDC7-PCB                                  
061300                           PRQU-SJKO-WDK6-PCB.                            
061400     EJECT                                                                
062101     PERFORM IMS-GET-MSG                                                  
062102     IF SEGMENT-FINNS                                                     
062103        PERFORM IMS-GN-MSG                                                
062104     END-IF                                                               
062106     IF SEGMENT-FINNS                                                     
062108        PERFORM A-INIT                                                    
062200        PERFORM C-KOLLA-NYCKLAR                                           
062300        IF NYCKLAR-OK                                                     
062400           PERFORM D-LAES-IN-ORDER                                        
062500           IF ALLT-OK                                                     
062610              PERFORM E-UPPDATERA                                         
062700           END-IF                                                         
062800        END-IF                                                            
062900        PERFORM Z-FINIT                                                   
063000     END-IF                                                               
063100     IF 2109-MID2-KVANTART > ZERO                                         
063200       PERFORM S16-STARTA-2109                                            
063300     END-IF                                                               
063400                                                                          
063500     MOVE ZERO TO RETURN-CODE                                             
063600     GOBACK                                                               
063700     .                                                                    
063800     EJECT                                                                
063900 A-INIT SECTION.                                                          
064000                                                                          
064100     MOVE MSG-INDATA-MINUS-1-TRANSKOD                                     
064200                               TO MID-AREA                                
064300     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
064400                                                                          
064500     MOVE LOW-VALUE TO MSG-AREA                                           
064600     MOVE SPACE                 TO 2109-MID2-W2I10902                     
064700     MOVE +1                    TO 2109-INDX                              
064900                                                                          
065000     ACCEPT WS-TINUDAT FROM DATE                                          
065100     ACCEPT WS-TINUTID FROM TIME                                          
065200                                                                          
065300     PERFORM AA-NOLLA-AVSR-TABELL                                         
065400     .                                                                    
065500     EJECT                                                                
065600 AA-NOLLA-AVSR-TABELL SECTION.                                            
065700                                                                          
065800     MOVE +1                   TO AVSR-INDX                               
065900     PERFORM UNTIL AVSR-INDX > MAX-AVSR-INDX                              
066000        MOVE +0                TO AVSR-ADLAGOMR(AVSR-INDX)                
066100        MOVE SPACE             TO AVSR-IDLEVNR(AVSR-INDX)                 
066200        MOVE ZERO              TO AVSR-IDDC(AVSR-INDX)                    
066300        MOVE +0                TO AVSR-KDSPEEMB(AVSR-INDX)                
066400        MOVE +0                TO AVSR-KVANNANT(AVSR-INDX)                
066500        MOVE +0                TO AVSR-KVBEART-Q(AVSR-INDX)               
066600        MOVE +0                TO AVSR-PRARTNTO(AVSR-INDX)                
066610        MOVE +0                TO AVSR-PRAVCOST(AVSR-INDX)                
066700        INITIALIZE             AVSR-DEAL-PR-LINE(AVSR-INDX)               
066800        MOVE +0                TO AVSR-VKART(AVSR-INDX)                   
066900        MOVE +0                TO AVSR-VLARTNTO(AVSR-INDX)                
067000        ADD +1                 TO AVSR-INDX                               
067100     END-PERFORM                                                          
067200                                                                          
067300     MOVE +1                   TO AVSR-INDX                               
067400     .                                                                    
067500     EJECT                                                                
067600 C-KOLLA-NYCKLAR SECTION.                                                 
067700                                                                          
067800     MOVE 'STA C-KOLLA   '                TO   WS-PGM-POSITION            
067900     MOVE JA TO NYCKLAR-SW                                                
068000*                                                                         
068100     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
068200        MOVE MID-IDDISTR    TO WS-IDDISTR                                 
068300        MOVE WS-IDDISTR     TO WS-IDDISTR-NUM                             
068400        MOVE WS-IDDISTR-NUM TO W-Q2CSEQ-IDDISTR                           
068500     ELSE                                                                 
072201        MOVE NEJ TO NYCKLAR-SW                                            
072202     END-IF                                                               
072203                                                                          
072204     IF MID-IDKUNDNR = SPACE                                              
072205        MOVE ZERO            TO WS-IDKUNDNR                               
072207     ELSE                                                                 
072208        MOVE MID-IDKUNDNR    TO WS-IDKUNDNR                               
072212     END-IF                                                               
072213                                                                          
072215     IF WS-IDKUNDNR NUMERIC                                               
072216        MOVE WS-IDKUNDNR     TO WS-IDKUNDNR-NUM                           
072217        MOVE WS-IDKUNDNR-NUM TO W-Q2CSEQ-IDKUNDNR                         
072218     ELSE                                                                 
072220        MOVE NEJ TO NYCKLAR-SW                                            
072221     END-IF                                                               
072222                                                                          
072223     IF MID-IDORDNR NUMERIC AND MID-IDORDNR > ZERO                        
072224        MOVE MID-IDORDNR     TO WS-IDKUNDRF                               
072225        MOVE WS-IDKUNDRF     TO WS-IDKUNDRF-NUM                           
072226        MOVE WS-IDKUNDRF-NUM TO W-Q2CSEQ-IDKUNDRF                         
072227     ELSE                                                                 
072228        MOVE NEJ TO NYCKLAR-SW                                            
072229     END-IF                                                               
072230                                                                          
072231     IF MID-IDARTNR NUMERIC AND                                           
072232        MID-IDARTNR > ZERO                                                
072248        CONTINUE                                                          
072249     ELSE                                                                 
072250       MOVE NEJ TO NYCKLAR-SW                                             
072253     END-IF                                                               
072254                                                                          
072255     IF (MID-KVBEART NUMERIC AND                                          
072256        MID-KVBEART > ZERO)     OR                                        
072257        ((MID-IDSYSTEM = 'LDC ' OR 'TACD') AND MID-KVBEART = ZERO)        
072260* TACDIS SKICKAR IN ANNULLATIONER MED KVBEART = 0  TL                     
072261        CONTINUE                                                          
072262     ELSE                                                                 
072300       MOVE NEJ TO NYCKLAR-SW                                             
072400     END-IF                                                               
072500                                                                          
072600*    -- DIREKTLEVERANS DC EJ TILLÅTET                                     
072700*    MOVE MID-IDDC TO WS-IDDC                                             
072800*    IF GOOD-DDC                                                          
072900*      MOVE NEJ TO NYCKLAR-SW                                             
073000*    END-IF                                                               
073100                                                                          
073200     IF NYCKLAR-FEL                                                       
073300        MOVE ERR-WRONG-KEY TO MSG-KOM-IDMFSMED                            
073310        IF (MID-IDSYSTEM (1:3) = 'LYN' OR 'POL' OR 'ECO' OR 'VOU'         
073320                                       OR 'TAD' OR 'ACC'                  
073330                                       OR 'APA' OR 'APB'                  
073340                                       OR 'APC' OR 'APD'                  
073350                                       OR 'APE' OR 'APF'                  
073360                                       OR 'APG' OR 'APH'                  
073370                                       OR 'API' OR 'APJ')                 
073400           MOVE '4'        TO MSG-KOM-KDSVAR                              
073410        ELSE                                                              
073412           MOVE 'R'        TO MSG-KOM-KDSVAR                              
073420        END-IF                                                            
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800 D-LAES-IN-ORDER SECTION.                                                 
073900                                                                          
074000     MOVE 'STA D-LAES    '                TO   WS-PGM-POSITION            
074100     MOVE JA TO ALLT-SW                                                   
074200                                                                          
074300     PERFORM IMS-GHU-ORQI-ORQI01                                          
074430                                                                          
074440                                                                          
074500     IF SEGMENT-FINNS                                                     
074600        MOVE OHUV-IDORDER   TO W-ODEL-IDORDER-MIN                         
074700                               W-ODEL-IDORDER-MAX                         
074800                                                                          
074900        IF OHUV-KDTPOTYP > 0                                              
075000           MOVE ERR-DEL-NOT-POSS TO MSG-KOM-IDMFSMED                      
075010           IF (MID-IDSYSTEM (1:3) = 'LYN' OR 'POL' OR 'ECO'               
075011                                          OR 'VOU' OR 'TAD'               
075012                                          OR 'ACC'                        
075013                                          OR 'APA' OR 'APB'               
075014                                          OR 'APC' OR 'APD'               
075015                                          OR 'APE' OR 'APF'               
075016                                          OR 'APG' OR 'APH'               
075017                                          OR 'API' OR 'APJ')              
075022              MOVE '4'        TO MSG-KOM-KDSVAR                           
075030           ELSE                                                           
075100              MOVE 'R'        TO MSG-KOM-KDSVAR                           
075110           END-IF                                                         
075200           MOVE NEJ TO ALLT-SW                                            
075300        END-IF                                                            
075400                                                                          
075500        IF ALLT-OK                                                        
075600          PERFORM DA-KOLLA-ORDERDELAR                                     
075700        END-IF                                                            
075800                                                                          
075900        PERFORM DB-FIXA-LOKAL-TID                                         
076000                                                                          
076100     ELSE                                                                 
076120                                                                          
076140      MOVE ERR-ORDER-MISSING TO MSG-KOM-IDMFSMED                          
076211      IF (MID-IDSYSTEM (1:3) = 'LYN' OR 'POL' OR 'ECO' OR 'VOU'           
076212                                     OR 'TAD' OR 'ACC'                    
076213                                     OR 'APA' OR 'APB'                    
076214                                     OR 'APC' OR 'APD'                    
076215                                     OR 'APE' OR 'APF'                    
076216                                     OR 'APG' OR 'APH'                    
076217                                     OR 'API' OR 'APJ')                   
076227           MOVE '4'            TO MSG-KOM-KDSVAR                          
076230      ELSE                                                                
076300           MOVE 'R'            TO MSG-KOM-KDSVAR                          
076310      END-IF                                                              
076400        MOVE NEJ TO ALLT-SW                                               
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800 DA-KOLLA-ORDERDELAR SECTION.                                             
076900                                                                          
077000     MOVE 'STA DA-ORDERDELAR'             TO   WS-PGM-POSITION            
077100     IF MID-IDDC = SPACE                                                  
077200       MOVE LOW-VALUE    TO W-ODEL-IDDC-MIN                               
077300       MOVE HIGH-VALUE   TO W-ODEL-IDDC-MAX                               
077400     ELSE                                                                 
077500       MOVE MID-IDDC     TO W-ODEL-IDDC-MIN                               
077600                            W-ODEL-IDDC-MAX                               
077700                            WS-IDDC                                       
077800     END-IF                                                               
077900                                                                          
077910                                                                          
078000     MOVE NEJ TO SKRIV-SW                                                 
078100                                                                          
078200     PERFORM IMS-GU-ORQA-ORQA01                                           
078300                                                                          
078400     IF SEGMENT-FINNS                                                     
078500                                                                          
078600        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR SKRIV-OK            
078700           IF ODEL-KDODELSTA = 'U' OR 'P'                                 
078800             IF ODEL-KDODELSTA = 'U'                                      
078900               IF ODEL-IDDC NOT = W-IDDC-B6                               
079000                  MOVE ODEL-IDDC TO W-IDDC-B6                             
079100                  PERFORM IMS-GU-WDB601                                   
079200               END-IF                                                     
079300               IF DCS-DDC                                                 
079400                 PERFORM DAA-KOLLA-BORTTAG-DDGS                           
079500               ELSE                                                       
079600                 CONTINUE                                                 
079821               END-IF                                                     
079830             ELSE                                                         
079840               CONTINUE                                                   
080010             END-IF                                                       
080100           ELSE                                                           
080200              MOVE JA TO SKRIV-SW                                         
080300           END-IF                                                         
080400           PERFORM IMS-GN-ORQA-ORQA01                                     
080500        END-PERFORM                                                       
080600                                                                          
080700        IF NOT SKRIV-OK                                                   
080800           MOVE ERR-LINES-WRITTEN TO MSG-KOM-IDMFSMED                     
080811           IF (MID-IDSYSTEM (1:3) = 'LYN' OR 'POL' OR 'ECO'               
080812                                          OR 'VOU' OR 'TAD'               
080813                                          OR 'ACC'                        
080814                                          OR 'APA' OR 'APB'               
080815                                          OR 'APC' OR 'APD'               
080816                                          OR 'APE' OR 'APF'               
080817                                          OR 'APG' OR 'APH'               
080818                                          OR 'API' OR 'APJ')              
080820              MOVE '4'        TO MSG-KOM-KDSVAR                           
080830           ELSE                                                           
080900              MOVE 'R'        TO MSG-KOM-KDSVAR                           
080910           END-IF                                                         
081000           MOVE NEJ TO ALLT-SW                                            
081010           IF MID-IDSYSTEM = 'TACD'                                       
081100              PERFORM DAB-SEND-REJ-TO-TACDIS                              
081110           END-IF                                                         
081200        END-IF                                                            
081300                                                                          
081400     END-IF                                                               
081500     .                                                                    
081600     EJECT                                                                
081700                                                                          
081800 DAA-KOLLA-BORTTAG-DDGS SECTION.                                          
081900     MOVE 'STA DAA-BORT-DDGS'             TO   WS-PGM-POSITION            
082000     MOVE NEJ                         TO RAD-HITTAD-SW                    
082100     MOVE ZERO                        TO SPAR-KVANNANT                    
082200                                         SPAR-IDPURAD                     
082300     MOVE ODEL-IDDISTR                TO W-401-IDDISTR                    
082400     MOVE ODEL-IDKUNDNR               TO W-401-IDKUNDNR                   
082500     MOVE ODEL-IDKUNDRF(3:5)          TO W-401-IDORDNR                    
082600     MOVE ODEL-IDPRODNR               TO W-401-IDPRODNR                   
082700     MOVE ODEL-IDPLKLST               TO W-401-IDPLKLST                   
082810     MOVE MID-IDARTNR                 TO WS-IDARTNR                       
082900     MOVE WS-IDARTNR                  TO WS-IDARTNR-NUM                   
083000     MOVE MID-KVBEART                 TO WS-KVBEART-NUM                   
083100     MOVE WS-KVBEART-NUM              TO WS-KVBEART                       
083200                                                                          
083300     PERFORM IMS-GU-WDE401                                                
083400     IF SEGMENT-FINNS                                                     
083500       PERFORM IMS-GNP-WDE411                                             
083600       PERFORM UNTIL SEGMENT-SAKNAS OR RAD-HITTAD                         
083700                                                                          
083800         IF E4-ORAD-IDARTNR = WS-IDARTNR-NUM                              
083900           IF E4-ORAD-KDANNULL = '0'                                      
084000             IF E4-ORAD-KVANNANT = ZERO                                   
084100               IF E4-ORAD-KVBEART = WS-KVBEART                            
084200               OR (MID-IDSYSTEM = 'LDC ' OR 'TACD')                       
084300*** TACDIS-FIX ANNULLATIONER INNEHÅLLER INGET ANTAL FRÅN                  
084400*** TACDIS. HÄR KOMPLETTERAR MAN MED ANTAL IFRÅN WDE4.                    
084500                IF (MID-IDSYSTEM = 'LDC ' OR 'TACD')                      
084600                AND MID-KVBEART = ZERO                                    
084700                    COMPUTE WS-KVBEART = E4-ORAD-KVAVBART                 
084800                                       - E4-ORAD-KVLEVART                 
084900                 END-IF                                                   
085000                 COMPUTE WS-KVANNANT = E4-ORAD-KVAVBART -                 
085100                                       E4-ORAD-KVLEVART                   
085200                 IF WS-KVANNANT = WS-KVBEART                              
085300                   MOVE JA TO RAD-HITTAD-SW                               
085400                   MOVE WS-KVANNANT       TO SPAR-KVANNANT                
085500                   MOVE E4-ORAD-IDPURAD   TO SPAR-IDPURAD                 
085600                 ELSE                                                     
085700                   IF WS-KVANNANT > SPAR-KVANNANT                         
085800                     MOVE WS-KVANNANT     TO SPAR-KVANNANT                
085900                     MOVE E4-ORAD-IDPURAD TO SPAR-IDPURAD                 
086000                   END-IF                                                 
086100                 END-IF                                                   
086200               END-IF                                                     
086300             END-IF                                                       
086400           END-IF                                                         
086500         END-IF                                                           
086600         PERFORM IMS-GNP-WDE411                                           
086700       END-PERFORM                                                        
086800     END-IF                                                               
086900                                                                          
087001     IF SPAR-IDPURAD > ZERO                                               
087100       MOVE SPAR-IDPURAD              TO W-411-IDPURAD                    
087200       PERFORM IMS-GHU-WDE411                                             
087300       MOVE '1'                       TO E4-ORAD-KDANNULL                 
087400       MOVE '4254'                    TO E4-ORAD-IDSYSTEM                 
087500       PERFORM IMS-REPL-WDE411                                            
087600       PERFORM DAAA-EDI-TRANS                                             
087700       MOVE JA                        TO SKRIV-SW                         
087800       MOVE NEJ                       TO ALLT-SW                          
087900     END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200                                                                          
088300 DAAA-EDI-TRANS SECTION.                                                  
088400                                                                          
088500     MOVE 'STA DAAA-EDI-TRANS'            TO   WS-PGM-POSITION            
088600     COMPUTE 4680-KVLL = LENGTH OF 4680-MID-W4I68001 + 17                 
088700                                                                          
088800     MOVE FUNCTION CURRENT-DATE (1:8) TO 4680-MID-DABEKDAT                
088900     ACCEPT 4680-MID-TIBEKR FROM TIME                                     
089000     MOVE ODEL-IDDC                   TO 4680-MID-IDDC                    
089100     MOVE ODEL-IDDISTR                TO 4680-MID-IDDISTR                 
089200     MOVE ODEL-IDKUNDNR               TO 4680-MID-IDKUNDNR                
089300     MOVE E4-ORAD-IDLEVNR             TO 4680-MID-IDLEVNR                 
089400     MOVE ODEL-IDORDNR7               TO 4680-MID-IDORDNR7                
089500     MOVE E4-ORAD-IDPRODNR            TO 4680-MID-IDPRODNR                
089600     MOVE ODEL-IDPLKLST               TO 4680-MID-IDPLKLST                
089700     MOVE E4-ORAD-IDARTNR             TO 4680-MID-IDARTNR(1)              
089800     MOVE E4-ORAD-IDPURAD             TO 4680-MID-IDRADNR(1)              
089900     MOVE WS-KVANNANT                 TO 4680-MID-KVBEART(1)              
090000                                                                          
090100     MOVE 'W4T680X '                  TO 4680-TRANSKOD                    
090200     MOVE '4254'                      TO 4680-IDTRANS                     
090300     MOVE SPACE                       TO 4680-KDMFSFOR                    
090400                                                                          
090500     PERFORM IMS-PURG-ALTMSG                                              
090600     .                                                                    
090700     EJECT                                                                
090800 DAB-SEND-REJ-TO-TACDIS SECTION.                                          
090900                                                                          
091000     ACCEPT DAGENS-DATUM       FROM DATE                                  
091100                                                                          
091200     MOVE 'PU1'                  TO 402-IDPTYP                            
091300     MOVE 01                     TO 402-IDVTYP-TACDIS                     
091400     MOVE 20                     TO WS-TIAA                               
091500     MOVE DAGENS-DATUM           TO WS-TIAAMMDD                           
091600     MOVE WS-TIAAAAMMDD          TO 402-DAREGDAT                          
091700     MOVE OHUV-IDDISTR           TO 402-IDDISTR                           
091800     MOVE OHUV-IDKUNDNR          TO 402-IDKUNDNR                          
091900     MOVE OHUV-IDORDNR7          TO 402-IDORDNR7                          
092000                                                                          
092100     MOVE 'VO '                  TO CIA-IDARTPRE-IN                       
092200     MOVE MID-IDARTNR            TO CIA-IDARTBET-IN                       
092300     CALL W009CIA             USING CIA-W009CIA                           
092400     MOVE CIA-IDARTBET-UT        TO 402-IDARTBET                          
092500                                                                          
092600     MOVE 20                     TO 402-KDORDBEK                          
092700     MOVE 0                      TO 402-KVBEART                           
092800     MOVE +1                     TO 402-IDSEKVNR                          
092900                                                                          
093000     MOVE SPACE                  TO 402-IDARTBET-TILLK                    
093100     MOVE ZERO                   TO 402-KVLEVART                          
093200     MOVE SPACE                  TO 402-IDDC                              
093210     MOVE ZERO                   TO 402-DADLEVDAT                         
093300                                                                          
093400     PERFORM S21-SEND-OPEN                                                
093500     MOVE OHUV-IDKUNDNR          TO WS-IDKUNDNR-NUM                       
093600     MOVE OHUV-IDORDNR7          TO WS-IDORDNR7                           
093700                                                                          
093800     PERFORM S22-PUT-HEADER                                               
093900     PERFORM S25-PUT-LINE                                                 
094000     PERFORM S29-SEND-CLOSE                                               
094100                                                                          
094200     .                                                                    
094300     EJECT                                                                
094400 DB-FIXA-LOKAL-TID SECTION.                                               
094500                                                                          
094600     MOVE ALL '+'              TO MSGI-WMSGINIT                           
094700     MOVE '013'                TO MSGI-KDCALL                             
094800     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
094900     IF OHUV-IDDC-TVS = SPACE                                             
095000       MOVE OHUV-IDDC-PRIM     TO MSGI-IDUSER(6:2)                        
095100     ELSE                                                                 
095200       MOVE OHUV-IDDC-TVS      TO MSGI-IDUSER(6:2)                        
095300     END-IF                                                               
095400     MOVE '4254'               TO MSGI-IDTRANS                            
095500     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
095600                                                                          
095700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
095800     .                                                                    
095900     EJECT                                                                
096000                                                                          
096100 E-UPPDATERA SECTION.                                                     
096200                                                                          
096300     MOVE 'STA E-UPPDATERA  '             TO   WS-PGM-POSITION            
096400     MOVE 'N' TO OHUV-FLKLAR                                              
096500     PERFORM IMS-REPL-ORQI                                                
096600                                                                          
096700     MOVE +1 TO AVSR-INDX                                                 
096800     MOVE NEJ TO AVSR-SW                                                  
096900     MOVE OHUV-IDORDER      TO W-Q4ASEQ-IDORDER-MIN                       
097000                               W-Q4ASEQ-IDORDER-MAX                       
097100                                                                          
097200     MOVE MID-IDARTNR          TO WS-IDARTNR                              
097300     MOVE WS-IDARTNR           TO WS-IDARTNR-NUM                          
097400     MOVE WS-IDARTNR-NUM       TO W-Q4ASEQ-IDARTNR-MIN                    
097500                               W-Q4ASEQ-IDARTNR-MAX                       
097600                               W-IDARTNR                                  
097700                                                                          
097800     MOVE MID-KVBEART          TO WS-KVBEART-NUM                          
097900     MOVE WS-KVBEART-NUM       TO WS-KVBEART                              
098000                                                                          
098100     MOVE MID-IDDC             TO WS1-IDDC                                
098210                                                                          
098300     IF MID-IDDC = SPACE                                                  
098400       PERFORM IMS-GU-ORQG-ORQG01                                         
098500     ELSE                                                                 
098600       PERFORM IMS-GU-ORQG-ORQG01-DC                                      
098700     END-IF                                                               
098800                                                                          
098900     IF SEGMENT-FINNS                                                     
099000        MOVE SEQA-IDORDER      TO W-Q401-IDORDER                          
099100        MOVE SEQA-IDDC         TO W-Q401-IDDC                             
099200        MOVE SEQA-ADLAGOMR     TO W-Q401-ADLAGOMR                         
099300        MOVE SEQA-ADGANG       TO W-Q401-ADGANG                           
099400        MOVE SEQA-ADPLATS      TO W-Q401-ADPLATS                          
099500        MOVE SEQA-IDARTNR      TO W-Q401-IDARTNR                          
099600        MOVE SEQA-IDLOPNR      TO W-Q401-IDLOPNR                          
099700        PERFORM IMS-GHU-ORQF-ORQF01                                       
099800***TACDIS-FIX ANNULLATIONER INNEHÅLLER INGET ANTAL FRÅN                   
099900*** TACDIS. HÄR KOMPLETTERAR MAN MED ANTAL IFRÅN WDQ40.                   
100000*** TL 030905                                                             
100100        IF SEGMENT-FINNS AND (MID-IDSYSTEM = 'LDC ' OR 'TACD') AND        
100200           MID-KVBEART = ZERO                                             
100300          MOVE ORAD-KVBEART-Q    TO WS-KVBEART                            
100400        END-IF                                                            
100500     END-IF                                                               
100600                                                                          
100700     IF SEGMENT-FINNS                                                     
100800        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
100900                      BASEN-SLUT        OR                                
101000                      ORAD-KVBEART-Q = WS-KVBEART                         
101100                                                                          
101200          IF MID-IDDC = SPACE                                             
101300            PERFORM IMS-GN-ORQG-ORQG01                                    
101400          ELSE                                                            
101500            PERFORM IMS-GN-ORQG-ORQG01-DC                                 
101600          END-IF                                                          
101700                                                                          
101800          IF SEGMENT-FINNS                                                
101900             MOVE SEQA-IDORDER      TO W-Q401-IDORDER                     
102000             MOVE SEQA-IDDC         TO W-Q401-IDDC                        
102100             MOVE SEQA-ADLAGOMR TO W-Q401-ADLAGOMR                        
102200             MOVE SEQA-ADGANG       TO W-Q401-ADGANG                      
102300             MOVE SEQA-ADPLATS      TO W-Q401-ADPLATS                     
102400             MOVE SEQA-IDARTNR      TO W-Q401-IDARTNR                     
102500             MOVE SEQA-IDLOPNR      TO W-Q401-IDLOPNR                     
102600             PERFORM IMS-GHU-ORQF-ORQF01                                  
102700          END-IF                                                          
102800                                                                          
102900        END-PERFORM                                                       
103000     END-IF                                                               
103100                                                                          
103200     IF SEGMENT-FINNS                                                     
103300        IF ORAD-KVBEART-Q = WS-KVBEART                                    
103400                                                                          
103500           IF ORAD-KDORDKL = 0                                            
103600              PERFORM S04C-DELBACKA-NYVORKO                               
103700           END-IF                                                         
103800                                                                          
103900           PERFORM S01-TABORT-RADEN                                       
104000        END-IF                                                            
104100     END-IF                                                               
104200                                                                          
104300     IF AVSR-OK                                                           
104400        IF AVSR-INDX > 1                                                  
104500           CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                
104600           AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                      
104700           AVSR-WDB2-PCB AVSR-WDB6-PCB                                    
104800           TRAN-XXKB-PCB                                                  
104900        END-IF                                                            
105000     END-IF                                                               
105100                                                                          
105200     IF MID-FLSLUT = 'J' OR 'Y'                                           
105300        PERFORM S09-SKAPA-AVSO                                            
105400        CALL W413AVSO USING AVSO-W413AVSO                                 
105500        AVSO-WDE6-PCB AVSO-ORQA-PCB                                       
105600        AVSO-WDQ2-PCB AVSO-GMTB-PCB                                       
105700        AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB                         
105800        AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB                         
105900        AVSO-XXKI-PCB AVSO-XXKP-PCB AVSO-WDB2-PCB AVSO-WDB6-PCB           
106000        USEA-PCB TRAN-XXKB-PCB                                            
106100        ORDN-ORQL-PCB ORDN-PROC-PCB                                       
106200        ORDN-ORQI-PCB ORDN-WDQ3-PCB                                       
106300                                                                          
106900                                                                          
106901        IF (MID-IDSYSTEM (1:3) = 'LYN' OR 'POL' OR 'ECO' OR 'VOU'         
106902                                       OR 'TAD' OR 'ACC'                  
106903                                       OR 'APA' OR 'APB'                  
106904                                       OR 'APC' OR 'APD'                  
106905                                       OR 'APE' OR 'APF'                  
106906                                       OR 'APG' OR 'APH'                  
106907                                       OR 'API' OR 'APJ')                 
106908**CHECK IF ALL THE LINES ARE CANCELLED THEN UPDATE FLBORT                 
106910           MOVE LOW-VALUE    TO W-ODEL-IDDC-MIN                           
106911           MOVE HIGH-VALUE   TO W-ODEL-IDDC-MAX                           
106912           PERFORM IMS-GU-ORQA-ORQA01                                     
106913                                                                          
106914           IF SEGMENT-FINNS                                               
106915              CONTINUE                                                    
106916           ELSE                                                           
106917              MOVE JA  TO FLBORT-SW                                       
107655           END-IF                                                         
107656        END-IF                                                            
107657        PERFORM IMS-GHU-ORQI-ORQI01                                       
107658        IF SEGMENT-FINNS                                                  
107660           MOVE JA    TO OHUV-FLKLAR                                      
107661           IF FLBORT-SW = 'J'                                             
107662              MOVE  JA  TO OHUV-FLBORT                                    
107664           END-IF                                                         
107665           PERFORM IMS-REPL-ORQI                                          
107666        END-IF                                                            
107669     END-IF                                                               
107670     .                                                                    
107671     EJECT                                                                
107672                                                                          
107673 Z-FINIT SECTION.                                                         
107674                                                                          
107675*    FEL/KLAR MEDDELANDE TILL DISPATCHERN                                 
107676                                                                          
107700     IF MSG-KOM-IDMFSMED = SPACE                                          
107800        MOVE OK-BEHANDLAD     TO MSG-KOM-IDMFSMED                         
107900     END-IF                                                               
108000     PERFORM IMS-INSERT-DISP-MSG                                          
108100     .                                                                    
108200     EJECT                                                                
108300 S01-TABORT-RADEN SECTION.                                                
108400                                                                          
108500     MOVE 'STA S01-TABORT   '             TO   WS-PGM-POSITION            
108600     MOVE ORAD-IDARTNR    TO W-IDARTNR                                    
108700     MOVE ORAD-IDDC       TO W-IDDC                                       
108800     PERFORM IMS-GNP-ORQI-ORQI12                                          
108900     IF SEGMENT-FINNS                                                     
109000        MOVE ARB-KDFRAKT  TO WS-KDFRAKT                                   
109100     ELSE                                                                 
109200        MOVE ZERO         TO WS-KDFRAKT                                   
109300     END-IF                                                               
109400                                                                          
109500     IF ORAD-IDKUNDRF-RO NOT = '0000000   ' AND                           
109600       ORAD-TIRODAT > ZERO                                                
109700       PERFORM S02-TA-BORT-WDA5                                           
109800     ELSE                                                                 
109900       MOVE ORAD-IDDC      TO WS-IDDC                                     
110000       IF WS-IDDC NOT = W-IDDC-B6                                         
110100          MOVE WS-IDDC TO W-IDDC-B6                                       
110200          PERFORM IMS-GU-WDB601                                           
110300       END-IF                                                             
110400       IF DCS-CDC                                                         
110500         PERFORM IMS-GHU-ARTM-ARTM01                                      
110600         IF SEGMENT-FINNS                                                 
110700            IF ORAD-KDORDKL = ZERO                                        
110800               SUBTRACT ORAD-KVBEART-Q FROM ART-KVOKS-VOR                 
110900               SUBTRACT ORAD-KVPREAVB FROM ART-KVPREAVB-VOR               
111000            END-IF                                                        
111100            IF ORAD-KDORDKL = +1                                          
111200               SUBTRACT ORAD-KVBEART-Q FROM ART-KVOKS-DAG                 
111300               SUBTRACT ORAD-KVPRERO FROM ART-KVPRERO-DAG                 
111400               SUBTRACT ORAD-KVPREAVB FROM ART-KVPREAVB-DAG               
111500            END-IF                                                        
111600            IF ORAD-KDORDKL = +2 OR +3 OR +4                              
111700               SUBTRACT ORAD-KVBEART-Q FROM ART-KVOKS-BULK                
111800               SUBTRACT ORAD-KVPRERO FROM ART-KVPRERO-BULK                
111900               SUBTRACT ORAD-KVPREAVB FROM ART-KVPREAVB-BULK              
112000            END-IF                                                        
112800                                                                          
112900            PERFORM IMS-REPL-ARTM                                         
113000         END-IF                                                           
113100       ELSE                                                               
113200         PERFORM IMS-GHU-ARTS-ARTS11                                      
113300         IF ORAD-KDORDKL = +0 OR +1                                       
113400           SUBTRACT ORAD-KVBEART-Q FROM SLAG-KVOKS-DAG                    
113500         ELSE                                                             
113600           IF ORAD-KDORDKL = +2 OR +3 OR +4                               
113610             IF ORAD-KVOKS-PREL = ZERO                                    
113700               SUBTRACT ORAD-KVBEART-Q FROM SLAG-KVOKS-BULK               
113710             END-IF                                                       
113800           END-IF                                                         
113900         END-IF                                                           
114000         PERFORM IMS-REPL-ARTS                                            
114100       END-IF                                                             
114200     END-IF                                                               
114300                                                                          
114400     PERFORM IMS-GU-ARTC11                                                
114500     MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                               
114600                                                                          
114700     PERFORM S07-SKAPA-AVSR                                               
114800*    IF MID-IDSYSTEM(1:3) = 'LDC'                                         
114900     IF (MID-IDSYSTEM = 'LDCC' OR 'LYNC' OR 'ECOC' OR 'VOUC'              
114920                               OR 'TADC' OR 'ACCC' OR 'APAC'              
114930                               OR 'APBC' OR 'APCC' OR 'APDC'              
114940                               OR 'APEC' OR 'APFC' OR 'APGC'              
114950                               OR 'APHC' OR 'APIC' OR 'APJC')             
115000*SKAPA INGA RYC FÖR LDC-FIXEN SE PGM W41242  TL 030603                    
115100       CONTINUE                                                           
115200     ELSE                                                                 
115300       PERFORM S03-SKAPA-ORDERBEKR                                        
115400     END-IF                                                               
115500     PERFORM S05-SKAPA-TRANSAR                                            
115600                                                                          
115700     IF ORAD-IDKAMPRF > 0                                                 
115800        PERFORM S13-BACKA-KAMPANJ                                         
115900     END-IF                                                               
116000                                                                          
116100     PERFORM IMS-DLET-ORQF                                                
116200     .                                                                    
116300     EJECT                                                                
116400                                                                          
116500 S02-TA-BORT-WDA5 SECTION.                                                
116600                                                                          
116700     MOVE 'STA S02-TABORT   '             TO   WS-PGM-POSITION            
116800     IF WS-IDDC NOT = W-IDDC-B6                                           
116900        MOVE WS-IDDC TO W-IDDC-B6                                         
117000        PERFORM IMS-GU-WDB601                                             
117100     END-IF                                                               
117200     IF DCS-CDC                                                           
117300       PERFORM IMS-GHU-ARTC-ARTC11                                        
117400       IF SEGMENT-FINNS                                                   
117500         SUBTRACT ORAD-KVBEART-Q FROM CLAG-KVRESS                         
117600         PERFORM IMS-REPL-ARTC                                            
117700       END-IF                                                             
117800     ELSE                                                                 
117900       PERFORM IMS-GHU-ARTS-ARTS11                                        
118000       IF SEGMENT-FINNS                                                   
118100         SUBTRACT ORAD-KVBEART-Q FROM SLAG-KVRESS                         
118200         PERFORM IMS-REPL-ARTS                                            
118300       END-IF                                                             
118400     END-IF                                                               
118500     MOVE ORAD-IDDISTR           TO W-IDDISTR-A5                          
118600     MOVE ORAD-IDKUNDNR          TO W-IDKUNDNR-A5                         
118700     MOVE ORAD-IDKUNDRF-RO(3:5)  TO W-IDKUNDRF-A5                         
118800     MOVE ORAD-IDARTNR           TO W-IDARTNR-A5                          
118900     MOVE ORAD-IDLOPNR-RO        TO W-IDLOPNR-A5                          
119000     PERFORM IMS-GHU-ORDP-WDA501                                          
119100     IF SEGMENT-FINNS                                                     
119200       PERFORM IMS-DLET-ORDP                                              
119300       PERFORM S23-DELETE-PRICE-Q-LINE                                    
119400     END-IF                                                               
119500     .                                                                    
119600     EJECT                                                                
119700                                                                          
119800 S03-SKAPA-ORDERBEKR SECTION.                                             
119900                                                                          
120000     MOVE 'STA S03-OBKR     '             TO   WS-PGM-POSITION            
120100     PERFORM S03A-RAEKNA-9KOMPL                                           
120200     MOVE OHUV-IDORDER           TO   OBKR-IDORDER                        
120300     MOVE ORAD-IDARTNR           TO   OBKR-IDARTNR                        
120400     MOVE +1                     TO   OBKR-IDLOPNR                        
120500     MOVE +1                     TO   OBKR-IDSEKVNR                       
120600     MOVE ORAD-IDDC              TO   OBKR-IDDC                           
120700     MOVE 83                     TO   OBKR-KDORDBEK                       
120800     MOVE IDPGM                  TO   OBKR-IDPGM                          
120900     MOVE SPACE                  TO   OBKR-BEERS                          
121000     MOVE SPACE                  TO   OBKR-IDBIL                          
121100     MOVE OHUV-BEKUNDRF          TO   OBKR-BEKUNDRF                       
121200     MOVE ORAD-BERADREF          TO   OBKR-BERADREF                       
121300     MOVE ORAD-BEVOLREF          TO   OBKR-BEVOLREF                       
121400     MOVE ORAD-IDKAMPRF          TO   OBKR-IDKAMPRF                       
121500     MOVE ZERO                   TO   OBKR-DIERS-KVOT                     
121600     MOVE ORAD-FLAKPLOC          TO   OBKR-FLAKPLOC                       
121700     MOVE ORAD-FLINVEST          TO   OBKR-FLINVEST                       
121800     MOVE 'J'                    TO   OBKR-FLOBOK                         
121900     MOVE 'J'                    TO   OBKR-FLOBTRAN                       
122000     MOVE 'N'                    TO   OBKR-FLOBPRT                        
122100     MOVE ORAD-FLPRTILL          TO   OBKR-FLPRTILL                       
122200     MOVE ORAD-FLRESTN           TO   OBKR-FLRESTN                        
122300     MOVE JA                     TO   OBKR-FLSLATT                        
122400     MOVE ORAD-FLTILLK           TO   OBKR-FLTILLK                        
122500     MOVE ZERO                   TO   OBKR-IDARTNR-TILLK                  
122600     MOVE ORAD-IDDISTR           TO   OBKR-IDDISTR                        
122700     MOVE ORAD-IDKUNDNR          TO   OBKR-IDKUNDNR                       
122800     MOVE ORAD-IDKUNDRF          TO   OBKR-IDKUNDRF                       
122900     MOVE ORAD-IDKUNDRF-RO       TO   OBKR-IDKUNDRF-RO                    
123000     MOVE ORAD-IDLEVNR           TO   OBKR-IDLEVNR                        
123100     MOVE ORAD-IDLOPNR-RO        TO   OBKR-IDLOPNR-RO                     
123200     MOVE ORAD-IDSYSTEM          TO   OBKR-IDSYSTEM                       
123300     MOVE ORAD-IDDC-RO           TO   OBKR-IDDC-RO                        
123400     MOVE ORAD-KDDSP             TO   OBKR-KDDSP                          
123500     MOVE ZERO                   TO   OBKR-KDERS                          
123600     MOVE ORAD-KDOI              TO   OBKR-KDOI                           
123700     MOVE ORAD-CLEARGROUP        TO   OBKR-CLEARGROUP                     
123800     MOVE ORAD-KDKVBRYT          TO   OBKR-KDKVBRYT                       
123900     MOVE ORAD-KDPRTYP           TO   OBKR-KDPRTYP                        
124000     MOVE ORAD-KDTPOTYP          TO   OBKR-KDTPOTYP                       
124100     MOVE ORAD-KDVRINFO          TO   OBKR-KDVRINFO                       
124200     MOVE ORAD-KVBEART-Q         TO   OBKR-KVANNANT                       
124300     MOVE ZERO                   TO   OBKR-KVAVBART                       
124400     MOVE ORAD-KVBEART           TO   OBKR-KVBEART                        
124500     MOVE ORAD-KVBEART-Q         TO   OBKR-KVBEART-Q                      
124600     MOVE ZERO                   TO   OBKR-KVBEART-TILLK                  
124700     MOVE ZERO                   TO   OBKR-KVPREAVB                       
124800     MOVE ZERO                   TO   OBKR-KVPRERO                        
124900     MOVE WS-KVQPACK-1           TO   OBKR-KVQPACK                        
125000     MOVE ZERO                   TO   OBKR-KVRO                           
125100     MOVE ORAD-KVSLATT           TO   OBKR-KVSLATT                        
125200     MOVE ORAD-PRARTNTO          TO   OBKR-PRARTNTO                       
125300     MOVE ORAD-DEAL-PR-LINE      TO   OBKR-DEAL-PR-LINE                   
125400     MOVE ORAD-PRBPRIS           TO   OBKR-PRBPRIS                        
125500     MOVE ORAD-REKSIFFR          TO   OBKR-REKSIFFR                       
125600     MOVE ZERO                   TO   OBKR-REKSIFFR-TILLK                 
125700     MOVE ORAD-RERF-RAD          TO   OBKR-RERF-RAD                       
125800     MOVE ZERO                   TO   OBKR-TIDISPIN                       
125900     MOVE OHUV-TIREGDAT          TO   OBKR-TIORDREG                       
126000     MOVE ORAD-TIPRIS            TO   OBKR-TIPRIS                         
126100     MOVE MSGI-TILOKDAT          TO   OBKR-TIREGDAT                       
126200     MOVE MSGI-TILOKTID          TO   WS-TIHHMM                           
126300     MOVE WS-TIHHMMSS            TO   OBKR-TIREGTID                       
126400     MOVE ZERO                   TO   OBKR-TIRODAT                        
126500     MOVE WS-TITIREGD-9KOMPL     TO   OBKR-TITIREGD-9KOMPL                
126600     MOVE ORAD-TITPO             TO   OBKR-TITPO                          
126700     MOVE WS-TITIORDD-9KOMPL     TO   OBKR-TITIORDD-9KOMPL                
126800     MOVE WS-KDFRAKT             TO   OBKR-KDFRAKT                        
126900     MOVE ORAD-KDORDKL           TO   OBKR-KDORDKL                        
127000                                                                          
127100     MOVE OHUV-KDORDTYP-LDC      TO   OBKR-KDORDTYP-LDC                   
127200     MOVE OHUV-TIREPDAT          TO   OBKR-TIREPDAT                       
127300     MOVE ORAD-IDKUNDRF-WIP      TO   OBKR-IDKUNDRF-WIP                   
127500     MOVE ZERO                   TO   OBKR-TIDLEVDAT                      
127510     MOVE ORAD-PRAVCOST          TO   OBKR-PRAVCOST                       
127520     MOVE ORAD-KDVALISO          TO   OBKR-KDVALISO                       
127600                                                                          
127700     PERFORM IMS-ISRT-ORQM-ORQM01                                         
127800     PERFORM UNTIL SEGMENT-FINNS                                          
127900        ADD +1 TO OBKR-IDLOPNR                                            
128000        PERFORM IMS-ISRT-ORQM-ORQM01                                      
128100     END-PERFORM                                                          
128200     .                                                                    
128300     EJECT                                                                
128400 S03A-RAEKNA-9KOMPL SECTION.                                              
128500                                                                          
128600     MOVE 'STA S03A-9KOMP   '             TO   WS-PGM-POSITION            
128700     MOVE OHUV-TIREGDAT TO WS-AAMMDD                                      
128800     IF WS-AAMMDD > 500000                                                
128900        MOVE 19              TO WS-CENTURY                                
129000     ELSE                                                                 
129100        MOVE 20              TO WS-CENTURY                                
129200     END-IF                                                               
129300     COMPUTE WS-TITIORDD-9KOMPL = 999999999                               
129400                                - WS-9KOMPL-DATUM                         
129500                                                                          
129600     MOVE OBKR-TIREGDAT      TO WS-AAMMDD                                 
129700     IF WS-AAMMDD > 500000                                                
129800        MOVE 19              TO WS-CENTURY                                
129900     ELSE                                                                 
130000        MOVE 20              TO WS-CENTURY                                
130100     END-IF                                                               
130200     COMPUTE WS-TITIREGD-9KOMPL = 999999999                               
130300                                - WS-9KOMPL-DATUM                         
130400     .                                                                    
130500     EJECT                                                                
130600 S04C-DELBACKA-NYVORKO SECTION.                                           
130700                                                                          
130800     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
130900     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
131000     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
131100                                    W-A601KY-MAX-IDDISTR                  
131200     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
131300                                    W-A601KY-MAX-IDKUNDNR                 
131400     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
131500                                    W-A601KY-MAX-IDKUNDRF                 
131600     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
131700                                    W-A601KY-MAX-TIREGDAT                 
131800     MOVE ORAD-IDARTNR           TO W-A601KY-MIN-IDARTNR                  
131900                                    W-A601KY-MAX-IDARTNR                  
132000                                                                          
132100     PERFORM IMS-GHN-WDA6B                                                
132200     PERFORM UNTIL SEGMENT-SAKNAS                                         
132300                OR BASEN-SLUT                                             
132400                                                                          
132500         IF  VOR-KDVORATG > '1'                                           
132600         AND VOR-KDVORATG < '6'                                           
132700         AND VOR-KVPREAVB >= ORAD-KVBEART-Q                               
132800             SUBTRACT ORAD-KVBEART-Q FROM VOR-KVPREAVB                    
132900             IF  VOR-KVPREAVB = 0                                         
133000                 MOVE '8'        TO VOR-KDVORATG                          
133100                 MOVE 83         TO VOR-KDORDBEK                          
133110                 IF VOR-TIKLAR = ZERO                                     
133200                    MOVE WS-TINUDAT TO VOR-TIKLAR                         
133300                    COMPUTE VOR-TIKLATID = WS-TINUTID                     
133400                                            / 100                         
133500                    END-COMPUTE                                           
133510                 END-IF                                                   
133600             END-IF                                                       
133700             PERFORM IMS-REPL-WDA6B                                       
133800         END-IF                                                           
133900                                                                          
134000         PERFORM IMS-GHN-WDA6B                                            
134100     END-PERFORM                                                          
134200     .                                                                    
134300     EJECT                                                                
134400 S05-SKAPA-TRANSAR SECTION.                                               
134500                                                                          
134600     MOVE 'STA S05-SKAPA-TRANSAR'         TO   WS-PGM-POSITION            
134700     IF ORAD-KDOI NOT = SPACE                                             
134800       IF (MID-IDSYSTEM = 'LDCC' OR 'LYNC' OR 'ECOC' OR 'VOUC' OR         
134820                          'TADC' OR 'ACCC' OR 'APAC' OR                   
134830                          'APBC' OR 'APCC' OR 'APDC' OR                   
134840                          'APEC' OR 'APFC' OR 'APGC' OR                   
134850                          'APHC' OR 'APIC' OR 'APJC')                     
134900         CONTINUE                                                         
135000       ELSE                                                               
135100         PERFORM S05A-SKAPA-W2I109MID                                     
135200       END-IF                                                             
135300     END-IF                                                               
135400                                                                          
135500*    IF MID-IDSYSTEM(1:3) = 'LDC'                                         
135600*SKAPA INGA RYC FÖR LDC-FIXEN SE PGM W41242  TL 030603                    
135700     IF (MID-IDSYSTEM  = 'LDCC' OR 'LYNC' OR 'ECOC' OR 'VOUC' OR          
135710                         'TADC' OR 'ACCC' OR 'APAC' OR                    
135720                         'APBC' OR 'APCC' OR 'APDC' OR                    
135730                         'APEC' OR 'APFC' OR 'APGC' OR                    
135740                         'APHC' OR 'APIC' OR 'APJC')                      
135800       CONTINUE                                                           
135900     ELSE                                                                 
136000       PERFORM S05B-SKAPA-RYC                                             
136100     END-IF                                                               
136200     .                                                                    
136300     EJECT                                                                
136400                                                                          
136500 S05A-SKAPA-W2I109MID SECTION.                                            
136600                                                                          
136610*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
136620     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
136630     IF NOT BYT03-OBJEKT                                                  
136640                                                                          
136700        MOVE 'STA S05A-ORDERINGÅNG '      TO   WS-PGM-POSITION            
136800        MOVE 2109-INDX          TO 2109-MID2-KVANTART                     
136900        MOVE ORAD-IDARTNR       TO 2109-MID2-IDARTNR (2109-INDX)          
137000        MOVE OHUV-IDDC-PRIM     TO 2109-MID2-IDDC (2109-INDX)             
137100        MOVE '-'                TO 2109-MID2-KDTECKEN (2109-INDX)         
137200        MOVE ORAD-KDOI          TO 2109-MID2-KDOI (2109-INDX)             
137300        MOVE ORAD-CLEARGROUP    TO 2109-MID2-CLEARGROUP(2109-INDX)        
137400        MOVE ORAD-KVBEART-Q     TO 2109-MID2-KVOI (2109-INDX)             
137500        MOVE ORAD-TIREGDAT      TO 2109-MID2-TIUPPDAT (2109-INDX)         
137600                                                                          
137700        ADD +1 TO 2109-INDX                                               
137800        IF 2109-INDX > MAX-2109-INDX                                      
137900          PERFORM S16-STARTA-2109                                         
138000          MOVE ZERO TO 2109-MID2-KVANTART                                 
138100        END-IF                                                            
138110     END-IF                                                               
138200     .                                                                    
138300     EJECT                                                                
138400                                                                          
138500 S05B-SKAPA-RYC SECTION.                                                  
138600                                                                          
138700     MOVE 'STA S05B-RYC         '         TO   WS-PGM-POSITION            
138800     MOVE 'RYC'               TO   W-RYC-IDPTYP                           
138900     MOVE ORAD-BERADREF       TO   W-RYC-BERADREF                         
139000     MOVE ORAD-BEVOLREF       TO   W-RYC-BEVOLREF                         
139100     MOVE ORAD-FLTILLK        TO   W-RYC-FLTILLK                          
139200     MOVE ORAD-IDARTNR        TO   W-RYC-IDARTNR                          
139300     MOVE ORAD-IDDISTR        TO   W-RYC-IDDISTR                          
139400     MOVE ORAD-IDKUNDNR       TO   W-RYC-IDKUNDNR                         
139500     MOVE ORAD-IDKUNDRF       TO   W-RYC-IDKUNDRF                         
139600     MOVE ORAD-IDKUNDRF-RO    TO   W-RYC-IDKUNDRF-RO                      
139700     MOVE ORAD-KDDSP          TO   W-RYC-KDDSP                            
139800     MOVE OHUV-KDFAKTYP       TO   W-RYC-KDFAKTYP                         
139900     MOVE WS-KDFRAKT          TO   W-RYC-KDFRAKT                          
140000     MOVE 83                  TO   W-RYC-KDORDBEK                         
140100     MOVE ORAD-KDORDKL        TO   W-RYC-KDORDKL                          
140200     MOVE ORAD-KDKVBRYT       TO   W-RYC-KDKVBRYT                         
140300     MOVE ORAD-KDVRINFO       TO   W-RYC-KDVRINFO                         
140400     MOVE 0                   TO   W-RYC-KDVRTPO                          
140500     MOVE ORAD-KVBEART-Q      TO   W-RYC-KVANNANT                         
140600     MOVE ORAD-REKSIFFR       TO   W-RYC-REKSIFFR                         
140700     MOVE OHUV-TIREGDAT       TO   W-RYC-TIORDREG                         
140800     MOVE ORAD-TIRODAT        TO   W-RYC-TIRODAT                          
140900                                                                          
141000     ACCEPT TIAAMMDD FROM DATE                                            
141100     ACCEPT TIKLOCK FROM TIME                                             
141200     MOVE +1 TO IDLOGLOP                                                  
141300     MOVE 'RYC' TO IDPTYP                                                 
141400     MOVE W-RYCPOST TO LOGGPOST                                           
141500                                                                          
141600     MOVE SPACE               TO W-RYCS-WDGZRYCS                          
141700     MOVE ORAD-IDDC           TO W-RYCS-IDDC                              
141800     MOVE W-RYCSPOST TO SORTPOST                                          
141900                                                                          
142000     PERFORM IMS-ISRT-ZZAC-ZZAC01                                         
142100     PERFORM UNTIL SEGMENT-FINNS                                          
142200        ADD +1 TO IDLOGLOP                                                
142300        PERFORM IMS-ISRT-ZZAC-ZZAC01                                      
142400     END-PERFORM                                                          
142500     .                                                                    
142600     EJECT                                                                
142700 S07-SKAPA-AVSR SECTION.                                                  
142800                                                                          
142900     MOVE 'STA S07-AVSR         '         TO   WS-PGM-POSITION            
143000     MOVE JA TO AVSR-SW                                                   
143100     MOVE 2                  TO   AVSR-KDCALL                             
143200     MOVE OHUV-IDORDER       TO   AVSR-IDORDER                            
143300     MOVE OHUV-KDORDKL       TO   AVSR-KDORDKL                            
143400     MOVE ZERO               TO   AVSR-KDFRAKT                            
143500     MOVE ZERO               TO   AVSR-KDROPACK                           
143600     MOVE MSGI-TILOKDAT      TO   AVSR-TIREGDAT                           
143700     MOVE MSGI-TILOKTID      TO   AVSR-TIHHMM                             
143800                                                                          
143900     MOVE ORAD-ADLAGOMR      TO   AVSR-ADLAGOMR(AVSR-INDX)                
144000     MOVE ORAD-IDLEVNR       TO   AVSR-IDLEVNR(AVSR-INDX)                 
144100     MOVE ORAD-IDDC          TO   AVSR-IDDC(AVSR-INDX)                    
144200     MOVE ORAD-KDSPEEMB      TO   AVSR-KDSPEEMB(AVSR-INDX)                
144300     MOVE ORAD-KVBEART-Q     TO   AVSR-KVANNANT(AVSR-INDX)                
144400     MOVE ORAD-KVBEART-Q     TO   AVSR-KVBEART-Q(AVSR-INDX)               
144500     MOVE ORAD-PRARTNTO      TO   AVSR-PRARTNTO(AVSR-INDX)                
144510     MOVE ORAD-PRAVCOST      TO   AVSR-PRAVCOST(AVSR-INDX)                
144600     MOVE ORAD-DEAL-PR-LINE  TO   AVSR-DEAL-PR-LINE(AVSR-INDX)            
144700     MOVE ORAD-VKART         TO   AVSR-VKART(AVSR-INDX)                   
144800     MOVE ORAD-VLARTNTO      TO   AVSR-VLARTNTO(AVSR-INDX)                
144900     ADD +1 TO AVSR-INDX                                                  
145000                                                                          
145100     IF AVSR-INDX > MAX-AVSR-INDX                                         
145200        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
145300        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
145400        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
145500        TRAN-XXKB-PCB                                                     
145600                                                                          
145700        PERFORM AA-NOLLA-AVSR-TABELL                                      
145800     END-IF                                                               
145900     .                                                                    
146000     EJECT                                                                
146100 S09-SKAPA-AVSO SECTION.                                                  
146200                                                                          
146300     MOVE 'STA S09-AVSO         '         TO   WS-PGM-POSITION            
146400     MOVE OHUV-IDDISTR       TO AVSO-IDDISTR                              
146500     MOVE OHUV-IDKUNDNR      TO AVSO-IDKUNDNR                             
146600     MOVE OHUV-IDKUNDRF      TO AVSO-IDKUNDRF                             
146700     MOVE OHUV-IDORDER       TO AVSO-IDORDER                              
146800     MOVE SPACE              TO AVSO-IDDC                                 
146900     MOVE ZERO               TO AVSO-TIRFS                                
147000     MOVE ZERO               TO AVSO-TIAAMMDD                             
147100     MOVE ZERO               TO AVSO-TIHHMM                               
147200     MOVE '4254'             TO AVSO-IDTRANS                              
147300     .                                                                    
147400     EJECT                                                                
147500 S13-BACKA-KAMPANJ SECTION.                                               
147700     MOVE 'STA S13-KAMP         '         TO   WS-PGM-POSITION            
147710                                                                          
147800     MOVE ORAD-IDKAMPRF        TO W-KAMP-IDKAMPRF                         
147900     MOVE ORAD-IDDC            TO W-KAMP-IDDC                             
148000     MOVE ORAD-IDARTNR         TO W-KART-IDARTNR                          
148300     MOVE ORAD-IDDISTR         TO W-KMRK-IDDISTR-FOM                      
148400     MOVE ORAD-IDDISTR         TO W-KMRK-IDDISTR-TOM                      
148500     MOVE ORAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-FOM                     
148600     MOVE ORAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-TOM                     
148700                                                                          
150300     PERFORM IMS-GHU-WDM211                                               
150500     IF SEGMENT-FINNS                                                     
150600       COMPUTE KART-KVBEART-KUND = KART-KVBEART-KUND                      
150700                                 - ORAD-KVBEART-Q                         
150800       IF KART-KVBEART-KUND < ZERO                                        
150810         MOVE 'WDM211 - ANTAL SALDO NEGATIV - ABEND' TO FELTEXT           
150900         MOVE 'ANTAL SALDO NEGATIV - ABEND' TO FELTEXT                    
151000         CALL ABEND USING RKOD-ABEND                                      
151100       ELSE                                                               
151200         PERFORM IMS-REPL-WDM211                                          
151300       END-IF                                                             
151400     ELSE                                                                 
151410       MOVE 'WDM211 SAKNAS - ABEND' TO FELTEXT                            
151600       CALL ABEND USING RKOD-ABEND                                        
151700     END-IF                                                               
151701                                                                          
151702     PERFORM S20-FINN-INTERVALL                                           
151710     PERFORM IMS-GHU-WDM221                                               
151730     IF SEGMENT-FINNS                                                     
151740       COMPUTE KMRK-KVBEART-KUND = KMRK-KVBEART-KUND                      
151750                                 - ORAD-KVBEART-Q                         
151760       IF KMRK-KVBEART-KUND < ZERO                                        
151770         MOVE 'WDM221 - ANTALSTABELL NEGATIV - ABEND' TO FELTEXT          
151780         CALL ABEND USING RKOD-ABEND                                      
151790       ELSE                                                               
151793         PERFORM IMS-REPL-WDM221                                          
151794       END-IF                                                             
151795     END-IF                                                               
151796                                                                          
151800     .                                                                    
151900     EJECT                                                                
152000 S16-STARTA-2109 SECTION.                                                 
152100                                                                          
152200     MOVE 'STA S16-2109         '         TO   WS-PGM-POSITION            
152300     COMPUTE MSG-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                 
152400     MOVE 'W2T109X '           TO MSG-KDTRANS-1                           
152500     MOVE '4254'               TO MSG-IDTRANS-1                           
152600     MOVE '1'                  TO MSG-KDMFSFOR-1                          
152700                                                                          
152800     PERFORM IMS-PURG-ALT-MSG-2109                                        
152900                                                                          
153000     MOVE SPACE                TO 2109-MID2-W2I10902                      
153100     MOVE +1                   TO 2109-INDX                               
153200     .                                                                    
153300     EJECT                                                                
153500 S20-FINN-INTERVALL SECTION.                                              
153510                                                                          
153520     PERFORM IMS-GU-WDM211                                                
153530     IF SEGMENT-FINNS                                                     
153540       PERFORM IMS-GNP-WDM221                                             
153900       PERFORM UNTIL SEGMENT-SAKNAS                                       
154000         IF  ORAD-IDDISTR > KMRK-IDDISTR-TOM                              
154100         OR  ORAD-IDDISTR < KMRK-IDDISTR-FOM                              
154200           CONTINUE                                                       
154300         ELSE                                                             
154400           IF  ORAD-IDDISTR  = KMRK-IDDISTR-TOM                           
154500           AND ORAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                          
154600             CONTINUE                                                     
154700           ELSE                                                           
154800             IF  ORAD-IDDISTR  = KMRK-IDDISTR-FOM                         
154900             AND ORAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                        
155000               CONTINUE                                                   
155100             ELSE                                                         
155200               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
155300               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
155400               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
155500               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
155600             END-IF                                                       
155700           END-IF                                                         
155800         END-IF                                                           
155810         PERFORM IMS-GNP-WDM221                                           
156000       END-PERFORM                                                        
156100     END-IF                                                               
156200     .                                                                    
156300     EJECT                                                                
156400                                                                          
156500 S23-DELETE-PRICE-Q-LINE SECTION.                                         
156600     MOVE RAD-IDDISTR            TO TEST-IDDISTR                          
156700     IF DIST79-DEALER-PRICE                                               
156800       IF RAD-IDPRQUES > ZERO                                             
156900         INITIALIZE PRQU-W335PRQU                                         
157000         MOVE RAD-IDDISTR        TO PRQU-IDDISTR                          
157100         MOVE RAD-IDKUNDNR       TO PRQU-IDKUNDNR                         
157200         MOVE RAD-IDKUNDRF(1:5)  TO PRQU-IDKUNDRF(3:5)                    
157300         MOVE '00'               TO PRQU-IDKUNDRF(1:2)                    
157400         MOVE RAD-IDPRQUES       TO PRQU-IDPRQUES                         
157500         MOVE 4                  TO PRQU-KDCALL                           
157600                                                                          
157700         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
157800                                            PRQU-WDC7-PCB                 
157900                                            PRQU-SJKO-WDK6-PCB            
158000       END-IF                                                             
158100     END-IF                                                               
158200     .                                                                    
158300     EJECT                                                                
158400                                                                          
158500 S21-SEND-OPEN SECTION.                                                   
158600     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
158700     MOVE 'OPEN'                  TO SEND-KDFUNC                          
158800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
158900                                     SEND-OPEN-AREA                       
159000     IF SEND-KDRC > ZERO                                                  
159100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
159200       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
159300       DELIMITED BY SIZE INTO FELTEXT                                     
159400       DISPLAY FELTEXT                                                    
159500       CALL FELLOG                                                        
159600     END-IF                                                               
159700     .                                                                    
159800     EJECT                                                                
159900 S22-PUT-HEADER SECTION.                                                  
160000     MOVE 1                       TO REQU-IDMSGVER                        
160100     MOVE 'R'                     TO REQU-KDPGMACT                        
160200     MOVE IDPGM                   TO REQU-IDUSER                          
160300     MOVE 'ORDERCONF'             TO HDR-IDOUTTYPE                        
160400     MOVE WS-IDKUNDNR-NUM         TO HDR-IDOUTREC                         
160500     MOVE WS-IDORDNR7             TO HDR-IDLIST                           
160600     MOVE 'PUT'                   TO SEND-KDFUNC                          
160700     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
160800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
160900                                     SEND-KVDLEN                          
161000                                     HDR-AREA                             
161100     IF SEND-KDRC > ZERO                                                  
161200       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
161300       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
161400       DELIMITED BY SIZE       INTO FELTEXT                               
161500       DISPLAY FELTEXT                                                    
161600       CALL FELLOG                                                        
161700     END-IF                                                               
161800     .                                                                    
161900     EJECT                                                                
162000 S25-PUT-LINE SECTION.                                                    
162100     MOVE 'PUT'                   TO SEND-KDFUNC                          
162200     MOVE LENGTH OF 402-W402TACD  TO SEND-KVDLEN                          
162300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
162400                                     SEND-KVDLEN                          
162500                                     402-W402TACD                         
162600     IF SEND-KDRC > ZERO                                                  
162700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
162800       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
162900       DELIMITED BY SIZE       INTO FELTEXT                               
163000       DISPLAY FELTEXT                                                    
163100       CALL FELLOG                                                        
163200     END-IF                                                               
163300     .                                                                    
163400     SKIP2                                                                
163500 S29-SEND-CLOSE SECTION.                                                  
163600     MOVE 'CLOSE'                TO SEND-KDFUNC                           
163700     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
163800     IF SEND-KDRC > 0                                                     
163900       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
164000       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
164100       DELIMITED BY SIZE       INTO FELTEXT                               
164200       DISPLAY FELTEXT                                                    
164300       CALL FELLOG                                                        
164400     END-IF                                                               
164500     .                                                                    
164600     EJECT                                                                
164700* --- IMS SEKTIONER ---                                                   
164800     SKIP3                                                                
164900 IMS-GET-MSG SECTION.                                                     
165000                                                                          
165100     MOVE '  QC' TO GODK-STATUSKODER                                      
165200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
165300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165400     PERFORM IMS-STATUSKONTROLL                                           
165500     .                                                                    
165600                                                                          
165700 IMS-GN-MSG SECTION.                                                      
165800                                                                          
165900     MOVE '  '   TO GODK-STATUSKODER                                      
166000     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
166100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
166200     PERFORM IMS-STATUSKONTROLL                                           
166300     .                                                                    
166400                                                                          
166500 IMS-INSERT-DISP-MSG SECTION.                                             
166600                                                                          
166700     MOVE SPACE  TO GODK-STATUSKODER                                      
166800     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
166900     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
167000     PERFORM IMS-STATUSKONTROLL                                           
167100     .                                                                    
167200                                                                          
167300 IMS-PURG-ALTMSG SECTION.                                                 
167400     MOVE LOW-VALUE TO 4680-Z1 4680-Z2                                    
167500     MOVE SPACE TO GODK-STATUSKODER                                       
167600     CALL CBLTDLI USING PURG 4680-PCB W-PROG-TO-PROG-SW                   
167700     MOVE 4680-STATUS-CODE TO STATUS-WS                                   
167800     PERFORM IMS-STATUSKONTROLL                                           
167900     .                                                                    
168000     SKIP2                                                                
168100 IMS-PURG-ALT-MSG-2109 SECTION.                                           
168200                                                                          
168300     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
168400     MOVE    '  '             TO    GODK-STATUSKODER                      
168500     CALL    CBLTDLI          USING PURG 2109-PCB MSG-IO-AREA             
168600     MOVE    2109-STATUS-CODE TO    STATUS-WS                             
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     .                                                                    
168900     EJECT                                                                
169000                                                                          
169100 IMS-GU-ARTC11 SECTION.                                                   
169200                                                                          
169300     STRING  'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                        
169400             DELIMITED BY SIZE INTO    SSA1                               
169500     MOVE    'WLARTC11 '         TO    SSA2                               
169600     MOVE    '  '                TO    GODK-STATUSKODER                   
169700     CALL    CBLTDLI             USING GU               ARTC-PCB          
169800                                       DLI-IO-AREA-ARTC SSA1 SSA2         
169900     MOVE    ARTC-STATUS-CODE    TO    STATUS-WS                          
170000     PERFORM IMS-STATUSKONTROLL                                           
170100     .                                                                    
170200     EJECT                                                                
170300 IMS-GHU-ARTM-ARTM01 SECTION.                                             
170400                                                                          
170500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
170600          DELIMITED BY SIZE INTO SSA1                                     
170700     MOVE '  GE' TO GODK-STATUSKODER                                      
170800     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
170900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
171000     PERFORM IMS-STATUSKONTROLL                                           
171100     .                                                                    
171200                                                                          
171300 IMS-REPL-ARTM SECTION.                                                   
171400                                                                          
171500     MOVE '  ' TO GODK-STATUSKODER                                        
171600     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
171700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
171800     PERFORM IMS-STATUSKONTROLL                                           
171900     .                                                                    
172000     EJECT                                                                
172100 IMS-GHU-ARTS-ARTS11 SECTION.                                             
172200                                                                          
172300     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
172400          DELIMITED BY SIZE INTO SSA1                                     
172500     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
172600          DELIMITED BY SIZE INTO SSA2                                     
172700     MOVE '    ' TO GODK-STATUSKODER                                      
172800     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA-ARTS SSA1 SSA2           
172900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
173000     PERFORM IMS-STATUSKONTROLL                                           
173100     .                                                                    
173200                                                                          
173300 IMS-REPL-ARTS SECTION.                                                   
173400                                                                          
173500     MOVE '  ' TO GODK-STATUSKODER                                        
173600     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA-ARTS                    
173700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
173800     PERFORM IMS-STATUSKONTROLL                                           
173900     .                                                                    
174000     EJECT                                                                
174100 IMS-GHU-ARTC-ARTC11 SECTION.                                             
174200                                                                          
174300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
174400          DELIMITED BY SIZE INTO SSA1                                     
174500     MOVE    'WLARTC11 '         TO    SSA2                               
174600     MOVE '    ' TO GODK-STATUSKODER                                      
174700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-ARTC SSA1 SSA2           
174800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
174900     PERFORM IMS-STATUSKONTROLL                                           
175000     .                                                                    
175100                                                                          
175200 IMS-REPL-ARTC SECTION.                                                   
175300                                                                          
175400     MOVE '  ' TO GODK-STATUSKODER                                        
175500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-ARTC                    
175600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900     EJECT                                                                
176000                                                                          
176100 IMS-GHU-ORDP-WDA501 SECTION.                                             
176200                                                                          
176300     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
176400          DELIMITED BY SIZE INTO SSA1                                     
176500     MOVE '  GE' TO GODK-STATUSKODER                                      
176600     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA-ORDP SSA1                
176700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
176800     PERFORM IMS-STATUSKONTROLL                                           
176900     .                                                                    
177000                                                                          
177120 IMS-DLET-ORDP SECTION.                                                   
177200                                                                          
177300     MOVE '  ' TO GODK-STATUSKODER                                        
177400     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-AREA-ORDP                    
177500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
177600     PERFORM IMS-STATUSKONTROLL                                           
177700     .                                                                    
177800   EJECT                                                                  
177910 IMS-GU-WDM211 SECTION.                                                   
177920                                                                          
177930     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
177940          DELIMITED BY SIZE INTO SSA1                                     
177950     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
177960          DELIMITED BY SIZE INTO SSA2                                     
177970     MOVE '  GE'              TO GODK-STATUSKODER                         
177980     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
177990     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
177991     PERFORM IMS-STATUSKONTROLL                                           
177992     .                                                                    
177993                                                                          
177994 IMS-GNP-WDM221 SECTION.                                                  
177996                                                                          
177997     MOVE 'WDM221 '           TO SSA1                                     
177998     MOVE '    GE'            TO GODK-STATUSKODER                         
177999     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
178000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
178001     PERFORM IMS-STATUSKONTROLL                                           
178002     .                                                                    
178003                                                                          
178004 IMS-GHU-WDM211 SECTION.                                                  
178006                                                                          
178007     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
178008          DELIMITED BY SIZE INTO SSA1                                     
178009     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
178010          DELIMITED BY SIZE INTO SSA2                                     
178011     MOVE '  GE'              TO GODK-STATUSKODER                         
178012     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
178013     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
178014     PERFORM IMS-STATUSKONTROLL                                           
178015     .                                                                    
178016                                                                          
178017 IMS-REPL-WDM211 SECTION.                                                 
178019                                                                          
178020     MOVE '  '             TO GODK-STATUSKODER                            
178021     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
178022     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
178023     PERFORM IMS-STATUSKONTROLL                                           
178024     .                                                                    
178025     EJECT                                                                
178026 IMS-GHU-WDM221 SECTION.                                                  
178028                                                                          
178029     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
178030          DELIMITED BY SIZE INTO SSA1                                     
178031     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
178032          DELIMITED BY SIZE INTO SSA2                                     
178033     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
178034          DELIMITED BY SIZE INTO SSA3                                     
178035     MOVE '  GE' TO GODK-STATUSKODER                                      
178036     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
178037     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
178038     PERFORM IMS-STATUSKONTROLL                                           
178039     .                                                                    
178040                                                                          
178041 IMS-REPL-WDM221 SECTION.                                                 
178043                                                                          
178044     MOVE '  '             TO GODK-STATUSKODER                            
178045     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
178046     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
178047     PERFORM IMS-STATUSKONTROLL                                           
178048     .                                                                    
179700                                                                          
181700     EJECT                                                                
183800 IMS-GU-ORQA-ORQA01 SECTION.                                              
183900                                                                          
184000     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
184100                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
184200          DELIMITED BY SIZE INTO SSA1                                     
184300     MOVE '  GE' TO GODK-STATUSKODER                                      
184400     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
184500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
184600     PERFORM IMS-STATUSKONTROLL                                           
184700     .                                                                    
184800                                                                          
184900 IMS-GN-ORQA-ORQA01 SECTION.                                              
185000                                                                          
185100     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
185200                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
185300          DELIMITED BY SIZE INTO SSA1                                     
185400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
185500     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
185600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
185700     PERFORM IMS-STATUSKONTROLL                                           
185800     .                                                                    
185900     EJECT                                                                
186000 IMS-GU-ORQG-ORQG01 SECTION.                                              
186100                                                                          
186200     STRING 'WLORQG01(WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
186300                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X ')'                    
186400          DELIMITED BY SIZE INTO SSA1                                     
186500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
186600     CALL CBLTDLI USING GU ORQG-PCB DLI-IO-AREA-SEQA SSA1                 
186700     MOVE ORQG-STATUS-CODE TO STATUS-WS                                   
186800     PERFORM IMS-STATUSKONTROLL                                           
186900     .                                                                    
187000 IMS-GN-ORQG-ORQG01 SECTION.                                              
187100                                                                          
187200     STRING 'WLORQG01(WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
187300                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X ')'                    
187400          DELIMITED BY SIZE INTO SSA1                                     
187500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
187600     CALL CBLTDLI USING GN ORQG-PCB DLI-IO-AREA-SEQA SSA1                 
187700     MOVE ORQG-STATUS-CODE TO STATUS-WS                                   
187800     PERFORM IMS-STATUSKONTROLL                                           
187900     .                                                                    
188000     EJECT                                                                
188100 IMS-GU-ORQG-ORQG01-DC SECTION.                                           
188200                                                                          
188300     STRING 'WLORQG01(WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
188400                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X                        
188500                    '&IDDC     =' WS1-IDDC ')'                            
188600          DELIMITED BY SIZE INTO SSA1                                     
188700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
188800     CALL CBLTDLI USING GU ORQG-PCB DLI-IO-AREA-SEQA SSA1                 
188900     MOVE ORQG-STATUS-CODE TO STATUS-WS                                   
189000     PERFORM IMS-STATUSKONTROLL                                           
189100     .                                                                    
189200 IMS-GN-ORQG-ORQG01-DC SECTION.                                           
189300                                                                          
189400     STRING 'WLORQG01(WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
189500                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X                        
189600                    '&IDDC     =' WS1-IDDC ')'                            
189700          DELIMITED BY SIZE INTO SSA1                                     
189800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
189900     CALL CBLTDLI USING GN ORQG-PCB DLI-IO-AREA-SEQA SSA1                 
190000     MOVE ORQG-STATUS-CODE TO STATUS-WS                                   
190100     PERFORM IMS-STATUSKONTROLL                                           
190200     .                                                                    
190300     EJECT                                                                
190400 IMS-GHU-ORQF-ORQF01 SECTION.                                             
190500                                                                          
190600     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-X ')'                        
190700          DELIMITED BY SIZE INTO SSA1                                     
190800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
190900     CALL CBLTDLI USING GHU ORQF-PCB DLI-IO-AREA-ORAD SSA1                
191000     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
191100     PERFORM IMS-STATUSKONTROLL                                           
191200     .                                                                    
191300                                                                          
191400 IMS-DLET-ORQF SECTION.                                                   
191500                                                                          
191600     MOVE '  ' TO GODK-STATUSKODER                                        
191700     CALL CBLTDLI USING DLET ORQF-PCB DLI-IO-AREA-ORAD                    
191800     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
191900     PERFORM IMS-STATUSKONTROLL                                           
192000     .                                                                    
192100     EJECT                                                                
192200 IMS-GHU-ORQI-ORQI01 SECTION.                                             
192300                                                                          
192400     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
192500          DELIMITED BY SIZE INTO SSA1                                     
192600     MOVE '  GE' TO GODK-STATUSKODER                                      
192700     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
192800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
192900     PERFORM IMS-STATUSKONTROLL                                           
193000     .                                                                    
193100                                                                          
193200 IMS-REPL-ORQI SECTION.                                                   
193300                                                                          
193400     MOVE '  ' TO GODK-STATUSKODER                                        
193500     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
193600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
193700     PERFORM IMS-STATUSKONTROLL                                           
193800     .                                                                    
193900                                                                          
194000 IMS-GNP-ORQI-ORQI12 SECTION.                                             
194100                                                                          
194200     STRING  'WLORQI12(IDDC     =' W-IDDC-X ')'                           
194300             DELIMITED BY SIZE INTO    SSA1                               
194400     MOVE    '  GE'              TO    GODK-STATUSKODER                   
194500     CALL    CBLTDLI             USING GNP             ORQI-PCB           
194600                                       DLI-IO-AREA-ARB SSA1               
194700     MOVE    ORQI-STATUS-CODE    TO    STATUS-WS                          
194800     PERFORM IMS-STATUSKONTROLL                                           
194900     .                                                                    
195000     EJECT                                                                
195100 IMS-ISRT-ORQM-ORQM01 SECTION.                                            
195200                                                                          
195300     MOVE    'WLORQM01 '      TO    SSA1                                  
195400     MOVE    '  II'           TO    GODK-STATUSKODER                      
195500     CALL    CBLTDLI          USING ISRT             ORQM-PCB             
195600                                    DLI-IO-AREA-OBKR SSA1                 
195700     MOVE    ORQM-STATUS-CODE TO    STATUS-WS                             
195800     PERFORM IMS-STATUSKONTROLL                                           
195900     .                                                                    
196000     EJECT                                                                
196120 IMS-ISRT-ZZAC-ZZAC01 SECTION.                                            
196200                                                                          
196300     MOVE    'WLZZAC01 '      TO    SSA1                                  
196400     MOVE    '  II'           TO    GODK-STATUSKODER                      
196500     CALL    CBLTDLI          USING ISRT             ZZAC-PCB             
196600                                    DLI-IO-AREA-ZZAC SSA1                 
196700     MOVE    ZZAC-STATUS-CODE TO    STATUS-WS                             
196800     PERFORM IMS-STATUSKONTROLL                                           
196900     .                                                                    
197000     EJECT                                                                
197100 IMS-GU-WDE401    SECTION.                                                
197200     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
197300            DELIMITED BY SIZE INTO SSA1                                   
197400     MOVE '  GE' TO GODK-STATUSKODER                                      
197500     CALL CBLTDLI USING GU  WDE4-PCB KORD-WDE401 SSA1                     
197600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
197700     PERFORM IMS-STATUSKONTROLL                                           
197800     SKIP2                                                                
197900     .                                                                    
198000 IMS-GNP-WDE411    SECTION.                                               
198100     MOVE   'WDE411   '         TO SSA1                                   
198200     MOVE '  GE' TO GODK-STATUSKODER                                      
198300     CALL CBLTDLI USING GNP  WDE4-PCB E4-ORAD-WDE411 SSA1                 
198400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
198500     PERFORM IMS-STATUSKONTROLL                                           
198600     SKIP2                                                                
198700     .                                                                    
198800 IMS-GHU-WDE411    SECTION.                                               
198900     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
199000            DELIMITED BY SIZE INTO SSA1                                   
199100     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
199200            DELIMITED BY SIZE INTO SSA2                                   
199300     MOVE '  ' TO GODK-STATUSKODER                                        
199400     CALL CBLTDLI USING GHU WDE4-PCB E4-ORAD-WDE411 SSA1 SSA2             
199500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     SKIP2                                                                
199800     .                                                                    
199900 IMS-REPL-WDE411        SECTION.                                          
200000     MOVE '    ' TO GODK-STATUSKODER                                      
200100     CALL CBLTDLI USING REPL WDE4-PCB E4-ORAD-WDE411                      
200200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200500 IMS-GHN-WDA6B SECTION.                                                   
200600     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
200700                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
200800            DELIMITED BY SIZE INTO SSA1                                   
200900     MOVE '  GEGB'               TO GODK-STATUSKODER                      
201000     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
201100     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
201200     PERFORM IMS-STATUSKONTROLL                                           
201300     .                                                                    
201400 IMS-REPL-WDA6B SECTION.                                                  
201500     MOVE 'WDA601  '           TO SSA1                                    
201600     MOVE '    '               TO GODK-STATUSKODER                        
201700     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
201800     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
201900     PERFORM IMS-STATUSKONTROLL                                           
202000     .                                                                    
202100     EJECT                                                                
202200 IMS-GU-WDB601    SECTION.                                                
202300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
202400          DELIMITED BY SIZE INTO SSA1                                     
202500     MOVE '  GE' TO GODK-STATUSKODER                                      
202600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
202700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
202800     PERFORM IMS-STATUSKONTROLL                                           
202900     IF SEGMENT-SAKNAS                                                    
203000         MOVE SPACE TO DCS-KDDC                                           
203100     END-IF                                                               
203200     .                                                                    
203340 IMS-STATUSKONTROLL SECTION.                                              
203400                                                                          
203500     SET      STATUS-IX TO 1                                              
203600     SEARCH   GODK-STATUS                                                 
203700       AT END CALL FELLOG                                                 
203800       WHEN   GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                
203900     END-SEARCH                                                           
204000     .                                                                    
