000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL014100.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/07/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.ORDERQUERYPART1'                           
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        FUNKTION.                                                        
001100*        PROGRAMMET VISAR ARTIKEL INFORMATION OM ANGIVET                  
001200*        ORDER.BEROENDE PÅ ORDERRADENS STATUS VISAS OLIKA                 
001300*        INFORMATION.                                                     
001400*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001500*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
001600*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
001700*        PROGRAMMET LÄSER      WLORQF (WDQ4)                              
001800*        PROGRAMMET LÄSER              WDE4 WDE6                          
001900*                                                                         
002000* WL014100 PROGRAM IS A REPLICA OF W4050200 PROGRAM                       
002100* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                                 
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSACTION: WL0141T                                             
002500*        REQUEST:     WL0141I1                                            
002600*                                                                         
002700*    OUTDATA.                                                             
002800*        RESPONSE:    WL0141O1                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'WL014100'.            
004300                                                                          
004400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004600 77  KDRC-DISPLAY                PIC Z(5).                                
004700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
005000                                                                          
005100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400 77  WS-IDDISTR                  PIC X(4)  VALUE SPACE.                   
005500 77  WS-IDDISTR-NUM              PIC 9(5)  VALUE ZERO.                    
005600 77  WS-IDKUNDNR                 PIC X(6)  VALUE SPACE.                   
005700 77  WS-IDKUNDNR-NUM             PIC 9(7)  VALUE ZERO.                    
005800 77  WS-IDKUNDRF                 PIC X(7)  VALUE SPACE.                   
005900 77  WS-IDKUNDRF-NUM             PIC 9(7)  VALUE ZERO.                    
006000 77  WS-IDARTNR                  PIC X(8)  VALUE SPACE.                   
006100 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
006200 77  WS-IDKOLLI                  PIC X(5)  VALUE SPACE.                   
006300 77  WS-IDKOLLI-NUM              PIC 9(5)  VALUE ZERO.                    
006400 77  WS-IDPRODNR                 PIC X(7)  VALUE SPACE.                   
006500 77  WS-IDPRODNR-NUM             PIC 9(7)  VALUE ZERO.                    
007000 77  WS-IDKUNDRF-KOLL            PIC X(10) VALUE SPACE.                   
007100 77  WS-IDKUNDRF-CONT            PIC X(10) VALUE SPACE.                   
007200                                                                          
007300 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
007400                                                                          
007500 77  WS-COUNT                    PIC 9(3)   VALUE ZERO.                   
007600                                                                          
007700 77  WS-IDELMT-ERROR             PIC X(16).                               
007800 77  WS-IDMSG-ERROR              PIC X(03).                               
007900 77  WS-IDMSG-INFO               PIC X(03).                               
008000                                                                          
008100 01  W-SPAR-IDKUNDRF.                                                     
008200     03  W-SPAR-IDORDNR7         PIC X(7)  VALUE ZERO.                    
008300     03  FILLER                  PIC X(3)  VALUE SPACE.                   
008400                                                                          
008500 01  IDKUNDRF-WS                 PIC X(10).                               
008600 01  IDKUNDRF5-WS  REDEFINES  IDKUNDRF-WS.                                
008700     03 IDORDNR5-WS              PIC 9(5).                                
008800     03 FILLER                   PIC X(5).                                
008900 01  IDKUNDRF7-WS  REDEFINES  IDKUNDRF-WS.                                
009000     03 IDORDNR7-WS              PIC 9(7).                                
009100     03 FILLER                   PIC X(3).                                
009200 01  FILLER        REDEFINES  IDKUNDRF-WS.                                
009300     03 FILLER                   PIC X(5).                                
009400     03 IDKUNDRF-WS-POS6-7       PIC X(2).                                
009500     03 FILLER                   PIC X(3).                                
009600                                                                          
009700 01  WS-MOD-PRIS.                                                         
009800     03 WS-MOD-PRIS-NUM          PIC Z(8)9   VALUE ZERO.                  
009900     03 FILLER                   PIC X(2)    VALUE ' *'.                  
010000                                                                          
010100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010200     88  NYCKLAR-OK                          VALUE 'J'.                   
010300     88  NYCKLAR-FEL                         VALUE 'N'.                   
010400                                                                          
010500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010600     88  ALLT-OK                             VALUE 'J'.                   
010700                                                                          
010800 77  IDARTNR-SW                  PIC X       VALUE 'N'.                   
010900     88  IDARTNR-IFYLLT                      VALUE 'J'.                   
011000                                                                          
011100 77  IFYLLT-SW                   PIC X       VALUE 'J'.                   
011200     88  IFYLLT-OK                           VALUE 'J'.                   
011300                                                                          
011400 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
011500     88  FIRST-TIME                          VALUE 'J'.                   
011600                                                                          
011700 77  SCROLL-SW                   PIC X       VALUE 'J'.                   
011800     88  SCROLL                              VALUE 'J'.                   
011900                                                                          
012000 77  WDQ4-SW                     PIC X       VALUE 'J'.                   
012100     88  WDQ4-FIRST                          VALUE 'J'.                   
012200                                                                          
012300 77  IDKOLLI-SW                  PIC X       VALUE 'N'.                   
012400     88  IDKOLLI-IFYLLT                      VALUE 'J'.                   
012500                                                                          
012600 77  FLER-KOLLI-SW               PIC X       VALUE 'J'.                   
012700     88  FLER-KOLLI                          VALUE 'J'.                   
012800                                                                          
012900 77  KOLLI-SW                    PIC X       VALUE 'J'.                   
013000     88  KOLLI-FINNS                         VALUE 'J'.                   
013100     88  KOLLI-SAKNAS                        VALUE 'N'.                   
013200                                                                          
013300 77  IDPRODNR-SW                 PIC X       VALUE 'N'.                   
013400     88  IDPRODNR-IFYLLT                     VALUE 'J'.                   
013500                                                                          
013600 77  ARTIKEL-SW                  PIC X       VALUE 'J'.                   
013700     88  ARTIKEL-FINNS                       VALUE 'J'.                   
013800     88  ARTIKEL-SAKNAS                      VALUE 'N'.                   
013900                                                                          
013910 77  BOUNCE-SW                   PIC X       VALUE 'J'.                   
013920     88  BOUNCEORDER                         VALUE 'J'.                   
013940                                                                          
014000*      --- VALID IDDC CODES                                               
014100*                                                                         
014200     EJECT                                                                
014300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
014400 01  GENERAL-SUBPROGRAMS.                                                 
014500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
014800     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
014900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015100     EJECT                                                                
015200 01  SPAR-AREOR.                                                          
015300     03  SPAR-STATUS             PIC X(2)    VALUE SPACE.                 
015400     03  SPAR-IDARTNR            PIC S9(9)   COMP-3 VALUE ZERO.           
015500     03  SPAR-IDKOLLI            PIC S9(5)   COMP-3 VALUE ZERO.           
015600     03  SPAR-IDPRODNR           PIC S9(7)   COMP-3 VALUE ZERO.           
015700     03  SPAR-KDODELSTA          PIC X       VALUE SPACE.                 
015800     03  SPAR-KDVALISO           PIC X(3)    VALUE SPACE.                 
015900*    --- PARAMETERS TO ABEND                                              
016000                                                                          
016100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016400     SKIP3                                                                
016500*01  MESSAGE-CODES.                                                       
016600*    03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
016700     EJECT                                                                
016800*                                                                         
016900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
017000     SKIP3                                                                
017100*01  -COPY WZ01SUB                                                        
017200     EJECT                                                                
017300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
017400     SKIP3                                                                
017500 01  REQU-AREA.                                                           
017600*    03  -COPY WZ01REQU                                                   
017700*    03  -COPY WL0141I1                                                   
017800     EJECT                                                                
017900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
018000     SKIP3                                                                
018100 01  RESP-AREA.                                                           
018200*    03  -COPY WZ01RESP                                                   
018300*    03  -COPY WL0141O1                                                   
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
018600*   -COPY WSECAREA                                                        
018700     SKIP3                                                                
018800*   -COPY W402W001                                                        
019200     SKIP3                                                                
019300 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
019400*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
019500*    ----DISTR-DEALER-PRICE-----                                          
019600     EJECT                                                                
019700 01  MESSAGE-CODES.                                                       
019800     03  INF-CASE-MISSING        PIC X(3)    VALUE '169'.                 
019900     03  INF-PART-MISSING        PIC X(3)    VALUE '202'.                 
020000     03  INF-ORDER-ANNULLED      PIC X(3)    VALUE '197'.                 
020100     03  INF-ORDERLINES-MISSING  PIC X(3)    VALUE '199'.                 
020200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
020300     03  ERR-OBEHORIG            PIC X(3)    VALUE '00A'.                 
020400     03  INF-ORDERINFO-BORTTAGEN PIC X(3)    VALUE '196'.                 
020500     03  ERR-ORDERHEAD-MISSING   PIC X(3)    VALUE '195'.                 
020600     03  INF-ORDER-MISSING       PIC X(3)    VALUE '125'.                 
020700     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
020800     03  TOO-MANY-LINES          PIC X(3)    VALUE '028'.                 
020900     EJECT                                                                
021000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021100*                                                                         
021200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021300     SKIP3                                                                
021400 01  NYCKLAR-TILL-DLI.                                                    
021500*    ---------TILL WDQ201                                                 
021600     03  W-IDORDER-X.                                                     
021700         05  W-OHUV-IDORDER       PIC S9(7)   VALUE ZERO COMP-3.          
021800*    ---------TILL WDQ201 VIA WDQ2C1(SEK. INDX)                           
021900     03  W-WDQ2CSEQ-X.                                                    
022000         05  W-Q2CSEQ-IDDISTR     PIC S9(5)    VALUE ZERO COMP-3.         
022100         05  W-Q2CSEQ-IDKUNDNR    PIC S9(7)    VALUE ZERO COMP-3.         
022200         05  W-Q2CSEQ-IDKUNDRF    PIC  X(10)   VALUE SPACE.               
022300*    ---------TILL WDQ301                                                 
022400     03  W-WDQ301KY-MIN-X.                                                
022500         05  W-ODEL-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
022600         05  W-ODEL-IDDC-MIN      PIC X(2)     VALUE SPACE.               
022700         05  W-ODEL-IDPRODNR-MIN  PIC S9(7)    VALUE ZERO COMP-3.         
022800         05  W-ODEL-IDPLKLST-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
022900     03  W-WDQ301KY-MAX-X.                                                
023000         05  W-ODEL-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
023100         05  W-ODEL-IDDC-MAX      PIC X(2)     VALUE SPACE.               
023200         05  W-ODEL-IDPRODNR-MAX  PIC S9(7)    VALUE ZERO COMP-3.         
023300         05  W-ODEL-IDPLKLST-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
023400*    ---------TILL WDQ401                                                 
023500     03  W-WDQ401KY-MIN-X.                                                
023600         05  W-ORAD-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
023700         05  W-ORAD-IDDC-MIN      PIC X(2)     VALUE SPACE.               
023800         05  W-ORAD-ADLAGOMR-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
023900         05  W-ORAD-ADGANG-MIN    PIC S9(3)    VALUE ZERO COMP-3.         
024000         05  W-ORAD-ADPLATS-MIN   PIC S9(5)    VALUE ZERO COMP-3.         
024100         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
024200         05  W-ORAD-IDLOPNR-MIN   PIC S9(3)    VALUE ZERO COMP-3.         
024300     03  W-ORAD-IDARTNR-X.                                                
024400         05  W-ORAD-IDARTNR       PIC S9(9)    VALUE ZERO COMP-3.         
024500     03  W-WDQ401KY-MAX-X.                                                
024600         05  W-ORAD-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
024700         05  W-ORAD-IDDC-MAX      PIC X(2)     VALUE SPACE.               
024800         05  W-ORAD-ADLAGOMR-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
024900         05  W-ORAD-ADGANG-MAX    PIC S9(3)    VALUE ZERO COMP-3.         
025000         05  W-ORAD-ADPLATS-MAX   PIC S9(5)    VALUE ZERO COMP-3.         
025100         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
025200         05  W-ORAD-IDLOPNR-MAX   PIC S9(3)    VALUE ZERO COMP-3.         
025300     03  W-WDQ401KY-MIN-MIN-X.                                            
025400         05  W-ORAD-IDORDER-MIN-MIN  PIC S9(7)  VALUE ZERO COMP-3.        
025500         05  W-ORAD-IDDC-MIN-MIN     PIC X(2)   VALUE SPACE.              
025600         05  W-ORAD-ADLAGOMR-MIN-MIN PIC S9(3)  VALUE ZERO COMP-3.        
025700         05  W-ORAD-ADGANG-MIN-MIN   PIC S9(3)  VALUE ZERO COMP-3.        
025800         05  W-ORAD-ADPLATS-MIN-MIN  PIC S9(5)  VALUE ZERO COMP-3.        
025900         05  W-ORAD-IDARTNR-MIN-MIN  PIC S9(9)  VALUE ZERO COMP-3.        
026000         05  W-ORAD-IDLOPNR-MIN-MIN  PIC S9(3)  VALUE ZERO COMP-3.        
026100     03  W-WDQ401KY-MAX-MAX-X.                                            
026200         05  W-ORAD-IDORDER-MAX-MAX  PIC S9(7)  VALUE ZERO COMP-3.        
026300         05  W-ORAD-IDDC-MAX-MAX     PIC X(2)   VALUE SPACE.              
026400         05  W-ORAD-ADLAGOMR-MAX-MAX PIC S9(3)  VALUE ZERO COMP-3.        
026500         05  W-ORAD-ADGANG-MAX-MAX   PIC S9(3)  VALUE ZERO COMP-3.        
026600         05  W-ORAD-ADPLATS-MAX-MAX  PIC S9(5)  VALUE ZERO COMP-3.        
026700         05  W-ORAD-IDARTNR-MAX-MAX  PIC S9(9)  VALUE ZERO COMP-3.        
026800         05  W-ORAD-IDLOPNR-MAX-MAX  PIC S9(3)  VALUE ZERO COMP-3.        
026900*    ---------TILL WDE401                                                 
027000     03  W-KORD-WDE4KEY-X.                                                
027100         05  W-KORD-IDDISTR          PIC S9(5)  VALUE ZERO COMP-3.        
027200         05  W-KORD-IDKUNDNR         PIC S9(7)  VALUE ZERO COMP-3.        
027300         05  W-KORD-IDKUNDRF         PIC X(10)  VALUE SPACE.              
027400         05  W-KORD-IDPRODNR         PIC S9(7)  VALUE ZERO COMP-3.        
027500         05  W-KORD-IDPLKLST         PIC S9(3)  VALUE ZERO COMP-3.        
027600     03  W-KORD-WDE4KEY-MIN-X.                                            
027700         05  W-KORD-IDDISTR-MIN      PIC S9(5)  VALUE ZERO COMP-3.        
027800         05  W-KORD-IDKUNDNR-MIN     PIC S9(7)  VALUE ZERO COMP-3.        
027900         05  W-KORD-IDKUNDRF-MIN     PIC X(10)  VALUE SPACE.              
028000         05  FILLER                  PIC S9(7)  VALUE ZERO COMP-3.        
028100         05  W-KORD-IDPLKLST-MIN     PIC S9(3)  VALUE ZERO COMP-3.        
028200     03  W-KORD-IDPRODNR-MIN-X.                                           
028300         05  W-KORD-IDPRODNR-MIN     PIC S9(7)  VALUE ZERO COMP-3.        
028400*    ---------TILL WDE411                                                 
028500     03  W-IDPURAD-X.                                                     
028600         05  W-ORAD-IDPURAD          PIC S9(5)  VALUE ZERO COMP-3.        
028700*    ---------TILL WDE411 VIA B-KEY                                       
028800     03  W-WDE4BSEQ-MIN-X.                                                
028900         05  W-SEK-IDPRODNR-MIN     PIC S9(7)  VALUE ZERO COMP-3.         
029000         05  W-SEK-IDPURAD-MIN      PIC S9(5)  VALUE ZERO COMP-3.         
029100     03  W-WDE4BSEQ-MAX-X.                                                
029200         05  W-SEK-IDPRODNR-MAX     PIC S9(7)  VALUE ZERO COMP-3.         
029300         05  W-SEK-IDPURAD-MAX      PIC S9(5)  VALUE ZERO COMP-3.         
029400*    ---------TILL WDE401 VIA A-KEY                                       
029500     03  W-WDE4ASEQ-X.                                                    
029600         05  W-SEQA-IDDISTR          PIC S9(5)  VALUE ZERO COMP-3.        
029700         05  W-SEQA-IDKUNDNR         PIC S9(7)  VALUE ZERO COMP-3.        
029800         05  W-SEQA-IDKUNDRF         PIC X(10)  VALUE SPACE.              
029900*    ---------TILL WDE421                                                 
030000     03  W-WDE4KEY-X.                                                     
030100         05  W-KKOLLI-IDPRODNR      PIC S9(7)  VALUE ZERO COMP-3.         
030200         05  W-KKOLLI-IDKOLLI       PIC S9(5)  VALUE ZERO COMP-3.         
030300                                                                          
030400     03  W-IDPRODNR-X.                                                    
030500         05  W-IDPRODNR-WDE6        PIC S9(7)  VALUE ZERO COMP-3.         
030600                                                                          
030700     03  W-IDKOLLI-X.                                                     
030800         05  W-IDKOLLI-WDE6         PIC S9(5)  VALUE ZERO COMP-3.         
030900                                                                          
033100*    --- STATUS-KOD FRÅN IMS                                              
033200 01  STATUS-WS                   PIC XX.                                  
033300     88  SEGMENT-FINNS                       VALUE '  '.                  
033400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033600     88  BASEN-SLUT                          VALUE 'GB'.                  
033700     SKIP2                                                                
033800 01  GODK-STATUSKODER.                                                    
033900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034000     SKIP3                                                                
034100 01  SSA1                        PIC X(160).                              
034200 01  SSA2                        PIC X(64).                               
034300     EJECT                                                                
034400*    --- IMS FUNKTIONSKODER                                               
034500*01  -COPY W0003                                                          
034600     EJECT                                                                
034700******************************************************************        
034800*                                                                *        
034900*        ARBETS-AREOR TILL IO-AREORNA                                     
035000*                                                                *        
035100*        DLI INPUT-OUTPUT AREA                                   *        
035200*                                                                *        
035300******************************************************************        
035400*    ---  DLI INPUT-OUTPUT                                                
035500*    ---  DLI-IO-AREA                                                     
035600*                                                                         
035700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OHUV'.             
035800 01  DLI-IO-AREA-OHUV.                                                    
035900*    03  WLORQI01   -COPY WDQ201                                          
036000     EJECT                                                                
036100                                                                          
036200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARB '.             
036300 01  DLI-IO-AREA-ARB.                                                     
036400*    03  WLORQI01   -COPY WDQ212                                          
036500     EJECT                                                                
036600                                                                          
036700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ODEL'.             
036800 01  DLI-IO-AREA-ODEL.                                                    
036900*    03  WLORQA01   -COPY WDQ301                                          
037000     EJECT                                                                
037100                                                                          
037200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORAD'.             
037300 01  DLI-IO-AREA-ORAD.                                                    
037400*    03  WLORQF01   -COPY WDQ401     -PRE QF01-                           
037500     EJECT                                                                
037600                                                                          
037700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-KORD'.             
037800 01  DLI-IO-AREA-KORD.                                                    
037900*    03  WDE401     -COPY WDE401                                          
038000     EJECT                                                                
038100                                                                          
038200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E411'.             
038300 01  DLI-IO-AREA-WDE411.                                                  
038400*    03  WDE411     -COPY WDE411     -PRE 411-                            
038500     EJECT                                                                
038600                                                                          
038700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE421'.           
038800 01  DLI-IO-AREA-WDE421.                                                  
038900*    03  WDE421-1   -COPY WDE421                                          
039000     EJECT                                                                
039010 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE611'.           
039020 01  DLI-IOAREA-WDE601.                                                   
039030*    03  WDE601   -COPY WDE601                                            
039100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE601'.           
039200 01  DLI-IOAREA-WDE611.                                                   
039300*    03  WDE611   -COPY WDE611                                            
039400     EJECT                                                                
040700 LINKAGE SECTION.                                                         
040800 01  MSG-PCB                     PIC X.                                   
040900     EJECT                                                                
041000*01  -COPY W0008      -PRE ORQI-                                          
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01  -COPY W0008      -PRE ORQA-                                          
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01  -COPY W0008      -PRE WDE4A-                                         
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900*01  -COPY W0008      -PRE WDE4B-                                         
042000     05  FILLER                  PIC X.                                   
042100     EJECT                                                                
042200*01  -COPY W0008      -PRE WDE6-                                          
042300     05  FILLER                  PIC X.                                   
042400     EJECT                                                                
042500*01  -COPY W0008      -PRE ORQF-                                          
042600     05  FILLER                  PIC X.                                   
042700     EJECT                                                                
043700 PROCEDURE DIVISION  USING MSG-PCB  ORQI-PCB  ORQA-PCB                    
043800                           WDE4A-PCB WDE4B-PCB                            
043900                           WDE6-PCB ORQF-PCB.                             
044100 MAIN SECTION.                                                            
044200     ENTRY 'DLITCBL' USING MSG-PCB  ORQI-PCB  ORQA-PCB                    
044300                           WDE4A-PCB WDE4B-PCB                            
044400                           WDE6-PCB ORQF-PCB.                             
044600                                                                          
044700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
044800     IF SUB-KDRC = 0                                                      
044900      IF  REQU-KDPGMACT  = 'S'                                            
045000       PERFORM A-INIT                                                     
045100       PERFORM C-KOLLA-NYCKLAR                                            
045200       IF NYCKLAR-OK                                                      
045300         PERFORM H-KOLLA-BEHOERIGHET                                      
045400         IF ALLT-OK                                                       
045500           PERFORM G-LAES-VISA-INFO                                       
045600         ELSE                                                             
045700           MOVE ERR-OBEHORIG TO RESP-IDMSG-ERROR                          
045800         END-IF                                                           
045900       END-IF                                                             
046000      ELSE                                                                
046100       MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                              
046200      END-IF                                                              
046300      MOVE WS-COUNT            TO RESP-KVRADER                            
046400                                                                          
046500       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
046600       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
046700       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
046800       IF WS-IDMSG-ERROR NOT = SPACE                                      
046900           MOVE ALL '+' TO RESP-AREA                                      
047000           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
047100           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
047200           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
047300           MOVE 001              TO RESP-IDMSGVER                         
047400           MOVE ZERO             TO RESP-KVRADER                          
047500       END-IF                                                             
047600                                                                          
047700      PERFORM S02-RETURN-RESPONSE                                         
047800     END-IF                                                               
047900                                                                          
048000     MOVE ZERO TO RETURN-CODE                                             
048100     GOBACK                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 A-INIT SECTION.                                                          
048500                                                                          
048600     MOVE JA TO ALLT-SW                                                   
049000     MOVE ALL '+'            TO RESP-AREA                                 
049100     MOVE SPACE              TO RESP-IDMSG-ERROR                          
049200                                RESP-IDMSG-INFO                           
049300                                RESP-IDELMT-ERROR                         
049400     MOVE 001                TO RESP-IDMSGVER                             
049500     MOVE ZERO               TO  RESP-KVRADER                             
049600     .                                                                    
049700     EJECT                                                                
049800 C-KOLLA-NYCKLAR SECTION.                                                 
049900                                                                          
050000     MOVE JA TO NYCKLAR-SW                                                
050100     MOVE LOW-VALUE         TO W-IDORDER-X                                
050200                             W-WDQ2CSEQ-X                                 
050300                             W-WDQ301KY-MIN-X                             
050400                             W-WDQ401KY-MIN-X                             
050500                             W-WDQ401KY-MIN-MIN-X                         
050600                             W-WDE4ASEQ-X                                 
050700                             W-WDE4BSEQ-MIN-X                             
050800                             W-WDE4KEY-X                                  
050900                             W-IDPURAD-X                                  
051000                             W-KORD-WDE4KEY-X                             
051100                             W-KORD-WDE4KEY-MIN-X                         
051200                                                                          
051300     MOVE HIGH-VALUE TO      W-WDQ301KY-MAX-X                             
051400                             W-WDE4BSEQ-MAX-X                             
051500                             W-WDQ401KY-MAX-X                             
051600                             W-WDQ401KY-MAX-MAX-X                         
051700                                                                          
051800     PERFORM CA-KOLLA-DISTRIKT                                            
051900     PERFORM CB-KOLLA-KUNDNUMMER                                          
052000     PERFORM CC-KOLLA-ORDERNUMMER                                         
052100                                                                          
052200     IF IFYLLT-OK                                                         
052300       PERFORM CD-KOLLA-ARTIKELNUMMER                                     
052400       PERFORM CE-KOLLA-KOLLINUMMER                                       
052500       PERFORM CF-KOLLA-PRODNUMMER                                        
052600     END-IF                                                               
052700                                                                          
052800     IF REQU-IDARTNR-KEY   NUMERIC OR                                     
052900        REQU-IDARTNR-KEY = ALL '+'                                        
053000        CONTINUE                                                          
053100     ELSE                                                                 
053200       MOVE NEJ        TO NYCKLAR-SW                                      
053300     END-IF                                                               
053400                                                                          
053500     IF REQU-IDKOLLI-KEY   NUMERIC OR                                     
053600        REQU-IDKOLLI-KEY = ALL '+'                                        
053700        CONTINUE                                                          
053800     ELSE                                                                 
053900       MOVE NEJ        TO NYCKLAR-SW                                      
054000     END-IF                                                               
054100                                                                          
054200     IF REQU-IDPRODNR-KEY  NUMERIC OR                                     
054300        REQU-IDPRODNR-KEY = ALL '+'                                       
054400        CONTINUE                                                          
054500     ELSE                                                                 
054600       MOVE NEJ        TO NYCKLAR-SW                                      
054700     END-IF                                                               
054800                                                                          
054900     IF NYCKLAR-FEL                                                       
055000       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
055100     END-IF                                                               
055200                                                                          
055300     PERFORM CG-KOLLA-IDDC                                                
055400     .                                                                    
055500     EJECT                                                                
055600                                                                          
055700 CA-KOLLA-DISTRIKT SECTION.                                               
055800                                                                          
055900     IF REQU-IDDISTR-KEY = ALL '+' AND                                    
056000       REQU-IDDISTR-KEY = ALL SPACE                                       
056100       MOVE NEJ TO IFYLLT-SW                                              
056200     END-IF                                                               
056300                                                                          
056400     IF REQU-IDDISTR-KEY NUMERIC                                          
056500       MOVE REQU-IDDISTR-KEY TO WS-IDDISTR-NUM                            
056600       MOVE WS-IDDISTR-NUM   TO W-Q2CSEQ-IDDISTR                          
056700                                W-SEQA-IDDISTR                            
056800                                W-KORD-IDDISTR                            
056900                                W-KORD-IDDISTR-MIN                        
057300     ELSE                                                                 
057400       MOVE NEJ TO NYCKLAR-SW                                             
057500     END-IF                                                               
057600                                                                          
057700      IF REQU-IDDISTR-KEY      NUMERIC                                    
057800       MOVE REQU-IDDISTR-KEY    TO RESP-IDDISTR-KEY                       
057900       INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE           
058000      END-IF                                                              
058100                                                                          
058110     IF REQU-IDDISTR-KEY NUMERIC AND RESP-IDDISTR-KEY > ZERO              
058200        MOVE REQU-IDDISTR-KEY TO TEST-IDDISTR                             
058300        IF DIST79-DEALER-PRICE                                            
058400            MOVE 'DEALERPRICE'   TO RESP-TEDDI                            
058500        ELSE                                                              
058900            MOVE SPACES          TO RESP-TEDDI                            
059100        END-IF                                                            
059110     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400                                                                          
059500 CB-KOLLA-KUNDNUMMER SECTION.                                             
059600                                                                          
059700     IF REQU-IDKUNDNR-KEY NUMERIC                                         
059800       MOVE REQU-IDKUNDNR-KEY  TO WS-IDKUNDNR-NUM                         
059900       MOVE WS-IDKUNDNR-NUM    TO W-Q2CSEQ-IDKUNDNR                       
060000                                  W-SEQA-IDKUNDNR                         
060100                                  W-KORD-IDKUNDNR                         
060200                                  W-KORD-IDKUNDNR-MIN                     
060400     ELSE                                                                 
060500       MOVE NEJ TO NYCKLAR-SW                                             
060600     END-IF                                                               
060700                                                                          
060800     IF REQU-IDKUNDNR-KEY NUMERIC                                         
060900      MOVE REQU-IDKUNDNR-KEY  TO RESP-IDKUNDNR-KEY                        
061000      INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE           
061100     END-IF                                                               
061200     .                                                                    
061300     EJECT                                                                
061400                                                                          
061500 CC-KOLLA-ORDERNUMMER SECTION.                                            
061600                                                                          
061700     IF REQU-IDKUNDRF-KEY = ALL '+' AND                                   
061800       REQU-IDKUNDRF-KEY = ALL SPACE                                      
061900       MOVE NEJ TO IFYLLT-SW                                              
062000     END-IF                                                               
062100                                                                          
062200     IF REQU-IDKUNDRF-KEY(1:7) NUMERIC                                    
062300       MOVE REQU-IDKUNDRF-KEY(1:7) TO WS-IDKUNDRF-NUM                     
062400                                      WS-IDKUNDRF-KOLL                    
062500       MOVE REQU-IDKUNDRF-KEY(3:5) TO WS-IDKUNDRF-CONT                    
062600       MOVE WS-IDKUNDRF-NUM TO W-Q2CSEQ-IDKUNDRF                          
062700       MOVE WS-IDKUNDRF-NUM (3:5) TO W-SEQA-IDKUNDRF                      
062800                                     W-KORD-IDKUNDRF                      
062900                                     W-KORD-IDKUNDRF-MIN                  
063000     ELSE                                                                 
063100       MOVE NEJ TO NYCKLAR-SW                                             
063200     END-IF                                                               
063300                                                                          
063400     IF REQU-IDKUNDRF-KEY  NUMERIC                                        
063500     MOVE REQU-IDKUNDRF-KEY(1:7) TO RESP-IDKUNDRF-KEY                     
063600     INSPECT  RESP-IDKUNDRF-KEY REPLACING LEADING ZERO BY SPACE           
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 CD-KOLLA-ARTIKELNUMMER SECTION.                                          
064100                                                                          
064200       MOVE REQU-IDARTNR-KEY TO WS-IDARTNR                                
064300       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
064400                                                                          
064500     IF WS-IDARTNR NUMERIC                                                
064600       IF WS-IDARTNR = ZERO                                               
064700         MOVE NEJ TO IDARTNR-SW                                           
064800       ELSE                                                               
064900         MOVE WS-IDARTNR     TO WS-IDARTNR-NUM                            
065000         MOVE WS-IDARTNR-NUM TO W-ORAD-IDARTNR                            
065100                                SPAR-IDARTNR                              
065200         MOVE JA TO IDARTNR-SW                                            
065300       END-IF                                                             
065400     END-IF                                                               
065500                                                                          
065600     IF WS-IDARTNR NUMERIC                                                
065700       MOVE WS-IDARTNR     TO RESP-IDARTNR-KEY                            
065800       INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE           
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 CE-KOLLA-KOLLINUMMER SECTION.                                            
066300                                                                          
066400       MOVE REQU-IDKOLLI-KEY TO WS-IDKOLLI                                
066500       INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
066600                                                                          
066700     IF WS-IDKOLLI NUMERIC                                                
066800       IF WS-IDKOLLI = ZERO                                               
066900         MOVE NEJ TO IDKOLLI-SW                                           
067000       ELSE                                                               
067100         MOVE WS-IDKOLLI TO WS-IDKOLLI-NUM                                
067200         MOVE WS-IDKOLLI-NUM TO SPAR-IDKOLLI                              
067300                                W-KKOLLI-IDKOLLI                          
067400         MOVE JA TO IDKOLLI-SW                                            
067500       END-IF                                                             
067600     END-IF                                                               
067700                                                                          
067800     IF WS-IDKOLLI  NUMERIC                                               
067900       MOVE WS-IDKOLLI     TO RESP-IDKOLLI-KEY                            
068000       INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE           
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400 CF-KOLLA-PRODNUMMER SECTION.                                             
068500                                                                          
068600       MOVE REQU-IDPRODNR-KEY TO WS-IDPRODNR                              
068700       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
068800                                                                          
068900     IF WS-IDPRODNR NUMERIC                                               
069000       IF WS-IDPRODNR = ZERO                                              
069100         MOVE NEJ TO IDPRODNR-SW                                          
069200       ELSE                                                               
069300         MOVE WS-IDPRODNR TO WS-IDPRODNR-NUM                              
069400         MOVE WS-IDPRODNR-NUM TO W-ODEL-IDPRODNR-MIN                      
069500                                 W-ODEL-IDPRODNR-MAX                      
069600                                 W-SEK-IDPRODNR-MIN                       
069700                                 W-SEK-IDPRODNR-MAX                       
069800         MOVE JA TO IDPRODNR-SW                                           
069900       END-IF                                                             
070000     END-IF                                                               
070100                                                                          
070200     IF WS-IDPRODNR NUMERIC                                               
070300       MOVE WS-IDPRODNR    TO RESP-IDPRODNR-KEY                           
070400       INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE          
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800                                                                          
070900 CG-KOLLA-IDDC SECTION.                                                   
071000                                                                          
071100     MOVE REQU-IDDC-KEY TO W-ORAD-IDDC-MIN                                
071200                           W-ORAD-IDDC-MAX                                
071300                           W-ORAD-IDDC-MIN-MIN                            
071400                           W-ORAD-IDDC-MAX-MAX                            
071500                           W-ODEL-IDDC-MIN                                
071600                           W-ODEL-IDDC-MAX                                
071700                           RESP-IDDC-KEY                                  
071800     .                                                                    
071900     EJECT                                                                
072000 G-LAES-VISA-INFO SECTION.                                                
072100                                                                          
072200     IF IDKOLLI-IFYLLT                                                    
072300       PERFORM GA-REDIGERA-WDE6                                           
072400     ELSE                                                                 
072500       PERFORM GB-LAES-WDQ2                                               
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 GA-REDIGERA-WDE6 SECTION.                                                
073000                                                                          
073100     MOVE +1  TO INDX                                                     
073200     MOVE NEJ TO KOLLI-SW                                                 
073300                 ARTIKEL-SW                                               
073400     MOVE JA  TO FIRST-TIME-SW                                            
073500     PERFORM IMS-GU-WDE401-ASEQ                                           
073600     IF SEGMENT-FINNS                                                     
073700       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
073800         INDX > MAX-INDX                                                  
073900         PERFORM GAB-REDIGERA-BILDEN                                      
074000       END-PERFORM                                                        
074100       IF KOLLI-SAKNAS                                                    
074200         MOVE INF-CASE-MISSING TO RESP-IDMSG-ERROR                        
074300       END-IF                                                             
074400       IF IDARTNR-IFYLLT                                                  
074500         IF ARTIKEL-SAKNAS                                                
074600           MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                     
074700           MOVE '025'            TO RESP-IDMSG-ERROR                      
074800         END-IF                                                           
074900       END-IF                                                             
075000     ELSE                                                                 
075100       MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR                        
075200       MOVE '025'             TO RESP-IDMSG-ERROR                         
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600 GAB-REDIGERA-BILDEN SECTION.                                             
075700                                                                          
075800     IF KORD-IDDC         = REQU-IDDC-KEY  AND                            
075900        KORD-IDPRODNR NOT = SPAR-IDPRODNR                                 
076000       MOVE KORD-IDPRODNR TO W-KORD-IDPRODNR                              
076100                             W-KORD-IDPRODNR-MIN                          
076200                             W-SEK-IDPRODNR-MIN                           
076300                             W-SEK-IDPRODNR-MAX                           
076400                             SPAR-IDPRODNR                                
076500                                                                          
076600       PERFORM IMS-GU-WDE411-BSEQ                                         
076700       IF SEGMENT-FINNS                                                   
076800         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
076900           INDX > MAX-INDX                                                
077000           IF FIRST-TIME                                                  
077100            MOVE 411-ORAD-IDPRODNR TO W-KKOLLI-IDPRODNR                   
077200            MOVE NEJ TO FIRST-TIME-SW                                     
077300           END-IF                                                         
077400           PERFORM GABA-LAES-KOLLI-SEGMENT                                
077500         END-PERFORM                                                      
077600       END-IF                                                             
077700     END-IF                                                               
077800     PERFORM IMS-GN-WDE401-ASEQ                                           
077900     .                                                                    
078000     EJECT                                                                
078100 GABA-LAES-KOLLI-SEGMENT SECTION.                                         
078200     IF IDARTNR-IFYLLT                                                    
078300       IF 411-ORAD-IDARTNR = SPAR-IDARTNR                                 
078400         MOVE JA TO ARTIKEL-SW                                            
078500         PERFORM GABAA-FLYTTA-INFO-TILL-MOD                               
078600         PERFORM IMS-GN-WDE411-BSEQ                                       
078700       ELSE                                                               
078800         PERFORM IMS-GN-WDE411-BSEQ                                       
078900       END-IF                                                             
079000     ELSE                                                                 
079100       PERFORM GABAA-FLYTTA-INFO-TILL-MOD                                 
079200       PERFORM IMS-GN-WDE411-BSEQ                                         
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600 GABAA-FLYTTA-INFO-TILL-MOD SECTION.                                      
079700                                                                          
079800     MOVE 411-ORAD-IDPURAD  TO W-ORAD-IDPURAD                             
079900     MOVE 411-ORAD-IDPRODNR TO W-KKOLLI-IDPRODNR                          
080000     PERFORM IMS-GNP-WDE421                                               
080100     IF SEGMENT-FINNS                                                     
080200       IF INDX = +1                                                       
080300         MOVE KKOLLI-IDKOLLI   TO W-IDKOLLI-WDE6                          
080400         MOVE W-KKOLLI-IDPRODNR  TO W-IDPRODNR-WDE6                       
080500       PERFORM IMS-GU-WDE611                                              
080600       END-IF                                                             
080700       MOVE JA TO KOLLI-SW                                                
080800       PERFORM S01-FLYTTA-WDE411-TILL-MOD                                 
080900       PERFORM S05-FLYTTA-WDE611-TILL-MOD                                 
081000       ADD +1 TO INDX                                                     
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400 GB-LAES-WDQ2 SECTION.                                                    
081500                                                                          
081600     PERFORM IMS-GU-ORQI-ORQI01-M-Q2CSEQ                                  
081700     IF SEGMENT-FINNS                                                     
081800       IF OHUV-KDTPOTYP = ZERO                                            
081900         MOVE OHUV-IDORDER TO W-OHUV-IDORDER                              
082000                              W-ORAD-IDORDER-MIN                          
082100                              W-ORAD-IDORDER-MAX                          
082200                              W-ORAD-IDORDER-MIN-MIN                      
082300                              W-ORAD-IDORDER-MAX-MAX                      
082400                              W-ODEL-IDORDER-MIN                          
082500                              W-ODEL-IDORDER-MAX                          
082600         IF OHUV-FLKLAR = 'N'                                             
082700           MOVE 'E' TO SPAR-STATUS                                        
082800         ELSE                                                             
082900           MOVE 'R' TO SPAR-STATUS                                        
083000         END-IF                                                           
083100         IF OHUV-FLBORT = 'J'                                             
083200           PERFORM IMS-GNP-ORQI-ORQI12                                    
083300           IF SEGMENT-SAKNAS                                              
083400             MOVE '025'                   TO RESP-IDMSG-ERROR             
083500             MOVE 'IDORDNR'               TO RESP-IDELMT-ERROR            
083600           ELSE                                                           
083700             MOVE INF-ORDER-ANNULLED TO RESP-IDMSG-ERROR                  
083800           END-IF                                                         
083900         ELSE                                                             
084000           IF SEGMENT-FINNS                                               
084100             PERFORM GBA-LAES-WDQ3                                        
084200           END-IF                                                         
084300         END-IF                                                           
084400       ELSE                                                               
084500         MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR                      
084600         MOVE '025'             TO RESP-IDMSG-ERROR                       
084700       END-IF                                                             
084800     ELSE                                                                 
084900       MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR                        
085000       MOVE '025'             TO RESP-IDMSG-ERROR                         
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400 GBA-LAES-WDQ3 SECTION.                                                   
085500                                                                          
085600     MOVE JA TO WDQ4-SW                                                   
085700     MOVE NEJ TO ARTIKEL-SW                                               
085710     MOVE NEJ TO BOUNCE-SW                                                
085800     MOVE +1 TO INDX                                                      
085801     MOVE LOW-VALUE          TO W-ODEL-IDDC-MIN                           
085802     MOVE HIGH-VALUE         TO W-ODEL-IDDC-MAX                           
085900     PERFORM IMS-GU-ORQA-ORQA01                                           
086000     IF SEGMENT-FINNS                                                     
086100       MOVE LOW-VALUE         TO W-WDQ301KY-MIN-X                         
086200       MOVE HIGH-VALUE        TO W-WDQ301KY-MAX-X                         
086300       MOVE OHUV-IDORDER      TO W-ODEL-IDORDER-MIN                       
086400                                 W-ODEL-IDORDER-MAX                       
086620       IF REQU-IDDC-KEY = ODEL-IDDC-EXP                                   
086621*        *BOUNCE VOR ORDER. ORDERLINE IS FROM DC 11.                      
086630         MOVE ODEL-IDPRODNR   TO W-IDPRODNR-WDE6                          
086640         PERFORM IMS-GU-WDE601                                            
086650         IF SEGMENT-FINNS AND VORD-KVKOLLI-FAKT > 0                       
086660            MOVE ODEL-IDDC    TO W-ODEL-IDDC-MIN                          
086670                                 W-ODEL-IDDC-MAX                          
086690         ELSE                                                             
086691            MOVE REQU-IDDC-KEY  TO W-ODEL-IDDC-MIN                        
086692                                   W-ODEL-IDDC-MAX                        
086693         END-IF                                                           
086694       ELSE                                                               
086695         MOVE REQU-IDDC-KEY   TO W-ODEL-IDDC-MIN                          
086696                                 W-ODEL-IDDC-MAX                          
086697       END-IF                                                             
086700       PERFORM IMS-GU-ORQA-ORQA01                                         
086800       IF SEGMENT-FINNS                                                   
086810         IF ODEL-IDDC-EXP NOT = SPACE                                     
086820           MOVE JA            TO BOUNCE-SW                                
086830         END-IF                                                           
086900         MOVE NEJ TO FIRST-TIME-SW                                        
087000         IF DIST79-DEALER-PRICE                                           
087100           MOVE ODEL-KDVALISO    TO SPAR-KDVALISO                         
087200         END-IF                                                           
087300         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
087400           INDX > MAX-INDX                                                
087500           PERFORM GBAD-LAES-VIDARE                                       
087600         END-PERFORM                                                      
087700       END-IF                                                             
087800     ELSE                                                                 
087900       MOVE +1 TO INDX                                                    
088000       PERFORM S07-LAES-WDQ401                                            
088100     END-IF                                                               
088200     .                                                                    
088300     EJECT                                                                
088400 GBAD-LAES-VIDARE SECTION.                                                
088500     IF ODEL-KDODELSTA = 'R'                                              
088600       IF WDQ4-FIRST                                                      
088700         PERFORM S07-LAES-WDQ401                                          
088800         MOVE NEJ TO WDQ4-SW                                              
088900       END-IF                                                             
089000     ELSE                                                                 
089100       IF ODEL-IDPRODNR NOT = SPAR-IDPRODNR                               
089200         MOVE ODEL-IDPRODNR TO SPAR-IDPRODNR                              
089300         MOVE ODEL-IDPRODNR TO W-SEK-IDPRODNR-MIN                         
089400                               W-SEK-IDPRODNR-MAX                         
089500         PERFORM GBADA-REDIGERA-WDE4                                      
089600       END-IF                                                             
089700     END-IF                                                               
089800     PERFORM IMS-GN-ORQA-ORQA01                                           
089900     .                                                                    
090000     EJECT                                                                
090100 GBADA-REDIGERA-WDE4 SECTION.                                             
090200     PERFORM IMS-GU-WDE411-BSEQ                                           
090300     IF SEGMENT-FINNS                                                     
090400       MOVE JA TO FIRST-TIME-SW                                           
090500       PERFORM GBADAA-REDIGERA-BILDEN                                     
090600     ELSE                                                                 
090700       IF WDQ4-FIRST                                                      
090800         PERFORM S07-LAES-WDQ401                                          
090900         MOVE NEJ TO WDQ4-SW                                              
091000       END-IF                                                             
091100     END-IF                                                               
091200     .                                                                    
091300     EJECT                                                                
091400 GBADAA-REDIGERA-BILDEN SECTION.                                          
091500     MOVE NEJ TO FLER-KOLLI-SW                                            
091600     MOVE JA TO FIRST-TIME-SW                                             
091700     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                      
091800       IF IDARTNR-IFYLLT                                                  
091900         IF 411-ORAD-IDARTNR = SPAR-IDARTNR                               
092000           MOVE JA TO ARTIKEL-SW                                          
092100           PERFORM GBADAAA-FLYTTA-TILL-MOD                                
092200         ELSE                                                             
092300           PERFORM IMS-GN-WDE411-BSEQ                                     
092400           IF SEGMENT-SAKNAS                                              
092500              MOVE LOW-VALUE TO W-WDE4BSEQ-MIN-X                          
092600              MOVE HIGH-VALUE TO W-WDE4BSEQ-MAX-X                         
092700           END-IF                                                         
092800         END-IF                                                           
092900       ELSE                                                               
093000         PERFORM GBADAAA-FLYTTA-TILL-MOD                                  
093100       END-IF                                                             
093200     END-PERFORM                                                          
093300     IF IDARTNR-IFYLLT                                                    
093400       IF ARTIKEL-SAKNAS                                                  
093500         MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                       
093600         MOVE '025'            TO RESP-IDMSG-ERROR                        
093700       END-IF                                                             
093800     END-IF                                                               
093900     .                                                                    
094000     EJECT                                                                
094100 GBADAAA-FLYTTA-TILL-MOD SECTION.                                         
094200     IF FIRST-TIME                                                        
094300       PERFORM S01-FLYTTA-WDE411-TILL-MOD                                 
094400       MOVE NEJ TO FIRST-TIME-SW                                          
094500     END-IF                                                               
094600     MOVE 411-ORAD-IDPRODNR TO W-KKOLLI-IDPRODNR                          
094700                                                                          
094800       PERFORM IMS-GNP-WDE421-BKEY                                        
094900                                                                          
095000     IF SEGMENT-FINNS                                                     
095100       IF INDX = +1                                                       
095200         CONTINUE                                                         
095300       END-IF                                                             
095400                                                                          
095500       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
095600         MOVE KKOLLI-IDKOLLI   TO W-IDKOLLI-WDE6                          
095700         MOVE W-KKOLLI-IDPRODNR  TO W-IDPRODNR-WDE6                       
095800         PERFORM IMS-GU-WDE611                                            
095900         MOVE 411-ORAD-IDARTNR TO RESP-IDARTNR(INDX)                      
096000                                                                          
096100         IF 411-ORAD-KDFARLIG = +4 OR 411-ORAD-KDFARLIG = +7              
096200           MOVE 'J'             TO RESP-KDFARLIG(INDX)                    
096300         ELSE                                                             
096400           MOVE SPACE           TO RESP-KDFARLIG(INDX)                    
096500         END-IF                                                           
096600                                                                          
096700         PERFORM S05-FLYTTA-WDE611-TILL-MOD                               
096800         PERFORM IMS-GNP-WDE421-BKEY                                      
096900         IF SEGMENT-FINNS                                                 
097000           ADD +1 TO INDX                                                 
097100           ADD +1 TO  WS-COUNT                                            
097200         END-IF                                                           
097300                                                                          
097400         IF WS-COUNT < 501                                                
097500            CONTINUE                                                      
097600         ELSE                                                             
097700           MOVE TOO-MANY-LINES    TO RESP-IDMSG-ERROR                     
097800         END-IF                                                           
097900       END-PERFORM                                                        
098000       IF SEGMENT-FINNS                                                   
098100         MOVE JA TO FLER-KOLLI-SW                                         
098200       END-IF                                                             
098300     END-IF                                                               
098400     IF NOT FLER-KOLLI                                                    
098500       ADD +1 TO INDX                                                     
098600       PERFORM IMS-GN-WDE411-BSEQ                                         
098700       MOVE JA TO FIRST-TIME-SW                                           
098800     END-IF                                                               
098900                                                                          
099000     IF SEGMENT-FINNS AND INDX > MAX-INDX                                 
099100       CONTINUE                                                           
099200     ELSE                                                                 
099300       IF INDX > MAX-INDX                                                 
099400          PERFORM IMS-GN-ORQA-ORQA01                                      
099500          IF SEGMENT-FINNS                                                
099600           CONTINUE                                                       
099700          END-IF                                                          
099800       END-IF                                                             
099900     END-IF                                                               
100000                                                                          
100100     IF SEGMENT-SAKNAS                                                    
100200        MOVE LOW-VALUE   TO W-WDE4BSEQ-MIN-X                              
100300        MOVE HIGH-VALUE  TO W-WDE4BSEQ-MAX-X                              
100400     END-IF                                                               
100500     .                                                                    
100600     EJECT                                                                
100700 H-KOLLA-BEHOERIGHET SECTION.                                             
100800                                                                          
100900*    MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
101000*THIS PROGRAM IS A COPY OF W4050200 PROGRAM  AND MODIFIED FOR             
101100*LDC PROJECT                                                              
101200     MOVE REQU-IDUSER       TO    SEC-IDUSER                              
101300     MOVE '4502'            TO    SEC-IDTRANS                             
101400     MOVE REQU-IDDISTR-KEY  TO    SEC-IDKEY                               
101500                                                                          
101600     CALL WSECURIT          USING SEC-IDUSER                              
101700                                  SEC-IDTRANS                             
101800                                  SEC-IDKEY                               
101900                                  SEC-KDSVAR                              
102000                                                                          
102100     IF SEC-KDSVAR = OBEHORIG                                             
102200        MOVE NEJ TO ALLT-SW                                               
102300     ELSE                                                                 
102400        CONTINUE                                                          
102500     END-IF                                                               
102600     .                                                                    
102700     EJECT                                                                
102800 S01-FLYTTA-WDE411-TILL-MOD SECTION.                                      
102900                                                                          
103000     IF IDARTNR-IFYLLT                                                    
103100       MOVE JA              TO ARTIKEL-SW                                 
103200     END-IF                                                               
103300                                                                          
103700     MOVE 411-ORAD-IDARTNR  TO RESP-IDARTNR(INDX)                         
103800     MOVE 411-ORAD-KVBEART  TO RESP-KVBEART-Q(INDX)                       
103900     MOVE 411-ORAD-KVAVBART TO RESP-KVAVBART(INDX)                        
104000     MOVE 411-ORAD-KVLEVART TO RESP-KVLEVART(INDX)                        
104100     MOVE 411-ORAD-IDKUNDRF-WIP TO RESP-IDKUNDRF-WIP(INDX)                
104200     ADD +1                TO  WS-COUNT                                   
104300                                                                          
104400     IF WS-COUNT < 501                                                    
104500        CONTINUE                                                          
104600     ELSE                                                                 
104700        MOVE TOO-MANY-LINES    TO RESP-IDMSG-ERROR                        
104800     END-IF                                                               
104900                                                                          
105000     IF 411-ORAD-KDFARLIG = +4 OR 411-ORAD-KDFARLIG = +7                  
105100       MOVE 'J'             TO RESP-KDFARLIG(INDX)                        
105200     ELSE                                                                 
105300       MOVE SPACE           TO RESP-KDFARLIG(INDX)                        
105400     END-IF                                                               
105500                                                                          
105600     IF 411-ORAD-IDKUNDRF-RO NOT = '00000     '                           
105700      IF 411-ORAD-IDKUNDRF-RO NOT = WS-IDKUNDRF-CONT                      
105800         MOVE 411-ORAD-IDKUNDRF-RO TO IDKUNDRF-WS                         
105900         IF IDKUNDRF-WS-POS6-7 = SPACE                                    
106000           MOVE IDORDNR5-WS TO RESP-IDKUNDRF-URS(INDX)                    
106100         ELSE                                                             
106200           MOVE IDORDNR7-WS TO RESP-IDKUNDRF-URS(INDX)                    
106300         END-IF                                                           
106400         INSPECT RESP-IDKUNDRF-URS(INDX) REPLACING LEADING                
106500                                        ZERO BY SPACE                     
106600      END-IF                                                              
106700     END-IF                                                               
106800                                                                          
106900     IF 411-ORAD-KDRADSTA < +4                                            
107000       IF DIST79-DEALER-PRICE                                             
107100                                                                          
107200         MOVE 411-ORAD-KDVALISO  TO SPAR-KDVALISO                         
107400                                                                          
107800       END-IF                                                             
107900       IF SEC-KDSVAR = 2 OR 6                                             
108000         CONTINUE                                                         
108100       ELSE                                                               
108200         IF DIST79-DEALER-PRICE                                           
108300           IF 411-ORAD-PRARTNTO-LOC > 0                                   
108400                                                                          
108500             IF SPAR-KDVALISO = SPACE                                     
108600               MOVE 411-ORAD-KDVALISO  TO SPAR-KDVALISO                   
108700             END-IF                                                       
109000                                                                          
109100           ELSE                                                           
109200             IF 411-ORAD-PRARTNTO-LOCPREL > 0                             
109300                                                                          
109400               IF SPAR-KDVALISO = SPACE                                   
109500                 MOVE 411-ORAD-KDVALISO  TO SPAR-KDVALISO                 
109600               END-IF                                                     
109900                                                                          
110000             END-IF                                                       
110100           END-IF                                                         
110200         END-IF                                                           
110300       END-IF                                                             
110400                                                                          
110500       MOVE 'U'            TO  RESP-KDKOLSTA(INDX)                        
110600       IF 411-ORAD-KVANNANT > +0                                          
110700         MOVE 'D' TO RESP-KDAVVIK(INDX)                                   
110800       ELSE                                                               
110900         MOVE SPACE TO RESP-KDAVVIK(INDX)                                 
111000       END-IF                                                             
111100     ELSE                                                                 
111200       IF 411-ORAD-KVAVBART = ZERO                                        
111300* * * INNEBÄR ATT RADEN ÄR NOLLAD VID PACKNINGSRAPPORTERINGEN,            
111400* * * 'N' SOM STATUS SKALL DÅ STÅ FÖR NOLLNINGEN.                         
111500         MOVE 'N' TO  RESP-KDKOLSTA(INDX)                                 
111600       ELSE                                                               
111700         MOVE 'P' TO  RESP-KDKOLSTA(INDX)                                 
111800       END-IF                                                             
111900       IF 411-ORAD-FLFYSAVV = JA                                          
112000           MOVE 'A' TO RESP-KDAVVIK(INDX)                                 
112100       ELSE                                                               
112200         IF 411-ORAD-KVANNANT > +0                                        
112300            MOVE 'D' TO RESP-KDAVVIK(INDX)                                
112400         ELSE                                                             
112500            MOVE SPACE TO RESP-KDAVVIK(INDX)                              
112600         END-IF                                                           
112700       END-IF                                                             
113600       IF SEC-KDSVAR = 2 OR 6                                             
113700           CONTINUE                                                       
113800       ELSE                                                               
113900          IF DIST79-DEALER-PRICE                                          
114000             IF 411-ORAD-PRARTNTO-LOC > 0                                 
114100                                                                          
114200               IF SPAR-KDVALISO = SPACE                                   
114300                 MOVE 411-ORAD-KDVALISO  TO SPAR-KDVALISO                 
114400               END-IF                                                     
114700                                                                          
114800             ELSE                                                         
114900               IF 411-ORAD-PRARTNTO-LOCPREL > 0                           
115000                                                                          
115100                 IF SPAR-KDVALISO = SPACE                                 
115200                   MOVE 411-ORAD-KDVALISO  TO SPAR-KDVALISO               
115300                 END-IF                                                   
115600                                                                          
115700               END-IF                                                     
115800             END-IF                                                       
115900          END-IF                                                          
116000                                                                          
116100       END-IF                                                             
116200     END-IF                                                               
116300                                                                          
116400                                                                          
116500     IF 411-ORAD-IDLEVNR NOT = SPACE                                      
116600       MOVE 411-ORAD-IDLEVNR TO  RESP-IDLEVNR(INDX)                       
116700     END-IF                                                               
116800     .                                                                    
116900     EJECT                                                                
121100 S03-FLYTTA-WDQ401-TILL-MOD SECTION.                                      
121200                                                                          
121300     IF IDARTNR-IFYLLT                                                    
121400       MOVE JA               TO ARTIKEL-SW                                
121500     END-IF                                                               
121600                                                                          
122000     MOVE QF01-ORAD-IDARTNR  TO  RESP-IDARTNR(INDX)                       
122100                                                                          
122200     IF QF01-ORAD-KDFARLIG = +4 OR QF01-ORAD-KDFARLIG = +7                
122300       MOVE 'J'                 TO RESP-KDFARLIG(INDX)                    
122400     ELSE                                                                 
122500       MOVE SPACE               TO RESP-KDFARLIG(INDX)                    
122600     END-IF                                                               
122700                                                                          
122800     MOVE QF01-ORAD-KVBEART-Q    TO RESP-KVBEART-Q(INDX)                  
122900     MOVE QF01-ORAD-IDKUNDRF-WIP TO RESP-IDKUNDRF-WIP(INDX)               
123000     ADD +1                TO  WS-COUNT                                   
123100                                                                          
123200       IF DIST79-DEALER-PRICE                                             
123300                                                                          
123400          IF SPAR-KDVALISO = SPACE                                        
123500            MOVE QF01-ORAD-KDVALISO  TO SPAR-KDVALISO                     
123600          END-IF                                                          
123800                                                                          
124200       END-IF                                                             
124300       IF SEC-KDSVAR = 2 OR 6                                             
124400         CONTINUE                                                         
124500       ELSE                                                               
124600         IF DIST79-DEALER-PRICE                                           
124700           IF QF01-ORAD-PRARTNTO-LOC > 0                                  
124800                                                                          
124900             IF SPAR-KDVALISO = SPACE                                     
125000               MOVE QF01-ORAD-KDVALISO  TO SPAR-KDVALISO                  
125100             END-IF                                                       
125400                                                                          
125500           ELSE                                                           
125600             IF QF01-ORAD-PRARTNTO-LOCPREL > 0                            
125700                                                                          
125800               IF SPAR-KDVALISO = SPACE                                   
125900                 MOVE QF01-ORAD-KDVALISO  TO SPAR-KDVALISO                
126000               END-IF                                                     
126200                                                                          
126300             END-IF                                                       
126400           END-IF                                                         
126500         END-IF                                                           
126600       END-IF                                                             
126700                                                                          
126800       MOVE SPAR-STATUS          TO   RESP-KDKOLSTA(INDX)                 
126900         IF QF01-ORAD-IDKUNDRF-RO NOT = '0000000   '                      
127000           IF QF01-ORAD-IDKUNDRF-RO NOT = WS-IDKUNDRF-KOLL                
127100             MOVE QF01-ORAD-IDKUNDRF-RO TO IDKUNDRF-WS                    
127200             IF IDKUNDRF-WS-POS6-7 = SPACE                                
127300               MOVE IDORDNR5-WS TO RESP-IDKUNDRF-URS(INDX)                
127400             ELSE                                                         
127500               MOVE IDORDNR7-WS TO RESP-IDKUNDRF-URS(INDX)                
127600             END-IF                                                       
127700             INSPECT RESP-IDKUNDRF-URS(INDX) REPLACING LEADING            
127800                                            ZERO BY SPACE                 
127900           END-IF                                                         
128000         END-IF                                                           
128100                                                                          
128200                                                                          
128300     IF QF01-ORAD-IDLEVNR NOT = SPACE                                     
128400       MOVE QF01-ORAD-IDLEVNR    TO   RESP-IDLEVNR(INDX)                  
128500     END-IF                                                               
128600     .                                                                    
128700     EJECT                                                                
131100 S05-FLYTTA-WDE611-TILL-MOD SECTION.                                      
131200                                                                          
131300     MOVE KKOLLI-KVLEVART TO  RESP-KVLEVART(INDX)                         
131400     MOVE KOLLI-IDKOLLI TO  RESP-IDKOLLI(INDX)                            
131500                                                                          
131600     IF KOLLI-KDKOLSTA = +0                                               
131700       MOVE 'U'         TO RESP-KDKOLSTA(INDX)                            
131800     ELSE                                                                 
131900       IF KOLLI-KDKOLSTA = +1                                             
132000         MOVE 'P'       TO RESP-KDKOLSTA(INDX)                            
132100       ELSE                                                               
132200         IF KOLLI-KDKOLSTA = +2 OR +3                                     
132300           MOVE 'L'     TO RESP-KDKOLSTA(INDX)                            
132400         ELSE                                                             
132500           IF KOLLI-KDKOLSTA = +4                                         
132600             MOVE 'LF'  TO RESP-KDKOLSTA(INDX)                            
132700           ELSE                                                           
132800             IF KOLLI-KDKOLSTA = +6 OR +7                                 
132900               MOVE 'F' TO RESP-KDKOLSTA(INDX)                            
133000             ELSE                                                         
133100               IF KOLLI-KDKOLSTA = +8 OR +9                               
133200                 MOVE 'FL' TO RESP-KDKOLSTA(INDX)                         
133300               END-IF                                                     
133400             END-IF                                                       
133500           END-IF                                                         
133600           IF KOLLI-KDKOLSTA = +6                                         
133700             MOVE 'SC' TO RESP-KDKOLSTA(INDX)                             
133800           ELSE                                                           
133900             IF KOLLI-KDKOLSTA = +7                                       
134000               MOVE 'S' TO RESP-KDKOLSTA(INDX)                            
134100             ELSE                                                         
134200               IF KOLLI-KDKOLSTA = +9                                     
134300                 MOVE 'SF' TO RESP-KDKOLSTA(INDX)                         
134400               END-IF                                                     
134500             END-IF                                                       
134600           END-IF                                                         
134610           IF BOUNCEORDER                                                 
134620              MOVE KOLLI-IDFAKT-EXP TO RESP-IDFAKT(INDX)                  
134630           ELSE                                                           
134700              MOVE KOLLI-IDFAKT     TO RESP-IDFAKT(INDX)                  
134710           END-IF                                                         
134800         END-IF                                                           
134900       END-IF                                                             
135000     END-IF                                                               
135100     .                                                                    
135200     EJECT                                                                
135300 S07-LAES-WDQ401 SECTION.                                                 
135400     IF IDARTNR-IFYLLT                                                    
135500       PERFORM IMS-GU-ORQF-ORQF01-M-IDARTNR                               
135600     ELSE                                                                 
135700       PERFORM IMS-GU-ORQF-ORQF01                                         
135800     END-IF                                                               
135900     IF SEGMENT-FINNS                                                     
136000                                                                          
136100       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
136200         PERFORM S03-FLYTTA-WDQ401-TILL-MOD                               
136300         ADD +1 TO INDX                                                   
136400         IF IDARTNR-IFYLLT                                                
136500           PERFORM IMS-GN-ORQF-ORQF01-M-IDARTNR                           
136600         ELSE                                                             
136700           PERFORM IMS-GN-ORQF-ORQF01                                     
136800         END-IF                                                           
136900       END-PERFORM                                                        
137000                                                                          
137100       IF SEGMENT-FINNS                                                   
137200         CONTINUE                                                         
137300       END-IF                                                             
137400     ELSE                                                                 
137500       IF IDARTNR-IFYLLT                                                  
137600        IF ARTIKEL-SAKNAS                                                 
137700         MOVE '025'                  TO RESP-IDMSG-ERROR                  
137800         MOVE 'IDARTNR'              TO RESP-IDELMT-ERROR                 
137900        END-IF                                                            
138000       ELSE                                                               
138100         MOVE '025'                  TO RESP-IDMSG-ERROR                  
138200         MOVE 'IDRADNR'              TO RESP-IDELMT-ERROR                 
138300       END-IF                                                             
138400     END-IF                                                               
138500     .                                                                    
138600     EJECT                                                                
138700                                                                          
142300*    --- DISPATCHER SECTIONS                                              
142400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
142500                                                                          
142600     MOVE 'GETARG'               TO SUB-KDFUNC                            
142700     MOVE 'CARPARTS.LDC.ORDERQUERYPART1'    TO SUB-ADDISPABS              
142800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
142900                                                                          
143000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
143100                                                                          
143200     IF SUB-KDRC > 0                                                      
143300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
143400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
143500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
143600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
143700     END-IF                                                               
143800     .                                                                    
143900     SKIP3                                                                
144000 S02-RETURN-RESPONSE SECTION.                                             
144100                                                                          
144200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
144300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
144400                                                                          
144500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
144600                                                                          
144700     IF SUB-KDRC > 0                                                      
144800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
144900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
145000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
145100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
145200     END-IF                                                               
145300     .                                                                    
145400     EJECT                                                                
145500* --- IMS SEKTIONER ---                                                   
145600 IMS-GU-WDE401-ASEQ SECTION.                                              
145700     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
145800          DELIMITED BY SIZE INTO SSA1                                     
145900     MOVE '  GE' TO GODK-STATUSKODER                                      
146000     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA-KORD SSA1                
146100     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     EJECT                                                                
146500 IMS-GN-WDE401-ASEQ SECTION.                                              
146600     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
146700          DELIMITED BY SIZE INTO SSA1                                     
146800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
146900     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA-KORD SSA1                
147000     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
147100     PERFORM IMS-STATUSKONTROLL                                           
147200     .                                                                    
147300     EJECT                                                                
147400 IMS-GU-WDE411-BSEQ SECTION.                                              
147500     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4BSEQ-MIN-X                        
147600                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ')'                    
147700          DELIMITED BY SIZE INTO SSA1                                     
147800     MOVE '  GE' TO GODK-STATUSKODER                                      
147900     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA-WDE411 SSA1              
148000     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
148100     PERFORM IMS-STATUSKONTROLL                                           
148200     .                                                                    
148300     EJECT                                                                
148400 IMS-GN-WDE411-BSEQ SECTION.                                              
148500     STRING 'WDE411  (WDE4BSEQ >' W-WDE4BSEQ-MIN-X                        
148600                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ')'                    
148700          DELIMITED BY SIZE INTO SSA1                                     
148800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
148900     CALL CBLTDLI USING GN WDE4B-PCB DLI-IO-AREA-WDE411 SSA1              
149000     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300     EJECT                                                                
149400 IMS-GNP-WDE421 SECTION.                                                  
149500     STRING 'WDE421  (WDE421KY =' W-WDE4KEY-X ')'                         
149600          DELIMITED BY SIZE INTO SSA1                                     
149700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
149800     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA-WDE421 SSA1             
149900     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
150000     PERFORM IMS-STATUSKONTROLL                                           
150100     .                                                                    
150200     EJECT                                                                
150300 IMS-GNP-WDE421-BKEY SECTION.                                             
150400     MOVE 'WDE421   ' TO SSA1                                             
150500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
150600     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA-WDE421 SSA1             
150700     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
150800     PERFORM IMS-STATUSKONTROLL                                           
150900     .                                                                    
151000     EJECT                                                                
151100 IMS-GU-ORQA-ORQA01 SECTION.                                              
151200                                                                          
151300     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
151400                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
151500          DELIMITED BY SIZE INTO SSA1                                     
151600     MOVE '    GE' TO GODK-STATUSKODER                                    
151700     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
151800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
151900     PERFORM IMS-STATUSKONTROLL                                           
152000     .                                                                    
152100     EJECT                                                                
152200 IMS-GN-ORQA-ORQA01 SECTION.                                              
152300                                                                          
152400     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
152500                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
152600          DELIMITED BY SIZE INTO SSA1                                     
152700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
152800     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
152900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
153000     PERFORM IMS-STATUSKONTROLL                                           
153100     .                                                                    
153200     EJECT                                                                
153300 IMS-GU-ORQF-ORQF01 SECTION.                                              
153400     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-MIN-X                    
153500                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
153600          DELIMITED BY SIZE INTO SSA1                                     
153700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
153800     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
153900     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
154000     PERFORM IMS-STATUSKONTROLL                                           
154100     .                                                                    
154200     EJECT                                                                
154300 IMS-GU-ORQF-ORQF01-M-IDARTNR SECTION.                                    
154400     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
154500                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
154600                    '&IDARTNR  =' W-ORAD-IDARTNR-X ')'                    
154700          DELIMITED BY SIZE INTO SSA1                                     
154800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
154900     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
155000     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
155100     PERFORM IMS-STATUSKONTROLL                                           
155200     .                                                                    
155300     EJECT                                                                
155400 IMS-GN-ORQF-ORQF01 SECTION.                                              
155500     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-MIN-X                    
155600                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
155700          DELIMITED BY SIZE INTO SSA1                                     
155800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
155900     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
156000     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
156100     PERFORM IMS-STATUSKONTROLL                                           
156200     .                                                                    
156300     EJECT                                                                
156400 IMS-GN-ORQF-ORQF01-M-IDARTNR SECTION.                                    
156500     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-X                        
156600                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
156700                    '&IDARTNR  =' W-ORAD-IDARTNR-X ')'                    
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
157000     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
157100     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     .                                                                    
157400     EJECT                                                                
157500 IMS-GU-ORQI-ORQI01-M-Q2CSEQ SECTION.                                     
157600                                                                          
157700     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
157800          DELIMITED BY SIZE INTO SSA1                                     
157900     MOVE '  GE' TO GODK-STATUSKODER                                      
158000     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
158100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
158200     PERFORM IMS-STATUSKONTROLL                                           
158300     .                                                                    
158400                                                                          
158500 IMS-GNP-ORQI-ORQI12 SECTION.                                             
158600     MOVE 'WLORQI12 ' TO SSA1                                             
158700     MOVE '  GE' TO GODK-STATUSKODER                                      
158800     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
158900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
159000     PERFORM IMS-STATUSKONTROLL                                           
159100     .                                                                    
159200     EJECT                                                                
159210 IMS-GU-WDE601 SECTION.                                                   
159220                                                                          
159230     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
159240          DELIMITED BY SIZE INTO    SSA1                                  
159250     MOVE    '  GE'           TO    GODK-STATUSKODER                      
159260     CALL    CBLTDLI          USING GU  WDE6-PCB DLI-IOAREA-WDE601        
159270                                         SSA1                             
159280     MOVE    WDE6-STATUS-CODE TO    STATUS-WS                             
159290     PERFORM IMS-STATUSKONTROLL                                           
159291     .                                                                    
159292                                                                          
159300 IMS-GU-WDE611 SECTION.                                                   
159400                                                                          
159500       STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                      
159600            DELIMITED BY SIZE INTO SSA1                                   
159700       STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                       
159800            DELIMITED BY SIZE INTO SSA2                                   
159900       MOVE '  ' TO GODK-STATUSKODER                                      
160000       CALL CBLTDLI USING GU WDE6-PCB DLI-IOAREA-WDE611 SSA1 SSA2         
160100       MOVE WDE6-STATUS-CODE TO STATUS-WS                                 
160200       PERFORM IMS-STATUSKONTROLL                                         
160300         .                                                                
160400     EJECT                                                                
165200 IMS-STATUSKONTROLL SECTION.                                              
165300                                                                          
165400     SET STATUS-IX TO 1                                                   
165500     SEARCH GODK-STATUS                                                   
165600       AT END CALL FELLOG                                                 
165700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
165800     END-SEARCH                                                           
165900     .                                                                    
