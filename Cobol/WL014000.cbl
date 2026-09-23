000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL014000.                                                
000300 AUTHOR.        SUBBARAO PARUCHURI V.                                     
000400 DATE-WRITTEN.   04/07/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.ORDERQUERY'                                
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        FUNKTION.                                                        
001100*        PROGRAMMET LÄSER ORDERHUVUDSREGISTER, ARBETSTABELLEN,            
001200*        DIREKTLEVERANTÖRS SEGMENT OCH ORDERDELSREGISTER OCH              
001300*        VISAR VISS INFORMATION. OM ORDERDELSTATUS ÄR PACKAD              
001400*        (P),                                                             
001500*        ELLER UTSKRIVEN (U) LÄSER PROGRAMMET KOLLIREGISTER               
001600*        OCH                                                              
001700*        LÄGGER UT YTTERLIGARE INFORMATION.                               
001800*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001900*        PROGRAMMET LÄSER   WLORQI  (WDQ2)                                
002000*        WLORQA  (WDQ3)                                                   
002100*        WDE6                                                             
002200*                                                                         
002300*        WL014000 PROGRAM IS A REPLICA OF W4050100 PROGRAM                
002400*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: WL0140T                                             
002800*        REQUEST:     WL0140I1                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        RESPONSE:    WL0140O1                                            
003200*                                                                         
003300*    E'TRACKER: 5281947 DATED 2007-08-30                                  
003400*                                                                         
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800 77  IDPGM                       PIC X(08)   VALUE 'WL014000'.            
004900                                                                          
005000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005100                                                                          
005200 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
005300 77  MAX-INDX                    PIC S9(9)  VALUE +8    COMP SYNC.        
005700                                                                          
005800 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
005900                                                                          
006000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
006200 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
006300 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
006400 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
006500 77  WS-IDKUNDRF                 PIC X(7)    VALUE SPACE.                 
006600 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
006700 77  WS-IDPRODNR-NUM             PIC 9(7)    VALUE ZERO.                  
006900 77  HELP-IDDISTR                PIC 9(4)    VALUE ZERO.                  
007000 77  HELP-IDKUNDNR               PIC 9(6)    VALUE ZERO.                  
007100 77  WS-IDTIDZON                 PIC X(2)    VALUE SPACE.                 
007200 77  WS-KDMATT                   PIC X(1)    VALUE SPACE.                 
007300 77  WS-COUNT                    PIC 9(3)    VALUE ZERO.                  
007400 77  WS-SEK                      PIC X(3)    VALUE 'SEK'.                 
007500                                                                          
007600 01  WS-TID-X.                                                            
007700     03  WS-TID-N                PIC 9(5).                                
007800 01  WS-TID-RED-X.                                                        
007900     03  WS-TID-HH               PIC X(2).                                
008000     03  WS-TID-MM               PIC X(2).                                
008100                                                                          
008200 77  KVRADER-RAKNARE-WS          PIC S9(5)      VALUE +0   COMP-3.        
008300 77  SUORDV-RAKNARE-WS           PIC S9(9)V9(2) VALUE +0   COMP-3.        
008400 77  SUORDV-EXP-RAKNARE-WS       PIC S9(9)V9(2) VALUE +0   COMP-3.        
008500 77  SUORDV-LOC-RAKNARE-WS       PIC S9(9)V9(2) VALUE +0   COMP-3.        
008600 77  SUORDV-LOCPREL-RAKNARE-WS   PIC S9(9)V9(2) VALUE +0   COMP-3.        
008700 77  VKORDNTO-RAKNARE-WS         PIC S9(6)V9(3) VALUE +0   COMP-3.        
008800 77  VLORDNTO-RAKNARE-WS         PIC S9(4)V9(3) VALUE +0   COMP-3.        
008900 77  KVKOLLI-FAKT-RAKNARE        PIC S9(5)      VALUE +0   COMP-3.        
009000 77  KVKOLLI-LAST-RAKNARE        PIC S9(5)      VALUE +0   COMP-3.        
009100 77  KVKOLPAC-RAKNARE            PIC S9(5)      VALUE +0   COMP-3.        
009200 77  KVORDRAD-PACK-RAKNARE       PIC S9(5)      VALUE +0   COMP-3.        
009300 77  VKORDBTO-RAKNARE            PIC S9(6)V9(3) VALUE +0   COMP-3.        
009400 77  VLORDBTO-RAKNARE            PIC S9(4)V9(3) VALUE +0   COMP-3.        
009500 77  HELP-SUMMA                  PIC S9(9)V9(2) VALUE +0   COMP-3.        
009600                                                                          
009700 77  KVRADER-RAKNARE-ALT         PIC S9(5)      VALUE +0   COMP-3.        
009800 77  SUORDV-RAKNARE-ALT          PIC S9(9)V9(2) VALUE +0   COMP-3.        
009900 77  SUORDV-LOC-RAKNARE-ALT      PIC S9(9)V9(2) VALUE +0   COMP-3.        
010000 77  SUORDV-LOCPREL-RAKNARE-ALT  PIC S9(9)V9(2) VALUE +0   COMP-3.        
010100 77  VKORDNTO-RAKNARE-ALT        PIC S9(6)V9(3) VALUE +0   COMP-3.        
010200 77  VLORDNTO-RAKNARE-ALT        PIC S9(4)V9(3) VALUE +0   COMP-3.        
010300                                                                          
010400 01  WS-TEDDI.                                                            
010500     03 FILLER                   PIC X(3) VALUE SPACE.                    
010600     03 WS-KDVALISO              PIC X(3).                                
010700     03 FILLER                   PIC X(5) VALUE SPACE.                    
010800                                                                          
010900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011000     88  NYCKLAR-OK                          VALUE 'J'.                   
011100     88  NYCKLAR-FEL                         VALUE 'N'.                   
011200                                                                          
011300 77  ARB-SW                      PIC X       VALUE 'J'.                   
011400     88  ARB-OK                              VALUE 'J'.                   
011500     88  ARB-FEL                             VALUE 'N'.                   
011600                                                                          
011700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011800     88  ALLT-OK                             VALUE 'J'.                   
011900                                                                          
012000 77  PROD-INGANG-SW              PIC X       VALUE 'J'.                   
012100     88  PROD-INGANG                         VALUE 'J'.                   
012200                                                                          
012300 77  DKO-INGANG-SW               PIC X       VALUE 'J'.                   
012400     88  DKO-INGANG                          VALUE 'J'.                   
012500                                                                          
012600*                                                                         
012700 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
012800*    ----DISTR-DEALER-PRICE----                                           
012900*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
013000     EJECT                                                                
013100*      --- VALID IDDC CODES                                               
013200*01    -COPY WWDCKONS                                                     
013300       EJECT                                                              
013400                                                                          
013500     EJECT                                                                
013600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
013700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
013800 77  KDRC-DISPLAY                PIC Z(5).                                
013900                                                                          
014000     EJECT                                                                
014100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
014200 01  GENERAL-SUBPROGRAMS.                                                 
014300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
014600     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
014700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014800     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
014900     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
015000     SKIP3                                                                
015100*    --- PARAMETERS TO ABEND                                              
015200                                                                          
015300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015600     EJECT                                                                
015700 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
015800*01  -COPY WTRAUTF8                                                       
015900     EJECT                                                                
016000*                                                                         
016100*01    -COPY WWDC99                                                       
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016400     SKIP3                                                                
016500*01  -COPY WZ01SUB                                                        
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
016800     SKIP3                                                                
016900 01  REQU-AREA.                                                           
017000*    03  -COPY WZ01REQU                                                   
017100*    03  -COPY WL0140I1                                                   
017200     EJECT                                                                
017300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
017400     SKIP3                                                                
017500 01  RESP-AREA.                                                           
017600*    03  -COPY WZ01RESP                                                   
017700*    03  -COPY WL0140O1                                                   
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM WWOMVAND                              
018000*01 -COPY WWOMVAND                                                        
018100     EJECT                                                                
018200 01  SPAR-AREOR.                                                          
018300     03 ODEL-IDDC-SPAR           PIC X(2)    VALUE SPACE.                 
018400     03 ODEL-IDPRODNR-SPAR       PIC S9(7)   VALUE ZERO.                  
018500     03 ODEL-IDLEVNR-SPAR        PIC  X(5)   VALUE SPACE.                 
018600     03 VORD-IDPRODNR-SPAR       PIC S9(7)   VALUE ZERO.                  
018700     EJECT                                                                
018800     SKIP3                                                                
018900 01  MESSAGE-CODES.                                                       
019000     03  ERR-OBEHORIG            PIC X(3)    VALUE '00A'.                 
019100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
019200     03  INF-LINES-MISSING-CL    PIC X(3)    VALUE '198'.                 
019300     03  INF-ORDER-ANNULLED      PIC X(3)    VALUE '197'.                 
019400     03  INF-ORDER-EJ-AVSLUT     PIC X(3)    VALUE '194'.                 
019500     03  INF-ORDER-EJ-KLAR       PIC X(3)    VALUE '201'.                 
019600     03  INF-ORDER-MISSING       PIC X(3)    VALUE '125'.                 
019700     03  INF-ORDERHEAD-MISSING   PIC X(3)    VALUE '195'.                 
019800     03  INF-ORDERINFO-BORTTAGEN PIC X(3)    VALUE '196'.                 
019900     03  INF-ORDERLINES-MISSING  PIC X(3)    VALUE '199'.                 
020000     03  INF-TRANSPORT-EJ-BOKAD  PIC X(3)    VALUE '200'.                 
020100     03  FEL-X1                  PIC X(3)    VALUE '099'.                 
020200     EJECT                                                                
020300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020400*                                                                         
020500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020600     SKIP3                                                                
020700 01  NYCKLAR-TILL-DLI.                                                    
020800     03  W-WDQ2CSEQ-X.                                                    
020900         05  W-SEQC-IDDISTR      PIC S9(5)    VALUE ZERO COMP-3.          
021000         05  W-SEQC-IDKUNDNR     PIC S9(7)    VALUE ZERO COMP-3.          
021100         05  W-SEQC-IDKUNDRF     PIC X(10)    VALUE SPACE.                
021200     03  W-WDQ211KY-X.                                                    
021300         05  W-DIRL-IDDC         PIC X(2)     VALUE SPACE.                
021400         05  W-DIRL-IDLEVNR      PIC  X(5)    VALUE SPACE.                
021500     03  W-IDDC-X.                                                        
021600         05  W-ARB-IDDC          PIC X(2)     VALUE SPACE.                
021700     03  W-IDPRODNR-X.                                                    
021800         05  W-VORD-IDPRODNR     PIC S9(7)    VALUE ZERO COMP-3.          
021900     03  W-IDGMT-X.                                                       
022000         05  W-IDDISTR-GMT       PIC S9(5)    VALUE ZERO COMP-3.          
022100         05  W-IDKUNDNR-GMT      PIC S9(7)    VALUE ZERO COMP-3.          
022200     03  W-IDGMT-MIN-X.                                                   
022300         05  W-IDDISTR-GMT-MIN   PIC S9(5)    VALUE ZERO COMP-3.          
022400         05  W-IDKUNDNR-GMT-MIN  PIC S9(7)    VALUE ZERO COMP-3.          
022500     03  W-IDGMT-MAX-X.                                                   
022600         05  W-IDDISTR-GMT-MAX   PIC S9(5)    VALUE ZERO COMP-3.          
022700         05  W-IDKUNDNR-GMT-MAX  PIC S9(7)    VALUE ZERO COMP-3.          
022800     03  W-WDB101KY-X.                                                    
022900         05  W-WDB1-IDPARTNR     PIC X(9)     VALUE SPACE.                
023000         05  W-WDB1-IDFTG        PIC 9(2)     VALUE ZERO.                 
023100     03  W-IDDISTR-X.                                                     
023200         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
023300     03  W-IDKUNDNR-X.                                                    
023400         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
023500     03  W-WDQ3DSEQ-MIN-X.                                                
023600         05  W-Q3DSEQ-IDPRODNR-MIN   PIC S9(7) VALUE ZERO COMP-3.         
023700         05  W-Q3DSEQ-IDPLKLST-MIN   PIC S9(3) VALUE ZERO COMP-3.         
023800     03  W-WDQ3DSEQ-MAX-X.                                                
023900         05  W-Q3DSEQ-IDPRODNR-MAX   PIC S9(7) VALUE ZERO COMP-3.         
024000         05  W-Q3DSEQ-IDPLKLST-MAX   PIC S9(3) VALUE ZERO COMP-3.         
024100     03  W-WDQ301KY-MIN-X.                                                
024200         05  W-ODEL-IDORDER-MIN  PIC S9(7)    VALUE ZERO COMP-3.          
024300         05  W-ODEL-IDDC-MIN     PIC X(2)     VALUE SPACE.                
024400         05  W-ODEL-IDPRODNR-MIN PIC S9(7)    VALUE ZERO COMP-3.          
024500         05  W-ODEL-IDPLKLST-MIN PIC S9(3)    VALUE ZERO COMP-3.          
024600     03  W-WDQ301KY-MAX-X.                                                
024700         05  W-ODEL-IDORDER-MAX  PIC S9(7)    VALUE ZERO COMP-3.          
024800         05  W-ODEL-IDDC-MAX     PIC X(2)     VALUE SPACE.                
024900         05  W-ODEL-IDPRODNR-MAX PIC S9(7)    VALUE ZERO COMP-3.          
025000         05  W-ODEL-IDPLKLST-MAX PIC S9(3)    VALUE ZERO COMP-3.          
025100*    --- STATUS-KOD FRÅN IMS                                              
025200 01  STATUS-WS                   PIC XX.                                  
025300     88  SEGMENT-FINNS                       VALUE '  '.                  
025400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025600     88  BASEN-SLUT                          VALUE 'GB'.                  
025700     SKIP2                                                                
025800 01  GODK-STATUSKODER.                                                    
025900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026000     SKIP3                                                                
026100 01  SSA1                        PIC X(128).                              
026200 01  SSA2                        PIC X(64).                               
026300     EJECT                                                                
026400 01   MEDDELANDE.                                                         
026500*                                                                         
026600   03 INTERNMED.                                                          
026700     05 INTERNMED-S            PIC X(22) VALUE                            
026800                                        'INTERN SPECIALORDER   '.         
026900     05 INTERNMED-GB           PIC X(22) VALUE                            
027000                                        'INTERNAL SPECIAL ORDER'.         
027100   03 FILLER REDEFINES INTERNMED.                                         
027200     05 INTERN-MED             PIC X(22) OCCURS 2.                        
027300*                                                                         
027400   03 SOFTMED.                                                            
027500     05 SOFTMED-S              PIC X(22) VALUE                            
027600                                        'SOFTWARE ORDER      '.           
027700     05 SOFTMED-GB             PIC X(22) VALUE                            
027800                                        'SOFTWARE ORDER      '.           
027900   03 FILLER REDEFINES SOFTMED.                                           
028000     05 SOFT-MED               PIC X(22) OCCURS 2.                        
028100*                                                                         
028200   03 SPAERRMED.                                                          
028300     05 SPAERRMED-S            PIC X(22) VALUE                            
028400                                        'BETALARE STOPPAD      '.         
028500     05 SPAERRMED-GB           PIC X(22) VALUE                            
028600                                        'CUSTOMER STOPPED      '.         
028700   03 FILLER REDEFINES SPAERRMED.                                         
028800     05 SPAERR-MED             PIC X(22) OCCURS 2.                        
028900     EJECT                                                                
029000*                            IMS FUNKTIONSKODER                           
029100*01  -COPY W0003                                                          
029200     EJECT                                                                
029300******************************************************************        
029400*                                                                         
029500*        ARBETS-AREOR TILL IO-AREORNA                                     
029600*                                                                         
029700*    ---  DLI INPUT-OUTPUT AREA 1                                         
029800*    ---  DLI-IO-AREA                                                     
029900                                                                          
030000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OHUV'.             
030100 01  DLI-IO-AREA-OHUV.                                                    
030200*    03  WLORQI01    -COPY WDQ201                                         
030300     EJECT                                                                
030400                                                                          
030500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-DIRL'.             
030600 01  DLI-IO-AREA-DIRL.                                                    
030700*    03  WLORQI11.   -COPY WDQ211                                         
030800     EJECT                                                                
030900                                                                          
031000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARB '.             
031100 01  DLI-IO-AREA-ARB.                                                     
031200*    03  WLORQI12.   -COPY WDQ212                                         
031300     EJECT                                                                
031400                                                                          
031500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-LOR '.             
031600 01  DLI-IO-AREA-LOR.                                                     
031700*    03  WLORQI21.   -COPY WDQ221                                         
031800     EJECT                                                                
031900                                                                          
032000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ODEL'.             
032100 01  DLI-IO-AREA-ODEL.                                                    
032200*    03  WLORQA01.   -COPY WDQ301                                         
032300     EJECT                                                                
032400                                                                          
032500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-VORD'.             
032600 01  DLI-IO-AREA-VORD.                                                    
032700*    03              -COPY WDE601                                         
032800     EJECT                                                                
032900                                                                          
033000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-GMTA'.             
033100 01  DLI-IO-AREA-GMTA.                                                    
033200*    03  WLGMTA01    -COPY WDB201                                         
033300                                                                          
033400     EJECT                                                                
033500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BETC'.             
033600 01  DLI-IO-AREA-BETC.                                                    
033700*    03  WLBETC01    -COPY WDB101                                         
033800     EJECT                                                                
033900 01  FILLER                  PIC X(16)   VALUE 'WSEC-AREA  '.             
034000*01  -COPY WSECAREA                                                       
034100     EJECT                                                                
034200 01  FILLER                  PIC X(16)   VALUE 'W402W001   '.             
034300*   -COPY W402W001                                                        
034400     EJECT                                                                
034500 LINKAGE SECTION.                                                         
034600 01  MSG-PCB                     PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008      -PRE ORQI-                                          
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008      -PRE ORQA-                                          
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008      -PRE ORQAD-                                         
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008      -PRE WDE6-                                          
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008      -PRE GMTA-                                          
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008      -PRE BETC-                                          
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600 PROCEDURE DIVISION  USING MSG-PCB                                        
036700                           ORQI-PCB                                       
036800                           ORQA-PCB ORQAD-PCB                             
036900                           WDE6-PCB GMTA-PCB                              
037000                           BETC-PCB.                                      
037100 MAIN SECTION.                                                            
037200     ENTRY 'DLITCBL' USING MSG-PCB                                        
037300                           ORQI-PCB                                       
037400                           ORQA-PCB ORQAD-PCB                             
037500                           WDE6-PCB GMTA-PCB                              
037600                           BETC-PCB.                                      
037700                                                                          
037800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
037900     IF SUB-KDRC = 0                                                      
038000      IF REQU-KDPGMACT = 'S'                                              
038100       PERFORM A-INIT                                                     
038200       PERFORM C-KOLLA-NYCKLAR                                            
038300       IF NYCKLAR-OK                                                      
038400          PERFORM D-LAES-BASEN                                            
038500          IF ALLT-OK                                                      
038600            PERFORM F-REDIGERA-BILDEN                                     
038700          END-IF                                                          
038800       END-IF                                                             
038900                                                                          
039000      ELSE                                                                
039100         MOVE FEL-X1       TO RESP-IDMSG-ERROR                            
039200      END-IF                                                              
039300                                                                          
039400      IF RESP-IDMSG-ERROR  = SPACE                                        
039500         MOVE REQU-IDDC-KEY     TO RESP-IDDC-KEY                          
039600*                                  WS-IDDC                                
039700        IF REQU-IDDISTR-KEY NUMERIC                                       
039800         MOVE REQU-IDDISTR-KEY  TO RESP-IDDISTR-KEY                       
039900     INSPECT RESP-IDDISTR-KEY    REPLACING LEADING ZERO BY SPACE          
040000        END-IF                                                            
040100        IF REQU-IDKUNDNR-KEY NUMERIC                                      
040200         MOVE REQU-IDKUNDNR-KEY TO  RESP-IDKUNDNR-KEY                     
040300     INSPECT RESP-IDKUNDNR-KEY   REPLACING LEADING ZERO BY SPACE          
040400        END-IF                                                            
040500       IF REQU-IDKUNDRF-KEY  NUMERIC                                      
040600         MOVE REQU-IDKUNDRF-KEY TO RESP-IDKUNDRF-KEY                      
040700     INSPECT RESP-IDKUNDRF-KEY   REPLACING LEADING ZERO BY SPACE          
040800       END-IF                                                             
040900        IF REQU-IDPRODNR-KEY   NUMERIC                                    
041000         MOVE  REQU-IDPRODNR-KEY TO  RESP-IDPRODNR-KEY                    
041100       INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE          
041200        END-IF                                                            
041300        IF REQU-IDKOLLI-KEY NUMERIC                                       
041400         MOVE  REQU-IDKOLLI-KEY TO RESP-IDKOLLI-KEY                       
041500       INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE           
041600        END-IF                                                            
041700        IF REQU-IDARTNR-KEY    NUMERIC                                    
041800         MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                        
041900       INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE           
042000        END-IF                                                            
042100      END-IF                                                              
042200                                                                          
042300      IF RESP-IDMSG-ERROR NOT = SPACE                                     
042400        MOVE ALL '+' TO RESP-WL0140O1(1:39)                               
042500      END-IF                                                              
042600                                                                          
042700       PERFORM S02-RETURN-RESPONSE                                        
042800     END-IF                                                               
042900                                                                          
043000     MOVE ZERO TO RETURN-CODE                                             
043100     GOBACK                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 A-INIT SECTION.                                                          
043500                                                                          
043600     MOVE +2 TO SPRAK-IX                                                  
043700     MOVE ALL '+'            TO RESP-AREA                                 
043800     MOVE SPACE              TO RESP-IDMSG-ERROR                          
043900                                RESP-IDMSG-INFO                           
044000                                RESP-IDELMT-ERROR                         
044100                                RESP-WL0140O1                             
044200     MOVE 001                TO RESP-IDMSGVER                             
044300     MOVE ZERO               TO RESP-KVRADER                              
044400     MOVE +0                 TO VKORDNTO-RAKNARE-WS                       
044500     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
044600                                                                          
044700     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
044800     .                                                                    
044900     EJECT                                                                
045000 C-KOLLA-NYCKLAR SECTION.                                                 
045100                                                                          
045200     MOVE REQU-KDMATT       TO WS-KDMATT                                  
045300                                                                          
045400                                                                          
045500     MOVE JA                TO NYCKLAR-SW                                 
045600                               DKO-INGANG-SW                              
045700     MOVE NEJ               TO PROD-INGANG-SW                             
045800                               ARB-SW                                     
045900                                                                          
046000                                                                          
046100     MOVE LOW-VALUE       TO W-WDQ2CSEQ-X                                 
046200                             W-WDQ211KY-X                                 
046300                             W-IDDC-X                                     
046400                             W-IDPRODNR-X                                 
046500                             W-WDQ3DSEQ-MIN-X                             
046600                             W-WDQ301KY-MIN-X                             
046700                                                                          
046800     MOVE HIGH-VALUE      TO W-WDQ301KY-MAX-X                             
046900                             W-WDQ3DSEQ-MAX-X                             
047000     MOVE REQU-IDDC-KEY   TO W-ARB-IDDC                                   
047100                             WS-IDDC                                      
047200                                                                          
047300     PERFORM CB-KOLLA-IDDISTR                                             
047400     PERFORM CC-KOLLA-IDKUNDNR                                            
047500     PERFORM CD-KOLLA-IDKUNDRF                                            
047600     PERFORM CE-KOLLA-IDPRODNR                                            
047700                                                                          
047800     IF NYCKLAR-FEL                                                       
047900       MOVE    ERR-WRONG-KEY TO    RESP-IDMSG-ERROR                       
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 CB-KOLLA-IDDISTR SECTION.                                                
048400                                                                          
048500     IF REQU-IDPRODNR-KEY = ALL '+'                                       
048600       IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO            
048700         MOVE REQU-IDDISTR-KEY TO WS-IDDISTR-NUM                          
048800         MOVE WS-IDDISTR-NUM TO W-SEQC-IDDISTR                            
048900                                W-IDDISTR                                 
049000                                W-IDDISTR-GMT                             
049100                                W-IDDISTR-GMT-MIN                         
049200                                W-IDDISTR-GMT-MAX                         
049300         MOVE JA TO DKO-INGANG-SW                                         
049400       ELSE                                                               
049500         MOVE NEJ TO NYCKLAR-SW                                           
049600       END-IF                                                             
049700     END-IF                                                               
049800                                                                          
049900     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
050000       MOVE REQU-IDDISTR-KEY      TO TEST-IDDISTR                         
050100       IF DIST79-DEALER-PRICE                                             
050200          MOVE 'DEALERPRICE'      TO RESP-TEDDI                           
050300       ELSE                                                               
050400          MOVE SPACE              TO RESP-TEDDI                           
050500       END-IF                                                             
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000 CC-KOLLA-IDKUNDNR SECTION.                                               
051100                                                                          
051200     IF REQU-IDPRODNR-KEY       = ALL '+'                                 
051300       IF REQU-IDKUNDNR-KEY NUMERIC                                       
051400         MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR-NUM                        
051500         MOVE WS-IDKUNDNR-NUM   TO W-SEQC-IDKUNDNR                        
051600                                   W-IDKUNDNR                             
051700                                   W-IDKUNDNR-GMT                         
051800       ELSE                                                               
051900         MOVE NEJ TO NYCKLAR-SW                                           
052000       END-IF                                                             
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
052500 CD-KOLLA-IDKUNDRF SECTION.                                               
052600                                                                          
052700     IF REQU-IDPRODNR-KEY          = ALL '+'                              
052800       IF REQU-IDKUNDRF-KEY(1:7)   NUMERIC AND                            
052900          REQU-IDKUNDRF-KEY(1:7)   > ZERO                                 
053000         MOVE REQU-IDKUNDRF-KEY(1:7) TO W-SEQC-IDKUNDRF                   
053100       ELSE                                                               
053200         MOVE NEJ                  TO NYCKLAR-SW                          
053300       END-IF                                                             
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800 CE-KOLLA-IDPRODNR SECTION.                                               
053900                                                                          
054000     IF REQU-IDPRODNR-KEY       NOT = ALL '+'                             
054100       IF REQU-IDPRODNR-KEY NUMERIC AND REQU-IDPRODNR-KEY > ZERO          
054200         MOVE REQU-IDPRODNR-KEY TO WS-IDPRODNR-NUM                        
054300         MOVE WS-IDPRODNR-NUM   TO W-Q3DSEQ-IDPRODNR-MIN                  
054400                                   W-Q3DSEQ-IDPRODNR-MAX                  
054500                                   W-ODEL-IDPRODNR-MIN                    
054600                                   W-ODEL-IDPRODNR-MAX                    
054700         MOVE JA   TO PROD-INGANG-SW                                      
054800         MOVE NEJ  TO DKO-INGANG-SW                                       
054900       ELSE                                                               
055000         MOVE NEJ TO NYCKLAR-SW                                           
055100       END-IF                                                             
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500                                                                          
055600 D-LAES-BASEN SECTION.                                                    
055700                                                                          
055800     MOVE JA TO ALLT-SW                                                   
055900     IF NOT PROD-INGANG                                                   
056000       PERFORM IMS-GU-ORQI01-M-Q2CSEQ                                     
056100       IF SEGMENT-FINNS                                                   
056200         IF REQU-IDUSER(1:2) = 'V0'                                       
056300         OR REQU-IDUSER(1:2) = 'VX'                                       
056400         OR REQU-IDUSER(1:3) = 'PCC'                                      
056500           MOVE OHUV-IDORDER    TO RESP-IDORDER                           
056600         ELSE                                                             
056700           MOVE ZERO            TO RESP-IDORDER                           
056800         END-IF                                                           
056900         MOVE OHUV-IDORDER      TO W-ODEL-IDORDER-MIN                     
057000                                   W-ODEL-IDORDER-MAX                     
057100         PERFORM IMS-GU-ORQA01                                            
057200         PERFORM DB-KOLLA-BEHOERIGHET                                     
057300         IF ALLT-OK                                                       
057400           PERFORM DA-LAES-OCH-REDIGERA                                   
057500         END-IF                                                           
057600       ELSE                                                               
057700         MOVE ZERO              TO RESP-IDORDER                           
057800         MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR                      
057900         MOVE '025'             TO RESP-IDMSG-ERROR                       
058000         MOVE NEJ               TO ALLT-SW                                
058100       END-IF                                                             
058200     ELSE                                                                 
058300       PERFORM IMS-GU-ORQA01-M-Q3DSEQ                                     
058400       IF SEGMENT-FINNS                                                   
058500         MOVE ODEL-IDORDER  TO W-ODEL-IDORDER-MIN                         
058600                               W-ODEL-IDORDER-MAX                         
058700         IF REQU-IDUSER(1:2) = 'V0'                                       
058800         OR REQU-IDUSER(1:2) = 'VX'                                       
058900         OR REQU-IDUSER(1:3) = 'PCC'                                      
059000           MOVE ODEL-IDORDER    TO RESP-IDORDER                           
059100         ELSE                                                             
059200           MOVE ZERO            TO RESP-IDORDER                           
059300         END-IF                                                           
059400         MOVE ODEL-IDDISTR  TO HELP-IDDISTR                               
059500                               W-SEQC-IDDISTR                             
059600         MOVE HELP-IDDISTR  TO WS-IDDISTR                                 
059700                               RESP-IDDISTR-KEY                           
059800         MOVE HELP-IDDISTR  TO WS-IDDISTR-NUM                             
059900         MOVE WS-IDDISTR-NUM TO   W-IDDISTR-GMT                           
060000                                  W-IDDISTR-GMT-MIN                       
060100                                  W-IDDISTR-GMT-MAX                       
060200         INSPECT  RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE        
060300         MOVE ODEL-IDKUNDNR TO HELP-IDKUNDNR                              
060400                               W-SEQC-IDKUNDNR                            
060500                               WS-IDKUNDNR-NUM                            
060600         MOVE WS-IDKUNDNR-NUM TO  W-IDKUNDNR-GMT                          
060700         MOVE HELP-IDKUNDNR TO RESP-IDKUNDNR-KEY                          
060800         INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE        
060900         MOVE ODEL-IDKUNDRF TO W-SEQC-IDKUNDRF                            
061000         MOVE ODEL-IDORDNR7(1:7) TO RESP-IDKUNDRF-KEY                     
061100                                                                          
061200         INSPECT RESP-IDKUNDRF-KEY REPLACING LEADING ZERO BY SPACE        
061300         MOVE ODEL-IDDC     TO W-ARB-IDDC                                 
061400         PERFORM IMS-GU-ORQI01-M-Q2CSEQ                                   
061500         IF SEGMENT-FINNS                                                 
061600           PERFORM DB-KOLLA-BEHOERIGHET                                   
061700           IF ALLT-OK                                                     
061800             PERFORM DA-LAES-OCH-REDIGERA                                 
061900           END-IF                                                         
062000         ELSE                                                             
062100           MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR                    
062200           MOVE '025'             TO RESP-IDMSG-ERROR                     
062300           MOVE NEJ               TO ALLT-SW                              
062400         END-IF                                                           
062500       ELSE                                                               
062600         MOVE ZERO              TO RESP-IDORDER                           
062700         MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR                      
062800         MOVE '025'             TO RESP-IDMSG-ERROR                       
062900         MOVE NEJ               TO ALLT-SW                                
063000       END-IF                                                             
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 DA-LAES-OCH-REDIGERA SECTION.                                            
063500                                                                          
063600     IF OHUV-FLBORT = 'J'                                                 
063700       PERFORM IMS-GNP-ORQI12                                             
063800       PERFORM DAA-KOLLA-ORQI12-SEGMENT                                   
063900     ELSE                                                                 
064000       IF OHUV-KDTPOTYP = ZERO                                            
064100         IF OHUV-FLKLAR = 'N'                                             
064200           MOVE INF-ORDER-EJ-AVSLUT TO    RESP-IDMSG-ERROR                
064300         END-IF                                                           
064400         IF OHUV-FLORDSPE = JA AND OHUV-IDSYSTEM NOT = 'W216'             
064500           IF OHUV-IDSYSTEM = 'SOFT'                                      
064600             MOVE SOFT-MED (SPRAK-IX)   TO RESP-INTERN                    
064700           ELSE                                                           
064800             MOVE INTERN-MED (SPRAK-IX) TO RESP-INTERN                    
064900           END-IF                                                         
065000         ELSE                                                             
065100           MOVE SPACE                   TO RESP-INTERN                    
065200         END-IF                                                           
065300         IF REQU-IDUSER(1:2) = 'V0'                                       
065400         OR REQU-IDUSER(1:2) = 'VX'                                       
065500         OR REQU-IDUSER(1:3) = 'PCC'                                      
065600           MOVE OHUV-IDSYSTEM           TO RESP-IDSYSTEM                  
065700         ELSE                                                             
065800           MOVE ZERO                    TO RESP-IDSYSTEM                  
065900         END-IF                                                           
066000         MOVE    OHUV-IDORDER TO W-ODEL-IDORDER-MIN                       
066100                                 W-ODEL-IDORDER-MAX                       
066200         PERFORM DAB-REDIGERA-ORDERHUVUDET                                
066300         IF ODEL-IDDC-EXP NOT = SPACE                                     
066400*           *BOUNCE ORDER                                                 
066500            PERFORM IMS-GNP-ORQI12                                        
066600         ELSE                                                             
066700            PERFORM IMS-GNP-ORQI12-KVAL                                   
066800         END-IF                                                           
066900         IF SEGMENT-FINNS                                                 
067100           PERFORM DAC-REDIGERA-ARBETSTABELLEN                            
067200         ELSE                                                             
067400           PERFORM DAD-ARBETSTABELL-SAKNAS                                
067500         END-IF                                                           
067600       ELSE                                                               
067700         MOVE '025'                 TO RESP-IDMSG-ERROR                   
067800         MOVE 'IDORDNR'             TO RESP-IDELMT-ERROR                  
067900         MOVE NEJ                   TO ALLT-SW                            
068000       END-IF                                                             
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400 DAA-KOLLA-ORQI12-SEGMENT SECTION.                                        
068500                                                                          
068600     IF SEGMENT-SAKNAS                                                    
068700       MOVE '025'                   TO    RESP-IDMSG-ERROR                
068800       MOVE 'IDORDNR'               TO    RESP-IDELMT-ERROR               
068900     ELSE                                                                 
069000       MOVE INF-ORDER-ANNULLED      TO    RESP-IDMSG-ERROR                
069100     END-IF                                                               
069200     MOVE NEJ                       TO    ALLT-SW                         
069300     .                                                                    
069400     EJECT                                                                
069500 DAB-REDIGERA-ORDERHUVUDET SECTION.                                       
069600                                                                          
069700     MOVE 35 TO TRAUTF8-KVMAXTL                                           
069800     MOVE OHUV-KDORDKL       TO  RESP-KDORDKL                             
069900     MOVE OHUV-KDFAKTYP      TO  RESP-KDFAKTYP                            
070000     PERFORM IMS-GU-GMTA-WDB201                                           
070100     IF SEGMENT-FINNS                                                     
070200      IF NDC-CN                                                           
070300        MOVE '935'                TO TRAUTF8-KDCP                         
070400        IF GMT-BEGMT-OVR NOT = SPACES                                     
070500          MOVE GMT-BEGMT-OVR-RAD1 TO TRAUTF8-TECONV-FROM                  
070600          CALL WTRAUTF8    USING TRAUTF8-AREA                             
070700          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                     
070800                                                                          
070900          MOVE GMT-BEGMT-OVR-RAD2 TO TRAUTF8-TECONV-FROM                  
071000          CALL WTRAUTF8    USING TRAUTF8-AREA                             
071100          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                     
071200                                                                          
071300          MOVE GMT-ADGMT-OVR-GATA    TO TRAUTF8-TECONV-FROM               
071400          CALL WTRAUTF8    USING TRAUTF8-AREA                             
071500          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-GATA                     
071600                                                                          
071700          MOVE GMT-ADGMT-OVR-PADR    TO TRAUTF8-TECONV-FROM               
071800          CALL WTRAUTF8    USING TRAUTF8-AREA                             
071900          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-PADR                     
072000                                                                          
072100          MOVE GMT-ADGMT-OVR-LAND    TO TRAUTF8-TECONV-FROM               
072200          CALL WTRAUTF8    USING TRAUTF8-AREA                             
072300          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-LAND                     
072400                                                                          
072500        ELSE                                                              
072600          MOVE GMT-ADGMT-OVR-GATA    TO TRAUTF8-TECONV-FROM               
072700          CALL WTRAUTF8    USING TRAUTF8-AREA                             
072800          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                     
072900                                                                          
073000          MOVE GMT-ADGMT-OVR-PADR    TO TRAUTF8-TECONV-FROM               
073100          CALL WTRAUTF8    USING TRAUTF8-AREA                             
073200          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                     
073300                                                                          
073400          MOVE ALL   X'20'           TO RESP-ADGMT-PADR                   
073500                                        RESP-ADGMT-GATA                   
073600                                        RESP-ADGMT-LAND                   
073700        END-IF                                                            
073800      ELSE                                                                
073900        MOVE '278'                TO TRAUTF8-KDCP                         
074000        IF OHUV-BEGMT      NOT = SPACES                                   
074100          MOVE OHUV-BEGMT-RAD1    TO  TRAUTF8-TECONV-FROM                 
074200          CALL WTRAUTF8    USING TRAUTF8-AREA                             
074300          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                     
074400          MOVE OHUV-BEGMT-RAD2    TO TRAUTF8-TECONV-FROM                  
074500          CALL WTRAUTF8    USING TRAUTF8-AREA                             
074600          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                     
074700          MOVE OHUV-ADGMT-GATA    TO TRAUTF8-TECONV-FROM                  
074800          CALL WTRAUTF8    USING TRAUTF8-AREA                             
074900          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-GATA                     
075000          MOVE OHUV-ADGMT-PADR    TO TRAUTF8-TECONV-FROM                  
075100          CALL WTRAUTF8    USING TRAUTF8-AREA                             
075200          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-PADR                     
075300          MOVE OHUV-ADGMT-LAND    TO TRAUTF8-TECONV-FROM                  
075400          CALL WTRAUTF8    USING TRAUTF8-AREA                             
075500          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-LAND                     
075600        ELSE                                                              
075700          MOVE OHUV-ADGMT-GATA       TO TRAUTF8-TECONV-FROM               
075800          CALL WTRAUTF8    USING TRAUTF8-AREA                             
075900          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                     
076000                                                                          
076100          MOVE OHUV-ADGMT-PADR       TO TRAUTF8-TECONV-FROM               
076200          CALL WTRAUTF8    USING TRAUTF8-AREA                             
076300          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                     
076400                                                                          
076500          MOVE ALL   X'20'           TO RESP-ADGMT-PADR                   
076600                                        RESP-ADGMT-GATA                   
076700                                        RESP-ADGMT-LAND                   
076800        END-IF                                                            
076900      END-IF                                                              
077000     ELSE                                                                 
077100        MOVE '278'                TO TRAUTF8-KDCP                         
077200        IF OHUV-BEGMT      NOT = SPACES                                   
077300          MOVE OHUV-BEGMT-RAD1    TO  TRAUTF8-TECONV-FROM                 
077400          CALL WTRAUTF8    USING TRAUTF8-AREA                             
077500          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                     
077600          MOVE OHUV-BEGMT-RAD2    TO TRAUTF8-TECONV-FROM                  
077700          CALL WTRAUTF8    USING TRAUTF8-AREA                             
077800          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                     
077900          MOVE OHUV-ADGMT-GATA    TO TRAUTF8-TECONV-FROM                  
078000          CALL WTRAUTF8    USING TRAUTF8-AREA                             
078100          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-GATA                     
078200          MOVE OHUV-ADGMT-PADR    TO TRAUTF8-TECONV-FROM                  
078300          CALL WTRAUTF8    USING TRAUTF8-AREA                             
078400          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-PADR                     
078500          MOVE OHUV-ADGMT-LAND    TO TRAUTF8-TECONV-FROM                  
078600          CALL WTRAUTF8    USING TRAUTF8-AREA                             
078700          MOVE TRAUTF8-TECONV-TO   TO RESP-ADGMT-LAND                     
078800        ELSE                                                              
078900          MOVE OHUV-ADGMT-GATA       TO TRAUTF8-TECONV-FROM               
079000          CALL WTRAUTF8    USING TRAUTF8-AREA                             
079100          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD1                     
079200                                                                          
079300          MOVE OHUV-ADGMT-PADR       TO TRAUTF8-TECONV-FROM               
079400          CALL WTRAUTF8    USING TRAUTF8-AREA                             
079500          MOVE TRAUTF8-TECONV-TO   TO RESP-BEGMT-RAD2                     
079600                                                                          
079700          MOVE ALL   X'20'           TO RESP-ADGMT-PADR                   
079800                                        RESP-ADGMT-GATA                   
079900                                        RESP-ADGMT-LAND                   
080000       END-IF                                                             
080100     END-IF                                                               
080200     IF OHUV-IDKONTO > 0                                                  
080300       MOVE OHUV-IDKONTO     TO RESP-IDKONTO                              
080400     END-IF                                                               
080500                                                                          
080600     IF OHUV-IDANALYS > 0                                                 
080700       MOVE OHUV-IDANALYS    TO RESP-IDANALYS                             
080800     END-IF                                                               
080900                                                                          
081000     IF OHUV-IDKST > SPACE                                                
081100       MOVE OHUV-IDKST       TO RESP-IDKST                                
081200     END-IF                                                               
081300     .                                                                    
081400     EJECT                                                                
081500 DAC-REDIGERA-ARBETSTABELLEN SECTION.                                     
081600                                                                          
081700     MOVE   ARB-KDFRAKT            TO  RESP-KDFRAKT                       
081800     MOVE   ARB-TIHHMM             TO  WS-TID-N                           
081900     MOVE   WS-TID-X(2:2)          TO  WS-TID-HH                          
082000     MOVE   WS-TID-X(4:2)          TO  WS-TID-MM                          
082100     MOVE   WS-TID-RED-X           TO  RESP-TIHHMM                        
082200     MOVE   ARB-DATRPAVD (3:6)     TO  RESP-TIAAMMDD                      
082300     MOVE   ARB-IDTRP              TO  RESP-IDTRP                         
082400                                                                          
082500     IF (ARB-KDTRPKAT = 'B' OR 'C') AND ARB-TIRFS = +0                    
082600       PERFORM IMS-GU-GMTA-WDB201                                         
082700       IF SEGMENT-FINNS                                                   
082800         MOVE GMT-IDPARTNR            TO W-WDB1-IDPARTNR                  
082900         MOVE GMT-IDFTG               TO W-WDB1-IDFTG                     
083000         PERFORM IMS-GU-BETC-WDB101                                       
083100         IF SEGMENT-FINNS                                                 
083200           AND W-WDB1-IDPARTNR NOT = SPACE                                
083300           AND W-WDB1-IDFTG    NOT = SPACE                                
083400           IF BET-KDKREDSP = '1'                                          
083500             MOVE SPAERR-MED (SPRAK-IX) TO RESP-INTERN                    
083600           ELSE                                                           
083700             MOVE SPACE                 TO RESP-INTERN                    
083800           END-IF                                                         
083900         ELSE                                                             
084000           MOVE SPACE                 TO RESP-INTERN                      
084100         END-IF                                                           
084200       END-IF                                                             
084300     END-IF                                                               
084400     .                                                                    
084500     EJECT                                                                
084600 DAD-ARBETSTABELL-SAKNAS SECTION.                                         
084700                                                                          
084800     PERFORM IMS-GNP-ORQI12-FIRST                                         
084900                                                                          
085000     IF SEGMENT-FINNS                                                     
085100       MOVE '025'                TO    RESP-IDMSG-ERROR                   
085200       MOVE 'IDRADNR'            TO    RESP-IDELMT-ERROR                  
085300     ELSE                                                                 
085400       MOVE 'IDORDNR'            TO    RESP-IDELMT-ERROR                  
085500       MOVE '025'                TO    RESP-IDMSG-ERROR                   
085600       MOVE NEJ                  TO    ALLT-SW                            
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000 DB-KOLLA-BEHOERIGHET SECTION.                                            
086100                                                                          
086200     MOVE REQU-IDUSER TO           SEC-IDUSER                             
086300*THIS PROGRAM IS A COPY OF W4050100 PROGRAM  AND MODIFIED FOR             
086400*LDC PROJECT                                                              
086500     MOVE '4501'            TO    SEC-IDTRANS                             
086600     IF REQU-IDDISTR-KEY = ALL '+'                                        
086700       MOVE '0000'            TO    SEC-IDKEY                             
086800     ELSE                                                                 
086900       MOVE REQU-IDDISTR-KEY  TO    SEC-IDKEY                             
087000     END-IF                                                               
087100                                                                          
087200     CALL WSECURIT          USING SEC-IDUSER                              
087300                                  SEC-IDTRANS                             
087400                                  SEC-IDKEY                               
087500                                  SEC-KDSVAR                              
087600                                                                          
087700     IF SEC-KDSVAR = OBEHORIG                                             
087800       MOVE ERR-OBEHORIG TO RESP-IDMSG-ERROR                              
087900       MOVE NEJ          TO ALLT-SW                                       
088000     ELSE                                                                 
088100        CONTINUE                                                          
088200     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088500 F-REDIGERA-BILDEN SECTION.                                               
088600                                                                          
088700     MOVE +1 TO INDX                                                      
088800     PERFORM IMS-GU-ORQA01                                                
088900     IF SEGMENT-FINNS                                                     
089000       MOVE ODEL-IDDC     TO ODEL-IDDC-SPAR                               
089100       MOVE ODEL-IDPRODNR TO ODEL-IDPRODNR-SPAR                           
089200       MOVE ODEL-IDLEVNR  TO ODEL-IDLEVNR-SPAR                            
089300       PERFORM FAA-REDIGERA-ORDERDELEN                                    
089400                                                                          
089500       MOVE +1 TO WS-COUNT                                                
089600                                                                          
089700       ADD +1 TO INDX                                                     
089800       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
089900         IF SEGMENT-FINNS                                                 
090000           IF INDX          NOT > MAX-INDX            AND                 
090100             (ODEL-IDDC     NOT = ODEL-IDDC-SPAR)     OR                  
090200             (ODEL-IDPRODNR NOT = ODEL-IDPRODNR-SPAR) OR                  
090300             (ODEL-IDLEVNR  NOT = ODEL-IDLEVNR-SPAR )                     
090400             PERFORM FAA-REDIGERA-ORDERDELEN                              
090500             ADD     +1 TO INDX                                           
090600                           WS-COUNT                                       
090700           END-IF                                                         
090800                                                                          
090900           IF ODEL-IDDC = REQU-IDDC-KEY                                   
091000             IF ODEL-KDODELSTA = 'R'                                      
091100               ADD ODEL-KVRADER  TO KVRADER-RAKNARE-WS                    
091200               ADD ODEL-SUORDV   TO SUORDV-RAKNARE-WS                     
091300               ADD ODEL-SUORDV-LOC TO SUORDV-LOC-RAKNARE-WS               
091400               ADD ODEL-SUORDV-LOCPREL                                    
091500                                 TO SUORDV-LOCPREL-RAKNARE-WS             
091600               ADD ODEL-VKORDNTO TO VKORDNTO-RAKNARE-WS                   
091700               ADD ODEL-VLORDNTO TO VLORDNTO-RAKNARE-WS                   
091800             END-IF                                                       
091900           ELSE                                                           
092000             ADD ODEL-KVRADER  TO KVRADER-RAKNARE-ALT                     
092100             ADD ODEL-SUORDV   TO SUORDV-RAKNARE-ALT                      
092200             ADD ODEL-SUORDV-LOC TO SUORDV-LOC-RAKNARE-ALT                
092300             ADD ODEL-SUORDV-LOCPREL                                      
092400                                 TO SUORDV-LOCPREL-RAKNARE-ALT            
092500             ADD ODEL-VKORDNTO TO VKORDNTO-RAKNARE-ALT                    
092600             ADD ODEL-VLORDNTO TO VLORDNTO-RAKNARE-ALT                    
092700           END-IF                                                         
092800           IF ODEL-KDODELSTA NOT = 'R'       AND                          
092900             (ODEL-IDDC          =  REQU-IDDC-KEY OR                      
093000              ODEL-IDDC-EXP      =  REQU-IDDC-KEY) AND                    
093100              ODEL-IDPRODNR  NOT =  VORD-IDPRODNR-SPAR                    
093200             MOVE ODEL-IDPRODNR TO W-VORD-IDPRODNR                        
093300             PERFORM IMS-GU-WDE601                                        
093400             IF SEGMENT-FINNS                                             
093500               IF VORD-IDDC = REQU-IDDC-KEY OR                            
093600                  VORD-IDDC-EXP = REQU-IDDC-KEY                           
093700                 IF VORD-IDDC-EXP = SPACE OR                              
093800                    VORD-IDDC-EXP = WC-CDC-SE                             
093900*                   *NORMAL ORDER OR BOUNCE AT DC 11                      
094000                    PERFORM FAD-HAMTA-SUORDV                              
094100                 END-IF                                                   
094200                 IF VORD-IDDC-EXP NOT = SPACE AND                         
094300                    VORD-IDDC-EXP NOT = 11                                
094400*                   *BOUNCE VOR                                           
094500                    PERFORM FAE-HAMTA-SUORDV-VOR                          
094600                 END-IF                                                   
094700                 ADD  VORD-KVORDRAD   TO KVRADER-RAKNARE-WS               
094800                 ADD  VORD-SUORDV-LOC TO SUORDV-LOC-RAKNARE-WS            
094900                 ADD  VORD-SUORDV-LOCPREL                                 
095000                                      TO SUORDV-LOCPREL-RAKNARE-WS        
095100                 ADD  VORD-VKORDNTO   TO VKORDNTO-RAKNARE-WS              
095200                 ADD  VORD-VLORDNTO   TO VLORDNTO-RAKNARE-WS              
095300                 PERFORM FAC-REDIGERA-KOLLIREGISTER                       
095400                 MOVE   VORD-IDPRODNR TO ODEL-IDPRODNR-SPAR               
095500                                         VORD-IDPRODNR-SPAR               
095600               END-IF                                                     
095700             END-IF                                                       
095800           END-IF                                                         
095900           PERFORM IMS-GN-ORQA01                                          
096000         END-IF                                                           
096100       END-PERFORM                                                        
096110                                                                          
096200       IF KVRADER-RAKNARE-WS > +0                                         
096300         PERFORM S01-REDIGERA-RAKNARE                                     
096400       ELSE                                                               
096500         MOVE    JA TO ARB-SW                                             
096602         PERFORM FAB-LAES-ORQI11-OCH-ORQI21                               
096700         IF KVRADER-RAKNARE-WS  = +0 AND                                  
096800            KVRADER-RAKNARE-ALT = +0                                      
096900           MOVE '025'                  TO RESP-IDMSG-ERROR                
097000           MOVE 'IDRADNR'              TO RESP-IDELMT-ERROR               
097100           MOVE NEJ                    TO ARB-SW                          
097200         END-IF                                                           
097300         IF KVRADER-RAKNARE-WS      = +0 AND                              
097400            KVRADER-RAKNARE-ALT NOT = +0                                  
097500           MOVE '041'                TO    RESP-IDMSG-ERROR               
097600           MOVE 'DC'                 TO    RESP-IDELMT-ERROR              
097700           MOVE NEJ                  TO ARB-SW                            
097800         END-IF                                                           
097900       END-IF                                                             
098000                                                                          
098100       MOVE WS-COUNT TO RESP-KVRADER                                      
098200                                                                          
098300     ELSE                                                                 
098402       PERFORM FAB-LAES-ORQI11-OCH-ORQI21                                 
098500       IF KVRADER-RAKNARE-WS  = +0 AND                                    
098600          KVRADER-RAKNARE-ALT = +0                                        
098700          MOVE '025'                 TO RESP-IDMSG-ERROR                  
098800          MOVE 'IDRADNR'             TO RESP-IDELMT-ERROR                 
098900         MOVE NEJ                    TO ARB-SW                            
099000       END-IF                                                             
099100       IF KVRADER-RAKNARE-WS      = +0 AND                                
099200          KVRADER-RAKNARE-ALT NOT = +0                                    
099300         MOVE '041'                TO    RESP-IDMSG-ERROR                 
099400         MOVE 'DC'                 TO    RESP-IDELMT-ERROR                
099500         MOVE NEJ                  TO ARB-SW                              
099600       END-IF                                                             
099700     END-IF                                                               
099800     IF ARB-OK                  AND                                       
099900        KVRADER-RAKNARE-WS > +0                                           
100000       PERFORM S01-REDIGERA-RAKNARE                                       
100100     END-IF                                                               
100200     .                                                                    
100300     EJECT                                                                
100400 FAA-REDIGERA-ORDERDELEN SECTION.                                         
100500                                                                          
100600     MOVE      ODEL-IDDC     TO RESP-IDDC-RAD    (INDX)                   
100700                                ODEL-IDDC-SPAR                            
100800*                               DDGS-WS-IDDC                              
100900     MOVE      ODEL-IDPRODNR TO RESP-IDPRODNR-RAD (INDX)                  
101000                                ODEL-IDPRODNR-SPAR                        
101100     IF ODEL-IDLEVNR NOT = SPACE                                          
101200       MOVE    ODEL-IDLEVNR  TO RESP-IDLEVNR-RAD  (INDX)                  
101300                                ODEL-IDLEVNR-SPAR                         
101400     ELSE                                                                 
101500       MOVE    SPACE         TO ODEL-IDLEVNR-SPAR                         
101600       MOVE    SPACE         TO RESP-IDLEVNR-RAD  (INDX)                  
101700     END-IF                                                               
101800*    IF DDGS-GOOD-DDC                                                     
101900*      MOVE ODEL-IDDC        TO W-DIRL-IDDC                               
102000*      MOVE ODEL-IDLEVNR     TO W-DIRL-IDLEVNR                            
102100*      PERFORM IMS-GNP-ORQI11-UNIK                                        
102200*      IF SEGMENT-FINNS                                                   
102300*        MOVE DIRL-TISKEPPN-DDC TO RESP-TISKEPPN-DDC (INDX)               
102400*      END-IF                                                             
102500*    ELSE                                                                 
102600       MOVE ZERO                TO RESP-TISKEPPN-DDC (INDX)               
102700*    END-IF                                                               
102800     IF ODEL-IDDC = REQU-IDDC-KEY                                         
102900       IF ODEL-IDDC-EXP NOT = SPACE                                       
103000          IF ODEL-IDDC-EXP = WC-CDC-SE                                    
103100*            *BOUNCE ORDER REFILL                                         
103200             MOVE ODEL-KDVALISO TO WS-KDVALISO                            
103300             MOVE WS-TEDDI      TO RESP-TEDDI                             
103400          ELSE                                                            
103500*            *BOUNCE ORDER VOR                                            
103600             MOVE WS-SEK        TO WS-KDVALISO                            
103700             MOVE WS-TEDDI      TO RESP-TEDDI                             
103800          END-IF                                                          
103900       ELSE                                                               
104000          MOVE ODEL-KDVALISO    TO WS-KDVALISO                            
104100          MOVE WS-TEDDI         TO RESP-TEDDI                             
104200       END-IF                                                             
104300     END-IF                                                               
104400     .                                                                    
104500     EJECT                                                                
104602 FAB-LAES-ORQI11-OCH-ORQI21 SECTION.                                      
104700                                                                          
104800     PERFORM FABA-LAES-ORQI11                                             
104900                                                                          
105000     IF ARB-TIRFS    = ZERO AND                                           
105100        ARB-DATRPAVD = ZERO AND                                           
105200        ARB-TIHHMM   = ZERO                                               
105300       MOVE INF-TRANSPORT-EJ-BOKAD TO RESP-IDMSG-ERROR                    
105400     END-IF                                                               
105500                                                                          
105600     MOVE ARB-IDDC TO W-ARB-IDDC                                          
105700     IF ARB-IDDC = REQU-IDDC-KEY                                          
105800       PERFORM IMS-GNP-ORQI21                                             
105900                                                                          
106000       PERFORM UNTIL SEGMENT-SAKNAS                                       
106100                                                                          
106200          ADD LOR-KVRADER        TO KVRADER-RAKNARE-WS                    
106300          ADD LOR-SUORDV         TO SUORDV-RAKNARE-WS                     
106400          ADD LOR-SUORDV-LOC     TO SUORDV-LOC-RAKNARE-WS                 
106500          ADD LOR-SUORDV-LOCPREL TO SUORDV-LOCPREL-RAKNARE-WS             
106600          ADD LOR-VKORDNTO       TO VKORDNTO-RAKNARE-WS                   
106700          ADD LOR-VLORDNTO       TO VLORDNTO-RAKNARE-WS                   
106800                                                                          
106900          PERFORM IMS-GNP-ORQI21                                          
107000                                                                          
107100       END-PERFORM                                                        
107200                                                                          
107300     ELSE                                                                 
107400       PERFORM IMS-GNP-ORQI21                                             
107500                                                                          
107600       PERFORM UNTIL SEGMENT-SAKNAS                                       
107700                                                                          
107800         ADD LOR-KVRADER        TO KVRADER-RAKNARE-ALT                    
107900         ADD LOR-SUORDV         TO SUORDV-RAKNARE-ALT                     
108000         ADD LOR-SUORDV-LOC     TO SUORDV-LOC-RAKNARE-ALT                 
108100         ADD LOR-SUORDV-LOCPREL TO SUORDV-LOCPREL-RAKNARE-ALT             
108200         ADD LOR-VKORDNTO       TO VKORDNTO-RAKNARE-ALT                   
108300         ADD LOR-VLORDNTO       TO VLORDNTO-RAKNARE-ALT                   
108400                                                                          
108500         PERFORM IMS-GNP-ORQI21                                           
108600                                                                          
108700       END-PERFORM                                                        
108800                                                                          
108900     END-IF                                                               
111000     .                                                                    
111100     EJECT                                                                
111200 FABA-LAES-ORQI11 SECTION.                                                
111300                                                                          
111400     PERFORM IMS-GNP-ORQI11-FIRST                                         
111500     IF SEGMENT-FINNS                                                     
111600       PERFORM UNTIL SEGMENT-SAKNAS                                       
111700         ADD DIRL-KVRADER  TO KVRADER-RAKNARE-WS                          
111800         ADD DIRL-SUORDV   TO SUORDV-RAKNARE-WS                           
111900         ADD DIRL-SUORDV-LOC  TO SUORDV-LOC-RAKNARE-WS                    
112000         ADD DIRL-SUORDV-LOCPREL  TO SUORDV-LOCPREL-RAKNARE-WS            
112100         ADD DIRL-VKORDNTO TO VKORDNTO-RAKNARE-WS                         
112200         ADD DIRL-VLORDNTO TO VLORDNTO-RAKNARE-WS                         
112300         PERFORM IMS-GNP-ORQI11                                           
112400       END-PERFORM                                                        
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800 FAC-REDIGERA-KOLLIREGISTER SECTION.                                      
112900                                                                          
113000     ADD VORD-KVKOLLI-FAKT    TO    KVKOLLI-FAKT-RAKNARE                  
113100     ADD VORD-KVKOLLI-LAST    TO    KVKOLLI-LAST-RAKNARE                  
113200     ADD VORD-KVKOLPAC        TO    KVKOLPAC-RAKNARE                      
113300     ADD VORD-KVORDRAD-PACK   TO    KVORDRAD-PACK-RAKNARE                 
113400     ADD VORD-VKORDBTO        TO    VKORDBTO-RAKNARE                      
113500     ADD VORD-VLORDBTO        TO    VLORDBTO-RAKNARE                      
113600     IF VORD-KVORDRAD = VORD-KVORDRAD-PACK AND                            
113700        VORD-KVKOLLI  = VORD-KVKOLPAC                                     
113800       CONTINUE                                                           
113900     ELSE                                                                 
114000       MOVE INF-ORDER-EJ-KLAR TO    RESP-IDMSG-ERROR                      
114100     END-IF                                                               
114200     .                                                                    
114300     EJECT                                                                
114400 FAD-HAMTA-SUORDV           SECTION.                                      
114500                                                                          
114600     IF VORD-IDDC = REQU-IDDC-KEY AND                                     
114700        VORD-SUORDV-EXP > 0                                               
114800*       *SUORDV-EXP BASED ON PRAVCOST.                                    
114900        ADD VORD-SUORDV-EXP TO SUORDV-EXP-RAKNARE-WS                      
115000        MOVE VORD-KDVALISO-EXP TO WS-KDVALISO                             
115100        MOVE WS-TEDDI     TO RESP-TEDDI                                   
115200     ELSE                                                                 
115300        IF VORD-IDDC-EXP NOT = SPACE                                      
115400*         *Export order, show only if order invoiced                      
115500          IF VORD-KVKOLLI-FAKT > 0                                        
115600            ADD  VORD-SUORDV  TO SUORDV-RAKNARE-WS                        
115700            MOVE VORD-KDVALISO TO WS-KDVALISO                             
115800            MOVE WS-TEDDI      TO RESP-TEDDI                              
115900          ELSE                                                            
116000            MOVE SPACE         TO WS-KDVALISO                             
116100            MOVE WS-TEDDI      TO RESP-TEDDI                              
116200          END-IF                                                          
116300        ELSE                                                              
116400          ADD  VORD-SUORDV   TO SUORDV-RAKNARE-WS                         
116500          MOVE VORD-KDVALISO TO WS-KDVALISO                               
116600          MOVE WS-TEDDI      TO RESP-TEDDI                                
116700        END-IF                                                            
116800     END-IF                                                               
116900     .                                                                    
117000     EJECT                                                                
117100 FAE-HAMTA-SUORDV-VOR       SECTION.                                      
117200                                                                          
117300     IF VORD-IDDC = REQU-IDDC-KEY                                         
117400*       *SUORDV BASED ON PRARTNTO (DC=11)                                 
117500        ADD VORD-SUORDV     TO SUORDV-EXP-RAKNARE-WS                      
117600        MOVE WS-SEK         TO WS-KDVALISO                                
117700        MOVE WS-TEDDI       TO RESP-TEDDI                                 
117800     ELSE                                                                 
117900        IF VORD-IDDC-EXP = REQU-IDDC-KEY                                  
118000*          *Bounce-dc wants to see the order                              
118100          IF VORD-KVKOLLI-FAKT > 0                                        
118200*          *SHOW ONLY IF ORDER IS INVOICED                                
118300            ADD  VORD-SUORDV-EXP   TO SUORDV-RAKNARE-WS                   
118400            MOVE VORD-KDVALISO-EXP TO WS-KDVALISO                         
118500            MOVE WS-TEDDI          TO RESP-TEDDI                          
118600          ELSE                                                            
118700            MOVE SPACE         TO WS-KDVALISO                             
118800            MOVE WS-TEDDI      TO RESP-TEDDI                              
118900          END-IF                                                          
119000        END-IF                                                            
119100     END-IF                                                               
119200                                                                          
119300     .                                                                    
119400     EJECT                                                                
119500 S01-REDIGERA-RAKNARE SECTION.                                            
119600                                                                          
119700     IF KVRADER-RAKNARE-WS    > ZERO                                      
119800       MOVE KVRADER-RAKNARE-WS     TO RESP-KVORDRAD                       
119900     ELSE                                                                 
120000       MOVE ZERO                   TO RESP-KVORDRAD                       
120100     END-IF                                                               
120200                                                                          
120300     IF DIST79-DEALER-PRICE                                               
120400                                                                          
120500        PERFORM S01A-RED-RAKNARE-DIST79                                   
120600                                                                          
120700     ELSE                                                                 
120800       IF SUORDV-RAKNARE-WS     > ZERO                                    
120900         IF SEC-KDSVAR = 2 OR 6                                           
121000           MOVE ZERO                 TO RESP-SUORDV                       
121100         ELSE                                                             
121200           MOVE SUORDV-RAKNARE-WS    TO RESP-SUORDV                       
121300         END-IF                                                           
121400       ELSE                                                               
121500         IF SUORDV-EXP-RAKNARE-WS > 0                                     
121600           IF SEC-KDSVAR = 2 OR 6                                         
121700             MOVE ZERO                   TO RESP-SUORDV                   
121800           ELSE                                                           
121900             MOVE SUORDV-EXP-RAKNARE-WS  TO RESP-SUORDV                   
122000           END-IF                                                         
122100         ELSE                                                             
122200           MOVE ZERO                   TO RESP-SUORDV                     
122300         END-IF                                                           
122400       END-IF                                                             
122500       MOVE ' '                      TO RESP-TEASTRIX                     
122600     END-IF                                                               
122700                                                                          
122800     IF VKORDNTO-RAKNARE-WS   > ZERO                                      
122900       IF WS-KDMATT = 'U'                                                 
123000         COMPUTE VKORDNTO-RAKNARE-WS =                                    
123100                 VKORDNTO-RAKNARE-WS * CONV-KG-TO-LB                      
123200                                                                          
123300         MOVE VKORDNTO-RAKNARE-WS  TO RESP-VKORDNTO                       
123400       ELSE                                                               
123500         MOVE VKORDNTO-RAKNARE-WS  TO RESP-VKORDNTO                       
123600       END-IF                                                             
123700     ELSE                                                                 
123800       MOVE ZERO                   TO RESP-VKORDNTO                       
123900     END-IF                                                               
124000                                                                          
124100     IF VLORDNTO-RAKNARE-WS   > ZERO                                      
124200       IF WS-KDMATT = 'U'                                                 
124300         COMPUTE VLORDNTO-RAKNARE-WS =                                    
124400                 VLORDNTO-RAKNARE-WS * CONV-M3-TO-FT3                     
124500                                                                          
124600         MOVE VLORDNTO-RAKNARE-WS  TO RESP-VLORDNTO                       
124700       ELSE                                                               
124800         MOVE VLORDNTO-RAKNARE-WS  TO RESP-VLORDNTO                       
124900       END-IF                                                             
125000     ELSE                                                                 
125100       MOVE ZERO                   TO RESP-VLORDNTO                       
125200     END-IF                                                               
125300                                                                          
125400     IF KVKOLLI-FAKT-RAKNARE  > ZERO                                      
125500       MOVE KVKOLLI-FAKT-RAKNARE   TO RESP-KVKOLLI-FAKT                   
125600     ELSE                                                                 
125700       MOVE ZERO                   TO RESP-KVKOLLI-FAKT                   
125800     END-IF                                                               
125900                                                                          
126000     IF KVKOLLI-LAST-RAKNARE  > ZERO                                      
126100       MOVE KVKOLLI-LAST-RAKNARE   TO RESP-KVKOLLI-LAST                   
126200     ELSE                                                                 
126300       MOVE ZERO                   TO RESP-KVKOLLI-LAST                   
126400     END-IF                                                               
126500                                                                          
126600     IF KVKOLPAC-RAKNARE      > ZERO                                      
126700       MOVE KVKOLPAC-RAKNARE       TO RESP-KVKOLPAC                       
126800     ELSE                                                                 
126900       MOVE ZERO                   TO RESP-KVKOLPAC                       
127000     END-IF                                                               
127100                                                                          
127200     IF KVORDRAD-PACK-RAKNARE > ZERO                                      
127300       MOVE KVORDRAD-PACK-RAKNARE  TO RESP-KVORDRAD-PACK                  
127400     ELSE                                                                 
127500       MOVE ZERO                   TO RESP-KVORDRAD-PACK                  
127600     END-IF                                                               
127700                                                                          
127800     IF VKORDBTO-RAKNARE      > ZERO                                      
127900       IF WS-KDMATT = 'U'                                                 
128000         COMPUTE VKORDBTO-RAKNARE =                                       
128100                 VKORDBTO-RAKNARE * CONV-KG-TO-LB                         
128200                                                                          
128300         MOVE VKORDBTO-RAKNARE     TO RESP-VKORDBTO                       
128400       ELSE                                                               
128500         MOVE VKORDBTO-RAKNARE     TO RESP-VKORDBTO                       
128600       END-IF                                                             
128700     ELSE                                                                 
128800       MOVE ZERO                   TO RESP-VKORDBTO                       
128900     END-IF                                                               
129000                                                                          
129100     IF VLORDBTO-RAKNARE      > ZERO                                      
129200       IF WS-KDMATT = 'U'                                                 
129300         COMPUTE VLORDBTO-RAKNARE =                                       
129400                 VLORDBTO-RAKNARE * CONV-M3-TO-FT3                        
129500                                                                          
129600         MOVE VLORDBTO-RAKNARE     TO RESP-VLORDBTO                       
129700       ELSE                                                               
129800         MOVE VLORDBTO-RAKNARE     TO RESP-VLORDBTO                       
129900       END-IF                                                             
130000     ELSE                                                                 
130100       MOVE ZERO                   TO RESP-VLORDBTO                       
130200     END-IF                                                               
130300     .                                                                    
130400     EJECT                                                                
130500 S01A-RED-RAKNARE-DIST79 SECTION.                                         
130600                                                                          
130700     COMPUTE HELP-SUMMA = SUORDV-LOC-RAKNARE-WS +                         
130800                          SUORDV-LOCPREL-RAKNARE-WS                       
130900     IF HELP-SUMMA     > ZERO                                             
131000       IF SEC-KDSVAR = 2 OR 6                                             
131100         MOVE ZERO                 TO RESP-SUORDV                         
131200       ELSE                                                               
131300         MOVE HELP-SUMMA           TO RESP-SUORDV                         
131400       END-IF                                                             
131500     ELSE                                                                 
131600       MOVE ZERO                   TO RESP-SUORDV                         
131700     END-IF                                                               
131800     IF SUORDV-LOCPREL-RAKNARE-WS = +0                                    
131900       MOVE ' '                    TO RESP-TEASTRIX                       
132000     ELSE                                                                 
132100       MOVE '*'                    TO RESP-TEASTRIX                       
132200     END-IF                                                               
132300     .                                                                    
132400     EJECT                                                                
132500                                                                          
132600*    --- DISPATCHER SECTIONS                                              
132700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
132800                                                                          
132900     MOVE 'GETARG'               TO SUB-KDFUNC                            
133000     MOVE 'CARPARTS.LDC.ORDERQUERYGENERAL'  TO SUB-ADDISPABS              
133100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
133200                                                                          
133300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
133400                                                                          
133500     IF SUB-KDRC > 0                                                      
133600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
133700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
133800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
133900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
134000     END-IF                                                               
134100     .                                                                    
134200     SKIP3                                                                
134300 S02-RETURN-RESPONSE SECTION.                                             
134400                                                                          
134500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
134600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
134700                                                                          
134800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
134900                                                                          
135000     IF SUB-KDRC > 0                                                      
135100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
135200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
135300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
135400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
135500     END-IF                                                               
135600     .                                                                    
135700     EJECT                                                                
135800* --- IMS SEKTIONER ---                                                   
135900 IMS-GU-ORQI01-M-Q2CSEQ SECTION.                                          
136000                                                                          
136100     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
136200          DELIMITED BY SIZE INTO SSA1                                     
136300     MOVE '  GE' TO GODK-STATUSKODER                                      
136400     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
136500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
136600     PERFORM IMS-STATUSKONTROLL                                           
136700     .                                                                    
136800     EJECT                                                                
136900*IMS-GNP-ORQI11-UNIK SECTION.                                             
137000*                                                                         
137100*    STRING 'WLORQI11*F(WDQ211KY =' W-WDQ211KY-X ')'                      
137200*         DELIMITED BY SIZE INTO SSA1                                     
137300*    MOVE '  GE' TO GODK-STATUSKODER                                      
137400*    CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
137500*    MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
137600*    PERFORM IMS-STATUSKONTROLL                                           
137700*    .                                                                    
137800*    SKIP2                                                                
137900 IMS-GNP-ORQI11-FIRST SECTION.                                            
138000                                                                          
138100     MOVE 'WLORQI11*F' TO SSA1                                            
138200     MOVE '  GE' TO GODK-STATUSKODER                                      
138300     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
138400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
138500     PERFORM IMS-STATUSKONTROLL                                           
138600     .                                                                    
138700     SKIP2                                                                
138800 IMS-GNP-ORQI11 SECTION.                                                  
138900                                                                          
139000     MOVE 'WLORQI11' TO SSA1                                              
139100     MOVE '  GE' TO GODK-STATUSKODER                                      
139200     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
139300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
139400     PERFORM IMS-STATUSKONTROLL                                           
139500     .                                                                    
139600     EJECT                                                                
139700 IMS-GNP-ORQI12-KVAL SECTION.                                             
139800                                                                          
139900     STRING 'WLORQI12(IDDC     =' W-IDDC-X    ')'                         
140000          DELIMITED BY SIZE INTO SSA1                                     
140100     MOVE '  GE' TO GODK-STATUSKODER                                      
140200     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
140300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     SKIP2                                                                
140700 IMS-GNP-ORQI12-FIRST SECTION.                                            
140800                                                                          
140900     MOVE   'WLORQI12*F'      TO SSA1                                     
141000     MOVE '  GE' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
141200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     SKIP2                                                                
141600 IMS-GNP-ORQI12 SECTION.                                                  
141700                                                                          
141800     MOVE 'WLORQI12' TO SSA1                                              
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
142100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     EJECT                                                                
142500 IMS-GNP-ORQI21 SECTION.                                                  
142600                                                                          
142700     STRING 'WLORQI12(IDDC     =' W-IDDC-X    ')'                         
142800          DELIMITED BY SIZE INTO SSA1                                     
142900     MOVE   'WLORQI21'        TO SSA2                                     
143000     MOVE '  GE' TO GODK-STATUSKODER                                      
143100     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-LOR SSA1 SSA2            
143200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
143300     PERFORM IMS-STATUSKONTROLL                                           
143400     .                                                                    
143500     EJECT                                                                
143600 IMS-GU-WDE601 SECTION.                                                   
143700                                                                          
143800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
143900          DELIMITED BY SIZE INTO SSA1                                     
144000     MOVE '  GE' TO GODK-STATUSKODER                                      
144100     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-VORD SSA1                 
144200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     EJECT                                                                
144600 IMS-GU-ORQA01 SECTION.                                                   
144700                                                                          
144800     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
144900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
145000          DELIMITED BY SIZE INTO SSA1                                     
145100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
145200     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
145300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
145400     PERFORM IMS-STATUSKONTROLL                                           
145500     .                                                                    
145600     EJECT                                                                
145700 IMS-GN-ORQA01 SECTION.                                                   
145800                                                                          
145900     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
146000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
146100          DELIMITED BY SIZE INTO SSA1                                     
146200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
146300     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
146400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
146500     PERFORM IMS-STATUSKONTROLL                                           
146600     .                                                                    
146700     EJECT                                                                
146800 IMS-GU-ORQA01-M-Q3DSEQ SECTION.                                          
146900                                                                          
147000     STRING 'WLORQA01(WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
147100                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
147200          DELIMITED BY SIZE INTO SSA1                                     
147300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
147400     CALL CBLTDLI USING GU ORQAD-PCB DLI-IO-AREA-ODEL SSA1                
147500     MOVE ORQAD-STATUS-CODE TO STATUS-WS                                  
147600     PERFORM IMS-STATUSKONTROLL                                           
147700     .                                                                    
147800     EJECT                                                                
147900 IMS-GU-GMTA-WDB201 SECTION.                                              
148000                                                                          
148100     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
148200          DELIMITED BY SIZE INTO SSA1                                     
148300     MOVE '  GE'              TO GODK-STATUSKODER                         
148400                                                                          
148500     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-GMTA SSA1                 
148600     MOVE GMTA-STATUS-CODE    TO STATUS-WS                                
148700     PERFORM IMS-STATUSKONTROLL                                           
148800     .                                                                    
148900     EJECT                                                                
149000 IMS-GU-BETC-WDB101 SECTION.                                              
149100                                                                          
149200     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
149300          DELIMITED BY SIZE INTO SSA1                                     
149400     MOVE '  GE'              TO GODK-STATUSKODER                         
149500                                                                          
149600     CALL CBLTDLI USING GU BETC-PCB DLI-IO-AREA-BETC SSA1                 
149700     MOVE BETC-STATUS-CODE    TO STATUS-WS                                
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     EJECT                                                                
150100 IMS-STATUSKONTROLL SECTION.                                              
150200                                                                          
150300     SET STATUS-IX TO 1                                                   
150400     SEARCH GODK-STATUS                                                   
150500       AT END CALL FELLOG                                                 
150600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
151000     END-SEARCH                                                           
160000     .                                                                    
