000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL014200.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/07/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.ORDERQUERYVALUE'                           
000800*                                                                         
000900*    FUNCTION:                                                            
001000*      4503 ÄR ETT FRÅGE-MPP.                                             
001100*      PROGRAMMET VISAR, PER DISTRIKT, KUND, ORDER OCH CLAGER,            
001200*      ORDERVÄRDE OCH ORDERRADER FÖR ORDERN. FAKTURANR OCH FAK-           
001300*      TURADATUM VISAS OM SÅDANA FINNS, MAX ÄR 12 ST OCH DE LÄGGS         
001400*      UT I KRONOLOGISK ORDNING PÅ SKÄRMEN (DEN ÄLDSTA FÖRST).            
001500*      FÖR ORDERVÄRDE VISAS EJ STATUS R UTAN DENNA BAKAS IN I             
001600*      TOTALEN, FÖR ORDERRADERNA BAKAS STATUS R OCH U IN I TOT-           
001700*      ALEN. FÖR ANTAL RADER FÖR STATUS P, F OCH L ÄR DET ANTAL           
001800*      ORDERRADER I KOLLISEGMENTET (WDE611) SOM SUMMERAS. DESSA           
001900*      'KOLLIRADER' KAN VARA FLER ÄN TOTALT REGISTRERAT ANTAL             
002000*      ORDERRADER FÖR ORDERN.                                             
002100*                                                                         
002200*        REGLER FÖR SUMMERING AV ORDERRADER PÅ KOLLISEGMENTEN:            
002300*                                                                         
002400*        KOLLISTATUS  INNEBÖRD                SUMMERAS TILL STATUS        
002500*                                                                         
002600*          1          FÄRDIGPACKAT KOLLI        P                         
002700*          6          FAKTURABEORDRAT           P                         
002800*                                                                         
002900*          2          LASTBEORDRAT              L                         
003000*          3          LASTAT                    L                         
003100*          4          LASTAT O. FAKT.BEORDRAT   L                         
003200*                                                                         
003300*          7          FAKTURERAT                F                         
003400*          8          FAKTURERAT O. LASTBEORD.  F                         
003500*          9          FAKTURERAT OCH LASTAT     F                         
003600*                     (LASTAT OCH FAKTURERAT)                             
003700*                                                                         
003800*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
003900*                              WLORQA (WDQ3)                              
004000*                              WDE6                                       
004400*                                                                         
004500*        WL014200 PROGRAM IS A REPLICA OF W4050300 PROGRAM                
004600*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
004700*                                                                         
004800*    INDATA.                                                              
004900*        TRANSACTION: WL0142T                                             
005000*        REQUEST:     WL0142I1                                            
005100*                                                                         
005200*    OUTDATA.                                                             
005300*        RESPONSE:    WL0142O1                                            
005400                                                                          
005500     SKIP3                                                                
005600 ENVIRONMENT DIVISION.                                                    
005700     SKIP2                                                                
005800 INPUT-OUTPUT SECTION.                                                    
005900                                                                          
006000 FILE-CONTROL.                                                            
006100     EJECT                                                                
006200 DATA DIVISION.                                                           
006300     SKIP3                                                                
006400 FILE SECTION.                                                            
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700*    -COPY WY2000W1                                                       
006800 77  IDPGM                       PIC X(08)   VALUE 'WL014200'.            
006900 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
007000                                                                          
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
007500 77  HJALP-MOD-IX                PIC S9(9)   VALUE +0   COMP SYNC.        
007600 77  HJALP-MOD-IX-MAX-12         PIC S9(9)   VALUE +12  COMP SYNC.        
007700 77  Y2K-IX                      PIC S9(2)   VALUE +0   COMP SYNC.        
007800 77  RKOD-ABEND-MED-DUMP         PIC S9(5)   VALUE +1000                  
007900                                                        COMP SYNC.        
008000                                                                          
008100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
008200 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
008300 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
008400                                                                          
008500 77  WS-IDTIDZON                 PIC X(2)    VALUE SPACE.                 
008600                                                                          
008700 77  SW-SKALL-VARDEN-LAGGAS-UT   PIC X       VALUE 'N'.                   
008800 77  SW-ORDER-KLAR               PIC X       VALUE 'J'.                   
008900 77  SW-FAKT-INFO-TAB-SORTERAD   PIC X       VALUE 'J'.                   
009000                                                                          
009100 77  WS-IDELMT-ERROR             PIC X(16).                               
009200 77  WS-IDMSG-ERROR              PIC X(03).                               
009300 77  WS-IDMSG-INFO               PIC X(03).                               
009310 77  WS-SEK                      PIC X(03)   VALUE 'SEK'.                 
009400                                                                          
009500 01  W-SPAR-IDKUNDRF.                                                     
009600     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
009700     03  FILLER                  PIC X(3)    VALUE '+++'.                 
009800                                                                          
009900*      --- VALID IDDC CODES                                               
010020*                                                                         
010030*01    -COPY WWDCKONS                                                     
010040       EJECT                                                              
010100 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
010200*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
010300*    ----DISTR-DEALER-PRICE-----                                          
010400     EJECT                                                                
010500                                                                          
010600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010700     88  NYCKLAR-OK                          VALUE 'J'.                   
010800     88  NYCKLAR-FEL                         VALUE 'N'.                   
010900                                                                          
011000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011100     88  ALLT-OK                             VALUE 'J'.                   
011200                                                                          
011210 77  AVERAGECOST-SW              PIC X       VALUE 'J'.                   
011220     88  AVERAGECOST                         VALUE 'J'.                   
011221                                                                          
011230 77  BOUNCE-SW                   PIC X       VALUE 'J'.                   
011240     88  BOUNCEORDER                         VALUE 'J'.                   
011300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
011400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
011500 77  KDRC-DISPLAY                PIC Z(5).                                
011600                                                                          
011700     EJECT                                                                
011800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011900 01  GENERAL-SUBPROGRAMS.                                                 
012000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012300     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
012700     SKIP3                                                                
012800*    --- PARAMETERS TO ABEND                                              
012900                                                                          
013000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013300     SKIP3                                                                
013400 01  MESSAGE-CODES.                                                       
013500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
013600     EJECT                                                                
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
013900     SKIP3                                                                
014000*01  -COPY WZ01SUB                                                        
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014300     SKIP3                                                                
014400 01  REQU-AREA.                                                           
014500*    03  -COPY WZ01REQU                                                   
014600*    03  -COPY WL0142I1                                                   
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
014900     SKIP3                                                                
015000 01  RESP-AREA.                                                           
015100*    03  -COPY WZ01RESP                                                   
015200*    03  -COPY WL0142O1                                                   
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
015500*   -COPY WSECAREA                                                        
015600     EJECT                                                                
016000*                                                                         
016100 01  FELMEDDELANDE-AREA.                                                  
016200     03  FILLER                  PIC X(16)   VALUE 'FELMEDD AREA'.        
016300     03  FELM-ORADER-SAKNAS-F-DC-028                                      
016400                                 PIC X(3)           VALUE '198'.          
016500     03  FELM-ORDER-ANNULLERAD-052                                        
016600                                 PIC X(3)           VALUE '197'.          
016700     03  FELM-ORDER-EJ-AVSLUTAD-053                                       
016800                                 PIC X(3)           VALUE '194'.          
016900     03  FELM-ORDER-KLAR-082     PIC X(3)           VALUE '203'.          
017000     03  FELM-NYCKEL-FEL-401     PIC X(3)           VALUE '043'.          
017100     03  FELM-UPPLYSTA-FALT-FEL-409                                       
017200                                 PIC X(3)           VALUE '409'.          
017300     03  FELM-ORDERINFO-BORTTAGEN-415                                     
017400                                 PIC X(3)           VALUE '196'.          
017500     03  FELM-ORDER-SAKNAS-701   PIC X(3)           VALUE '125'.          
017600     03  FELM-SYSTEM-ERROR       PIC X(3)           VALUE '099'.          
017700     SKIP2                                                                
017800*                                                                         
017900 01  ABEND-TEXT-AREA.                                                     
018000     03  FILLER                  PIC X(16)   VALUE                        
018100                                                 'ABENDTEXT AREA'.        
018200     03  ABTXT-E611-SEG-SAKNAS   PIC X(64)   VALUE                        
018300         'WDE611-SEG (KOLLI-SEG) SAKNAS FÖR LASTAD ELLER FAKTURERA        
018400-        'D ORDER.'.                                                      
018500     EJECT                                                                
018600*                                                                         
018700 01  KONSTANT-AREA.                                                       
018800     03  FILLER                  PIC  X(16) VALUE 'KONSTANT AREA'.        
018900     03  K-KDODELSTA-R           PIC  X(1)         VALUE 'R'.             
019000     03  K-KDODELSTA-U           PIC  X(1)         VALUE 'U'.             
019100     03  K-KDODELSTA-P           PIC  X(1)         VALUE 'P'.             
019200     03  K-KDORDSTA-2-PACKRAPP-STARTAD                                    
019300                                 PIC S9(1)         VALUE +2.              
019400     03  K-KDORDSTA-3-VORD-FARDIGPACKAD                                   
019500                                 PIC S9(1)         VALUE +3.              
019600     03  K-KDKOLSTA-1-FARDIG-PACKAT                                       
019700                                 PIC S9(1)  COMP-3 VALUE +1.              
019800     03  K-KDKOLSTA-2-LASTREL    PIC S9(1)  COMP-3 VALUE +2.              
019900     03  K-KDKOLSTA-3-LASTAT     PIC S9(1)  COMP-3 VALUE +3.              
020000     03  K-KDKOLSTA-4-LASTAT-FAKTREL                                      
020100                                 PIC S9(1)  COMP-3 VALUE +4.              
020200     03  K-KDKOLSTA-6-FAKTREL    PIC S9(1)  COMP-3 VALUE +6.              
020300     03  K-KDKOLSTA-7-FAKTURERAT PIC S9(1)  COMP-3 VALUE +7.              
020400     03  K-KDKOLSTA-8-FAKT-LASTREL                                        
020500                                 PIC S9(1)  COMP-3 VALUE +8.              
020600     03  K-KDKOLSTA-9-FAKT-LASTAT                                         
020700                                 PIC S9(1)  COMP-3 VALUE +9.              
020800     03  K-KDTRPKAT-A            PIC  X(1)         VALUE 'A'.             
020900     03  K-IDKUNDNR-UT-NOLL      PIC  X(6)         VALUE '     0'.        
021100     EJECT                                                                
021200*                                                                         
021300 01  SPAR-AREA.                                                           
021400     03  FILLER                  PIC X(16)  VALUE 'SPAR AREA'.            
021500     03  SPAR-KVRADER-TOT        PIC S9(5)   COMP-3 VALUE +0.             
021600     03  SPAR-KVRADER-P          PIC S9(5)   COMP-3 VALUE +0.             
021700     03  SPAR-KVRADER-F          PIC S9(5)   COMP-3 VALUE +0.             
021800     03  SPAR-KVRADER-L          PIC S9(5)   COMP-3 VALUE +0.             
021900     03  SPAR-SUORDV-TOT         PIC S9(9)V9(2)                           
022000                                             COMP-3 VALUE +0.             
022100     03  SPAR-SUORDV-TOT-LOC     PIC S9(9)V9(2)                           
022200                                             COMP-3 VALUE +0.             
022300     03  SPAR-SUORDV-TOT-LOCPREL PIC S9(9)V9(2)                           
022400                                             COMP-3 VALUE +0.             
022500     03  SPAR-SUORDV-U           PIC S9(9)V9(2)                           
022600                                             COMP-3 VALUE +0.             
022700     03  SPAR-SUORDV-U-LOC       PIC S9(9)V9(2)                           
022800                                             COMP-3 VALUE +0.             
022900     03  SPAR-SUORDV-U-LOCPREL   PIC S9(9)V9(2)                           
023000                                             COMP-3 VALUE +0.             
023100     03  SPAR-SUORDV-P           PIC S9(9)V9(2)                           
023200                                             COMP-3 VALUE +0.             
023300     03  SPAR-SUORDV-P-LOC       PIC S9(9)V9(2)                           
023400                                             COMP-3 VALUE +0.             
023500     03  SPAR-SUORDV-P-LOCPREL   PIC S9(9)V9(2)                           
023600                                             COMP-3 VALUE +0.             
023700     03  SPAR-SUORDV-F           PIC S9(9)V9(2)                           
023800                                             COMP-3 VALUE +0.             
023900     03  SPAR-SUORDV-F-LOC      PIC S9(9)V9(2)                            
024000                                             COMP-3 VALUE +0.             
024100     03  SPAR-SUORDV-F-LOCPREL   PIC S9(9)V9(2)                           
024200                                             COMP-3 VALUE +0.             
024300     03  SPAR-SUORDV-L           PIC S9(9)V9(2)                           
024400                                             COMP-3 VALUE +0.             
024500     03  SPAR-SUORDV-L-LOC       PIC S9(9)V9(2)                           
024600                                             COMP-3 VALUE +0.             
024700     03  SPAR-SUORDV-L-LOCPREL   PIC S9(9)V9(2)                           
024800                                             COMP-3 VALUE +0.             
024900*                                                                         
025000     03  SPAR-ARB-IDDC           PIC X(2)           VALUE SPACE.          
025100     03  SPAR-ARB-TIRFS          PIC S9(11)  COMP-3 VALUE +0.             
025200     03  SPAR-ARB-KDTRPKAT       PIC S9(11)  COMP-3 VALUE +0.             
025300     03  SPAR-ODEL-IDPRODNR      PIC S9(7)   COMP-3 VALUE +0.             
025400     03  SPAR-KDVALISO           PIC X(3)    VALUE SPACE.                 
025500     EJECT                                                                
025600*                                                                         
025700 01  HELP-AREA.                                                           
025800     03  FILLER                  PIC X(16)      VALUE 'HELP AREA'.        
025900     03  HELP-KVRADER-OPACK      PIC  9(5)          VALUE ZERO.           
026000     03  HELP-SUORDV-OPACK       PIC  9(9)V9(2)     VALUE ZERO.           
026100     03  HELP-SUMMA              PIC S9(9)V9(2) COMP-3 VALUE +0.          
026200     03  HELP-IDKAMPRF           PIC  9(7)          VALUE ZERO.           
026300     03  HELP-TIREGDAT           PIC  9(7)          VALUE ZERO.           
026400     03  HELP-TIRFS              PIC  9(11)         VALUE ZERO.           
026500     03  FILLER             REDEFINES HELP-TIRFS.                         
026600         05  FILLER              PIC  9(1).                               
026700         05  HELP-TIRFS-AAMMDD   PIC  9(6).                               
026800         05  HELP-TIRFS-HHMM     PIC  9(4).                               
026900     03  HELP-TIAAMMDD-TRP       PIC  9(6)          VALUE ZERO.           
027000     03  HELP-TIHHMM-TRP         PIC  9(4)          VALUE ZERO.           
027100                                                                          
027200 01  WS-TID-X.                                                            
027300     03  WS-TID-N                PIC 9(5).                                
027400 01  WS-TID-RED-X.                                                        
027500     03  WS-TID-HH               PIC X(2).                                
027600*    03  WS-TID-PKT              PIC X(1).                                
027700     03  WS-TID-MM               PIC X(2).                                
027800                                                                          
027900* ARBETSVARIABLER FÖR REG.TID START                                       
028000 01  WS-RTID-X.                                                           
028100     03  WS-RTID-N                PIC 9(7).                               
028200 01  WS-RTID-RED-X.                                                       
028300     03  WS-RTID-HH               PIC X(2).                               
028400*    03  WS-RTID-PKT              PIC X(1).                               
028500     03  WS-RTID-MM               PIC X(2).                               
028600*                                                                         
028700* ARBETSVARIABLER FÖR REG.TID STOPP                                       
028800 01  WS-RTIDS-X.                                                          
028900     03  WS-RTIDS-N                PIC 9(7).                              
029000 01  WS-RTIDS-RED-X.                                                      
029100     03  WS-RTIDS-HH               PIC X(2).                              
029200*    03  WS-RTIDS-PKT              PIC X(1).                              
029300     03  WS-RTIDS-MM               PIC X(2).                              
029400*                                                                         
029500 01  TAB-FAKT-HJALP-AREA.                                                 
029600     03  TAB-FAKT-IX-MAX-12      PIC S9(9)   COMP   VALUE +12.            
029700     03  TAB-FAKT-IX-MIN-1       PIC S9(9)   COMP   VALUE +1.             
029800     03  TAB-FAKT-VERKLIGT-ANTAL PIC S9(9)   COMP   VALUE +0.             
029900     03  TAB-FAKT-WINTSOR-STEG-LANGD                                      
030000                                 PIC S9(9)   COMP   VALUE +13.            
030100     03  TAB-FAKT-WINTSOR-ANTAL  PIC S9(9)   COMP   VALUE +12.            
030200     03  TAB-FAKT-WINTSOR-SORT-LANGD                                      
030300                                 PIC S9(9)   COMP   VALUE +13.            
030400*                                                                         
030500*                       ASCENDING KEY IS TAB-FAKT-TIFAKT                  
030600*                                                                         
030700 01  TABELL-FAKTURA-INFO.                                                 
030800     03  TAB-FAKT-ELEM  OCCURS           12 TIMES                         
030900                        INDEXED BY       TAB-FAKT-IX.                     
031000                                                                          
031100         05  TAB-FAKT-TIFAKT     PIC  9(6).                               
031200         05  TAB-FAKT-IDFAKT     PIC  9(7).                               
031300     EJECT                                                                
031400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031500*                                                                         
031600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031700     SKIP3                                                                
031800 01  NYCKLAR-TILL-DLI.                                                    
031900     03  W-WDQ2CSEQ-MIN-X.                                                
032000         05  W-WDQ2C-IDDISTR-MIN  PIC S9(5)   COMP-3 VALUE +0.            
032100         05  W-WDQ2C-IDKUNDNR-MIN PIC S9(7)   COMP-3 VALUE +0.            
032200         05  W-WDQ2C-IDKUNDRF-MIN.                                        
032300             07  W-WDQ2C-IDORDNR7-MIN                                     
032400                                  PIC  9(7)          VALUE ZERO.          
032500             07  FILLER           PIC  X(3)          VALUE SPACE.         
032600*                                                                         
032700     03  W-WDQ2CSEQ-MAX-X.                                                
032800         05  W-WDQ2C-IDDISTR-MAX  PIC S9(5)   COMP-3 VALUE +0.            
032900         05  W-WDQ2C-IDKUNDNR-MAX PIC S9(7)   COMP-3 VALUE +0.            
033000         05  W-WDQ2C-IDKUNDRF-MAX.                                        
033100             07  W-WDQ2C-IDORDNR7-MAX                                     
033200                                  PIC  9(7)          VALUE ZERO.          
033300             07  FILLER           PIC  X(3)          VALUE SPACE.         
033400*                                                                         
033500     03  W-IDDC-X.                                                        
033600         05  W-WDQ2-IDDC          PIC X(2)           VALUE SPACE.         
033700*                                                                         
033800     03  W-WDQ301KY-MIN-X.                                                
033900         05  W-WDQ3-MIN-IDORDER   PIC S9(7)   COMP-3 VALUE +0.            
034000         05  W-WDQ3-MIN-IDDC      PIC X(2)           VALUE SPACE.         
034100         05  W-WDQ3-MIN-IDPRODNR  PIC S9(7)   COMP-3 VALUE +0.            
034200         05  W-WDQ3-MIN-IDPLKLST  PIC S9(3)   COMP-3 VALUE +0.            
034300*                                                                         
034400     03  W-WDQ301KY-MAX-X.                                                
034500         05  W-WDQ3-MAX-IDORDER   PIC S9(7)   COMP-3 VALUE +0.            
034600         05  W-WDQ3-MAX-IDDC      PIC X(2)           VALUE SPACE.         
034700         05  W-WDQ3-MAX-IDPRODNR  PIC S9(7)   COMP-3 VALUE +0.            
034800         05  W-WDQ3-MAX-IDPLKLST  PIC S9(3)   COMP-3 VALUE +0.            
034900*                                                                         
035000     03  W-IDPRODNR-X.                                                    
035100         05  W-WDE6-IDPRODNR      PIC S9(7)   COMP-3 VALUE +0.            
035200*                                                                         
037800     EJECT                                                                
037900*    --- STATUS-KOD FRÅN IMS                                              
038000 01  STATUS-WS                    PIC XX.                                 
038100     88  SEGMENT-FINNS                       VALUE '  '.                  
038200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
038300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
038400     88  ANNAT-SEGMENT                       VALUE 'GK'.                  
038500     SKIP2                                                                
038600 01  GODK-STATUSKODER.                                                    
038700     03  GODK-STATUS OCCURS  5  TIMES                                     
038800                     INDEXED BY STATUS-IX                                 
038900                                  PIC X(2).                               
039000     SKIP3                                                                
039100 01  SSA1                         PIC X(128).                             
039200 01  SSA2                         PIC X(128).                             
039300     EJECT                                                                
039400*    --- IMS FUNKTIONSKODER                                               
039500*01  -COPY W0003                                                          
039600     EJECT                                                                
039700******************************************************************        
039800*                                                                *        
039900*        ARBETS-AREOR TILL IO-AREORNA                            *        
040000*                                                                *        
040100*        DLI INPUT-OUTPUT AREA                                   *        
040200*                                                                *        
040300******************************************************************        
040400*    ---  DLI INPUT-OUTPUT                                                
040500*    ---  DLI-IO-AREA                                                     
040600*                                                                         
040700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI01'.           
040800 01  DLI-IO-WLORQI01.                                                     
040900*    03  WLORQI01   -COPY WDQ201                                          
041000     EJECT                                                                
041100                                                                          
041200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI11'.           
041300 01  DLI-IO-WLORQI11.                                                     
041400*    03  WLORQI11   -COPY WDQ211                                          
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI12'.           
041800 01  DLI-IO-WLORQI12.                                                     
041900*    03  WLORQI12   -COPY WDQ212                                          
042000     EJECT                                                                
042100                                                                          
042101 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI21'.           
042102 01  DLI-IO-WLORQI21.                                                     
042103*    03  WLORQI21   -COPY WDQ221                                          
042104     EJECT                                                                
042105                                                                          
042200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQA01'.           
042300 01  DLI-IO-WLORQA01.                                                     
042400*    03  WLORQA01   -COPY WDQ301                                          
042500     EJECT                                                                
042600                                                                          
042700 01  FILLER                  PIC X(16)   VALUE 'MLI-IO-WDE601'.           
042800 01  DLI-IO-WDE601.                                                       
042900*    03             -COPY WDE601                                          
043000     EJECT                                                                
043100                                                                          
043200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE611'.           
043300 01  DLI-IO-WDE611.                                                       
043400*    03             -COPY WDE611                                          
043500     EJECT                                                                
043600                                                                          
044900 LINKAGE SECTION.                                                         
045000 01  MSG-PCB                     PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008      -PRE ORQI-                                          
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008      -PRE ORQA-                                          
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008      -PRE WDE6-                                          
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
047000 PROCEDURE DIVISION  USING MSG-PCB                                        
047100                           ORQI-PCB                                       
047200                           ORQA-PCB  WDE6-PCB.                            
047500 MAIN SECTION.                                                            
047600     ENTRY 'DLITCBL' USING MSG-PCB                                        
047700                           ORQI-PCB                                       
047800                           ORQA-PCB  WDE6-PCB.                            
048100                                                                          
048200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
048300     IF SUB-KDRC = 0                                                      
048400      IF REQU-KDPGMACT = 'S'                                              
048500       PERFORM A-INIT                                                     
048600       PERFORM B-KOLLA-NYCKLAR                                            
048700       IF NYCKLAR-OK                                                      
048800         PERFORM C-KOLLA-BEHORIGHET                                       
048900         PERFORM F-LAES-VISA-INFO                                         
049000       END-IF                                                             
049100      ELSE                                                                
049200       MOVE FELM-SYSTEM-ERROR          TO RESP-IDMSG-ERROR                
049300      END-IF                                                              
049400       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
049500       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
049600       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
049700       IF WS-IDMSG-ERROR NOT = SPACE                                      
049800           MOVE ALL '+' TO RESP-AREA                                      
049900           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
050000           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
050100           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
050200           MOVE 001              TO RESP-IDMSGVER                         
050300       END-IF                                                             
050400       PERFORM S02-RETURN-RESPONSE                                        
050500     END-IF                                                               
050600                                                                          
050700     MOVE ZERO TO RETURN-CODE                                             
050800     GOBACK                                                               
050900     .                                                                    
051000     EJECT                                                                
051100 A-INIT SECTION.                                                          
051200                                                                          
051700     MOVE ALL '+'            TO RESP-AREA                                 
051800     MOVE SPACE              TO RESP-IDMSG-ERROR                          
051900                                RESP-IDMSG-INFO                           
052000                                RESP-IDELMT-ERROR                         
052100     MOVE 001                TO RESP-IDMSGVER                             
052200     .                                                                    
052300     EJECT                                                                
052400 B-KOLLA-NYCKLAR SECTION.                                                 
052500                                                                          
052600                                                                          
052700     MOVE    JA               TO    NYCKLAR-SW                            
052800     MOVE    LOW-VALUE        TO    W-WDQ301KY-MIN-X                      
052900                                    W-WDQ2CSEQ-MIN-X                      
053000     MOVE    HIGH-VALUE       TO    W-WDQ301KY-MAX-X                      
053100                                    W-WDQ2CSEQ-MAX-X                      
053200                                                                          
053400     PERFORM BA-KONTROLLERA-IDDISTR                                       
053500     PERFORM BB-KONTROLLERA-IDKUNDNR                                      
053600     PERFORM BC-KONTROLLERA-IDKUNDRF                                      
053700     PERFORM BD-KONTROLLERA-IDDC                                          
053800     PERFORM BE-KONTROLLERA-IDARTNR                                       
053900     PERFORM BF-KONTROLLERA-IDKOLLI                                       
054000     PERFORM BG-KONTROLLERA-IDPRODNR                                      
054100                                                                          
054200     IF REQU-IDARTNR-KEY   NUMERIC OR                                     
054300        REQU-IDARTNR-KEY = ALL '+'                                        
054400        CONTINUE                                                          
054500     ELSE                                                                 
054600       MOVE NEJ        TO NYCKLAR-SW                                      
054700     END-IF                                                               
054800                                                                          
054900     IF REQU-IDKOLLI-KEY   NUMERIC OR                                     
055000        REQU-IDKOLLI-KEY = ALL '+'                                        
055100        CONTINUE                                                          
055200     ELSE                                                                 
055300       MOVE NEJ        TO NYCKLAR-SW                                      
055400     END-IF                                                               
055500                                                                          
055600     IF REQU-IDPRODNR-KEY  NUMERIC OR                                     
055700        REQU-IDPRODNR-KEY = ALL '+'                                       
055800        CONTINUE                                                          
055900     ELSE                                                                 
056000       MOVE NEJ        TO NYCKLAR-SW                                      
056100     END-IF                                                               
056200     IF NYCKLAR-FEL                                                       
056300       MOVE    FELM-NYCKEL-FEL-401 TO    RESP-IDMSG-ERROR                 
056400                                                                          
056500     END-IF                                                               
056600                                                                          
056700     .                                                                    
056800     EJECT                                                                
056900 BA-KONTROLLERA-IDDISTR SECTION.                                          
057000                                                                          
057100     IF REQU-IDDISTR-KEY   NUMERIC                                        
057200      MOVE      REQU-IDDISTR-KEY  TO        RESP-IDDISTR-KEY              
057300      INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE           
057400     END-IF                                                               
057500                                                                          
057600     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
057700       MOVE    REQU-IDDISTR-KEY  TO      W-WDQ2C-IDDISTR-MIN              
057800                                         W-WDQ2C-IDDISTR-MAX              
057900     ELSE                                                                 
058000       MOVE    NEJ               TO        NYCKLAR-SW                     
058100     END-IF                                                               
058200                                                                          
059400     .                                                                    
059500     EJECT                                                                
059600 BB-KONTROLLERA-IDKUNDNR SECTION.                                         
059700                                                                          
059800                                                                          
059900      IF REQU-IDKUNDNR-KEY  NUMERIC                                       
060000       MOVE    REQU-IDKUNDNR-KEY    TO      RESP-IDKUNDNR-KEY             
060100       INSPECT RESP-IDKUNDNR-KEY  REPLACING LEADING ZERO BY SPACE         
060200      END-IF                                                              
060300                                                                          
060400     IF REQU-IDKUNDNR-KEY NUMERIC                                         
060500       MOVE    REQU-IDKUNDNR-KEY  TO        W-WDQ2C-IDKUNDNR-MIN          
060600                                            W-WDQ2C-IDKUNDNR-MAX          
060700     ELSE                                                                 
060800       MOVE    NEJ                TO        NYCKLAR-SW                    
060900     END-IF                                                               
061000     .                                                                    
061100     EJECT                                                                
061200 BC-KONTROLLERA-IDKUNDRF SECTION.                                         
061300                                                                          
061400     IF REQU-IDORDNR7-KEY   NUMERIC                                       
061500      MOVE   REQU-IDORDNR7-KEY     TO        RESP-IDORDNR7-KEY            
061600      INSPECT RESP-IDORDNR7-KEY  REPLACING LEADING ZERO BY SPACE          
061700     END-IF                                                               
061800                                                                          
061900     IF REQU-IDORDNR7-KEY       NUMERIC AND                               
062000        REQU-IDORDNR7-KEY       > ZERO                                    
062100                                                                          
062200       MOVE    SPACE                  TO  W-WDQ2C-IDKUNDRF-MIN            
062300                                          W-WDQ2C-IDKUNDRF-MAX            
062400       MOVE    REQU-IDORDNR7-KEY      TO  W-WDQ2C-IDORDNR7-MIN            
062500                                          W-WDQ2C-IDORDNR7-MAX            
062600     ELSE                                                                 
062700       MOVE    NEJ                    TO  NYCKLAR-SW                      
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 BD-KONTROLLERA-IDDC SECTION.                                             
063200                                                                          
063300     MOVE REQU-IDDC-KEY   TO RESP-IDDC-KEY                                
063400                             W-WDQ2-IDDC                                  
063500                             W-WDQ3-MIN-IDDC                              
063600                             W-WDQ3-MAX-IDDC                              
063700     .                                                                    
063800     EJECT                                                                
063900 BE-KONTROLLERA-IDARTNR SECTION.                                          
064000                                                                          
064100      IF REQU-IDARTNR-KEY NUMERIC                                         
064200       MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                          
064300      END-IF                                                              
064400     .                                                                    
064500     EJECT                                                                
064600 BF-KONTROLLERA-IDKOLLI SECTION.                                          
064700                                                                          
064800      IF REQU-IDKOLLI-KEY NUMERIC                                         
064900       MOVE REQU-IDKOLLI-KEY TO RESP-IDKOLLI-KEY                          
065000      END-IF                                                              
065100     .                                                                    
065200     EJECT                                                                
065300 BG-KONTROLLERA-IDPRODNR SECTION.                                         
065400                                                                          
065500      IF REQU-IDPRODNR-KEY NUMERIC                                        
065600       MOVE REQU-IDPRODNR-KEY TO RESP-IDPRODNR-KEY                        
065700       INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE          
065800      END-IF                                                              
065900     .                                                                    
066000     EJECT                                                                
066100 C-KOLLA-BEHORIGHET SECTION.                                              
066200                                                                          
066300     MOVE REQU-IDUSER       TO    SEC-IDUSER                              
066400*THIS PROGRAM IS A COPY OF W4050300 PROGRAM  AND MODIFIED FOR             
066500*LDC PROJECT                                                              
066600     MOVE '4503'            TO    SEC-IDTRANS                             
066700     MOVE REQU-IDDISTR-KEY  TO    SEC-IDKEY                               
066800                                                                          
066900     CALL WSECURIT          USING SEC-IDUSER                              
067000                                  SEC-IDTRANS                             
067100                                  SEC-IDKEY                               
067200                                  SEC-KDSVAR                              
067300                                                                          
067400     .                                                                    
067500     EJECT                                                                
067600 F-LAES-VISA-INFO SECTION.                                                
067700                                                                          
067800     PERFORM FA-INITIERA-TABELLER                                         
067900     PERFORM FB-LAS-ORDERHUVUD                                            
068000                                                                          
068100     IF ALLT-OK                                                           
068200       PERFORM FC-LAS-ARBTAB                                              
068300                                                                          
068400       IF ALLT-OK           AND                                           
068500          SPAR-ARB-TIRFS = ZERO                                           
068600         PERFORM FD-LAS-DIRLEV                                            
068700         PERFORM FE-FYLL-MOD-UTAN-WDQ3-E6-INFO                            
068800       ELSE                                                               
068900                                                                          
069000         IF ALLT-OK           AND                                         
069100            SPAR-ARB-TIRFS > ZERO                                         
069200                                                                          
069300           PERFORM FF-LAS-ORDERDELAR                                      
069400                                                                          
069500         END-IF                                                           
069600       END-IF                                                             
069601       IF ALLT-OK                                                         
069603          MOVE SPAR-KDVALISO  TO RESP-TEDDI                               
069604       END-IF                                                             
069605*********************************************************                 
069606**     BELOW IF-STATMENT IS COPIED FROM W40503         **                 
069607**     WHERE THE HEADING 'VÄRDE SEK' IS CHANGED TO     **                 
069608**     'VÄRDE ' + KDVALISO                             **                 
069609**     IN THE WEB-PULS THE HEADING IS JUST 'VALUE'     **                 
069610**     SO, FOR THE TIME BEEING, NO CHANGE IS NEEDED    **                 
069611**                                                     **                 
069612*********************************************************                 
069613*                                                                         
069614*      IF ALLT-OK                                                         
069620*         IF DIST79-LOCAL-CURRENCY                                        
069630*         OR DIST35-NONVCC-REFILL                                         
069640*         OR DIST35-CDC-IN-REFILL                                         
069650*         OR TEST-IDDISTR = 6010                                          
069660*            MOVE SPAR-KDVALISO TO MOD-KDVALISO                           
069670*         ELSE                                                            
069680*            MOVE 'SEK'         TO MOD-KDVALISO                           
069690*         END-IF                                                          
069691*      END-IF                                                             
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 FA-INITIERA-TABELLER SECTION.                                            
070100                                                                          
070200     SET TAB-FAKT-IX TO +1                                                
070300                                                                          
070400     PERFORM UNTIL TAB-FAKT-IX > TAB-FAKT-IX-MAX-12                       
070500       MOVE ALL '9'     TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)                   
070600                           TAB-FAKT-TIFAKT(TAB-FAKT-IX)                   
070700       SET  TAB-FAKT-IX UP BY +1                                          
070800     END-PERFORM                                                          
070900     .                                                                    
071000     EJECT                                                                
071100 FB-LAS-ORDERHUVUD SECTION.                                               
071200                                                                          
071300     PERFORM IMS-GET-WDQ201-CSEQ                                          
071400                                                                          
071500     IF SEGMENT-SAKNAS                                                    
071600        MOVE NEJ                   TO ALLT-SW                             
071700        MOVE 'IDORDNR'             TO RESP-IDELMT-ERROR                   
071800        MOVE '025'                 TO RESP-IDMSG-ERROR                    
071900     ELSE                                                                 
072000       IF OHUV-FLBORT = JA                                                
072100         PERFORM IMS-GNP-WDQ212-OKVAL                                     
072200         IF SEGMENT-SAKNAS                                                
072300            MOVE NEJ                       TO ALLT-SW                     
072400            MOVE 'IDORDNR'                 TO RESP-IDELMT-ERROR           
072500            MOVE '025'                     TO RESP-IDMSG-ERROR            
072600         ELSE                                                             
072700            MOVE NEJ                       TO ALLT-SW                     
072800            MOVE FELM-ORDER-ANNULLERAD-052 TO RESP-IDMSG-ERROR            
072900         END-IF                                                           
073000       ELSE                                                               
073100         IF OHUV-KDTPOTYP > ZERO                                          
073200           MOVE NEJ                   TO ALLT-SW                          
073300           MOVE 'IDORDNR'             TO RESP-IDELMT-ERROR                
073400           MOVE '025'                 TO RESP-IDMSG-ERROR                 
073500         END-IF                                                           
073600       END-IF                                                             
073700     END-IF                                                               
073800                                                                          
073900     IF ALLT-OK                                                           
074000       IF OHUV-FLKLAR = NEJ                                               
074100         MOVE FELM-ORDER-EJ-AVSLUTAD-053 TO RESP-IDMSG-ERROR              
074200       END-IF                                                             
074300       PERFORM FBA-FYLL-MOD-MED-OHUV-INFO                                 
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700 FBA-FYLL-MOD-MED-OHUV-INFO SECTION.                                      
074800                                                                          
074900     MOVE OHUV-TIREPDAT     TO RESP-TIREPDAT                              
075000     MOVE OHUV-BEKUNDRF     TO RESP-BEKUNDRF                              
075100     MOVE OHUV-IDKAMPRF     TO HELP-IDKAMPRF                              
075200     MOVE HELP-IDKAMPRF     TO RESP-IDKAMPRF                              
075300     MOVE OHUV-TIREGDAT     TO HELP-TIREGDAT                              
075400     MOVE HELP-TIREGDAT     TO RESP-TIREGDAT                              
075500     MOVE OHUV-TIREGDAT-STO TO RESP-TIREGDAT-STO                          
075600                                                                          
075700     MOVE OHUV-TIREGTID  TO WS-RTID-N                                     
075800     MOVE WS-RTID-X(2:2) TO WS-RTID-HH                                    
075900     MOVE WS-RTID-X(4:2) TO WS-RTID-MM                                    
076000     MOVE WS-RTID-RED-X  TO RESP-TIHHMM-REG                               
076100                                                                          
076200     MOVE OHUV-TIREGTID-STO   TO WS-RTIDS-N                               
076300     MOVE WS-RTIDS-X(2:2)     TO WS-RTIDS-HH                              
076400     MOVE WS-RTIDS-X(4:2)     TO WS-RTIDS-MM                              
076500     MOVE WS-RTIDS-RED-X      TO RESP-TIHHMM-REG-STO                      
077100     .                                                                    
077200     EJECT                                                                
077300 FC-LAS-ARBTAB SECTION.                                                   
077400                                                                          
077410     MOVE OHUV-IDORDER     TO W-WDQ3-MIN-IDORDER                          
077420                              W-WDQ3-MAX-IDORDER                          
077430     MOVE LOW-VALUE        TO W-WDQ3-MIN-IDDC                             
077440     MOVE HIGH-VALUE       TO W-WDQ3-MAX-IDDC                             
077450     PERFORM IMS-GN-WDQ301-INTERV                                         
077460     IF ODEL-IDDC-EXP = REQU-IDDC-KEY                                     
077470        MOVE ODEL-IDPRODNR TO W-WDE6-IDPRODNR                             
077480        PERFORM IMS-GET-WDE601                                            
077490        IF SEGMENT-FINNS AND                                              
077491           VORD-KVKOLLI-FAKT > 0                                          
077492*          *BOUNCE-DC WILL NOT SEE ORDERLINES BEFORE                      
077493*          *SOME LINES ARE INVOICED                                       
077494           MOVE ODEL-IDDC  TO W-WDQ2-IDDC                                 
077495        ELSE                                                              
077496           MOVE REQU-IDDC-KEY TO W-WDQ3-MIN-IDDC                          
077498                                 W-WDQ3-MAX-IDDC                          
077499        END-IF                                                            
077500     ELSE                                                                 
077501        MOVE REQU-IDDC-KEY TO W-WDQ3-MIN-IDDC                             
077502                              W-WDQ3-MAX-IDDC                             
077503     END-IF                                                               
077504                                                                          
077510     PERFORM IMS-GNP-WDQ212                                               
077600                                                                          
077700     IF SEGMENT-FINNS                                                     
077800       PERFORM FCA-FYLL-MOD-MED-ARB-TAB-INFO                              
077900       MOVE    ARB-KDTRPKAT TO SPAR-ARB-KDTRPKAT                          
078000       MOVE    ARB-TIRFS    TO SPAR-ARB-TIRFS                             
078100       MOVE    ARB-IDDC     TO SPAR-ARB-IDDC                              
078200       IF ARB-TIRFS = ZERO                                                
078300         PERFORM FCB-SUMMERA-FRAN-WDQ221                                  
078400       END-IF                                                             
078600     ELSE                                                                 
078700       MOVE    NEJ                         TO    ALLT-SW                  
078800       MOVE    'IDRADNR'                   TO RESP-IDELMT-ERROR           
078900       MOVE    '025'                       TO RESP-IDMSG-ERROR            
079000       MOVE    SPACE                       TO    SPAR-KDVALISO            
079100     END-IF                                                               
079200     .                                                                    
079300     EJECT                                                                
079400 FCA-FYLL-MOD-MED-ARB-TAB-INFO SECTION.                                   
079500                                                                          
079600     MOVE ARB-TIRFS             TO HELP-TIRFS                             
079700     MOVE HELP-TIRFS-AAMMDD     TO RESP-TIAAMMDD-RFS                      
079800     MOVE ARB-IDTRP             TO RESP-IDTRP                             
079900     MOVE ARB-DATRPAVD (3:6)    TO HELP-TIAAMMDD-TRP                      
080000     MOVE HELP-TIAAMMDD-TRP     TO RESP-TIAAMMDD-TRP                      
080100                                                                          
080200     MOVE ARB-TIHHMM            TO WS-TID-N                               
080300     MOVE WS-TID-X(2:2)         TO WS-TID-HH                              
080400     MOVE WS-TID-X(4:2)         TO WS-TID-MM                              
080500     MOVE WS-TID-RED-X          TO RESP-TIHHMM-TRP                        
080600     .                                                                    
080700     EJECT                                                                
080800 FCB-SUMMERA-FRAN-WDQ221 SECTION.                                         
080900                                                                          
081000     PERFORM IMS-GNP-WDQ221                                               
081100                                                                          
081200     PERFORM UNTIL SEGMENT-SAKNAS                                         
081300                                                                          
081400        IF LOR-SUORDV > ZERO                                              
081500          COMPUTE SPAR-SUORDV-TOT  =   SPAR-SUORDV-TOT                    
081600                              + LOR-SUORDV                                
081700          MOVE LOR-KDVALISO   TO SPAR-KDVALISO                            
081800        END-IF                                                            
081900        IF LOR-SUORDV-LOC > ZERO                                          
082000          COMPUTE SPAR-SUORDV-TOT-LOC  =   SPAR-SUORDV-TOT-LOC            
082100                              + LOR-SUORDV-LOC                            
082200        END-IF                                                            
082300        IF LOR-SUORDV-LOCPREL > ZERO                                      
082400          COMPUTE SPAR-SUORDV-TOT-LOCPREL =                               
082500                                SPAR-SUORDV-TOT-LOCPREL                   
082600                              + LOR-SUORDV-LOCPREL                        
082700        END-IF                                                            
082800        IF LOR-KVRADER        > ZERO                                      
082900          COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                   
083000                              + LOR-KVRADER                               
083100        END-IF                                                            
083200        PERFORM IMS-GNP-WDQ221                                            
083300                                                                          
083310     END-PERFORM                                                          
083400     .                                                                    
083500     EJECT                                                                
083600 FD-LAS-DIRLEV SECTION.                                                   
083700                                                                          
083800     PERFORM IMS-GNP-FIRST-WDQ211                                         
083900     PERFORM UNTIL SEGMENT-SAKNAS                                         
084000                                                                          
084100       IF DIRL-SUORDV  > ZERO                                             
084200         COMPUTE SPAR-SUORDV-TOT  = SPAR-SUORDV-TOT                       
084300                                  + DIRL-SUORDV                           
084400       END-IF                                                             
084500       IF DIRL-SUORDV-LOC  > ZERO                                         
084600         COMPUTE SPAR-SUORDV-TOT-LOC  = SPAR-SUORDV-TOT-LOC               
084700                                  + DIRL-SUORDV-LOC                       
084800       END-IF                                                             
084900       IF DIRL-SUORDV-LOCPREL  > ZERO                                     
085000         COMPUTE SPAR-SUORDV-TOT-LOCPREL                                  
085100                                      = SPAR-SUORDV-TOT-LOCPREL           
085200                                  + DIRL-SUORDV-LOCPREL                   
085300       END-IF                                                             
085400       IF DIRL-KVRADER > ZERO                                             
085500         COMPUTE SPAR-KVRADER-TOT = SPAR-KVRADER-TOT                      
085600                                  + DIRL-KVRADER                          
085700       END-IF                                                             
085800       PERFORM IMS-GNP-WDQ211                                             
085900                                                                          
086000     END-PERFORM                                                          
086100     .                                                                    
086200     EJECT                                                                
086300 FE-FYLL-MOD-UTAN-WDQ3-E6-INFO SECTION.                                   
086400                                                                          
086500     IF DIST79-DEALER-PRICE                                               
086600       COMPUTE HELP-SUMMA = SPAR-SUORDV-TOT-LOC +                         
086700                            SPAR-SUORDV-TOT-LOCPREL                       
086900       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
087000       IF SPAR-SUORDV-TOT-LOCPREL = +0                                    
087100         MOVE ' '                TO RESP-ASTERIX1                         
087200       ELSE                                                               
087300         MOVE '*'                TO RESP-ASTERIX1                         
087400       END-IF                                                             
087500     ELSE                                                                 
087600       MOVE SPAR-SUORDV-TOT      TO HELP-SUORDV-OPACK                     
087700       MOVE ' '                  TO RESP-ASTERIX1                         
087800     END-IF                                                               
087900     MOVE HELP-SUORDV-OPACK      TO RESP-SUORDV-TOT                       
088000     MOVE SPAR-KVRADER-TOT       TO HELP-KVRADER-OPACK                    
088100     MOVE HELP-KVRADER-OPACK     TO RESP-KVRADER-TOT                      
088200     MOVE ZERO                   TO RESP-SUORDV-U                         
088300                                    RESP-SUORDV-P                         
088400                                    RESP-SUORDV-F                         
088500                                    RESP-SUORDV-L                         
088600                                    RESP-KVRADER-P                        
088700                                    RESP-KVRADER-F                        
088800                                    RESP-KVRADER-L                        
088900     MOVE ' '                    TO RESP-ASTERIX2                         
089000                                    RESP-ASTERIX3                         
089100                                    RESP-ASTERIX4                         
089200                                    RESP-ASTERIX5                         
089300     .                                                                    
089400     EJECT                                                                
089500 FF-LAS-ORDERDELAR SECTION.                                               
089600                                                                          
089700     MOVE    OHUV-IDORDER TO W-WDQ3-MIN-IDORDER                           
089800                             W-WDQ3-MAX-IDORDER                           
089900                                                                          
089920                                                                          
090000     PERFORM IMS-GU-WDQ301-FIRST                                          
090010                                                                          
090100     MOVE    NEJ          TO SW-FAKT-INFO-TAB-SORTERAD                    
090200                                                                          
090300     PERFORM UNTIL SEGMENT-SAKNAS                                         
090310       IF ODEL-IDDC-EXP NOT = SPACE                                       
090320          MOVE JA         TO BOUNCE-SW                                    
090330       ELSE                                                               
090340          MOVE NEJ        TO BOUNCE-SW                                    
090350       END-IF                                                             
090400       IF ODEL-KDODELSTA = K-KDODELSTA-R                                  
090500         PERFORM FFA-SUM-VARDE-RADER-STATUS-R                             
090600         MOVE    NEJ TO SW-ORDER-KLAR                                     
090700         MOVE    JA  TO SW-SKALL-VARDEN-LAGGAS-UT                         
090800       ELSE                                                               
090900         IF ODEL-KDODELSTA = K-KDODELSTA-U                                
091000           MOVE    NEJ TO SW-ORDER-KLAR                                   
091100           PERFORM FFB-SUMMERA-STATUS-U-P-L-F                             
091200         ELSE                                                             
091300           IF ODEL-KDODELSTA = K-KDODELSTA-P                              
091400             PERFORM FFC-SUMMERA-STATUS-P-L-F                             
091500           END-IF                                                         
091600         END-IF                                                           
091700       END-IF                                                             
091800                                                                          
091900       PERFORM IMS-GN-WDQ301-INTERV                                       
092000     END-PERFORM                                                          
092100                                                                          
092200     IF SW-SKALL-VARDEN-LAGGAS-UT = JA                                    
092300       PERFORM FFE-FYLL-MOD-MED-TOTALER                                   
092400       PERFORM FFF-FYLL-MOD-MED-FAKT-INFO                                 
092500                                                                          
092600       IF SW-ORDER-KLAR = JA                                              
092700         MOVE FELM-ORDER-KLAR-082 TO    RESP-IDMSG-INFO                   
092800       END-IF                                                             
092900     ELSE                                                                 
093000       MOVE    'IDRADNR'                   TO RESP-IDELMT-ERROR           
093100       MOVE    '025'                       TO RESP-IDMSG-ERROR            
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500 FFA-SUM-VARDE-RADER-STATUS-R SECTION.                                    
093600                                                                          
093700     IF ODEL-SUORDV  > ZERO                                               
093800       COMPUTE SPAR-SUORDV-TOT  =   SPAR-SUORDV-TOT                       
093900                                  + ODEL-SUORDV                           
094000     END-IF                                                               
094100     IF ODEL-SUORDV-LOC  > ZERO                                           
094200       COMPUTE SPAR-SUORDV-TOT-LOC = SPAR-SUORDV-TOT-LOC                  
094300                                   + ODEL-SUORDV-LOC                      
094400     END-IF                                                               
094500     IF ODEL-SUORDV-LOCPREL  > ZERO                                       
094600       COMPUTE SPAR-SUORDV-TOT-LOCPREL = SPAR-SUORDV-TOT-LOCPREL          
094700                                   + ODEL-SUORDV-LOCPREL                  
094800     END-IF                                                               
094900     IF ODEL-IDDC-EXP = SPACE                                             
094910        MOVE ODEL-KDVALISO          TO SPAR-KDVALISO                      
094920     ELSE                                                                 
094930        IF ODEL-IDDC-EXP = WC-CDC-SE                                      
094940           MOVE ODEL-KDVALISO       TO SPAR-KDVALISO                      
094950        ELSE                                                              
094960           MOVE WS-SEK              TO SPAR-KDVALISO                      
094970        END-IF                                                            
094980     END-IF                                                               
095200     IF ODEL-KVRADER > ZERO                                               
095300       COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                      
095400                                  + ODEL-KVRADER                          
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800 FFB-SUMMERA-STATUS-U-P-L-F SECTION.                                      
095900                                                                          
096000     IF ODEL-IDPRODNR NOT = SPAR-ODEL-IDPRODNR                            
096100       MOVE    ODEL-IDPRODNR TO W-WDE6-IDPRODNR                           
096200                                SPAR-ODEL-IDPRODNR                        
096300       PERFORM IMS-GET-WDE601                                             
096301       IF VORD-IDDC-EXP NOT = SPACE                                       
096302          IF VORD-IDDC-EXP = WC-CDC-SE                                    
096303*           *BOUNCE DC = 11                                               
096305            IF VORD-IDDC = REQU-IDDC-KEY                                  
096306               MOVE JA  TO AVERAGECOST-SW                                 
096307            ELSE                                                          
096308               MOVE NEJ TO AVERAGECOST-SW                                 
096309            END-IF                                                        
096310          ELSE                                                            
096311*           *BOUNCE DC NOT 11                                             
096312            IF VORD-IDDC = REQU-IDDC-KEY                                  
096313               MOVE NEJ TO AVERAGECOST-SW                                 
096314            ELSE                                                          
096315               MOVE JA  TO AVERAGECOST-SW                                 
096316            END-IF                                                        
096317          END-IF                                                          
096322       ELSE                                                               
096323          IF VORD-SUORDV-EXP > 0                                          
096324             MOVE JA  TO AVERAGECOST-SW                                   
096325          ELSE                                                            
096326             MOVE NEJ TO AVERAGECOST-SW                                   
096327          END-IF                                                          
096330       END-IF                                                             
096400                                                                          
096500       IF SEGMENT-FINNS                                                   
096600         MOVE    JA  TO SW-SKALL-VARDEN-LAGGAS-UT                         
096700         PERFORM FFBA-SUMMERA-VARDEN-FRAN-E601                            
096800         IF VORD-KVORDRAD-PACK > ZERO                                     
096900           PERFORM IMS-GNP-WDE611                                         
097000           PERFORM UNTIL NOT SEGMENT-FINNS                                
097100                                                                          
097200             IF KOLLI-KDKOLSTA = K-KDKOLSTA-1-FARDIG-PACKAT OR            
097300                                 K-KDKOLSTA-6-FAKTREL                     
097400               PERFORM FFBB-SUM-VARDE-RADER-STAT-P                        
097500               IF KOLLI-KDKOLSTA = K-KDKOLSTA-6-FAKTREL                   
097600                 PERFORM S02-SPARA-FAKT-INFO-I-TABELL                     
097700               END-IF                                                     
097800             ELSE                                                         
097900               IF KOLLI-KDKOLSTA = K-KDKOLSTA-2-LASTREL     OR            
098000                                   K-KDKOLSTA-3-LASTAT      OR            
098100                                   K-KDKOLSTA-4-LASTAT-FAKTREL            
098200                 PERFORM FFBC-SUM-VARDE-RADER-STATUS-L                    
098300                 IF KOLLI-KDKOLSTA = K-KDKOLSTA-4-LASTAT-FAKTREL          
098400                   PERFORM S02-SPARA-FAKT-INFO-I-TABELL                   
098500                 END-IF                                                   
098600               ELSE                                                       
098700                 IF KOLLI-KDKOLSTA = K-KDKOLSTA-7-FAKTURERAT   OR         
098800                                     K-KDKOLSTA-8-FAKT-LASTREL OR         
098900                                     K-KDKOLSTA-9-FAKT-LASTAT             
099000                   PERFORM FFBD-SUM-VARDE-RADER-STATUS-F                  
099100                   PERFORM S02-SPARA-FAKT-INFO-I-TABELL                   
099200                 END-IF                                                   
099300               END-IF                                                     
099400             END-IF                                                       
099500             PERFORM IMS-GNP-WDE611                                       
099600           END-PERFORM                                                    
099700         END-IF                                                           
099800       END-IF                                                             
099900     END-IF                                                               
100000     .                                                                    
100100     EJECT                                                                
100200 FFBA-SUMMERA-VARDEN-FRAN-E601 SECTION.                                   
100300                                                                          
100400     COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                        
100500                                + VORD-KVORDRAD                           
100710     IF AVERAGECOST                                                       
100720        COMPUTE SPAR-SUORDV-U =   SPAR-SUORDV-U                           
100730                                + VORD-SUORDV-EXP                         
100740     ELSE                                                                 
100750        COMPUTE SPAR-SUORDV-U =   SPAR-SUORDV-U                           
100760                                + VORD-SUORDV                             
100770     END-IF                                                               
100800     COMPUTE SPAR-SUORDV-U-LOC =   SPAR-SUORDV-U-LOC                      
100900                                + VORD-SUORDV-LOC                         
101000     COMPUTE SPAR-SUORDV-U-LOCPREL  =  SPAR-SUORDV-U-LOCPREL              
101100                                + VORD-SUORDV-LOCPREL                     
101310     IF AVERAGECOST                                                       
101320        COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                       
101330                                + VORD-SUORDV-EXP                         
101340     ELSE                                                                 
101350        COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                       
101360                                + VORD-SUORDV                             
101370     END-IF                                                               
101400     COMPUTE SPAR-SUORDV-TOT-LOC =   SPAR-SUORDV-TOT-LOC                  
101500                                + VORD-SUORDV-LOC                         
101600     COMPUTE SPAR-SUORDV-TOT-LOCPREL = SPAR-SUORDV-TOT-LOCPREL            
101700                                + VORD-SUORDV-LOCPREL                     
102010     IF AVERAGECOST                                                       
102020        IF VORD-KDVALISO-EXP NOT = SPACE                                  
102030          MOVE VORD-KDVALISO-EXP TO SPAR-KDVALISO                         
102040        END-IF                                                            
102050     ELSE                                                                 
102060        IF VORD-KDVALISO NOT = SPACE                                      
102070          MOVE VORD-KDVALISO   TO SPAR-KDVALISO                           
102080        END-IF                                                            
102090     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102300 FFBB-SUM-VARDE-RADER-STAT-P SECTION.                                     
102400                                                                          
102500     IF KOLLI-KVORDRAD     > ZERO                                         
102600       COMPUTE SPAR-KVRADER-P =   SPAR-KVRADER-P                          
102700                                + KOLLI-KVORDRAD                          
102800     END-IF                                                               
103410     IF AVERAGECOST                                                       
103420        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
103430          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
103440                                   + KOLLI-SUORDV-KLI-EXP                 
103450          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
103460                                   - KOLLI-SUORDV-KLI-EXP                 
103470        END-IF                                                            
103480     ELSE                                                                 
103490        IF KOLLI-SUORDV-KOLLI > ZERO                                      
103491          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
103492                                   + KOLLI-SUORDV-KOLLI                   
103493          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
103494                                   - KOLLI-SUORDV-KOLLI                   
103495        END-IF                                                            
103496     END-IF                                                               
103500     IF KOLLI-SUORDV-LOC     > ZERO                                       
103600       COMPUTE SPAR-SUORDV-P-LOC  =  SPAR-SUORDV-P-LOC                    
103700                                + KOLLI-SUORDV-LOC                        
103800       COMPUTE SPAR-SUORDV-U-LOC  =  SPAR-SUORDV-U-LOC                    
103900                                - KOLLI-SUORDV-LOC                        
104000     END-IF                                                               
104100     IF KOLLI-SUORDV-LOCPREL > ZERO                                       
104200       COMPUTE SPAR-SUORDV-P-LOCPREL = SPAR-SUORDV-P-LOCPREL              
104300                                + KOLLI-SUORDV-LOCPREL                    
104400       COMPUTE SPAR-SUORDV-U-LOCPREL = SPAR-SUORDV-U-LOCPREL              
104500                                - KOLLI-SUORDV-LOCPREL                    
104600     END-IF                                                               
104910     IF AVERAGECOST                                                       
104920        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
104930          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
104940        END-IF                                                            
104950     ELSE                                                                 
104960        IF KOLLI-KDVALISO NOT = SPACE                                     
104970          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
104980        END-IF                                                            
104990     END-IF                                                               
105000     .                                                                    
105100     EJECT                                                                
105200                                                                          
105300 FFBC-SUM-VARDE-RADER-STATUS-L SECTION.                                   
105400                                                                          
105500     IF KOLLI-KVORDRAD     > ZERO                                         
105600       COMPUTE SPAR-KVRADER-L =   SPAR-KVRADER-L                          
105700                                + KOLLI-KVORDRAD                          
105800     END-IF                                                               
106410     IF AVERAGECOST                                                       
106420        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
106430          COMPUTE SPAR-SUORDV-L = SPAR-SUORDV-L                           
106440                                   + KOLLI-SUORDV-KLI-EXP                 
106450          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
106460                                   - KOLLI-SUORDV-KLI-EXP                 
106470        END-IF                                                            
106480     ELSE                                                                 
106490        IF KOLLI-SUORDV-KOLLI > ZERO                                      
106491          COMPUTE SPAR-SUORDV-L = SPAR-SUORDV-L                           
106492                                   + KOLLI-SUORDV-KOLLI                   
106493          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
106494                                   - KOLLI-SUORDV-KOLLI                   
106495        END-IF                                                            
106496     END-IF                                                               
106500     IF KOLLI-SUORDV-LOC      > ZERO                                      
106600       COMPUTE SPAR-SUORDV-L-LOC  =   SPAR-SUORDV-L-LOC                   
106700                                + KOLLI-SUORDV-LOC                        
106800       COMPUTE SPAR-SUORDV-U-LOC  =   SPAR-SUORDV-U-LOC                   
106900                                - KOLLI-SUORDV-LOC                        
107000     END-IF                                                               
107100     IF KOLLI-SUORDV-LOCPREL  > ZERO                                      
107200       COMPUTE SPAR-SUORDV-L-LOCPREL = SPAR-SUORDV-L-LOCPREL              
107300                                + KOLLI-SUORDV-LOCPREL                    
107400       COMPUTE SPAR-SUORDV-U-LOCPREL = SPAR-SUORDV-U-LOCPREL              
107500                                - KOLLI-SUORDV-LOCPREL                    
107600     END-IF                                                               
107910     IF AVERAGECOST                                                       
107920        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
107930          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
107940        END-IF                                                            
107950     ELSE                                                                 
107960        IF KOLLI-KDVALISO NOT = SPACE                                     
107970          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
107980        END-IF                                                            
107990     END-IF                                                               
108000     .                                                                    
108100     EJECT                                                                
108200 FFBD-SUM-VARDE-RADER-STATUS-F SECTION.                                   
108300                                                                          
108400     IF KOLLI-KVORDRAD     > ZERO                                         
108500       COMPUTE SPAR-KVRADER-F =   SPAR-KVRADER-F                          
108600                                + KOLLI-KVORDRAD                          
108700     END-IF                                                               
109310     IF AVERAGECOST                                                       
109320       IF KOLLI-SUORDV-KLI-EXP > ZERO                                     
109330         COMPUTE SPAR-SUORDV-F =  SPAR-SUORDV-F                           
109340                                  + KOLLI-SUORDV-KLI-EXP                  
109350         COMPUTE SPAR-SUORDV-U =  SPAR-SUORDV-U                           
109360                                  - KOLLI-SUORDV-KLI-EXP                  
109370       END-IF                                                             
109380     ELSE                                                                 
109390       IF KOLLI-SUORDV-KOLLI   > ZERO                                     
109391         COMPUTE SPAR-SUORDV-F =  SPAR-SUORDV-F                           
109392                                  + KOLLI-SUORDV-KOLLI                    
109393         COMPUTE SPAR-SUORDV-U =  SPAR-SUORDV-U                           
109394                                  - KOLLI-SUORDV-KOLLI                    
109395      END-IF                                                              
109396     END-IF                                                               
109400     IF KOLLI-SUORDV-LOC       > ZERO                                     
109500       COMPUTE SPAR-SUORDV-F-LOC = SPAR-SUORDV-F-LOC                      
109600                                + KOLLI-SUORDV-LOC                        
109700       COMPUTE SPAR-SUORDV-U-LOC = SPAR-SUORDV-U-LOC                      
109800                                - KOLLI-SUORDV-LOC                        
109900     END-IF                                                               
110000     IF KOLLI-SUORDV-LOCPREL   > ZERO                                     
110100       COMPUTE SPAR-SUORDV-F-LOCPREL = SPAR-SUORDV-F-LOCPREL              
110200                                + KOLLI-SUORDV-LOCPREL                    
110300       COMPUTE SPAR-SUORDV-U-LOCPREL = SPAR-SUORDV-U-LOCPREL              
110400                                - KOLLI-SUORDV-LOCPREL                    
110500     END-IF                                                               
110810     IF AVERAGECOST                                                       
110820        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
110830          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
110840        END-IF                                                            
110850     ELSE                                                                 
110860        IF KOLLI-KDVALISO NOT = SPACE                                     
110870          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
110880        END-IF                                                            
110890     END-IF                                                               
110900     .                                                                    
111000     EJECT                                                                
111100 FFC-SUMMERA-STATUS-P-L-F SECTION.                                        
111200                                                                          
111300     IF ODEL-IDPRODNR NOT = SPAR-ODEL-IDPRODNR                            
111400       MOVE    ODEL-IDPRODNR TO W-WDE6-IDPRODNR                           
111500                                SPAR-ODEL-IDPRODNR                        
111600       PERFORM IMS-GET-WDE601                                             
111601       IF VORD-IDDC-EXP NOT = SPACE                                       
111602          IF VORD-IDDC-EXP = WC-CDC-SE                                    
111603*           *BOUNCE DC = 11                                               
111604            IF VORD-IDDC = REQU-IDDC-KEY                                  
111605               MOVE JA  TO AVERAGECOST-SW                                 
111606            ELSE                                                          
111607               MOVE NEJ TO AVERAGECOST-SW                                 
111608            END-IF                                                        
111609          ELSE                                                            
111610*           *BOUNCE DC NOT 11                                             
111611            IF VORD-IDDC = REQU-IDDC-KEY                                  
111612               MOVE NEJ TO AVERAGECOST-SW                                 
111613            ELSE                                                          
111614               MOVE JA  TO AVERAGECOST-SW                                 
111615            END-IF                                                        
111616          END-IF                                                          
111622       ELSE                                                               
111623          IF VORD-SUORDV-EXP > 0                                          
111624             MOVE JA  TO AVERAGECOST-SW                                   
111625          ELSE                                                            
111626             MOVE NEJ TO AVERAGECOST-SW                                   
111627          END-IF                                                          
111630       END-IF                                                             
111700                                                                          
111800       IF SEGMENT-FINNS                                                   
111900         MOVE    JA  TO SW-SKALL-VARDEN-LAGGAS-UT                         
112000         COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                    
112100                                    + VORD-KVORDRAD                       
112310         IF AVERAGECOST                                                   
112320            COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                   
112330                                      + VORD-SUORDV-EXP                   
112340         ELSE                                                             
112350            COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                   
112360                                        + VORD-SUORDV                     
112370         END-IF                                                           
112400     COMPUTE SPAR-SUORDV-TOT-LOC =   SPAR-SUORDV-TOT-LOC                  
112500                                    + VORD-SUORDV-LOC                     
112600     COMPUTE SPAR-SUORDV-TOT-LOCPREL  =   SPAR-SUORDV-TOT-LOCPREL         
112700                                    + VORD-SUORDV-LOCPREL                 
112800         PERFORM IMS-GNP-WDE611                                           
112900         PERFORM UNTIL NOT SEGMENT-FINNS                                  
113000                                                                          
113100           IF KOLLI-KDKOLSTA = K-KDKOLSTA-1-FARDIG-PACKAT OR              
113200                               K-KDKOLSTA-6-FAKTREL                       
113300             MOVE    NEJ TO SW-ORDER-KLAR                                 
113400             PERFORM FFCA-SUM-VARDE-RADER-STAT-P                          
113500             IF KOLLI-KDKOLSTA = K-KDKOLSTA-6-FAKTREL                     
113600               PERFORM S02-SPARA-FAKT-INFO-I-TABELL                       
113700             END-IF                                                       
113800           ELSE                                                           
113900             IF KOLLI-KDKOLSTA = K-KDKOLSTA-2-LASTREL     OR              
114000                                 K-KDKOLSTA-3-LASTAT      OR              
114100                                 K-KDKOLSTA-4-LASTAT-FAKTREL              
114200               MOVE    NEJ TO SW-ORDER-KLAR                               
114300               PERFORM FFCB-SUM-VARDE-RADER-STATUS-L                      
114400               IF KOLLI-KDKOLSTA = K-KDKOLSTA-4-LASTAT-FAKTREL            
114500                 PERFORM S02-SPARA-FAKT-INFO-I-TABELL                     
114600               END-IF                                                     
114700             ELSE                                                         
114800               IF KOLLI-KDKOLSTA = K-KDKOLSTA-7-FAKTURERAT   OR           
114900                                   K-KDKOLSTA-8-FAKT-LASTREL OR           
115000                                   K-KDKOLSTA-9-FAKT-LASTAT               
115100                 IF KOLLI-KDKOLSTA = K-KDKOLSTA-7-FAKTURERAT  OR          
115200                                     K-KDKOLSTA-8-FAKT-LASTREL            
115300                   MOVE NEJ TO SW-ORDER-KLAR                              
115400                 END-IF                                                   
115500                 PERFORM FFCC-SUM-VARDE-RADER-STATUS-F                    
115600                 PERFORM S02-SPARA-FAKT-INFO-I-TABELL                     
115700               END-IF                                                     
115800             END-IF                                                       
115900           END-IF                                                         
116000           PERFORM IMS-GNP-WDE611                                         
116100         END-PERFORM                                                      
116200       END-IF                                                             
116300     END-IF                                                               
116400     .                                                                    
116500     EJECT                                                                
116600 FFCA-SUM-VARDE-RADER-STAT-P SECTION.                                     
116700                                                                          
116800     IF KOLLI-KVORDRAD     > ZERO                                         
116900       COMPUTE SPAR-KVRADER-P =   SPAR-KVRADER-P                          
117000                                + KOLLI-KVORDRAD                          
117100     END-IF                                                               
117510     IF AVERAGECOST                                                       
117520        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
117530          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
117540                                   + KOLLI-SUORDV-KLI-EXP                 
117550        END-IF                                                            
117560     ELSE                                                                 
117570        IF KOLLI-SUORDV-KOLLI > ZERO                                      
117580          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
117590                                   + KOLLI-SUORDV-KOLLI                   
117591        END-IF                                                            
117592     END-IF                                                               
117600     IF KOLLI-SUORDV-LOC     > ZERO                                       
117700       COMPUTE SPAR-SUORDV-P-LOC = SPAR-SUORDV-P-LOC                      
117800                                + KOLLI-SUORDV-LOC                        
117900     END-IF                                                               
118000     IF KOLLI-SUORDV-LOCPREL > ZERO                                       
118100       COMPUTE SPAR-SUORDV-P-LOCPREL = SPAR-SUORDV-P-LOCPREL              
118200                                + KOLLI-SUORDV-LOCPREL                    
118300     END-IF                                                               
118610     IF AVERAGECOST                                                       
118620        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
118630          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
118640        END-IF                                                            
118650     ELSE                                                                 
118660        IF KOLLI-KDVALISO NOT = SPACE                                     
118670          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
118680        END-IF                                                            
118690     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900                                                                          
119000 FFCB-SUM-VARDE-RADER-STATUS-L SECTION.                                   
119100                                                                          
119200     IF KOLLI-KVORDRAD     > ZERO                                         
119300       COMPUTE SPAR-KVRADER-L =   SPAR-KVRADER-L                          
119400                                + KOLLI-KVORDRAD                          
119500     END-IF                                                               
119910     IF AVERAGECOST                                                       
119920       IF KOLLI-SUORDV-KLI-EXP > ZERO                                     
119930         COMPUTE SPAR-SUORDV-L =  SPAR-SUORDV-L                           
119940                                  + KOLLI-SUORDV-KLI-EXP                  
119950       END-IF                                                             
119960     ELSE                                                                 
119970       IF KOLLI-SUORDV-KOLLI > ZERO                                       
119980         COMPUTE SPAR-SUORDV-L =  SPAR-SUORDV-L                           
119990                                  + KOLLI-SUORDV-KOLLI                    
119991       END-IF                                                             
119992     END-IF                                                               
120000     IF KOLLI-SUORDV-LOC     > ZERO                                       
120100       COMPUTE SPAR-SUORDV-L-LOC = SPAR-SUORDV-L-LOC                      
120200                                + KOLLI-SUORDV-LOC                        
120300     END-IF                                                               
120400     IF KOLLI-SUORDV-LOCPREL > ZERO                                       
120500       COMPUTE SPAR-SUORDV-L-LOCPREL = SPAR-SUORDV-L-LOCPREL              
120600                                + KOLLI-SUORDV-LOCPREL                    
120700     END-IF                                                               
121010     IF AVERAGECOST                                                       
121020        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
121030          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
121040        END-IF                                                            
121050     ELSE                                                                 
121060        IF KOLLI-KDVALISO NOT = SPACE                                     
121070          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
121080        END-IF                                                            
121090     END-IF                                                               
121100     .                                                                    
121200     EJECT                                                                
121300 FFCC-SUM-VARDE-RADER-STATUS-F SECTION.                                   
121400                                                                          
121500     IF KOLLI-KVORDRAD     > ZERO                                         
121600       COMPUTE SPAR-KVRADER-F =   SPAR-KVRADER-F                          
121700                                + KOLLI-KVORDRAD                          
121800     END-IF                                                               
122210     IF AVERAGECOST                                                       
122220        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
122230          COMPUTE SPAR-SUORDV-F = SPAR-SUORDV-F                           
122240                                   + KOLLI-SUORDV-KLI-EXP                 
122250        END-IF                                                            
122260     ELSE                                                                 
122270        IF KOLLI-SUORDV-KOLLI > ZERO                                      
122280          COMPUTE SPAR-SUORDV-F = SPAR-SUORDV-F                           
122290                                   + KOLLI-SUORDV-KOLLI                   
122291        END-IF                                                            
122292     END-IF                                                               
122300     IF KOLLI-SUORDV-LOC      > ZERO                                      
122400       COMPUTE SPAR-SUORDV-F-LOC = SPAR-SUORDV-F-LOC                      
122500                                + KOLLI-SUORDV-LOC                        
122600     END-IF                                                               
122700     IF KOLLI-SUORDV-LOCPREL  > ZERO                                      
122800       COMPUTE SPAR-SUORDV-F-LOCPREL = SPAR-SUORDV-F-LOCPREL              
122900                                + KOLLI-SUORDV-LOCPREL                    
123000     END-IF                                                               
123110     IF AVERAGECOST                                                       
123120        MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                          
123130     ELSE                                                                 
123140        MOVE KOLLI-KDVALISO     TO SPAR-KDVALISO                          
123150     END-IF                                                               
123200     .                                                                    
123300     EJECT                                                                
123400 FFE-FYLL-MOD-MED-TOTALER SECTION.                                        
123500                                                                          
123600     IF DIST79-DEALER-PRICE                                               
123700       COMPUTE HELP-SUMMA = SPAR-SUORDV-TOT-LOC +                         
123800                            SPAR-SUORDV-TOT-LOCPREL                       
124000       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
124100       IF SPAR-SUORDV-TOT-LOCPREL = +0                                    
124200         MOVE ' '                TO RESP-ASTERIX1                         
124300       ELSE                                                               
124400         MOVE '*'                TO RESP-ASTERIX1                         
124500       END-IF                                                             
124600     ELSE                                                                 
124700       MOVE SPAR-SUORDV-TOT      TO HELP-SUORDV-OPACK                     
124800       MOVE ' '                  TO RESP-ASTERIX1                         
124900     END-IF                                                               
125000     IF SEC-KDSVAR = 2 OR 6                                               
125100       MOVE ZERO                 TO RESP-SUORDV-TOT                       
125200     ELSE                                                                 
125300       MOVE HELP-SUORDV-OPACK    TO RESP-SUORDV-TOT                       
125400     END-IF                                                               
125500                                                                          
125600     MOVE SPAR-KVRADER-TOT       TO HELP-KVRADER-OPACK                    
125700     MOVE HELP-KVRADER-OPACK     TO RESP-KVRADER-TOT                      
125800                                                                          
125900     IF DIST79-DEALER-PRICE                                               
126000       COMPUTE HELP-SUMMA = SPAR-SUORDV-U-LOC +                           
126100                            SPAR-SUORDV-U-LOCPREL                         
126300       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
126400       IF SPAR-SUORDV-U-LOCPREL = +0                                      
126500         MOVE ' '                TO RESP-ASTERIX2                         
126600       ELSE                                                               
126700         MOVE '*'                TO RESP-ASTERIX2                         
126800       END-IF                                                             
126900     ELSE                                                                 
127000       MOVE SPAR-SUORDV-U        TO HELP-SUORDV-OPACK                     
127100       MOVE ' '                  TO RESP-ASTERIX2                         
127200     END-IF                                                               
127300     IF SEC-KDSVAR = 2 OR 6                                               
127400       MOVE ZERO                 TO RESP-SUORDV-U                         
127500     ELSE                                                                 
127600       MOVE HELP-SUORDV-OPACK    TO RESP-SUORDV-U                         
127700     END-IF                                                               
127800                                                                          
127900     IF DIST79-DEALER-PRICE                                               
128000       COMPUTE HELP-SUMMA = SPAR-SUORDV-P-LOC +                           
128100                            SPAR-SUORDV-P-LOCPREL                         
128300       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
128400       IF SPAR-SUORDV-P-LOCPREL = +0                                      
128500         MOVE ' '                TO RESP-ASTERIX3                         
128600       ELSE                                                               
128700         MOVE '*'                TO RESP-ASTERIX3                         
128800       END-IF                                                             
128900     ELSE                                                                 
129000       MOVE SPAR-SUORDV-P        TO HELP-SUORDV-OPACK                     
129100       MOVE ' '                  TO RESP-ASTERIX3                         
129200     END-IF                                                               
129300     IF SEC-KDSVAR = 2 OR 6                                               
129400       MOVE ZERO                 TO RESP-SUORDV-P                         
129500     ELSE                                                                 
129600       MOVE HELP-SUORDV-OPACK    TO RESP-SUORDV-P                         
129700     END-IF                                                               
129800                                                                          
129900     MOVE SPAR-KVRADER-P         TO HELP-KVRADER-OPACK                    
130000     MOVE HELP-KVRADER-OPACK     TO RESP-KVRADER-P                        
130100                                                                          
130200     IF DIST79-DEALER-PRICE                                               
130300       COMPUTE HELP-SUMMA = SPAR-SUORDV-F-LOC +                           
130400                            SPAR-SUORDV-F-LOCPREL                         
130600       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
130700       IF SPAR-SUORDV-F-LOCPREL = +0                                      
130800         MOVE ' '                TO RESP-ASTERIX4                         
130900       ELSE                                                               
131000         MOVE '*'                TO RESP-ASTERIX4                         
131100       END-IF                                                             
131200     ELSE                                                                 
131300       MOVE SPAR-SUORDV-F        TO HELP-SUORDV-OPACK                     
131400       MOVE ' '                  TO RESP-ASTERIX4                         
131500     END-IF                                                               
131600     IF SEC-KDSVAR = 2 OR 6                                               
131700       MOVE ZERO                 TO RESP-SUORDV-F                         
131800     ELSE                                                                 
131900       MOVE HELP-SUORDV-OPACK    TO RESP-SUORDV-F                         
132000     END-IF                                                               
132100                                                                          
132200     MOVE SPAR-KVRADER-F         TO HELP-KVRADER-OPACK                    
132300     MOVE HELP-KVRADER-OPACK     TO RESP-KVRADER-F                        
132400                                                                          
132500     IF DIST79-DEALER-PRICE                                               
132600       COMPUTE HELP-SUMMA = SPAR-SUORDV-L-LOC +                           
132700                            SPAR-SUORDV-L-LOCPREL                         
132900       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
133000       IF SPAR-SUORDV-L-LOCPREL = +0                                      
133100         MOVE ' '                TO RESP-ASTERIX5                         
133200       ELSE                                                               
133300         MOVE '*'                TO RESP-ASTERIX5                         
133400       END-IF                                                             
133500     ELSE                                                                 
133600       MOVE SPAR-SUORDV-L        TO HELP-SUORDV-OPACK                     
133700       MOVE ' '                  TO RESP-ASTERIX5                         
133800     END-IF                                                               
133900     IF SEC-KDSVAR = 2 OR 6                                               
134000       MOVE ZERO                 TO RESP-SUORDV-L                         
134100     ELSE                                                                 
134200       MOVE HELP-SUORDV-OPACK    TO RESP-SUORDV-L                         
134300     END-IF                                                               
134400                                                                          
134500     MOVE SPAR-KVRADER-L         TO HELP-KVRADER-OPACK                    
134600     MOVE HELP-KVRADER-OPACK     TO RESP-KVRADER-L                        
134700     .                                                                    
134800     EJECT                                                                
134900 FFF-FYLL-MOD-MED-FAKT-INFO SECTION.                                      
135000                                                                          
135100     PERFORM S01-SORTERA-FAKT-INFO-TABELL                                 
135200                                                                          
135300     IF TAB-FAKT-IDFAKT(1) = ALL '9'                                      
135400       MOVE ZERO               TO RESP-IDFAKT(1)                          
135500                                  RESP-TIFAKT(1)                          
135600     ELSE                                                                 
135700       MOVE TAB-FAKT-IDFAKT(1) TO RESP-IDFAKT(1)                          
135800       MOVE TAB-FAKT-TIFAKT(1) TO RESP-TIFAKT(1)                          
135900     END-IF                                                               
136000                                                                          
136100     IF TAB-FAKT-IDFAKT(2) = ALL '9'                                      
136200       MOVE ZERO               TO RESP-IDFAKT(5)                          
136300                                  RESP-TIFAKT(5)                          
136400     ELSE                                                                 
136500       MOVE TAB-FAKT-IDFAKT(2) TO RESP-IDFAKT(5)                          
136600       MOVE TAB-FAKT-TIFAKT(2) TO RESP-TIFAKT(5)                          
136700     END-IF                                                               
136800                                                                          
136900     IF TAB-FAKT-IDFAKT(3) = ALL '9'                                      
137000       MOVE ZERO               TO RESP-IDFAKT(9)                          
137100                                  RESP-TIFAKT(9)                          
137200     ELSE                                                                 
137300       MOVE TAB-FAKT-IDFAKT(3) TO RESP-IDFAKT(9)                          
137400       MOVE TAB-FAKT-TIFAKT(3) TO RESP-TIFAKT(9)                          
137500     END-IF                                                               
137600                                                                          
137700     IF TAB-FAKT-IDFAKT(4) = ALL '9'                                      
137800       MOVE ZERO               TO RESP-IDFAKT(2)                          
137900                                  RESP-TIFAKT(2)                          
138000     ELSE                                                                 
138100       MOVE TAB-FAKT-IDFAKT(4) TO RESP-IDFAKT(2)                          
138200       MOVE TAB-FAKT-TIFAKT(4) TO RESP-TIFAKT(2)                          
138300     END-IF                                                               
138400                                                                          
138500     IF TAB-FAKT-IDFAKT(5) = ALL '9'                                      
138600       MOVE ZERO               TO RESP-IDFAKT(6)                          
138700                                  RESP-TIFAKT(6)                          
138800     ELSE                                                                 
138900       MOVE TAB-FAKT-IDFAKT(5) TO RESP-IDFAKT(6)                          
139000       MOVE TAB-FAKT-TIFAKT(5) TO RESP-TIFAKT(6)                          
139100     END-IF                                                               
139200                                                                          
139300     IF TAB-FAKT-IDFAKT(6) = ALL '9'                                      
139400       MOVE ZERO               TO RESP-IDFAKT(10)                         
139500                                  RESP-TIFAKT(10)                         
139600     ELSE                                                                 
139700       MOVE TAB-FAKT-IDFAKT(6) TO RESP-IDFAKT(10)                         
139800       MOVE TAB-FAKT-TIFAKT(6) TO RESP-TIFAKT(10)                         
139900     END-IF                                                               
140000                                                                          
140100     IF TAB-FAKT-IDFAKT(7) = ALL '9'                                      
140200       MOVE ZERO               TO RESP-IDFAKT(3)                          
140300                                  RESP-TIFAKT(3)                          
140400     ELSE                                                                 
140500       MOVE TAB-FAKT-IDFAKT(7) TO RESP-IDFAKT(3)                          
140600       MOVE TAB-FAKT-TIFAKT(7) TO RESP-TIFAKT(3)                          
140700     END-IF                                                               
140800                                                                          
140900     IF TAB-FAKT-IDFAKT(8) = ALL '9'                                      
141000       MOVE ZERO               TO RESP-IDFAKT(7)                          
141100                                  RESP-TIFAKT(7)                          
141200     ELSE                                                                 
141300       MOVE TAB-FAKT-IDFAKT(8) TO RESP-IDFAKT(7)                          
141400       MOVE TAB-FAKT-TIFAKT(8) TO RESP-TIFAKT(7)                          
141500     END-IF                                                               
141600                                                                          
141700     IF TAB-FAKT-IDFAKT(9) = ALL '9'                                      
141800       MOVE ZERO               TO RESP-IDFAKT(11)                         
141900                                  RESP-TIFAKT(11)                         
142000     ELSE                                                                 
142100       MOVE TAB-FAKT-IDFAKT(9) TO RESP-IDFAKT(11)                         
142200       MOVE TAB-FAKT-TIFAKT(9) TO RESP-TIFAKT(11)                         
142300     END-IF                                                               
142400                                                                          
142500     IF TAB-FAKT-IDFAKT(10) = ALL '9'                                     
142600       MOVE ZERO               TO RESP-IDFAKT(4)                          
142700                                  RESP-TIFAKT(4)                          
142800     ELSE                                                                 
142900       MOVE TAB-FAKT-IDFAKT(10) TO RESP-IDFAKT(4)                         
143000       MOVE TAB-FAKT-TIFAKT(10) TO RESP-TIFAKT(4)                         
143100     END-IF                                                               
143200                                                                          
143300     IF TAB-FAKT-IDFAKT(11) = ALL '9'                                     
143400       MOVE ZERO               TO RESP-IDFAKT(8)                          
143500                                  RESP-TIFAKT(8)                          
143600     ELSE                                                                 
143700       MOVE TAB-FAKT-IDFAKT(11) TO RESP-IDFAKT(8)                         
143800       MOVE TAB-FAKT-TIFAKT(11) TO RESP-TIFAKT(8)                         
143900     END-IF                                                               
144000                                                                          
144100     IF TAB-FAKT-IDFAKT(12) = ALL '9'                                     
144200       MOVE ZERO               TO RESP-IDFAKT(12)                         
144300                                  RESP-TIFAKT(12)                         
144400     ELSE                                                                 
144500       MOVE TAB-FAKT-IDFAKT(12) TO RESP-IDFAKT(12)                        
144600       MOVE TAB-FAKT-TIFAKT(12) TO RESP-TIFAKT(12)                        
144700     END-IF                                                               
144800     .                                                                    
144900     EJECT                                                                
145000 S01-SORTERA-FAKT-INFO-TABELL SECTION.                                    
145100                                                                          
145200************* FIX FÖR ATT KLARA SEKELSKIFTET ****************             
145300************* ÅR < 50 BLIR ÅR + 50           ****************             
145400************* ÅR > 50 BLIR ÅR - 50           ****************             
145500     MOVE 1             TO Y2K-IX                                         
145600     PERFORM UNTIL Y2K-IX > 12                                            
145700       MOVE TAB-FAKT-TIFAKT (Y2K-IX) TO TMP1-YYMMDD                       
145800       PERFORM WY2000P1                                                   
145900       MOVE TMP1-YYMMDD     TO TAB-FAKT-TIFAKT (Y2K-IX)                   
146000       ADD 1                TO Y2K-IX                                     
146100     END-PERFORM                                                          
146200                                                                          
146300     CALL WINTSOR USING TABELL-FAKTURA-INFO                               
146400                        TAB-FAKT-WINTSOR-STEG-LANGD                       
146500                        TAB-FAKT-WINTSOR-ANTAL                            
146600                        TAB-FAKT-ELEM(1)                                  
146700                        TAB-FAKT-WINTSOR-SORT-LANGD                       
146800                                                                          
146900************* FIX FÖR ATT KLARA SEKELSKIFTET ****************             
147000************* ÅTERSTÄLLER DATUMEN            ****************             
147100     MOVE 1             TO Y2K-IX                                         
147200     PERFORM UNTIL Y2K-IX > 12                                            
147300       MOVE TAB-FAKT-TIFAKT (Y2K-IX) TO TMP1-YYMMDD                       
147400       PERFORM WY2000P1                                                   
147500       MOVE TMP1-YYMMDD     TO TAB-FAKT-TIFAKT (Y2K-IX)                   
147600       ADD 1                TO Y2K-IX                                     
147700     END-PERFORM                                                          
147800                                                                          
147900     .                                                                    
148000     EJECT                                                                
148100 S02-SPARA-FAKT-INFO-I-TABELL SECTION.                                    
148200                                                                          
148300     IF AVERAGECOST AND BOUNCEORDER                                       
148400        PERFORM S02A-FAKT-INFO-AVERAGECOST                                
148500     ELSE                                                                 
148600        PERFORM S02B-FAKT-INFO-NORMALORDER                                
148700     END-IF                                                               
148800     .                                                                    
148900     EJECT                                                                
149000 S02A-FAKT-INFO-AVERAGECOST   SECTION.                                    
149100                                                                          
149200     IF KOLLI-IDFAKT-EXP > ZERO                                           
149300       IF TAB-FAKT-VERKLIGT-ANTAL < TAB-FAKT-IX-MAX-12                    
149400         SET TAB-FAKT-IX TO +1                                            
149500         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
149600                 KOLLI-IDFAKT-EXP = TAB-FAKT-IDFAKT(TAB-FAKT-IX)          
149700           IF TAB-FAKT-IDFAKT(TAB-FAKT-IX) = ALL '9'                      
149800             MOVE KOLLI-IDFAKT-EXP TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
149900             MOVE KOLLI-TIFAKT-EXP TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
150000             SET  TAB-FAKT-VERKLIGT-ANTAL TO TAB-FAKT-IX                  
150100           ELSE                                                           
150200             SET  TAB-FAKT-IX UP BY +1                                    
150300           END-IF                                                         
150400         END-PERFORM                                                      
150500       ELSE                                                               
150600         IF SW-FAKT-INFO-TAB-SORTERAD = NEJ                               
150700           PERFORM S01-SORTERA-FAKT-INFO-TABELL                           
150800           MOVE    JA TO SW-FAKT-INFO-TAB-SORTERAD                        
150900         END-IF                                                           
151000         SET TAB-FAKT-IX TO +1                                            
151100         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
151200                   KOLLI-IDFAKT-EXP = TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
151300           MOVE KOLLI-TIFAKT-EXP               TO TMP1-YYMMDD             
151400           MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX)   TO TMP2-YYMMDD             
151500           PERFORM WY2000P1                                               
151600           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
151700             SET     TAB-FAKT-IX TO TAB-FAKT-IX-MAX-12                    
151800             MOVE KOLLI-TIFAKT-EXP             TO TMP1-YYMMDD             
151900             MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD             
152000             PERFORM WY2000P1                                             
152100             PERFORM UNTIL TMP1-YYMMDD >= TMP2-YYMMDD           OR        
152200                           TAB-FAKT-IX  = TAB-FAKT-IX-MIN-1               
152300               MOVE TAB-FAKT-IDFAKT(TAB-FAKT-IX - 1) TO                   
152400                                      TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
152500               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX - 1) TO                   
152600                                      TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
152700               SET  TAB-FAKT-IX DOWN BY +1                                
152800               MOVE KOLLI-TIFAKT-EXP             TO TMP1-YYMMDD           
152900               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD           
153000               PERFORM WY2000P1                                           
153100             END-PERFORM                                                  
153200             IF TAB-FAKT-IX = 1                                           
153300               MOVE KOLLI-IDFAKT-EXP                                      
153400                                   TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
153500               MOVE KOLLI-TIFAKT-EXP                                      
153600                                   TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
153700               SET  TAB-FAKT-IX DOWN BY +1                                
153800             ELSE                                                         
153900               MOVE KOLLI-IDFAKT-EXP TO TAB-FAKT-IDFAKT                   
153910                                                 (TAB-FAKT-IX + 1)        
153920               MOVE KOLLI-TIFAKT-EXP TO TAB-FAKT-TIFAKT                   
153930                                                 (TAB-FAKT-IX + 1)        
153940             END-IF                                                       
153950           END-IF                                                         
153960           SET TAB-FAKT-IX UP BY +1                                       
153970         END-PERFORM                                                      
153980       END-IF                                                             
153990     END-IF                                                               
153991     .                                                                    
153992     EJECT                                                                
153993 S02B-FAKT-INFO-NORMALORDER   SECTION.                                    
153994                                                                          
153995     IF KOLLI-IDFAKT > ZERO                                               
153996       IF TAB-FAKT-VERKLIGT-ANTAL < TAB-FAKT-IX-MAX-12                    
153997         SET TAB-FAKT-IX TO +1                                            
153998         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
153999                       KOLLI-IDFAKT = TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
154000           IF TAB-FAKT-IDFAKT(TAB-FAKT-IX) = ALL '9'                      
154001             MOVE KOLLI-IDFAKT TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)            
154002             MOVE KOLLI-TIFAKT TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)            
154003             SET  TAB-FAKT-VERKLIGT-ANTAL TO TAB-FAKT-IX                  
154004           ELSE                                                           
154005             SET  TAB-FAKT-IX UP BY +1                                    
154006           END-IF                                                         
154007         END-PERFORM                                                      
154008       ELSE                                                               
154009         IF SW-FAKT-INFO-TAB-SORTERAD = NEJ                               
154010           PERFORM S01-SORTERA-FAKT-INFO-TABELL                           
154011           MOVE    JA TO SW-FAKT-INFO-TAB-SORTERAD                        
154012         END-IF                                                           
154013         SET TAB-FAKT-IX TO +1                                            
154014         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
154015                       KOLLI-IDFAKT = TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
154016           MOVE KOLLI-TIFAKT                   TO TMP1-YYMMDD             
154017           MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX)   TO TMP2-YYMMDD             
154018           PERFORM WY2000P1                                               
154019           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
154020             SET     TAB-FAKT-IX TO TAB-FAKT-IX-MAX-12                    
154021             MOVE KOLLI-TIFAKT                 TO TMP1-YYMMDD             
154022             MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD             
154023             PERFORM WY2000P1                                             
154024             PERFORM UNTIL TMP1-YYMMDD >= TMP2-YYMMDD           OR        
154025                           TAB-FAKT-IX  = TAB-FAKT-IX-MIN-1               
154026               MOVE TAB-FAKT-IDFAKT(TAB-FAKT-IX - 1) TO                   
154027                                      TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
154028               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX - 1) TO                   
154029                                      TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
154030               SET  TAB-FAKT-IX DOWN BY +1                                
154031               MOVE KOLLI-TIFAKT                 TO TMP1-YYMMDD           
154032               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD           
154033               PERFORM WY2000P1                                           
154034             END-PERFORM                                                  
154035             IF TAB-FAKT-IX = 1                                           
154036               MOVE KOLLI-IDFAKT TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)          
154037               MOVE KOLLI-TIFAKT TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)          
154038               SET  TAB-FAKT-IX DOWN BY +1                                
154039             ELSE                                                         
154040               MOVE KOLLI-IDFAKT TO TAB-FAKT-IDFAKT                       
154041                                                 (TAB-FAKT-IX + 1)        
154042               MOVE KOLLI-TIFAKT TO TAB-FAKT-TIFAKT                       
154043                                                 (TAB-FAKT-IX + 1)        
154044             END-IF                                                       
154045           END-IF                                                         
154046           SET TAB-FAKT-IX UP BY +1                                       
154047         END-PERFORM                                                      
154048       END-IF                                                             
154049     END-IF                                                               
154050     .                                                                    
154051     EJECT                                                                
157300*    --- DISPATCHER SECTIONS                                              
157400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
157500                                                                          
157600     MOVE 'GETARG'               TO SUB-KDFUNC                            
157700     MOVE 'CARPARTS.LDC.ORDERQUERYVALUE'     TO SUB-ADDISPABS             
157800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
157900                                                                          
158000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
158100                                                                          
158200     IF SUB-KDRC > 0                                                      
158300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
158400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
158500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
158600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
158700     END-IF                                                               
158800     .                                                                    
158900     SKIP3                                                                
159000 S02-RETURN-RESPONSE SECTION.                                             
159100                                                                          
159200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
159300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
159400                                                                          
159500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
159600                                                                          
159700     IF SUB-KDRC > 0                                                      
159800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
159900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
160000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
160100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
160200     END-IF                                                               
160300     .                                                                    
160400     EJECT                                                                
160500 IMS-GET-WDQ201-CSEQ SECTION.                                             
160600                                                                          
160700     STRING 'WLORQI01(WDQ2CSEQ>=' W-WDQ2CSEQ-MIN-X                        
160800                    '&WDQ2CSEQ<=' W-WDQ2CSEQ-MAX-X ')'                    
160900          DELIMITED BY SIZE INTO    SSA1                                  
161000     MOVE    '  GE'           TO    GODK-STATUSKODER                      
161100     CALL    CBLTDLI          USING GU   ORQI-PCB DLI-IO-WLORQI01         
161200                                         SSA1                             
161300     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
161400     PERFORM IMS-STATUSKONTROLL                                           
161500     .                                                                    
161600     EJECT                                                                
161700 IMS-GNP-FIRST-WDQ211 SECTION.                                            
161800                                                                          
161900     MOVE   'WLORQI11*F'      TO    SSA1                                  
162000     MOVE    '  GE'           TO    GODK-STATUSKODER                      
162100     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI11         
162200                                         SSA1                             
162300     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
162400     PERFORM IMS-STATUSKONTROLL                                           
162500     .                                                                    
162600                                                                          
162700 IMS-GNP-WDQ211 SECTION.                                                  
162800                                                                          
162900     MOVE   'WLORQI11'        TO    SSA1                                  
163000     MOVE    '  GE'           TO    GODK-STATUSKODER                      
163100     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI11         
163200                                         SSA1                             
163300     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
163400     PERFORM IMS-STATUSKONTROLL                                           
163500     .                                                                    
163600     EJECT                                                                
163700 IMS-GNP-WDQ212 SECTION.                                                  
163800                                                                          
163900     STRING 'WLORQI12(IDDC     =' W-IDDC-X     ')'                        
164000          DELIMITED BY SIZE INTO    SSA1                                  
164100     MOVE    '  GE'           TO    GODK-STATUSKODER                      
164200     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI12         
164300                                         SSA1                             
164400     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700     EJECT                                                                
164800 IMS-GNP-WDQ212-OKVAL SECTION.                                            
164900                                                                          
165000     MOVE    'WLORQI12'       TO    SSA1                                  
165100     MOVE    '  GE'           TO    GODK-STATUSKODER                      
165200     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI12         
165300                                         SSA1                             
165400     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
165500     PERFORM IMS-STATUSKONTROLL                                           
165600     .                                                                    
165700     EJECT                                                                
165800 IMS-GNP-WDQ221 SECTION.                                                  
165900                                                                          
166000     STRING 'WLORQI12(IDDC     =' W-IDDC-X     ')'                        
166100          DELIMITED BY SIZE INTO    SSA1                                  
166200     MOVE   'WLORQI21'        TO    SSA2                                  
166300     MOVE    '  GE'           TO    GODK-STATUSKODER                      
166400     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI21         
166500                                         SSA1 SSA2                        
166600     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
166700     PERFORM IMS-STATUSKONTROLL                                           
166800     .                                                                    
166801     EJECT                                                                
166802 IMS-GU-WDQ301-FIRST SECTION.                                             
166803     STRING 'WLORQA01*F(WDQ301KY>=' W-WDQ301KY-MIN-X                      
166804                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
166805          DELIMITED BY SIZE INTO    SSA1                                  
166806     MOVE    '  GE'           TO    GODK-STATUSKODER                      
166807     CALL    CBLTDLI          USING GU   ORQA-PCB DLI-IO-WLORQA01         
166808                                         SSA1                             
166809     MOVE    ORQA-STATUS-CODE TO    STATUS-WS                             
166810     PERFORM IMS-STATUSKONTROLL                                           
166811     .                                                                    
166812     EJECT                                                                
166813 IMS-GN-WDQ301-INTERV SECTION.                                            
166814     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
166815                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
166816          DELIMITED BY SIZE INTO    SSA1                                  
166817     MOVE    '  GE'           TO    GODK-STATUSKODER                      
166818     CALL    CBLTDLI          USING GN   ORQA-PCB DLI-IO-WLORQA01         
166819                                         SSA1                             
166820     MOVE    ORQA-STATUS-CODE TO    STATUS-WS                             
166821     PERFORM IMS-STATUSKONTROLL                                           
166822     .                                                                    
166823     EJECT                                                                
166900 IMS-GET-WDE601 SECTION.                                                  
167000                                                                          
167100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
167200          DELIMITED BY SIZE INTO    SSA1                                  
167300     MOVE    '  GE'           TO    GODK-STATUSKODER                      
167400     CALL    CBLTDLI          USING GU   WDE6-PCB DLI-IO-WDE601           
167500                                         SSA1                             
167600     MOVE    WDE6-STATUS-CODE TO    STATUS-WS                             
167700     PERFORM IMS-STATUSKONTROLL                                           
167800     .                                                                    
167900                                                                          
168000 IMS-GNP-WDE611 SECTION.                                                  
168100                                                                          
168200     MOVE   'WDE611  '        TO    SSA1                                  
168300     MOVE    '  GE'           TO    GODK-STATUSKODER                      
168400     CALL    CBLTDLI          USING GNP  WDE6-PCB DLI-IO-WDE611           
168500                                         SSA1                             
168600     MOVE    WDE6-STATUS-CODE TO    STATUS-WS                             
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     .                                                                    
168900     EJECT                                                                
173700 IMS-STATUSKONTROLL SECTION.                                              
173800                                                                          
173900     SET    STATUS-IX TO +1                                               
174000     SEARCH GODK-STATUS                                                   
174100       AT END                                                             
174200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
174300              DELIMITED BY SIZE INTO FELTEXT                              
174400         CALL FELLOG                                                      
174500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
174600         CONTINUE                                                         
174700     END-SEARCH                                                           
174800     .                                                                    
174900     EJECT                                                                
175000*    -COPY WY2000P1                                                       
