000100*COMPOPT DB2BIND=YES                                                      
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W2133400.                                                
000500 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000600 DATE-WRITTEN.   96/10/03.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900                                                                          
001000*    FUNKTION:                                                            
001100*        LÄSER FIL MED DB-UPPDATERINGAR FRÅN W21332                       
001200*        WDK6 OCH WDD9 UPPDATERAS MED DESSA                               
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001500*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
001600*        PROGRAMMET UPPDATERAR WLINLB (WDD6)                              
001700*        PROGRAMMET UPPDATERAR WLARTG (WDD2)                              
001800*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001900*        PROGRAMMET UPPDATERAR WLORDL (WDE3)                              
002000*        PROGRAMMET UPPDATERAR WLXXAV (WDG2 HT-1141)                      
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600******************************************************************        
002700* ÄNDRINGAR:                                                              
002800* 2015-12-10  E'TRACKER 10243132 CHINA EXPORT 2015                        
002900* 2021-07-12  STORY 2230234/UPDATE PB-HIST TO PB-SEP,                     
003000*                           MANUAL ADJ DATE IS CURRENT DATE               
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*          --- INFIL MED DB-UPPDATERINGAR FRÅN W213P032                   
004100     SELECT W21334                     ASSIGN TO W21334D1.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W21334                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY W21334      -L.                                                
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400     SKIP2                                                                
005500                                                                          
005600*    -- CHECKED BY WY2000                                                 
005700 77  IDPGM                       PIC X(8)    VALUE 'W2133400'.            
005710 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17  COMP SYNC.        
005800 01  CHKP-VAR.                                                            
005900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006400     03 CHKP-MAX                 PIC S9(3)   VALUE +300 COMP-3.           
006500*    03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
006900 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
007000 77  DADATTID                    PIC 9(14).                               
007100 77  TODAYS-DATE                 PIC S9(8)   VALUE ZERO.                  
007200 01  WS-KDAVROP                  PIC 9       VALUE ZERO.                  
007300 01  WS-KVBEART                  PIC S9(7)   VALUE ZERO COMP-3.           
007400 01  W-KVPB-SEP                  PIC S9(6)V9(1) VALUE 0 COMP-3.           
007410 01  WS-IDLEVNR                  PIC X(5)    VALUE SPACES.                
007500                                                                          
007600 01  W-IDARTNR-SPAR              PIC S9(9)   VALUE ZERO COMP-3.           
007700 01  IX                          PIC 9(2)    VALUE ZERO.                  
007800 01  WS-CURR-AAVVD               PIC 9(5)  VALUE ZERO.                    
007900                                                                          
008000*01  -COPY WWPRODSL                                                       
008100                                                                          
008200*01  -COPY WWDCKONS                                                       
008300                                                                          
008400     SKIP2                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800                                                                          
008900 77  UPD-BUYER-NDCNA-SW          PIC X       VALUE 'N'.                   
009000     88  UPD-BUYER-NDCNA-JA                  VALUE 'J'.                   
009100     EJECT                                                                
009200 77  W21334-EOF-SW               PIC X       VALUE 'N'.                   
009300     88  END-OF-W21334                       VALUE 'J'.                   
009400     EJECT                                                                
009500 77  WS-UPD-VOLUME               PIC X       VALUE 'N'.                   
009510 01  P-TO-P-SW2.                                                          
009520     02     P-TO-P2-KVLL             PIC S9(4) COMP SYNC.                 
009530     02     P-TO-P2-KDZ1             PIC X(1)  VALUE LOW-VALUE.           
009540     02     P-TO-P2-KDZ2             PIC X(1)  VALUE LOW-VALUE.           
009550     02     P-TO-P2-KDTRANS          PIC X(8).                            
009560     02     P-TO-P2-IDTRANS          PIC X(4).                            
009570     02     P-TO-P2-KDMFSFOR         PIC X(1).                            
009580     02     MID -COPY W4I28901   -PRE P-TO-P2-                            
009590                                                                          
009600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009700 01  FILLER REDEFINES DAGENS-DATUM.                                       
009800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010100     SKIP3                                                                
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300*                                                                         
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010700     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
010800     03  W272UTUP                PIC X(8)    VALUE 'W272UTUP'.            
010900     03  WDATKONV                PIC X(08)   VALUE 'WDATKONV'.            
011000     03  W488PRMA                PIC X(8)    VALUE 'W488PRMA'.            
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL POSTSUM                                          
011300*                                                                         
011400*01  -COPY WWDC99                                                         
011500     SKIP2                                                                
011600*01  -COPY W0005   -PRE  POSTSUM-                                         
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
011900*01 -COPY W005WDK7                                                        
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL W272UTUP                                         
012200*01 -COPY W272UTUP                                                        
012300     EJECT                                                                
012400*01  -COPY WDATAREA                                                       
012500     SKIP2                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'W488PRMA'.            
012700     SKIP3                                                                
012800*01  -COPY W488PRMA                                                       
012900     EJECT                                                                
013000 01  IN-AREA-START               PIC X(24)   VALUE                        
013100                                             'IN-AREA-START'.             
013200     SKIP2                                                                
013300                                                                          
013400*01  AREA -COPY W21334     -PRE IN-                                       
013500*                                                                         
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800     SKIP3                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-IDARTNR-X.                                                     
014100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014200     03  W-KDSEGKEY-X.                                                    
014300         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
014400                                                                          
014500     03  W-IDDCREF-X.                                                     
014600         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
014700                                                                          
014800     03  W-IDDC-K7-X.                                                     
014900         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
015000                                                                          
015100     03  W-IDLAND-X.                                                      
015200         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
015300                                                                          
015400     03  W-WDD901KY-X.                                                    
015500         05  W-IDARTNR-WDD9      PIC S9(9)   VALUE ZERO COMP-3.           
015600         05  W-IDDC-WDD9         PIC X(2)    VALUE SPACE.                 
015700                                                                          
015800     03  W-IDLEVNR-X.                                                     
015900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
016000                                                                          
016100     03  W-WDD601KY-X.                                                    
016200         05 W-IDDC-WDD6-X.                                                
016300             07 W-IDDC-WDD6      PIC X(2)    VALUE SPACE.                 
016400         05 W-IDLEVNR-WDD6-X.                                             
016500             07 W-IDLEVNR-WDD6   PIC X(5)    VALUE SPACE.                 
016600         05 W-IDARTNR-WDD6-X.                                             
016700             07 W-IDARTNR-WDD6   PIC S9(9)   VALUE ZERO COMP-3.           
016800         05 W-IDANSK-WDD6-X.                                              
016900             07 W-IDANSK-WDD6    PIC S9(3)   VALUE ZERO COMP-3.           
017000                                                                          
017100     03  W-WDD601KY-MIN-X.                                                
017200         05 W-IDDC-D6-MIN        PIC X(2)  VALUE SPACE.                   
017300         05 W-IDLEVNR-D6-MIN     PIC X(5)  VALUE SPACE.                   
017400         05 W-IDARTNR-D6-MIN     PIC S9(9) VALUE ZERO COMP-3.             
017500         05 W-IDANSK-D6-MIN      PIC S9(3) VALUE +000 COMP-3.             
017600                                                                          
017700     03  W-WDD601KY-MAX-X.                                                
017800         05 W-IDDC-D6-MAX        PIC X(2)  VALUE SPACE.                   
017900         05 W-IDLEVNR-D6-MAX     PIC X(5)  VALUE SPACE.                   
018000         05 W-IDARTNR-D6-MAX     PIC S9(9) VALUE ZERO COMP-3.             
018100         05 W-IDANSK-D6-MAX      PIC S9(3) VALUE +999 COMP-3.             
018200                                                                          
018300     03  W-WDD905KY-X.                                                    
018400         05  W-DAAVROP-X.                                                 
018500             07  W-DAAVROP       PIC  9(6)   VALUE ZERO.                  
018600         05  W-TILEVDAG-X.                                                
018700             07  W-TILEVDAG      PIC  S9     VALUE ZERO COMP-3.           
018800                                                                          
018900     03  W-KDAVROP-X.                                                     
019000         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
019100                                                                          
019200     03  W-1141-KEY-X.                                                    
019300         05  FILLER              PIC X(04)  VALUE '1141'.                 
019400         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
019500                                                                          
019600     03  W-1142-KEY-X.                                                    
019700         05  W-1142-KDSEGKEY     PIC X(01)  VALUE SPACE.                  
019800                                                                          
019900     03 W-WDE301KY-MIN-X.                                                 
020000         05  W-IDDC-MIN-E3       PIC X(2)  VALUE SPACE.                   
020100         05  FILLER              PIC X(11) VALUE LOW-VALUE.               
020200                                                                          
020300     03  W-KDREFTYP-MIN-X.                                                
020400         05  W-KDREFTYP-MIN      PIC X       VALUE 'A'.                   
020500                                                                          
020600     03 W-WDE301KY-MAX-X.                                                 
020700         05  W-IDDC-MAX-E3       PIC X(2)  VALUE SPACE.                   
020800         05  FILLER              PIC X(11) VALUE HIGH-VALUE.              
020900                                                                          
021000     03  W-KDREFTYP-MAX-X.                                                
021100         05  W-KDREFTYP-MAX      PIC X       VALUE 'O'.                   
021200                                                                          
021300     SKIP2                                                                
021400*    --- STATUS-KOD FRÅN IMS                                              
021500 01  STATUS-WS                   PIC XX.                                  
021600     88  SEGMENT-FINNS                       VALUE '  '.                  
021700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
022000     88  IMS-EJ-OK                           VALUE 'XD'.                  
022100     SKIP2                                                                
022200 01  GODK-STATUSKODER.                                                    
022300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022400     SKIP3                                                                
022500 01  ALL-SSA.                                                             
022600     03  SSA1                        PIC X(128).                          
022700     03  SSA2                        PIC X(64).                           
022800     03  SSA3                        PIC X(64).                           
022900     EJECT                                                                
023000*    --- IMS FUNKTIONSKODER                                               
023100*01  -COPY W0003                                                          
023200     EJECT                                                                
023300*    ---  DLI INPUT-OUTPUT AREA                                           
023400                                                                          
023500 01  FILLER         PIC X(24) VALUE 'DLI-IO-AREA    '.                    
023600                                                                          
023700 01  DLI-IO-AREA           PIC X(1000).                                   
023800                                                                          
023900 01  DLI-IO-WLINLB01    REDEFINES  DLI-IO-AREA.                           
024000*    03  -COPY WDD901  -PRE INLB-                                         
024100     EJECT                                                                
024200 01  DLI-IO-WLINLB11    REDEFINES  DLI-IO-AREA.                           
024300*    03  -COPY WDD902  -PRE INLB-                                         
024400     EJECT                                                                
024500 01  DLI-IO-WLINLB22    REDEFINES  DLI-IO-AREA.                           
024600*    03  -COPY WDD904  -PRE INLB-                                         
024700     EJECT                                                                
024800 01  DLI-IO-WLINLB23    REDEFINES  DLI-IO-AREA.                           
024900*    03  -COPY WDD905  -PRE INLB-                                         
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
025100 01  DLI-IO-WDD601.                                                       
025200*    03  -COPY WDD601                                                     
025300     EJECT                                                                
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
025500 01  DLI-IO-WDK601.                                                       
025600*    03  -COPY WDK601                                                     
025700     EJECT                                                                
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
025900 01  DLI-IO-WDK611.                                                       
026000*    03  -COPY WDK611                                                     
026100     EJECT                                                                
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK626'.                      
026300 01  DLI-IO-WDK626.                                                       
026400*    03  -COPY WDK626                                                     
026500     EJECT                                                                
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
026700 01  DLI-IO-WDK629.                                                       
026800*    03  -COPY WDK629                                                     
026900     EJECT                                                                
027000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
027100 01  DLI-IO-WDD201.                                                       
027200*    03  -COPY WDD201  -PRE NYPON-                                        
027300     EJECT                                                                
027400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
027500 01  DLI-IO-WDK701.                                                       
027600*    03  -COPY WDK701                                                     
027700     EJECT                                                                
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
027900 01  DLI-IO-WDK712.                                                       
028000*    03  -COPY WDK712                                                     
028100     EJECT                                                                
028200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028300 01  DLI-IO-WDK711.                                                       
028400*    03  -COPY WDK711                                                     
028500     EJECT                                                                
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-1141  '.                      
028700 01  DLI-IO-1141.                                                         
028800*    03  WDG2 -COPY WDGX01                                                
028900     EJECT                                                                
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-1142  '.                      
029100 01  DLI-IO-1142.                                                         
029200*    03  WDG2 -COPY WDGX1142                                              
029300     EJECT                                                                
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE301'.                      
029500 01  DLI-IO-WDE301.                                                       
029600*    03  -COPY WDE301                                                     
029700     EJECT                                                                
029800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT501'.                      
029900 01  DLI-IO-WDT501.                                                       
030000*    03  -COPY WDT501   -PRE WDT501-                                      
030100     EJECT                                                                
030200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT511'.                      
030300 01  DLI-IO-WDT511.                                                       
030400*    03  -COPY WDT511                                                     
030500     EJECT                                                                
030600                                                                          
030700                                                                          
030800     EJECT                                                                
030900 LINKAGE SECTION.                                                         
031000                                                                          
031100*01  -COPY W0009   -PRE MSG-                                              
031200     EJECT                                                                
031210*01  -COPY W0009   -PRE 4289-                                             
031220     EJECT                                                                
031300*01  -COPY W0009   -PRE SYNQ-                                             
031400     EJECT                                                                
031500*01  -COPY W0008     -PRE WDK6-                                           
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008     -PRE INLB-                                           
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008     -PRE WDD6-                                           
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400*01  -COPY W0008     -PRE WDD2-                                           
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700*01  -COPY W0008     -PRE WDK7-                                           
032800     05  FILLER                  PIC X.                                   
032900     EJECT                                                                
033000*01  -COPY W0008     -PRE WDB6-                                           
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300*01  -COPY W0008     -PRE WDK6-2-                                         
033400     05  FILLER                  PIC X.                                   
033500     EJECT                                                                
033600*01  -COPY W0008     -PRE WDK7-2-                                         
033700     05  FILLER                  PIC X.                                   
033800     EJECT                                                                
033900*01  -COPY W0008     -PRE 1142-                                           
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034200*01  -COPY W0008     -PRE WDE3-                                           
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008     -PRE WDT5-                                           
034600      05 FILLER                  PIC X.                                   
034700 01  U2-WDK6-PCB                         PIC X.                           
034800 01  U2-WDB6-PCB                         PIC X.                           
034900 01  U2-PBTO-W222-WDK6-PCB               PIC X.                           
035000 01  U2-PBTO-W222-WDK7-PCB               PIC X.                           
035100 01  U2-PBTO-W222-ARTM-PCB               PIC X.                           
035200 01  U2-PBTO-W222-R1-2501-PCB            PIC X.                           
035300 01  U2-PBTO-W222-R1-WDB6R-PCB           PIC X.                           
035400 01  U2-PBTO-W222-R1-WDK7R-PCB           PIC X.                           
035500 01  U2-PBTO-W222-WDB6-PCB               PIC X.                           
035600 01  U2-PBTO-W222-WDD7-PCB               PIC X.                           
035700 01  U2-PBTO-W222-WDK7E-PCB              PIC X.                           
035800 01  U2-PBTO-W222-R1-UTIL-WDK6-PCB       PIC X.                           
035900 01  U2-PBTO-W222-R1-UTIL-WDK7-PCB       PIC X.                           
036000 01  U2-PBTO-W222-R1-UTIL-WDB6-PCB       PIC X.                           
036100 01  U2-PBTO-W222-U1-WDK7-PCB            PIC X.                           
036200 01  U2-PBTO-W222-U1-WDB6-PCB            PIC X.                           
036300 01  U2-PBTO-W222-U1-UTIL-WDK6-PCB       PIC X.                           
036400 01  U2-PBTO-W222-U1-UTIL-WDK7-PCB       PIC X.                           
036500 01  U2-PBTO-W222-U1-UTIL-WDB6-PCB       PIC X.                           
036600 01  U2-REFL2-2501-PCB                   PIC X.                           
036700 01  U2-REFL2-WDB6-PCB                   PIC X.                           
036800 01  U2-REFL2-UTIL-WDK6-PCB              PIC X.                           
036900 01  U2-REFL2-UTIL-WDK7-PCB              PIC X.                           
037000 01  U2-REFL2-UTIL-WDB6-PCB              PIC X.                           
037100 01  U2-W222-WDK6-PCB                    PIC X.                           
037200 01  U2-W222-WDK7-PCB                    PIC X.                           
037300 01  U2-W222-ARTM-PCB                    PIC X.                           
037400 01  U2-W222-2501-PCB                    PIC X.                           
037500 01  U2-W222-WDB6R-PCB                   PIC X.                           
037600 01  U2-W222-WDK7R-PCB                   PIC X.                           
037700 01  U2-W222-WDB6-PCB                    PIC X.                           
037800 01  U2-W222-WDD7-PCB                    PIC X.                           
037900 01  U2-W222-WDK7E-PCB                   PIC X.                           
038000 01  U2-W222-UTIL-WDK6-PCB               PIC X.                           
038100 01  U2-W222-UTIL-WDK7-PCB               PIC X.                           
038200 01  U2-W222-UTIL-WDB6-PCB               PIC X.                           
038300 01  U2-W222-U1-WDK7-PCB                 PIC X.                           
038400 01  U2-W222-U1-WDB6-PCB                 PIC X.                           
038500 01  U2-W222-U1-UTIL-WDK6-PCB            PIC X.                           
038600 01  U2-W222-U1-UTIL-WDK7-PCB            PIC X.                           
038700 01  U2-W222-U1-UTIL-WDB6-PCB            PIC X.                           
038800*    PCB'ER FÖR SUBPGM                                                    
038900 01  SYNQ-ATAB-PCB                       PIC X.                           
039000 01  SYNQ-WDK6-PCB                       PIC X.                           
039100 01  SYNQ-WDD3-PCB                       PIC X.                           
039200     EJECT                                                                
039300                                                                          
039400 PROCEDURE DIVISION  USING MSG-PCB 4289-PCB SYNQ-PCB                      
039500                           WDK6-PCB INLB-PCB WDD6-PCB                     
039600                           WDD2-PCB   WDK7-PCB WDB6-PCB                   
039700                           WDK6-2-PCB WDK7-2-PCB                          
039800                           1142-PCB   WDE3-PCB                            
039900                           WDT5-PCB                                       
040000                           U2-WDK6-PCB                                    
040100                           U2-WDB6-PCB                                    
040200                           U2-PBTO-W222-WDK6-PCB                          
040300                           U2-PBTO-W222-WDK7-PCB                          
040400                           U2-PBTO-W222-ARTM-PCB                          
040500                           U2-PBTO-W222-R1-2501-PCB                       
040600                           U2-PBTO-W222-R1-WDB6R-PCB                      
040700                           U2-PBTO-W222-R1-WDK7R-PCB                      
040800                           U2-PBTO-W222-WDB6-PCB                          
040900                           U2-PBTO-W222-WDD7-PCB                          
041000                           U2-PBTO-W222-WDK7E-PCB                         
041100                           U2-PBTO-W222-R1-UTIL-WDK6-PCB                  
041200                           U2-PBTO-W222-R1-UTIL-WDK7-PCB                  
041300                           U2-PBTO-W222-R1-UTIL-WDB6-PCB                  
041400                           U2-PBTO-W222-U1-WDK7-PCB                       
041500                           U2-PBTO-W222-U1-WDB6-PCB                       
041600                           U2-PBTO-W222-U1-UTIL-WDK6-PCB                  
041700                           U2-PBTO-W222-U1-UTIL-WDK7-PCB                  
041800                           U2-PBTO-W222-U1-UTIL-WDB6-PCB                  
041900                           U2-REFL2-2501-PCB                              
042000                           U2-REFL2-WDB6-PCB                              
042100                           U2-REFL2-UTIL-WDK6-PCB                         
042200                           U2-REFL2-UTIL-WDK7-PCB                         
042300                           U2-REFL2-UTIL-WDB6-PCB                         
042400                           U2-W222-WDK6-PCB                               
042500                           U2-W222-WDK7-PCB                               
042600                           U2-W222-ARTM-PCB                               
042700                           U2-W222-2501-PCB                               
042800                           U2-W222-WDB6R-PCB                              
042900                           U2-W222-WDK7R-PCB                              
043000                           U2-W222-WDB6-PCB                               
043100                           U2-W222-WDD7-PCB                               
043200                           U2-W222-WDK7E-PCB                              
043300                           U2-W222-UTIL-WDK6-PCB                          
043400                           U2-W222-UTIL-WDK7-PCB                          
043500                           U2-W222-UTIL-WDB6-PCB                          
043600                           U2-W222-U1-WDK7-PCB                            
043700                           U2-W222-U1-WDB6-PCB                            
043800                           U2-W222-U1-UTIL-WDK6-PCB                       
043900                           U2-W222-U1-UTIL-WDK7-PCB                       
044000                           U2-W222-U1-UTIL-WDB6-PCB                       
044100                           SYNQ-ATAB-PCB SYNQ-WDK6-PCB                    
044200                           SYNQ-WDD3-PCB                                  
044300                           .                                              
044400                                                                          
044500 MAIN SECTION.                                                            
044600     ENTRY 'DLITCBL' USING MSG-PCB 4289-PCB SYNQ-PCB                      
044700                           WDK6-PCB INLB-PCB WDD6-PCB                     
044800                           WDD2-PCB   WDK7-PCB WDB6-PCB                   
044900                           WDK6-2-PCB WDK7-2-PCB                          
045000                           1142-PCB   WDE3-PCB                            
045100                           WDT5-PCB                                       
045200                           U2-WDK6-PCB                                    
045300                           U2-WDB6-PCB                                    
045400                           U2-PBTO-W222-WDK6-PCB                          
045500                           U2-PBTO-W222-WDK7-PCB                          
045600                           U2-PBTO-W222-ARTM-PCB                          
045700                           U2-PBTO-W222-R1-2501-PCB                       
045800                           U2-PBTO-W222-R1-WDB6R-PCB                      
045900                           U2-PBTO-W222-R1-WDK7R-PCB                      
046000                           U2-PBTO-W222-WDB6-PCB                          
046100                           U2-PBTO-W222-WDD7-PCB                          
046200                           U2-PBTO-W222-WDK7E-PCB                         
046300                           U2-PBTO-W222-R1-UTIL-WDK6-PCB                  
046400                           U2-PBTO-W222-R1-UTIL-WDK7-PCB                  
046500                           U2-PBTO-W222-R1-UTIL-WDB6-PCB                  
046600                           U2-PBTO-W222-U1-WDK7-PCB                       
046700                           U2-PBTO-W222-U1-WDB6-PCB                       
046800                           U2-PBTO-W222-U1-UTIL-WDK6-PCB                  
046900                           U2-PBTO-W222-U1-UTIL-WDK7-PCB                  
047000                           U2-PBTO-W222-U1-UTIL-WDB6-PCB                  
047100                           U2-REFL2-2501-PCB                              
047200                           U2-REFL2-WDB6-PCB                              
047300                           U2-REFL2-UTIL-WDK6-PCB                         
047400                           U2-REFL2-UTIL-WDK7-PCB                         
047500                           U2-REFL2-UTIL-WDB6-PCB                         
047600                           U2-W222-WDK6-PCB                               
047700                           U2-W222-WDK7-PCB                               
047800                           U2-W222-ARTM-PCB                               
047900                           U2-W222-2501-PCB                               
048000                           U2-W222-WDB6R-PCB                              
048100                           U2-W222-WDK7R-PCB                              
048200                           U2-W222-WDB6-PCB                               
048300                           U2-W222-WDD7-PCB                               
048400                           U2-W222-WDK7E-PCB                              
048500                           U2-W222-UTIL-WDK6-PCB                          
048600                           U2-W222-UTIL-WDK7-PCB                          
048700                           U2-W222-UTIL-WDB6-PCB                          
048800                           U2-W222-U1-WDK7-PCB                            
048900                           U2-W222-U1-WDB6-PCB                            
049000                           U2-W222-U1-UTIL-WDK6-PCB                       
049100                           U2-W222-U1-UTIL-WDK7-PCB                       
049200                           U2-W222-U1-UTIL-WDB6-PCB                       
049300                           SYNQ-ATAB-PCB SYNQ-WDK6-PCB                    
049400                           SYNQ-WDD3-PCB                                  
049500                           .                                              
049600                                                                          
049700                                                                          
049800     PERFORM A-INIT                                                       
049900     PERFORM S01-LAES-W21334                                              
050000     PERFORM UNTIL END-OF-W21334                                          
050100       IF CHKP-ANT > CHKP-MAX                                             
050200         PERFORM X-TAG-CHECKPOINT                                         
050300       END-IF                                                             
050400       IF IN-ATGARD = REPL                                                
050500          IF IN-IDSEGM = 'WDK601'                                         
050600             PERFORM B-REPL-WDK601                                        
050700          END-IF                                                          
050800          IF IN-IDSEGM = 'WDK611'                                         
050900             PERFORM C-REPL-WDK611                                        
051000          END-IF                                                          
051100          IF IN-IDSEGM = 'WDK626'                                         
051200             PERFORM L-REPL-WDK626                                        
051300          END-IF                                                          
051400          IF IN-IDSEGM = 'WDD905'                                         
051500             PERFORM D-REPL-WDD905                                        
051600          END-IF                                                          
051700          IF IN-IDSEGM = 'WDD201'                                         
051800             PERFORM E-REPL-WDD201                                        
051900          END-IF                                                          
052000       END-IF                                                             
052100       IF IN-ATGARD = DLET                                                
052200          IF IN-IDSEGM = 'WDD904'                                         
052300             PERFORM F-DLET-WDD904                                        
052400          END-IF                                                          
052500          IF IN-IDSEGM = 'WDD905'                                         
052600             PERFORM G-DLET-WDD905                                        
052700          END-IF                                                          
052800          IF IN-IDSEGM = 'WDG202'                                         
052900             PERFORM I-DLET-HTR-1142                                      
053000          END-IF                                                          
053100          IF IN-IDSEGM = 'WDK629'                                         
053200             PERFORM J-DLET-WDK629                                        
053300          END-IF                                                          
053400          IF IN-IDSEGM = 'WDE301'                                         
053500             PERFORM K-DLET-WDE301                                        
053600          END-IF                                                          
053700       END-IF                                                             
053800       IF IN-ATGARD = ISRT                                                
053900          IF IN-IDSEGM = 'WDK629'                                         
054000             PERFORM H-ISRT-WDK629                                        
054100          END-IF                                                          
054200       END-IF                                                             
054300       PERFORM S01-LAES-W21334                                            
054400     END-PERFORM                                                          
054500                                                                          
054600     PERFORM Z-FINIT                                                      
054700                                                                          
054800     MOVE ZERO TO RETURN-CODE                                             
054900     GOBACK                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 A-INIT SECTION.                                                          
055300     SKIP2                                                                
055400                                                                          
055500     PERFORM IMS-RESTART                                                  
055600                                                                          
055700     OPEN INPUT W21334                                                    
055800                                                                          
055900     ACCEPT DAGENS-DATUM      FROM DATE                                   
056000                                                                          
056100     PERFORM AA-GET-DATE-AAVVD                                            
056200                                                                          
056300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
056400                                                                          
056500     MOVE WC-CDC-SE TO W-IDDC-WDD6                                        
056600     .                                                                    
056700     EJECT                                                                
056800 AA-GET-DATE-AAVVD SECTION.                                               
056900     MOVE 'AA-GET-DATE-AAVVD'          TO CURRENT-SECTION                 
057000                                                                          
057100     MOVE 'IDAG  '       TO      DAT-KDDATFORM                            
057200     CALL WDATKONV USING         DAT-KDDATFORM                            
057300                                 DAT-I-TIDATUM                            
057400                                 DAT-O-TIDATUM                            
057500                                 DAT-KDSVAR                               
057600                                                                          
057700     IF DAT-KDSVAR-FEL                                                    
057800        DISPLAY '**** ERROR IN WDATKONV CALL IN PGM W21334**'             
057900        CALL FELLOG                                                       
058000     ELSE                                                                 
058100        MOVE DAT-TIAAVVD TO WS-CURR-AAVVD                                 
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500 B-REPL-WDK601  SECTION.                                                  
058600     SKIP2                                                                
058700     MOVE IN-IDARTNR     TO W-IDARTNR                                     
058800     PERFORM IMS-GET-WDK6-ROT                                             
           MOVE ART-IDLEVNR TO WS-IDLEVNR                                       
058900                                                                          
059000     MOVE IN-IDLEVNR     TO ART-IDLEVNR                                   
059010                                                                          
059100                                                                          
059200     PERFORM IMS-REPL-WDK601                                              
059300                                                                          
059400     ADD +1 TO CHKP-ANT                                                   
059500                                                                          
059600***NOLLA BUYER NÄR EN ARTIKEL BLI CLASSIC                                 
059700     IF IN-IDLEVNR = 'BQ8VA'                                              
059800       PERFORM BA-NOLLA-BUYER-WDK7                                        
059900*                                                                         
060000***    INTITIALIZE BUYER FOR NDC-NA                                       
060100       PERFORM BB-CHECK-BUYER-NDCNA                                       
060200       IF UPD-BUYER-NDCNA-JA                                              
060300          PERFORM BC-NOLLA-BUYER-NDCNA-K7                                 
060400       END-IF                                                             
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800                                                                          
060900 BA-NOLLA-BUYER-WDK7  SECTION.                                            
061000                                                                          
061100     PERFORM IMS-GU-WDK701                                                
061200     IF SEGMENT-FINNS                                                     
061300       PERFORM IMS-GHNP-WDK711                                            
061400       PERFORM UNTIL SEGMENT-SAKNAS                                       
061500         MOVE SLAG-IDDC     TO WS-IDDC                                    
061600         IF NDC-NA                                                        
061700            CONTINUE                                                      
061800         ELSE                                                             
061900           IF SLAG-IDDC-REF = '11'                                        
062000              IF SLAG-IDPERSON-BUY NOT = ZERO                             
062100                 MOVE ZERO TO SLAG-IDPERSON-BUY                           
062200                 PERFORM IMS-REPL-WDK711                                  
062300                 ADD +1 TO CHKP-ANT                                       
062400              END-IF                                                      
062500           END-IF                                                         
062600         END-IF                                                           
062700         PERFORM IMS-GHNP-WDK711                                          
062800       END-PERFORM                                                        
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200                                                                          
063300 BB-CHECK-BUYER-NDCNA  SECTION.                                           
063400                                                                          
063500     IF W-IDARTNR NOT = W-IDARTNR-SPAR                                    
063600        MOVE NEJ               TO UPD-BUYER-NDCNA-SW                      
063700        PERFORM IMS-GU-WDK701                                             
063800        IF SEGMENT-FINNS                                                  
063900          PERFORM IMS-GNP-WDK711                                          
064000          PERFORM UNTIL SEGMENT-SAKNAS                                    
064100            MOVE SLAG-IDDC     TO WS-IDDC                                 
064200            IF  NDC-NA                                                    
064300            AND SLAG-IDDC-REF   = '11'                                    
064400               MOVE JA         TO UPD-BUYER-NDCNA-SW                      
064500            END-IF                                                        
064600            PERFORM IMS-GNP-WDK711                                        
064700          END-PERFORM                                                     
064800        END-IF                                                            
064900        MOVE W-IDARTNR         TO W-IDARTNR-SPAR                          
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300                                                                          
065400 BC-NOLLA-BUYER-NDCNA-K7 SECTION.                                         
065500                                                                          
065600     PERFORM IMS-GU-WDK701                                                
065700     IF SEGMENT-FINNS                                                     
065800       PERFORM IMS-GHNP-WDK711                                            
065900       PERFORM UNTIL SEGMENT-SAKNAS                                       
066000         MOVE SLAG-IDDC        TO WS-IDDC                                 
066100         IF NDC-NA                                                        
066200            IF SLAG-IDPERSON-BUY NOT = ZERO                               
066300               MOVE ZERO       TO SLAG-IDPERSON-BUY                       
066400               PERFORM IMS-REPL-WDK711                                    
066500               ADD +1          TO CHKP-ANT                                
066600            END-IF                                                        
066700         END-IF                                                           
066800         PERFORM IMS-GHNP-WDK711                                          
066900       END-PERFORM                                                        
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400 C-REPL-WDK611  SECTION.                                                  
067500     MOVE 'C-REPL-WDK611 '  TO CURRENT-SECTION                            
067600*-------------------------------------------------------                  
067700*--- VID EXT-TO-REF SÄTTER MAN SAMMA IDLEVNR I MFG O SHIP                 
067800*-------------------------------------------------------                  
067900     SKIP2                                                                
068000     MOVE IN-IDARTNR     TO W-IDARTNR                                     
068100     MOVE NEJ TO WS-UPD-VOLUME                                            
068200     PERFORM IMS-GHU-WDK6-CLAG                                            
068300     MOVE CLAG-IDANSK  TO W-IDANSK-WDD6                                   
068400     MOVE CLAG-KVPB-SEP  TO W-KVPB-SEP                                    
068500     MOVE W-KVPB-SEP     TO CLAG-KVPB-HIST                                
068600                                                                          
068700     MOVE WS-CURR-AAVVD  TO CLAG-TIPBDAT                                  
068800                                                                          
068900     MOVE IN-KDGK        TO CLAG-KDGK                                     
069000     MOVE IN-KVDAGAR-TT  TO CLAG-KVDAGAR-TT                               
069100     MOVE IN-KVVECKOR-LT TO CLAG-KVVECKOR-LT                              
069200     MOVE IN-KVVECKOR-AT TO CLAG-KVVECKOR-AT                              
069300     MOVE IN-KDAVT       TO CLAG-KDAVT                                    
069400     MOVE IN-KDKSP       TO CLAG-KDKSP                                    
069500     MOVE IN-IDANSK      TO CLAG-IDANSK                                   
069600     MOVE IN-IDPLANGR-AG TO CLAG-IDPLANGR-AG                              
069700     MOVE IN-KDLPSP      TO CLAG-KDLPSP                                   
069800     MOVE IN-IDLEVNR-SHIP TO CLAG-IDLEVNR-SHIP                            
069900                                                                          
070000     IF CLAG-IDDC-REF = SPACE                                             
070100       IF IN-IDDC-REF NOT = SPACE                                         
070200         PERFORM CA-UPPDATERA-TILL-REFILL                                 
070300                                                                          
070400         MOVE IN-IDDC-REF    TO WS-IDDC                                   
070500         IF NDC                                                           
070600            MOVE ZERO        TO CLAG-DAPBPLAN                             
070700                                CLAG-KVPB-PLAN                            
070800         END-IF                                                           
070900                                                                          
071000         MOVE IN-IDDC-REF    TO W-IDDC-K7                                 
071100         PERFORM IMS-GU-WDK711                                            
071200         IF SEGMENT-SAKNAS                                                
071300           PERFORM CB-SKAPA-NY-WDK7                                       
071400         END-IF                                                           
071500                                                                          
071600         MOVE IN-IDLANDX2          TO W-IDLANDX2                          
071700         PERFORM IMS-GU-WDK712                                            
071800         IF SEGMENT-FINNS                                                 
071900           IF LART-KDARTURS = SPACE                                       
072000             CONTINUE                                                     
072100           ELSE                                                           
072200             MOVE LART-KDARTURS    TO CLAG-KDARTURS                       
072300           END-IF                                                         
072400                                                                          
072500           IF LART-VKART > 0                                              
072600             MOVE LART-VKART       TO CLAG-VKART                          
072700             MOVE 'W2133400'       TO CLAG-IDUSER-VUPD                    
072800             MOVE DAGENS-DATUM     TO CLAG-TIUPPDAT-VUPD                  
072900             IF CLAG-VKART-NTO = ZERO OR                                  
073000                CLAG-VKART-NTO > CLAG-VKART                               
073100               MOVE CLAG-VKART     TO CLAG-VKART-NTO                      
073200               MOVE '4'            TO CLAG-KDUVKNTO                       
073300             END-IF                                                       
073400           END-IF                                                         
073500                                                                          
073600           IF LART-VLARTNTO > 0                                           
073700               IF CLAG-VLARTNTO = ZERO                                    
073800                 MOVE 002        TO SYNQ-KDCALL                           
073900               ELSE                                                       
074000                 MOVE 003        TO SYNQ-KDCALL                           
074100               END-IF                                                     
074200               MOVE LART-VLARTNTO  TO CLAG-VLARTNTO                       
074300               MOVE 'W2133400'     TO CLAG-IDUSER-VUPD                    
074400               MOVE DAGENS-DATUM   TO CLAG-TIUPPDAT-VUPD                  
074500               MOVE JA TO WS-UPD-VOLUME                                   
074600           END-IF                                                         
074700         END-IF                                                           
074800       END-IF                                                             
074900     ELSE                                                                 
075000       IF IN-IDDC-REF = SPACE                                             
075100         MOVE SPACE   TO CLAG-IDDC-REF                                    
075200       END-IF                                                             
075300     END-IF                                                               
075400                                                                          
075500     PERFORM IMS-REPL-WDK611                                              
075600     IF WS-UPD-VOLUME = JA                                                
075700       MOVE W-IDARTNR         TO SYNQ-IDARTNR                             
075800       CALL W488PRMA USING  SYNQ-W488PRMA                                 
075900       SYNQ-ATAB-PCB SYNQ-WDK6-PCB SYNQ-WDD3-PCB                          
075901                                                                          
075910       PERFORM CC-STARTA-W40289                                           
076000     END-IF                                                               
076100                                                                          
076200     IF CLAG-KDARTURS > ' '                                               
076300       PERFORM IMS-GU-WDT501                                              
076310       IF SEGMENT-FINNS                                                   
076320        PERFORM IMS-GHNP-WDT511                                           
076321*       IF WS-IDLEVNR = SPACES                                            
076330*        MOVE ART-IDLEVNR TO GLO-IDLEVNR                                  
076331*       ELSE                                                              
076332        MOVE WS-IDLEVNR TO GLO-IDLEVNR                                    
076333*       END-IF                                                            
076340        PERFORM IMS-REPL-WDT511                                           
              MOVE SPACES TO WS-IDLEVNR                                         
076350       END-IF                                                             
076400       IF SEGMENT-SAKNAS                                                  
076500          MOVE W-IDARTNR TO WDT501-ARTU-IDARTNR                           
076600          PERFORM IMS-ISRT-WDT501                                         
076700       END-IF                                                             
076800       MOVE ART-IDLEVNR   TO GLO-IDUSER                                   
076810       MOVE SPACES        TO GLO-IDLEVNR                                  
076900       MOVE CLAG-KDARTURS TO GLO-KDARTURS                                 
077000       MOVE FUNCTION CURRENT-DATE(1:14) TO DADATTID                       
077100       MOVE FUNCTION CURRENT-DATE(1:8)  TO TODAYS-DATE                    
077200       COMPUTE GLO-DADATTID-9KOMPL =                                      
077300               99999999999999 - DADATTID                                  
077400                                                                          
077500       PERFORM IMS-ISRT-WDT511                                            
077600                                                                          
077700     END-IF                                                               
077800                                                                          
077900     ADD +1 TO CHKP-ANT                                                   
078000     .                                                                    
078100     EJECT                                                                
078200 CA-UPPDATERA-TILL-REFILL SECTION.                                        
078300     MOVE 'CA-UPPDATERA-TILL-REFILL '  TO CURRENT-SECTION                 
078400                                                                          
078500     MOVE IN-IDDC-REF    TO CLAG-IDDC-REF                                 
078600     MOVE ZERO           TO CLAG-KVBEART                                  
078700     MOVE ZERO           TO CLAG-KVEOQ                                    
078800     MOVE 'N'            TO CLAG-FLMANQ                                   
078900     MOVE ZERO           TO CLAG-KVQ-JUST                                 
079000     MOVE ZERO           TO CLAG-TIQJUST                                  
079100     MOVE ZERO           TO CLAG-KVKP                                     
079200     MOVE 'N'            TO CLAG-FLMANKP                                  
079300     MOVE ZERO           TO CLAG-KDKSP                                    
079400     MOVE ZERO           TO CLAG-KVBK                                     
079500     MOVE 'N'            TO CLAG-FLMANBK                                  
079600     MOVE ZERO           TO CLAG-KVPALL                                   
079700     MOVE ZERO           TO CLAG-KVULOAD                                  
079800     MOVE 'N'            TO CLAG-FLOREGPB                                 
079900     MOVE 'J'            TO CLAG-FLMPB                                    
080000     MOVE ZERO           TO CLAG-RESLJUST                                 
080100     MOVE ZERO           TO CLAG-TISLJUST                                 
080200     MOVE ZERO           TO CLAG-KVSLUTKP                                 
080300     MOVE ZERO           TO CLAG-TISLUTKP                                 
080400     MOVE 'N'            TO CLAG-KDOPPLAN                                 
080500     MOVE 'N'            TO CLAG-FLMANOSK                                 
080600     MOVE ZERO           TO CLAG-KVSLAGER-OPT                             
080700     MOVE ZERO           TO CLAG-KVPB-TREND                               
080800     MOVE ZERO           TO CLAG-KVVECKOR-TREND                           
080900     MOVE ZERO           TO CLAG-TIDATUM-TREND                            
081000     MOVE SPACE          TO CLAG-KDPRISKL                                 
081100     MOVE SPACE          TO CLAG-KDFREKKL                                 
081200                                                                          
081300     .                                                                    
081400     EJECT                                                                
081500 CB-SKAPA-NY-WDK7  SECTION.                                               
081600     MOVE 'CB-SKAPA-NY-WDK7 '  TO CURRENT-SECTION                         
081700***- LÄGG UPP ARTIKELN SOM LOKALT ANSKAFFAD PÅ DC71/DC4X NDC-US           
081800***- MED FIKTIV LEVERANTÖR 9998.GLOBAL EXPORT = EJ SKAPA WDL7             
081900                                                                          
082000     MOVE ALL '+'      TO WDK7-W005WDK7                                   
082100     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
082200     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
082300     MOVE IN-IDDC-REF  TO WDK7-IDDC-KFB                                   
082400                          WDK7-IDDC                                       
082500     MOVE '9998'       TO WDK7-IDLEVNR                                    
082600     MOVE SPACE        TO WDK7-IDDC-REF                                   
082700                                                                          
082800     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-2-PCB                
082900                                       WDK7-2-PCB                         
083000                                                                          
083100     ADD +1 TO CHKP-ANT                                                   
083200                                                                          
083300*- LÄGG UPP WDK722 SEGMENT SOM MÅSTE FINNAS FÖR LOKAL ANSKAFFNING.        
083400     MOVE ALL '+'      TO WDK7-W005WDK7                                   
083500     MOVE 'WDK722'     TO WDK7-IDSEGM                                     
083600     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
083700     MOVE IN-IDDC-REF  TO WDK7-IDDC-KFB                                   
083800     MOVE 1            TO WDK7-IDPLANGR-AG                                
083900                                                                          
084000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-2-PCB                
084100                                       WDK7-2-PCB                         
084200                                                                          
084300                                                                          
084400     .                                                                    
084500     EJECT                                                                
084510 CC-STARTA-W40289 SECTION.                                                
084520     MOVE 'STA HA-STARTA SEC'  TO FELTEXT                                 
084530                                                                          
084540     MOVE W-IDARTNR            TO P-TO-P2-MID-IDARTNR-IN                  
084590     MOVE CLAG-VKART           TO P-TO-P2-MID-VKART-IN                    
084591     MOVE CLAG-VLARTNTO        TO P-TO-P2-MID-VLARTNTO-IN                 
084592     MOVE ZERO                 TO P-TO-P2-MID-IDDISTR-IN                  
084593                                  P-TO-P2-MID-IDKUNDNR-IN                 
084594                                  P-TO-P2-MID-IDORDNR5-IN                 
084595                                  P-TO-P2-MID-IDORDER-IN                  
084596                                  P-TO-P2-MID-IDLOPNR-IN                  
084597                                  P-TO-P2-MID-IDDC-IN                     
084598                                  P-TO-P2-MID-ADLAGOMR-IN                 
084599                                  P-TO-P2-MID-ADGANG-IN                   
084600                                  P-TO-P2-MID-ADPLATS-IN                  
084601                                                                          
084602     COMPUTE P-TO-P2-KVLL   =  LENGTH OF P-TO-P2-MID-W4I28901 + 25        
084603     END-COMPUTE                                                          
084604                                                                          
084605     MOVE 'W4T289X '           TO P-TO-P2-KDTRANS                         
084606     MOVE '2133'               TO P-TO-P2-IDTRANS                         
084607     MOVE 1                    TO P-TO-P2-KDMFSFOR                        
084608     PERFORM IMS-PURG-4289                                                
084609     .                                                                    
084610                                                                          
084620 L-REPL-WDK626  SECTION.                                                  
084700                                                                          
084800     MOVE IN-IDARTNR     TO W-IDARTNR                                     
084900     PERFORM IMS-GHU-WDK626                                               
085000     IF SEGMENT-FINNS                                                     
085100       IF JUST-KVPB-JUST (1) NOT = ZERO                                   
085200       OR JUST-KVPB-JUST (2) NOT = ZERO                                   
085300         MOVE ZERO    TO JUST-KVPB-JUST (1)                               
085400                         JUST-KVPB-JUST (2)                               
085500                         JUST-TIPBJUST  (1)                               
085600                         JUST-TIPBJUST  (2)                               
085700         PERFORM IMS-REPL-WDK626                                          
085800       END-IF                                                             
085900     END-IF                                                               
086000     .                                                                    
086100     EJECT                                                                
086200 D-REPL-WDD905  SECTION.                                                  
086300     SKIP2                                                                
086400     MOVE IN-IDARTNR     TO W-IDARTNR-WDD9                                
086500     MOVE WC-CDC-SE      TO W-IDDC-WDD9                                   
086600     MOVE IN-IDLEVNR     TO W-IDLEVNR                                     
086700     MOVE IN-DAAVROP-AVS TO W-DAAVROP                                     
086800     MOVE IN-TILEVDAG    TO W-TILEVDAG                                    
086900     MOVE IN-KDAVROP     TO W-KDAVROP                                     
087000     PERFORM IMS-GHU-INLB-WDD905                                          
087100                                                                          
087200     MOVE IN-KVAVROP     TO INLB-KVAVROP                                  
087300                                                                          
087400     PERFORM IMS-REPL-INLB                                                
087500                                                                          
087600     ADD +1 TO CHKP-ANT                                                   
087700     .                                                                    
087800     EJECT                                                                
087900 E-REPL-WDD201  SECTION.                                                  
088000     MOVE 'E-REPL-WDD201 '  TO CURRENT-SECTION                            
088100                                                                          
088200     MOVE IN-IDARTNR    TO W-IDARTNR                                      
088300                                                                          
088400     PERFORM IMS-GHU-WDD201                                               
088500                                                                          
088600     IF SEGMENT-FINNS                                                     
088700       MOVE ZERO        TO NYPON-ART-KVPROG                               
088800                                                                          
088900       IF NYPON-ART-KDANSKQ = '4'                                         
089000         MOVE '9'       TO NYPON-ART-KDANSKQ                              
089100       END-IF                                                             
089200                                                                          
089300       IF NYPON-ART-KDANSKQ = '2'                                         
089400         MOVE ART-KDPRODSL       TO TEST-KDPRODSL                         
089500         IF KDPRODSL-BRANDON OR                                           
089600           ((KDPRODSL-TOOLS OR KDPRODSL-VOLVO-EMB) AND                    
089700            (CLAG-IDINK > 399 AND CLAG-IDINK < 500))                      
089800                                                                          
089900           CONTINUE                                                       
090000         ELSE                                                             
090100           MOVE '9'       TO NYPON-ART-KDANSKQ                            
090200           MOVE ZERO      TO NYPON-ART-TILEVBEG                           
090300                                                                          
090400           MOVE '1'              TO W-1142-KDSEGKEY                       
090500           MOVE IN-IDARTNR       TO W-IDARTNR                             
090600           PERFORM IMS-GU-HT-1142                                         
090700                                                                          
090800           IF SEGMENT-SAKNAS                                              
090900             MOVE SPACE          TO 1142-WDGX1142                         
091000             MOVE '1'            TO 1142-KDSEGKEY                         
091100             MOVE IN-IDARTNR     TO 1142-IDARTNR                          
091200             MOVE SPACE          TO 1142-KDSVAR                           
091300                                                                          
091400             PERFORM IMS-ISRT-1142                                        
091500             ADD +1 TO CHKP-ANT                                           
091600           END-IF                                                         
091700         END-IF                                                           
091800       END-IF                                                             
091900                                                                          
092000       PERFORM IMS-REPL-WDD201                                            
092100       ADD +1 TO CHKP-ANT                                                 
092200     END-IF                                                               
092300     .                                                                    
092400     EJECT                                                                
092500 F-DLET-WDD904  SECTION.                                                  
092600*---                                                                      
092700* OM DET ÄR EN TOM-PLAN,TAG ÄVEN BORT FRÅN KÖN 2147                       
092800* FÖRSLAGEN TAS BORT I WDD905-ANROPET, SE W2133200                        
092900*---                                                                      
093000     SKIP2                                                                
093100     MOVE IN-IDARTNR    TO W-IDARTNR-WDD9                                 
093200     MOVE WC-CDC-SE     TO W-IDDC-WDD9                                    
093300     MOVE IN-IDLEVNR    TO W-IDLEVNR                                      
093400     PERFORM IMS-GET-INLB-WDD904                                          
093500                                                                          
093600     IF SEGMENT-FINNS                                                     
093700        PERFORM IMS-DLET-INLB                                             
093800                                                                          
093900        ADD +1 TO CHKP-ANT                                                
094000                                                                          
094100                                                                          
094200       MOVE +1   TO W-KDAVROP                                             
094300       PERFORM IMS-GU-WDD905-FORSLAG                                      
094400       IF SEGMENT-FINNS                                                   
094500         CONTINUE                                                         
094600       ELSE                                                               
094700         MOVE WC-CDC-SE     TO W-IDDC-D6-MIN                              
094800                               W-IDDC-D6-MAX                              
094900         MOVE IN-IDLEVNR    TO W-IDLEVNR-D6-MIN                           
095000                               W-IDLEVNR-D6-MAX                           
095100         MOVE IN-IDARTNR    TO W-IDARTNR-D6-MIN                           
095200                               W-IDARTNR-D6-MAX                           
095300                                                                          
095400         PERFORM IMS-GHU-WDD6-MIN-MAX                                     
095500         IF SEGMENT-FINNS                                                 
095600            PERFORM IMS-DLET-WDD601                                       
095700            ADD +1 TO CHKP-ANT                                            
095800         END-IF                                                           
095900       END-IF                                                             
096000     END-IF                                                               
096100                                                                          
096200     .                                                                    
096300     EJECT                                                                
096400 G-DLET-WDD905  SECTION.                                                  
096500     SKIP2                                                                
096600     MOVE IN-IDARTNR     TO W-IDARTNR-WDD9                                
096700                            W-IDARTNR-WDD6                                
096800     MOVE WC-CDC-SE      TO W-IDDC-WDD9                                   
096900     MOVE IN-IDLEVNR     TO W-IDLEVNR                                     
097000                            W-IDLEVNR-WDD6                                
097100     MOVE IN-DAAVROP-AVS TO W-DAAVROP                                     
097200     MOVE IN-TILEVDAG    TO W-TILEVDAG                                    
097300     MOVE IN-KDAVROP     TO W-KDAVROP                                     
097400******LÄS WDD905 FÖR DELETE AV SEGMENT                                    
097500     PERFORM IMS-GHU-INLB-WDD905                                          
097600     MOVE ZERO           TO WS-KDAVROP                                    
097700     IF SEGMENT-FINNS                                                     
097800       MOVE INLB-KDAVROP TO WS-KDAVROP                                    
097900       PERFORM IMS-DLET-INLB                                              
098000                                                                          
098100       ADD +1 TO CHKP-ANT                                                 
098200                                                                          
098300******LÄS WDD601 OM KDAVROP = 1 FÖR ATT SEDAN DELETA                      
098400       IF WS-KDAVROP = 1                                                  
098500         PERFORM IMS-GHU-WDD601                                           
098600         IF SEGMENT-FINNS                                                 
098700            PERFORM IMS-DLET-WDD601                                       
098800            ADD +1 TO CHKP-ANT                                            
098900         END-IF                                                           
099000       END-IF                                                             
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400 H-ISRT-WDK629  SECTION.                                                  
099500     MOVE 'H-ISRT-WDK629 '  TO CURRENT-SECTION                            
099600                                                                          
099700     MOVE IN-IDDC-REF TO W-IDDC-REF                                       
099800     MOVE IN-IDARTNR  TO W-IDARTNR                                        
099900     PERFORM IMS-GHU-WDK629                                               
100000     IF SEGMENT-FINNS                                                     
100100       DISPLAY '**WDK629 FINNS REDAN IDDC-REF = '  IN-IDDC-REF            
100200                                                                          
100300       PERFORM IMS-DLET-WDK629                                            
100400       ADD +1 TO CHKP-ANT                                                 
100500     END-IF                                                               
100600                                                                          
100700     MOVE IN-IDDC-REF      TO CREF-IDDC-REF                               
100800     MOVE 'N'              TO CREF-FLFLYG                                 
100900     MOVE 'N'              TO CREF-FLPB-FLYTT                             
101000     MOVE 'N'              TO CREF-FLREFBEO                               
101100     MOVE 'J'              TO CREF-FLREFILL                               
101200     MOVE 'N'              TO CREF-FLREFNYO                               
101300     MOVE 'N'              TO CREF-FLBUYUPD                               
101400     MOVE 'N'              TO CREF-FLTABUPD                               
101500     MOVE 'N'              TO CREF-FLWILSON                               
101600     MOVE ZERO             TO CREF-IDPERSON-BUY                           
101700     MOVE ZERO             TO CREF-IDREFTAB                               
101800     MOVE IN-KVPB-PLAN     TO CREF-KVPB-PLAN                              
101900     MOVE 'A'              TO CREF-KDREFSTA                               
102000     MOVE ZERO             TO CREF-KVREFOVL                               
102100     MOVE ZERO             TO CREF-KVREFPKT                               
102200                                                                          
102300     MOVE 1   TO IX                                                       
102400     PERFORM UNTIL IX  > 12                                               
102500       MOVE IN-RESEASON-PLAN(IX)   TO CREF-RESEASON-PLAN (IX)             
102600                                                                          
102700       ADD 1  TO IX                                                       
102800     END-PERFORM                                                          
102900                                                                          
103000     MOVE ZERO             TO CREF-TIORDREG                               
103100     MOVE ZERO             TO CREF-TIREFEFT                               
103200     MOVE ZERO             TO CREF-TIREFPAF                               
103300     MOVE ZERO             TO CREF-TIREFPKT                               
103400     MOVE DAGENS-DATUM     TO CREF-TIREFSTA                               
103500     MOVE ZERO             TO CREF-TIREFSTO                               
103600                                                                          
103700     MOVE IN-IDARTNR       TO W-IDARTNR                                   
103800     PERFORM IMS-ISRT-WDK629                                              
103900     ADD +1 TO CHKP-ANT                                                   
104000                                                                          
104100     PERFORM HA-UPD-PB-PLAN                                               
104200     .                                                                    
104300     EJECT                                                                
104400 HA-UPD-PB-PLAN        SECTION.                                           
104500                                                                          
104600*--- THIS SECTION CALLS UTILITY PROGRAM W272UTUP TO UPDATE                
104700*--- MASK KVPB-PLAN IN WDK6. ITS MANDATORY TO CALL THE UTILITY            
104800*--- USING CORRECT KDCALL VALUE.                                          
104900                                                                          
105000     INITIALIZE UTUP-W272UTUP                                             
105100     MOVE 001                   TO UTUP-KDCALL                            
105200     MOVE IN-IDARTNR            TO UTUP-IDARTNR                           
105300     MOVE IN-IDDC-REF           TO UTUP-IDDC-REF                          
105400     MOVE '11'                  TO UTUP-IDDC                              
105500                                                                          
105600     CALL W272UTUP USING UTUP-W272UTUP                                    
105700                         U2-WDK6-PCB                                      
105800                         U2-WDB6-PCB                                      
105900                         U2-PBTO-W222-WDK6-PCB                            
106000                         U2-PBTO-W222-WDK7-PCB                            
106100                         U2-PBTO-W222-ARTM-PCB                            
106200                         U2-PBTO-W222-R1-2501-PCB                         
106300                         U2-PBTO-W222-R1-WDB6R-PCB                        
106400                         U2-PBTO-W222-R1-WDK7R-PCB                        
106500                         U2-PBTO-W222-WDB6-PCB                            
106600                         U2-PBTO-W222-WDD7-PCB                            
106700                         U2-PBTO-W222-WDK7E-PCB                           
106800                         U2-PBTO-W222-R1-UTIL-WDK6-PCB                    
106900                         U2-PBTO-W222-R1-UTIL-WDK7-PCB                    
107000                         U2-PBTO-W222-R1-UTIL-WDB6-PCB                    
107100                         U2-PBTO-W222-U1-WDK7-PCB                         
107200                         U2-PBTO-W222-U1-WDB6-PCB                         
107300                         U2-PBTO-W222-U1-UTIL-WDK6-PCB                    
107400                         U2-PBTO-W222-U1-UTIL-WDK7-PCB                    
107500                         U2-PBTO-W222-U1-UTIL-WDB6-PCB                    
107600                         U2-REFL2-2501-PCB                                
107700                         U2-REFL2-WDB6-PCB                                
107800                         U2-REFL2-UTIL-WDK6-PCB                           
107900                         U2-REFL2-UTIL-WDK7-PCB                           
108000                         U2-REFL2-UTIL-WDB6-PCB                           
108100                         U2-W222-WDK6-PCB                                 
108200                         U2-W222-WDK7-PCB                                 
108300                         U2-W222-ARTM-PCB                                 
108400                         U2-W222-2501-PCB                                 
108500                         U2-W222-WDB6R-PCB                                
108600                         U2-W222-WDK7R-PCB                                
108700                         U2-W222-WDB6-PCB                                 
108800                         U2-W222-WDD7-PCB                                 
108900                         U2-W222-WDK7E-PCB                                
109000                         U2-W222-UTIL-WDK6-PCB                            
109100                         U2-W222-UTIL-WDK7-PCB                            
109200                         U2-W222-UTIL-WDB6-PCB                            
109300                         U2-W222-U1-WDK7-PCB                              
109400                         U2-W222-U1-WDB6-PCB                              
109500                         U2-W222-U1-UTIL-WDK6-PCB                         
109600                         U2-W222-U1-UTIL-WDK7-PCB                         
109700                         U2-W222-U1-UTIL-WDB6-PCB                         
109800                                                                          
109900     IF UTUP-KDSVAR-OK                                                    
110000        CONTINUE                                                          
110100     ELSE                                                                 
110200        DISPLAY 'W272UTUP-ERROR :' UTUP-TEXT                              
110300        CALL FELLOG                                                       
110400     END-IF                                                               
110500     .                                                                    
110600     EJECT                                                                
110700 I-DLET-HTR-1142  SECTION.                                                
110800     MOVE 'I-DLET-HTR-1142 '    TO CURRENT-SECTION                        
110900                                                                          
111000     MOVE '4'         TO W-1142-KDSEGKEY                                  
111100     MOVE IN-IDARTNR  TO W-IDARTNR                                        
111200     PERFORM IMS-GU-HT-1141-ROT                                           
111300     PERFORM IMS-GHNP-HT-1142                                             
111400     IF SEGMENT-FINNS                                                     
111500       PERFORM IMS-DLET-1142                                              
111600       ADD +1 TO CHKP-ANT                                                 
111700     END-IF                                                               
111800                                                                          
111900     .                                                                    
112000     EJECT                                                                
112100 J-DLET-WDK629    SECTION.                                                
112200     MOVE 'J-DLET-WDK629   '    TO CURRENT-SECTION                        
112300                                                                          
112400     MOVE IN-IDDC-REF TO W-IDDC-REF                                       
112500     MOVE IN-IDARTNR  TO W-IDARTNR                                        
112600     PERFORM IMS-GHU-WDK629                                               
112700     IF SEGMENT-FINNS                                                     
112800       PERFORM IMS-DLET-WDK629                                            
112900       ADD +1 TO CHKP-ANT                                                 
113000     END-IF                                                               
113100                                                                          
113200     .                                                                    
113300     EJECT                                                                
113400 K-DLET-WDE301    SECTION.                                                
113500     MOVE 'K-DLET-WDE301   '    TO CURRENT-SECTION                        
113600                                                                          
113700     MOVE WC-CDC-SE   TO W-IDDC-MIN-E3                                    
113800                         W-IDDC-MAX-E3                                    
113900     MOVE IN-IDARTNR  TO W-IDARTNR                                        
114000                                                                          
114100     MOVE ZERO        TO WS-KVBEART                                       
114200     PERFORM IMS-GHU-WDE301                                               
114300     IF SEGMENT-FINNS                                                     
114400       IF (REF-KDREFTYP = 'O') OR (REF-KDREFORS = 'O')                    
114500         MOVE REF-KVBEART TO WS-KVBEART                                   
114600       END-IF                                                             
114700                                                                          
114800       PERFORM IMS-DLET-WDE301                                            
114900       ADD +1 TO CHKP-ANT                                                 
115000     END-IF                                                               
115100                                                                          
115200     IF WS-KVBEART > ZERO                                                 
115300       MOVE IN-IDARTNR     TO W-IDARTNR                                   
115400       PERFORM IMS-GHU-WDK6-CLAG                                          
115500       IF SEGMENT-FINNS                                                   
115600         COMPUTE CLAG-KVBEART = CLAG-KVBEART - WS-KVBEART                 
115700                                                                          
115800         PERFORM IMS-REPL-WDK611                                          
115900         ADD +1 TO CHKP-ANT                                               
116000       END-IF                                                             
116100     END-IF                                                               
116200     .                                                                    
116300     EJECT                                                                
116400                                                                          
116500 Z-FINIT SECTION.                                                         
116600                                                                          
116700                                                                          
116800     CLOSE W21334                                                         
116900     SKIP2                                                                
117000     MOVE 'S' TO POSTSUM-OPKOD                                            
117100     CALL POSTSUM USING POSTSUM-PARM                                      
117200     .                                                                    
117300     EJECT                                                                
117400 S01-LAES-W21334  SECTION.                                                
117500     SKIP2                                                                
117600     READ W21334 INTO IN-AREA                                             
117700     AT END                                                               
117800        SET END-OF-W21334 TO TRUE                                         
117900                                                                          
118000     NOT AT END                                                           
118100        MOVE 'W21334'  TO POSTSUM-FDNAMN                                  
118200        MOVE IN-IDSEGM TO POSTSUM-DDNAMN2                                 
118300        MOVE IN-ATGARD TO POSTSUM-TRANSTYP                                
118400        CALL POSTSUM USING POSTSUM-PARM                                   
118500                                                                          
118600***     ADD 1 TO W-W21334-KVPOST-IN                                       
118700     END-READ                                                             
118800     .                                                                    
118900     EJECT                                                                
119000 X-TAG-CHECKPOINT   SECTION.                                              
119100                                                                          
119200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
119300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
119400     PERFORM IMS-CHECKPOINT                                               
119500     MOVE ZERO TO CHKP-ANT                                                
119600* --- LÄS OM DATABAS OM DET BEHÖVS                                        
119700     .                                                                    
119800     EJECT                                                                
119900* --- IMS SEKTIONER ---                                                   
120000     SKIP3                                                                
120100 IMS-GET-WDK6-ROT SECTION.                                                
120200     MOVE 'IMS-GET-WDK6-ROT '  TO DBS-SECTION                             
120300                                                                          
120400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
120500          DELIMITED BY SIZE INTO SSA1                                     
120600     MOVE '  GE' TO GODK-STATUSKODER                                      
120700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
120800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
120900     PERFORM IMS-STATUSKONTROLL                                           
121000     .                                                                    
121100     EJECT                                                                
121200 IMS-GHU-WDK6-CLAG SECTION.                                               
121300     MOVE 'IMS-GHU-WDK6-CLAG '  TO DBS-SECTION                            
121400                                                                          
121500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
121600          DELIMITED BY SIZE INTO SSA1                                     
121700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
121800          DELIMITED BY SIZE INTO SSA2                                     
121900     MOVE '  GE' TO GODK-STATUSKODER                                      
122000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
122100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
122200     PERFORM IMS-STATUSKONTROLL                                           
122300     .                                                                    
122400     SKIP3                                                                
122500 IMS-REPL-WDK601 SECTION.                                                 
122600     MOVE 'IMS-REPL-WDK601 '  TO DBS-SECTION                              
122700                                                                          
122800     MOVE '  ' TO GODK-STATUSKODER                                        
122900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK601                       
123000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
123100     PERFORM IMS-STATUSKONTROLL                                           
123200     .                                                                    
123300     EJECT                                                                
123400 IMS-REPL-WDK611 SECTION.                                                 
123500     MOVE 'IMS-REPL-WDK611 '  TO DBS-SECTION                              
123600                                                                          
123700     MOVE '  ' TO GODK-STATUSKODER                                        
123800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
123900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
124000     PERFORM IMS-STATUSKONTROLL                                           
124100     .                                                                    
124200     EJECT                                                                
124300 IMS-GHU-WDK626 SECTION.                                                  
124400     MOVE 'IMS-GU-WDK626        ' TO DBS-SECTION                          
124500                                                                          
124600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
124700          DELIMITED BY SIZE INTO SSA1                                     
124800     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
124900     MOVE 'WDK626   ' TO SSA3                                             
125000     MOVE '  GE' TO GODK-STATUSKODER                                      
125100     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3         
125200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
125300     PERFORM IMS-STATUSKONTROLL                                           
125400     .                                                                    
125500     EJECT                                                                
125600 IMS-REPL-WDK626 SECTION.                                                 
125700     MOVE 'IMS-REPL-WDK626 '  TO DBS-SECTION                              
125800                                                                          
125900     MOVE '  ' TO GODK-STATUSKODER                                        
126000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK626                       
126100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
126200     PERFORM IMS-STATUSKONTROLL                                           
126300     .                                                                    
126400     EJECT                                                                
126500 IMS-ISRT-WDK629  SECTION.                                                
126600     MOVE 'IMS-ISRT-WDK629 '  TO DBS-SECTION                              
126700                                                                          
126800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
126900          DELIMITED BY SIZE INTO SSA1                                     
127000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
127100          DELIMITED BY SIZE INTO SSA2                                     
127200     MOVE 'WDK629   ' TO SSA3                                             
127300     MOVE '    ' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK629 SSA1 SSA2             
127500                                                    SSA3                  
127600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
127700     PERFORM IMS-STATUSKONTROLL                                           
127800     .                                                                    
127900     SKIP3                                                                
128000 IMS-GHU-WDK629  SECTION.                                                 
128100     MOVE 'IMS-GHU-WDK629 '  TO DBS-SECTION                               
128200                                                                          
128300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
128400          DELIMITED BY SIZE INTO SSA1                                     
128500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
128600          DELIMITED BY SIZE INTO SSA2                                     
128700     STRING 'WDK629  (IDDCREF  =' W-IDDCREF-X ')'                         
128800          DELIMITED BY SIZE INTO SSA3                                     
128900     MOVE '  GE' TO GODK-STATUSKODER                                      
129000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
129100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400     SKIP3                                                                
129500 IMS-DLET-WDK629 SECTION.                                                 
129600     MOVE 'IMS-DLET-WDK629 '  TO DBS-SECTION                              
129700                                                                          
129800     MOVE '  ' TO GODK-STATUSKODER                                        
129900     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK629                       
130000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
130100     PERFORM IMS-STATUSKONTROLL                                           
130200     .                                                                    
130300     EJECT                                                                
130400                                                                          
130500 IMS-GET-INLB-WDD904 SECTION.                                             
130600     MOVE 'IMS-GET-INLB-WDD904  '  TO DBS-SECTION                         
130700                                                                          
130800     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
130900          DELIMITED BY SIZE INTO SSA1                                     
131000     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
131100          DELIMITED BY SIZE INTO SSA2                                     
131200     STRING 'WLINLB22    '                                                
131300          DELIMITED BY SIZE INTO SSA3                                     
131400     MOVE '  GE' TO GODK-STATUSKODER                                      
131500     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
131600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
131700     PERFORM IMS-STATUSKONTROLL                                           
131800     .                                                                    
131900     EJECT                                                                
132000 IMS-GHU-INLB-WDD905 SECTION.                                             
132100     MOVE 'IMS-GHU-INLB-WDD905  '  TO DBS-SECTION                         
132200                                                                          
132300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
132400          DELIMITED BY SIZE INTO SSA1                                     
132500     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
132600          DELIMITED BY SIZE INTO SSA2                                     
132700     STRING 'WLINLB23(WDD905KY =' W-WDD905KY-X                            
132800                    '&KDAVROP  =' W-KDAVROP-X ')'                         
132900          DELIMITED BY SIZE INTO SSA3                                     
133000     MOVE '  GE' TO GODK-STATUSKODER                                      
133100     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
133200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     SKIP3                                                                
133600 IMS-GU-WDD905-FORSLAG SECTION.                                           
133700     MOVE 'IMS-GU-WDD905-FORSLAG ' TO DBS-SECTION                         
133800                                                                          
133900     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
134000          DELIMITED BY SIZE INTO SSA1                                     
134100     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
134200          DELIMITED BY SIZE INTO SSA2                                     
134300     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
134400          DELIMITED BY SIZE INTO SSA3                                     
134500     MOVE '  GE' TO GODK-STATUSKODER                                      
134600     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3            
134700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000     SKIP3                                                                
135100 IMS-REPL-INLB SECTION.                                                   
135200     MOVE 'IMS-REPL-INLB  '  TO DBS-SECTION                               
135300                                                                          
135400     MOVE '  ' TO GODK-STATUSKODER                                        
135500     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
135600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
135700     PERFORM IMS-STATUSKONTROLL                                           
135800     .                                                                    
135900     SKIP3                                                                
136000 IMS-DLET-INLB SECTION.                                                   
136100     MOVE 'IMS-DLET-INLB  '  TO DBS-SECTION                               
136200                                                                          
136300     MOVE '  ' TO GODK-STATUSKODER                                        
136400     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA                         
136500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
136600     PERFORM IMS-STATUSKONTROLL                                           
136700     .                                                                    
136800     EJECT                                                                
136900 IMS-GHU-WDD601 SECTION.                                                  
137000     MOVE 'IMS-GHU-WDD601 '  TO DBS-SECTION                               
137100                                                                          
137200     STRING 'WDD601  (WDD601KY =' W-WDD601KY-X ')'                        
137300     DELIMITED BY SIZE INTO SSA1                                          
137400     MOVE '  GE' TO GODK-STATUSKODER                                      
137500     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
137600     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
137700     PERFORM IMS-STATUSKONTROLL                                           
137800     .                                                                    
137900     EJECT                                                                
138000                                                                          
138100 IMS-GHU-WDD6-MIN-MAX  SECTION.                                           
138200     MOVE 'IMS-GHU-WDD6-MIN-MAX '  TO DBS-SECTION                         
138300                                                                          
138400     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
138500                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
138600     DELIMITED BY SIZE INTO SSA1                                          
138700     MOVE '  GE' TO GODK-STATUSKODER                                      
138800     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
138900     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
139000     PERFORM IMS-STATUSKONTROLL                                           
139100     .                                                                    
139200     EJECT                                                                
139300                                                                          
139400 IMS-DLET-WDD601        SECTION.                                          
139500     MOVE 'IMS-DLET-WDD601 '  TO DBS-SECTION                              
139600                                                                          
139700     MOVE '  ' TO GODK-STATUSKODER                                        
139800     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
139900     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
140000     PERFORM IMS-STATUSKONTROLL                                           
140100     .                                                                    
140200     EJECT                                                                
140300                                                                          
140400 IMS-GHU-WDD201    SECTION.                                               
140500     MOVE 'IMS-GHU-WDD201 '  TO DBS-SECTION                               
140600                                                                          
140700     MOVE SPACE              TO ALL-SSA                                   
140800     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
140900     DELIMITED BY SIZE INTO SSA1                                          
141000     MOVE '  GE' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
141200     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     EJECT                                                                
141600 IMS-REPL-WDD201   SECTION.                                               
141700     MOVE 'IMS-REPL-WDD201 '  TO DBS-SECTION                              
141800                                                                          
141900     MOVE SPACE              TO ALL-SSA                                   
142000     MOVE '  ' TO GODK-STATUSKODER                                        
142100     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
142200     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     EJECT                                                                
142600 IMS-GU-WDK712    SECTION.                                                
142700     MOVE 'IMS-GU-WDK712 '  TO DBS-SECTION                                
142800                                                                          
142900     MOVE SPACE              TO ALL-SSA                                   
143000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
143100     DELIMITED BY SIZE INTO SSA1                                          
143200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
143300     DELIMITED BY SIZE INTO SSA2                                          
143400     MOVE '  GE' TO GODK-STATUSKODER                                      
143500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
143600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
143700     PERFORM IMS-STATUSKONTROLL                                           
143800     .                                                                    
143900     EJECT                                                                
144000 IMS-GU-WDK711    SECTION.                                                
144100     MOVE 'IMS-GU-WDK711 '  TO DBS-SECTION                                
144200                                                                          
144300     MOVE SPACE              TO ALL-SSA                                   
144400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
144500     DELIMITED BY SIZE INTO SSA1                                          
144600     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
144700     DELIMITED BY SIZE INTO SSA2                                          
144800     MOVE '  GE' TO GODK-STATUSKODER                                      
144900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
145000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300     EJECT                                                                
145400 IMS-GU-WDK701 SECTION.                                                   
145500     MOVE 'IMS-GU-WDK701 '  TO DBS-SECTION                                
145600                                                                          
145700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X  ')'                        
145800          DELIMITED BY SIZE INTO SSA1                                     
145900     MOVE 'GE  ' TO GODK-STATUSKODER                                      
146000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
146100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     EJECT                                                                
146500 IMS-GNP-WDK711 SECTION.                                                  
146600     MOVE 'IMS-GNP-WDK711 '  TO DBS-SECTION                               
146700                                                                          
146800     MOVE 'WDK711 ' TO SSA1                                               
146900     MOVE '  GE' TO GODK-STATUSKODER                                      
147000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
147100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
147200     PERFORM IMS-STATUSKONTROLL                                           
147300     .                                                                    
147400     EJECT                                                                
147500 IMS-GHNP-WDK711 SECTION.                                                 
147600     MOVE 'IMS-GHNP-WDK711 '  TO DBS-SECTION                              
147700                                                                          
147800     MOVE 'WDK711 ' TO SSA1                                               
147900     MOVE '  GE' TO GODK-STATUSKODER                                      
148000     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
148100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
148200     PERFORM IMS-STATUSKONTROLL                                           
148300     .                                                                    
148400 IMS-REPL-WDK711   SECTION.                                               
148500     MOVE 'IMS-REPL-WDK711 '  TO DBS-SECTION                              
148600                                                                          
148700     MOVE SPACE              TO ALL-SSA                                   
148800     MOVE '  ' TO GODK-STATUSKODER                                        
148900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
149000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300     EJECT                                                                
149400 IMS-GU-HT-1142  SECTION.                                                 
149500     MOVE 'IMS-GU-HT-1142 '  TO DBS-SECTION .                             
149600                                                                          
149700     MOVE SPACE              TO ALL-SSA                                   
149800     STRING 'WDG201  (WDGXKEY  =' W-1141-KEY-X ')'                        
149900     DELIMITED BY SIZE INTO SSA1                                          
150000     STRING 'WDG202  (KDSEGKEY =' W-1142-KEY-X                            
150100                    '&IDARTNR  =' W-IDARTNR-X ')'                         
150200     DELIMITED BY SIZE INTO SSA2                                          
150300     MOVE '  GE' TO GODK-STATUSKODER                                      
150400     CALL CBLTDLI USING GU 1142-PCB DLI-IO-1142 SSA1 SSA2                 
150500     MOVE 1142-STATUS-CODE TO STATUS-WS                                   
150600     PERFORM IMS-STATUSKONTROLL                                           
150700     .                                                                    
150800     EJECT                                                                
150900 IMS-ISRT-1142    SECTION.                                                
151000     MOVE 'IMS-ISRT-1142 '  TO DBS-SECTION                                
151100                                                                          
151200     MOVE SPACE              TO ALL-SSA                                   
151300     STRING 'WDG201  (WDGXKEY  =' W-1141-KEY-X ')'                        
151400     DELIMITED BY SIZE INTO SSA1                                          
151500     MOVE 'WDG202 '      TO SSA2                                          
151600     MOVE '  ' TO GODK-STATUSKODER                                        
151700     CALL CBLTDLI USING ISRT 1142-PCB DLI-IO-1142 SSA1 SSA2               
151800     MOVE 1142-STATUS-CODE TO STATUS-WS                                   
151900     PERFORM IMS-STATUSKONTROLL                                           
152000     .                                                                    
152100     EJECT                                                                
152200 IMS-GU-HT-1141-ROT  SECTION.                                             
152300     MOVE 'IMS-GU-HT-1141-ROT '  TO DBS-SECTION                           
152400                                                                          
152500     MOVE SPACE              TO ALL-SSA                                   
152600     STRING 'WDG201  (WDGXKEY  =' W-1141-KEY-X ')'                        
152700     DELIMITED BY SIZE INTO SSA1                                          
152800     MOVE '  ' TO GODK-STATUSKODER                                        
152900     CALL CBLTDLI USING GU 1142-PCB DLI-IO-1141 SSA1                      
153000     MOVE 1142-STATUS-CODE TO STATUS-WS                                   
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     EJECT                                                                
153400 IMS-GHNP-HT-1142  SECTION.                                               
153500     MOVE 'IMS-GHNP-HT-1142 '  TO DBS-SECTION                             
153600                                                                          
153700     MOVE SPACE              TO ALL-SSA                                   
153800     STRING 'WDG202  (KDSEGKEY =' W-1142-KEY-X                            
153900                    '&IDARTNR  =' W-IDARTNR-X ')'                         
154000     DELIMITED BY SIZE INTO SSA1                                          
154100     MOVE '  GE' TO GODK-STATUSKODER                                      
154200     CALL CBLTDLI USING GHNP 1142-PCB DLI-IO-1142 SSA1                    
154300     MOVE 1142-STATUS-CODE TO STATUS-WS                                   
154400     PERFORM IMS-STATUSKONTROLL                                           
154500     .                                                                    
154600     EJECT                                                                
154700 IMS-DLET-1142    SECTION.                                                
154800     MOVE 'IMS-DLET-1142 '  TO DBS-SECTION                                
154900                                                                          
155000     MOVE '  ' TO GODK-STATUSKODER                                        
155100     CALL CBLTDLI USING DLET 1142-PCB DLI-IO-1142                         
155200     MOVE 1142-STATUS-CODE TO STATUS-WS                                   
155300     PERFORM IMS-STATUSKONTROLL                                           
155400     .                                                                    
155500     EJECT                                                                
155600 IMS-GHU-WDE301   SECTION.                                                
155700     MOVE 'IMS-GHU-WDE301 '  TO DBS-SECTION                               
155800                                                                          
155900     MOVE SPACE              TO ALL-SSA                                   
156000     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
156100                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
156200                    '&KDREFTYP>=' W-KDREFTYP-MIN-X                        
156300                    '&KDREFTYP<=' W-KDREFTYP-MAX-X                        
156400                    '&IDARTNR  =' W-IDARTNR-X ')'                         
156500          DELIMITED BY SIZE INTO SSA1                                     
156600     MOVE '  GE' TO GODK-STATUSKODER                                      
156700     CALL CBLTDLI USING GHU WDE3-PCB DLI-IO-WDE301 SSA1                   
156800     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
156900     PERFORM IMS-STATUSKONTROLL                                           
157000     .                                                                    
157100     EJECT                                                                
157200 IMS-DLET-WDE301        SECTION.                                          
157300     MOVE 'IMS-DLET-WDE301 '  TO DBS-SECTION                              
157400                                                                          
157500     MOVE '  ' TO GODK-STATUSKODER                                        
157600     CALL CBLTDLI USING DLET WDE3-PCB DLI-IO-WDE301                       
157700     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000     EJECT                                                                
158100                                                                          
158200 IMS-GU-WDT501 SECTION.                                                   
158300     STRING 'WDT501  (IDARTNR = ' W-IDARTNR-X ')'                         
158400          DELIMITED BY SIZE INTO SSA1                                     
158500     MOVE '  GE' TO GODK-STATUSKODER                                      
158600     CALL CBLTDLI USING GU  WDT5-PCB DLI-IO-WDT501 SSA1                   
158700     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
158800     PERFORM IMS-STATUSKONTROLL                                           
158900     .                                                                    
159000     EJECT                                                                
159100 IMS-ISRT-WDT501 SECTION.                                                 
159200     MOVE 'WDT501 ' TO SSA1                                               
159300     MOVE '  II' TO GODK-STATUSKODER                                      
159400     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT501 SSA1                  
159500     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
159600     PERFORM IMS-STATUSKONTROLL                                           
159700     .                                                                    
159710 IMS-GHNP-WDT511 SECTION.                                                 
159720     MOVE   'WDT511  *F' TO SSA1                                          
159730     MOVE '  ' TO GODK-STATUSKODER                                        
159740     CALL CBLTDLI USING GHNP  WDT5-PCB DLI-IO-WDT511 SSA1                 
159750     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
159760     PERFORM IMS-STATUSKONTROLL                                           
159770     .                                                                    
159780     EJECT                                                                
159790 IMS-REPL-WDT511 SECTION.                                                 
159791     MOVE '  ' TO GODK-STATUSKODER                                        
159792     CALL CBLTDLI USING REPL  WDT5-PCB DLI-IO-WDT511                      
159793     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
159794     PERFORM IMS-STATUSKONTROLL                                           
159795     .                                                                    
159796     EJECT                                                                
159800 IMS-ISRT-WDT511 SECTION.                                                 
159900     STRING 'WDT501  (IDARTNR  =' W-IDARTNR-X ')'                         
160000            DELIMITED BY SIZE INTO SSA1                                   
160100     MOVE 'WDT511 ' TO SSA2                                               
160200     MOVE '  II' TO GODK-STATUSKODER                                      
160300     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT511 SSA1 SSA2             
160400     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
160500     PERFORM IMS-STATUSKONTROLL                                           
160600     .                                                                    
160700 IMS-RESTART SECTION.                                                     
160800     SKIP2                                                                
160900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
161000     MOVE '  ' TO GODK-STATUSKODER                                        
161100     CALL CBLTDLI USING XRST MSG-PCB                                      
161200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
161300                        CHKP-AREA-LENGTH CHKP-AREA                        
161400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
161500     PERFORM IMS-STATUSKONTROLL                                           
161600     .                                                                    
161700     EJECT                                                                
161800 IMS-CHECKPOINT SECTION.                                                  
161900     SKIP2                                                                
162000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
162100     MOVE '  XD' TO GODK-STATUSKODER                                      
162200     CALL CBLTDLI USING CHKP MSG-PCB                                      
162300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
162400                        CHKP-AREA-LENGTH CHKP-AREA                        
162500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
162600     PERFORM IMS-STATUSKONTROLL                                           
162700                                                                          
162800     IF IMS-EJ-OK                                                         
162900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
163000       DISPLAY FELTEXT                                                    
163100       CALL FELLOG                                                        
163200     END-IF                                                               
163300     .                                                                    
163400     EJECT                                                                
163410 IMS-PURG-4289    SECTION.                                                
163420                                                                          
163430     MOVE LOW-VALUE TO P-TO-P2-KDZ1 P-TO-P2-KDZ2                          
163440     MOVE SPACE TO GODK-STATUSKODER                                       
163450     CALL  CBLTDLI  USING PURG 4289-PCB P-TO-P-SW2                        
163460     MOVE 4289-STATUS-CODE TO STATUS-WS                                   
163470     PERFORM IMS-STATUSKONTROLL                                           
163480     .                                                                    
163500 IMS-STATUSKONTROLL SECTION.                                              
163600     SKIP2                                                                
163700     SET STATUS-IX TO 1                                                   
163800     SEARCH GODK-STATUS                                                   
163900       AT END                                                             
164000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
164100           DELIMITED BY SIZE INTO FELTEXT                                 
164200         DISPLAY FELTEXT                                                  
164300         CALL FELLOG                                                      
164400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
164500         CONTINUE                                                         
164600     END-SEARCH                                                           
164700     .                                                                    
