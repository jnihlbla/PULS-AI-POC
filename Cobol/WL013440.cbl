000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL013440.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000400 DATE-WRITTEN.   MAJ  2006.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        UPPDATERING AV ORDERBASER NÄR UTSKRIFT AV                        
000900*        ETIKETTER OCH UNDERLAG ÄR KLAR                                   
001000*                                                                         
001100*                                                                         
001200*    LÄNKAREA :       WL013440                                            
001300*                                                                         
001400*    CHANGE LOG                                                           
001500*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
001600*    E-TRACKER:10254592  2015       DECOMISSION VOHF                      
001700*                                                                         
001800 DATA DIVISION.                                                           
001900                                                                          
002000 WORKING-STORAGE SECTION.                                                 
002100                                                                          
002200 77  IDPGM                    PIC X(8)       VALUE 'WL013440'.            
002300                                                                          
002400 77  FILLER                   PIC X(8)       VALUE 'ERRORTEX'.            
002500 77  ERROR-TEXT               PIC X(80)      VALUE SPACE.                 
002600                                                                          
002700 77  WS-CURRENT-SECTION       PIC X(16)      VALUE 'CURRENT'.             
002800 77  WS-CURRENT-IMS-SECTION   PIC X(16)      VALUE SPACE.                 
002900                                                                          
003000 77  JA                       PIC X          VALUE 'J'.                   
003100 77  YES                      PIC X          VALUE 'Y'.                   
003200 77  NEJ                      PIC X          VALUE 'N'.                   
003300 77  DATUM-SW                 PIC X          VALUE 'N'.                   
003400                                                                          
003500 77  CP-SWE-EBCDIC            PIC 9(5)  VALUE 278.                        
003600 77  CP-UTF8                  PIC 9(5)  VALUE 1208.                       
003700 77  UCS-TEXT                 PIC N(100).                                 
003800                                                                          
003900 77  WS-TIRFS-ALFA            PIC X(11)      VALUE SPACE.                 
004000 77  FG-INDX                  PIC S9(9)      VALUE ZERO.                  
004100 77  MAX-FG-INDX              PIC S9(9)      VALUE +10.                   
004200 77  IX                       PIC S9(9)      VALUE ZERO.                  
004300 77  IX-MAX                   PIC S9(9)      VALUE 7.                     
004400 77  ORDER-IX                 PIC S9(9)      VALUE ZERO.                  
004500 77  WS-IDKONTO-ALFA          PIC X(11)      VALUE SPACE.                 
004600 77  ORDER-IX-MAX             PIC S9(9)      VALUE +99.                   
004700                                                                          
004800 77  FILLER                   PIC X(8)       VALUE 'AAAAAAAA'.            
004900 77  KVORDRAD-RAEKNARE        PIC S9(6)      VALUE ZERO COMP-3.           
005000 77  SUORDV-RAEKNARE          PIC S9(9)V9(2) VALUE ZERO COMP-3.           
005100 77  SUORDV-RAEKNARE-EXP      PIC S9(9)V9(2) VALUE ZERO COMP-3.           
005200 77  SUORDV-RAEKNARE-LOC      PIC S9(9)V9(2) VALUE ZERO COMP-3.           
005300 77  SUORDV-RAEKNARE-LOCPREL  PIC S9(9)V9(2) VALUE ZERO COMP-3.           
005400 77  VKORDNTO-RAEKNARE        PIC S9(9)V9(1) VALUE ZERO COMP-3.           
005500 77  VLORDNTO-RAEKNARE        PIC S9(4)V9(1) VALUE ZERO COMP-3.           
005600 77  WS-SPAR-4010-IDPLKLST    PIC S9(3)      VALUE ZERO COMP-3.           
005700 77  WS-PRC-IDKOLLI           PIC S9(5)      VALUE ZERO COMP-3.           
005800 77  WS-PRC-KDKOLLI           PIC  X(8).                                  
005900 77  WS-ORAD-RAKNARE          PIC S9(5)      VALUE ZERO COMP-3.           
006000 77  FILLER                   PIC X(8)       VALUE 'BBBBBBBB'.            
006100 77  WS-KOLLI-KDKOLSTA        PIC S9         VALUE ZERO COMP-3.           
006200 77  WS-VORD-KVKOLLI          PIC S9(5)      VALUE ZERO COMP-3.           
006300 77  WS-KORD-IDUSER           PIC  X(8)   VALUE SPACE.                    
006400 77  WS-ORAD-BEART            PIC  X(25)  VALUE SPACE.                    
006500 77  WS-VKTARA                PIC S9(6)V9(3) VALUE ZERO.                  
006600 77  WS-KOLLI-VKORDBTO-KOLLI  PIC S9(6)V9(1) VALUE ZERO.                  
006700 77  ADD-1-HEKTO              PIC S9(6)V9(1) VALUE 00000.1.               
006800                                                                          
006900 77  SEG-4010-SW              PIC X.                                      
007000     88  SEG-4010-FINNS                      VALUE 'J'.                   
007100     88  SEG-4010-SAKNAS                     VALUE 'N'.                   
007200                                                                          
007300 77  SEG-WDE601-SW            PIC X.                                      
007400     88  SEG-WDE601-FINNS                    VALUE 'J'.                   
007500     88  SEG-WDE601-FINNS-EJ                 VALUE 'N'.                   
007600                                                                          
007700 77  AUTOMAT-PACK-SW          PIC X.                                      
007800     88  AUTOMAT-PACKNING                    VALUE 'J'.                   
007900                                                                          
008000 77  BEKUNDRF-SW                 PIC X       VALUE 'N'.                   
008100     88  BEKUNDRF-OK                         VALUE 'J'.                   
008200     88  BEKUNDRF-FEL                        VALUE 'N'.                   
008300                                                                          
008400 01  WS-SPAR-PRODNR-PLKLST.                                               
008500     03  WS-SPAR-IDPRODNR     PIC S9(7)      VALUE ZERO COMP-3.           
008600     03  WS-SPAR-IDPLKLST     PIC S9(3)      VALUE ZERO COMP-3.           
008700                                                                          
008800 01  WS-4010-PRODNR-PLKLST.                                               
008900     03  WS-4010-PRODNR       PIC S9(7)      VALUE ZERO COMP-3.           
009000     03  WS-4010-IDPLKLST     PIC S9(3)      VALUE ZERO COMP-3.           
009100                                                                          
009200 01  WS-SPAR-4010-IDPRODNR    PIC S9(7)      VALUE ZERO COMP-3.           
009300                                                                          
009400 01  WS-SPAR-IDORDER-WDQ3      PIC S9(7)   VALUE ZERO COMP-3.             
009500 01  WS-SPAR-IDDC-WDQ3         PIC X(2).                                  
009600 01  WS-SPAR-IDPRODNR-WDQ3     PIC S9(7)   VALUE ZERO COMP-3.             
009700 01  WS-SPAR-IDPLKLST-WDQ3     PIC S9(3)   VALUE ZERO COMP-3.             
009800                                                                          
009900 01  FILLER                     PIC X(16)   VALUE 'WDG6-TRANS'.           
010000*    --- ARBETSAREA FÖR WDG6-TRANSAKTION                                  
010100 01  W-LOGGPOST.                                                          
010200*    03  -COPY WDGZRYE                                                    
010300     EJECT                                                                
010400 01  W-SORTPOST.                                                          
010500*    03  -COPY WDGZRYES                                                   
010600                                                                          
010700 01  TEST-IDDISTR             PIC 9(5)       COMP-3.                      
010800 01  FILLER REDEFINES TEST-IDDISTR.                                       
010900*    03   -COPY WWDIST07.                                                 
011000 01  FILLER REDEFINES TEST-IDDISTR.                                       
011100*    03   -COPY WWDIST18.                                                 
011200 01  FILLER REDEFINES TEST-IDDISTR.                                       
011300*    03   -COPY WWDIST19.                                                 
011400                                                                          
011500*01    -COPY WWDC99                                                       
011600       EJECT                                                              
011700                                                                          
011800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011900 01  GENERAL-SUBPROGRAMS.                                                 
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012300                                                                          
012400 01  GEMENSAMMA-SUBPROGRAM.                                               
012500     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
012600*                                                                         
012700*    --- PARAMETRAR TILL GEMESAMMA SUBPROGRAM                             
012800 01  FILLER                      PIC X(8)    VALUE 'W411DNOT'.            
012900*    -COPY W411DNOT                                                       
013000                                                                          
013100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013300                                                                          
013400 01  NYCKLAR-TILL-DLI.                                                    
013500                                                                          
013600     03  W-4007-IDHTYP-X.                                                 
013700         05  W-4007-IDHTYP       PIC  X(4)  VALUE '4007'.                 
013800         05  W-4007-IDPRODNR     PIC  9(7).                               
013900         05  W-4007-IDPLKLST     PIC  9(3).                               
014000         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
014100                                                                          
014200     03  W-4017-IDHTYP-X.                                                 
014300         05  W-4017-IDHTYP       PIC  X(4)  VALUE '4017'.                 
014400         05  W-4017-IDPRODNR     PIC  9(7).                               
014500         05  W-4017-IDPLKLST     PIC  9(3).                               
014600         05  W-4017-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
014700                                                                          
014800     03  W-WDGXKEY-4447-X.                                                
014900         05  W-IDHDTYP-4447      PIC X(4)    VALUE '4447'.                
015000         05  W-IDDC-4447         PIC X(2).                                
015100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
015200                                                                          
015300     03  W-WDGXKEY-4448-X.                                                
015400         05  W-IDPRC-4448        PIC X(4)    VALUE SPACE.                 
015500         05  FILLER              PIC X       VALUE LOW-VALUE.             
015600                                                                          
015700     03  W-WDGXKEY-4487-X.                                                
015800         05  W-IDHDTYP-4487      PIC X(4)    VALUE '4487'.                
015900         05  W-IDDC-4487         PIC X(2).                                
016000         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016100                                                                          
016200     03  W-WDGXKEY-4488-X.                                                
016300         05  W-KDPRCGRP-4488     PIC X(5)    VALUE SPACE.                 
016400                                                                          
016500     03  W-WDGXKEY-4726-ROT-X.                                            
016600         05  W-IDHDTYP-4726-ROT  PIC X(4)    VALUE '4726'.                
016700         05  W-FLBATCH-4726-ROT  PIC X       VALUE SPACE.                 
016800         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
016900                                                                          
017000     03  W-WDGXKEY-4726-X.                                                
017100         05  W-IDDISTR-4726      PIC S9(5)   VALUE ZERO COMP-3.           
017200         05  W-IDKUNDNR-4726     PIC S9(7)   VALUE ZERO COMP-3.           
017300         05  W-IDDC-4726         PIC X(2).                                
017400         05  W-KDFAKTYP-4726     PIC X       VALUE SPACE.                 
017500                                                                          
017600     03  W-WDE401KY-X.                                                    
017700         05  W-IDDISTR-WDE4      PIC S9(5)   VALUE ZERO COMP-3.           
017800         05  W-IDKUNDNR-WDE4     PIC S9(7)   VALUE ZERO COMP-3.           
017900         05  W-IDKUNDRF-WDE4     PIC X(10)   VALUE SPACE.                 
018000         05  W-IDPRODNR-WDE4     PIC S9(7)   VALUE ZERO COMP-3.           
018100         05  W-IDPLKLST-WDE4     PIC S9(3)   VALUE ZERO COMP-3.           
018200                                                                          
018300     03  W-WDE411-IDPURAD-X.                                              
018400         05  W-IDPURAD-WDE4      PIC S9(5)   VALUE ZERO COMP-3.           
018500                                                                          
018600     03  W-WDE601-IDPRODNR-X.                                             
018700         05  W-IDPRODNR-WDE6     PIC S9(7)   VALUE ZERO COMP-3.           
018800                                                                          
018900     03  W-WDE611-IDKOLLI-X.                                              
019000         05  W-IDKOLLI-WDE6      PIC S9(5)   VALUE ZERO COMP-3.           
019100                                                                          
019200     03  W-WDQ201-IDORDER-X.                                              
019300         05  W-IDORDER-WDQ2      PIC S9(7)   VALUE ZERO COMP-3.           
019400                                                                          
019500     03  W-WDQ212-IDDC-X.                                                 
019600         05  W-IDDC-WDQ2         PIC X(2).                                
019700                                                                          
019800     03  W-WDQ2CSEQ.                                                      
019900         05  W-IDDISTR-CSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
020000         05  W-IDKUNDNR-CSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
020100         05  FILLER              PIC 9(2)    VALUE ZERO.                  
020200         05  W-IDORDNR5-CSEQ     PIC 9(5).                                
020300         05  FILLER              PIC X(3)    VALUE SPACE.                 
020400                                                                          
020500     03  W-WDQ301KY-X.                                                    
020600         05  W-IDORDER-WDQ3      PIC S9(7)   VALUE ZERO COMP-3.           
020700         05  W-IDDC-WDQ3         PIC X(2).                                
020800         05  W-IDPRODNR-WDQ3     PIC S9(7)   VALUE ZERO COMP-3.           
020900         05  W-IDPLKLST-WDQ3     PIC S9(3)   VALUE ZERO COMP-3.           
021000                                                                          
021100     03  W-IDDC-B6-X.                                                     
021200         05 W-IDDC-B6            PIC X(2).                                
021300                                                                          
021400     03  W-IDPRC-B6-X.                                                    
021500         05  W-IDPRC-B6          PIC X(4)    VALUE SPACE.                 
021600                                                                          
021700     03  W-WDK501KY-X.                                                    
021800         05  W-KDKOLLI-K5        PIC X(8)    VALUE SPACE.                 
021900                                                                          
021910     03  W-KDSEGKEY-X.                                                    
021920         05 W-KDSEGKEY           PIC X(1)    VALUE '1'.                   
021930                                                                          
022000 01  FILLER               PIC X(16)   VALUE 'IMS-WS STATUS-WS'.           
022100                                                                          
022200 01  STATUS-WS                   PIC XX.                                  
022300     88  SEGMENT-FINNS           VALUE '  '.                              
022400     88  SEGMENT-SAKNAS          VALUE 'GE'.                              
022500     88  SEGMENT-FINNS-REDAN     VALUE 'II'.                              
022600 01  WDK5-STATUS-WS              PIC XX.                                  
022700     88  WDK5-SEGMENT-FOUND                VALUE '  '.                    
022800     88  WDK5-SEGMENT-MISSING              VALUE 'GE'.                    
022900     88  WDK5-SEGMENT-END                  VALUE 'GB'.                    
023000                                                                          
023100 01  GODK-STATUSKODER.                                                    
023200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023300                                                                          
023400 01  SSA1                        PIC X(128).                              
023500 01  SSA2                        PIC X(128).                              
023600 01  SSA3                        PIC X(128).                              
023700                                                                          
023800*    --- IMS FUNKTIONSKODER                                               
023900*01  -COPY W0003                                                          
024000                                                                          
024100 01  FILLER               PIC X(16)   VALUE 'WDGX4007'.                   
024200 01  DLI-IO-WDGX4007.                                                     
024300*    03  -COPY WDGX4007                                                   
024400                                                                          
024500 01  FILLER               PIC X(16)   VALUE 'WDGX4008'.                   
024600 01  DLI-IO-WDGX4008.                                                     
024700*    03  -COPY WDGX4008                                                   
024800                                                                          
024900 01  FILLER               PIC X(16)   VALUE 'WDGX4010'.                   
025000 01  DLI-IO-WDGX4010.                                                     
025100*    03  -COPY WDGX4010                                                   
025200                                                                          
025300 01  FILLER               PIC X(16)   VALUE 'WDGX4017'.                   
025400 01  DLI-IO-WDGX4017.                                                     
025500*    03  -COPY WDGX4017                                                   
025600                                                                          
025700 01  FILLER               PIC X(16)   VALUE 'WDGX4447'.                   
025800 01  DLI-IO-WDGX4447.                                                     
025900*    03  -COPY WDGX4447                                                   
026000                                                                          
026100 01  FILLER               PIC X(16)   VALUE 'WDGX4448'.                   
026200 01  DLI-IO-WDGX4448.                                                     
026300*    03  -COPY WDGX4448                                                   
026400                                                                          
026500 01  FILLER               PIC X(16)   VALUE 'WDGX4487'.                   
026600 01  DLI-IO-WDGX4487.                                                     
026700*    03  -COPY WDGX4487                                                   
026800                                                                          
026900 01  FILLER               PIC X(16)   VALUE 'WDGX4488'.                   
027000 01  DLI-IO-WDGX4488.                                                     
027100*    03  -COPY WDGX4488                                                   
027200                                                                          
027300 01  FILLER               PIC X(16)   VALUE 'WDGX4490'.                   
027400 01  DLI-IO-WDGX4490.                                                     
027500*    03  -COPY WDGX4490                                                   
027600                                                                          
027700 01  FILLER               PIC X(16)   VALUE 'WDGX4726'.                   
027800 01  DLI-IO-WDGX4726.                                                     
027900*    03  -COPY WDGX4726                                                   
028000                                                                          
028100 01  FILLER               PIC X(16)   VALUE 'WDGX4727'.                   
028200 01  DLI-IO-WDGX4727.                                                     
028300*    03  -COPY WDGX4727                                                   
028400                                                                          
028500 01  FILLER               PIC X(16)   VALUE 'WDE401  '.                   
028600 01  DLI-IO-WDE401.                                                       
028700*    03  -COPY WDE401                                                     
028800                                                                          
028900 01  FILLER               PIC X(16)   VALUE 'WDE411  '.                   
029000 01  DLI-IO-WDE411.                                                       
029100*    03  -COPY WDE411                                                     
029200                                                                          
029300 01  FILLER               PIC X(16)   VALUE 'WDE421  '.                   
029400 01  DLI-IO-WDE421.                                                       
029500*    03  -COPY WDE421                                                     
029600                                                                          
029700 01  FILLER               PIC X(16)   VALUE 'WDE601  '.                   
029800 01  DLI-IO-WDE601.                                                       
029900*    03  -COPY WDE601                                                     
030000                                                                          
030100 01  FILLER               PIC X(16)   VALUE 'WDE611  '.                   
030200 01  DLI-IO-WDE611.                                                       
030300*    03  -COPY WDE611                                                     
030400                                                                          
030500 01  FILLER               PIC X(16)   VALUE 'WDE621  '.                   
030510 01  DLI-IO-WDE621.                                                       
030520*    03  -COPY WDE621                                                     
030530                                                                          
030600 01  FILLER               PIC X(16)   VALUE 'WDG601  '.                   
030700 01  DLI-IO-WDG601.                                                       
030800*    03  -COPY WDGZ01    -PRE WDG6-                                       
030900                                                                          
031000 01  FILLER               PIC X(16)   VALUE 'WDQ201  '.                   
031100 01  DLI-IO-WDQ201.                                                       
031200*    03  -COPY WDQ201                                                     
031300                                                                          
031400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-Q201-OC'.        
031500 01  DLI-IO-AREA-WDQ201.                                                  
031600*    03  -COPY WDQ201 -PRE OC-                                            
031700                                                                          
031800 01  FILLER               PIC X(16)   VALUE 'WDQ212  '.                   
031900 01  DLI-IO-WDQ212.                                                       
032000*    03  -COPY WDQ212                                                     
032100                                                                          
032200 01  FILLER               PIC X(16)   VALUE 'WDQ301  '.                   
032300 01  DLI-IO-WDQ301.                                                       
032400*    03  -COPY WDQ301                                                     
032500                                                                          
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
032700 01  DLI-IO-WDB601.                                                       
032800*    03  -COPY WDB601                                                     
032900                                                                          
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB612'.                      
033100 01  DLI-IO-WDB612.                                                       
033200*    03  -COPY WDB612                                                     
033300     EJECT                                                                
033400 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDK501'.                       
033500 01  DLI-IO-WDK501.                                                       
033600*  03    EMBB01   -COPY WDK501                                            
033700     EJECT                                                                
033800                                                                          
033900 LINKAGE SECTION.                                                         
034000*                                                                         
034100                                                                          
034200*01  -COPY WL013440                                                       
034300                                                                          
034400*01  -COPY W0008  -PRE 4007-                                              
034500     05  FILLER                  PIC X.                                   
034600                                                                          
034700*01  -COPY W0008  -PRE 4017-                                              
034800     05  FILLER                  PIC X.                                   
034900                                                                          
035000*01  -COPY W0008  -PRE 4447-                                              
035100     05  FILLER                  PIC X.                                   
035200                                                                          
035300*01  -COPY W0008  -PRE 4487-                                              
035400     05  FILLER                  PIC X.                                   
035500                                                                          
035600*01  -COPY W0008  -PRE 4726-                                              
035700     05  FILLER                  PIC X.                                   
035800                                                                          
035900*01  -COPY W0008  -PRE WDE4-                                              
036000     05  FILLER                  PIC X.                                   
036100                                                                          
036200*01  -COPY W0008  -PRE WDE6-                                              
036300     05  FILLER                  PIC X.                                   
036400                                                                          
036500*01  -COPY W0008  -PRE WDG6-                                              
036600     05  FILLER                  PIC X.                                   
036700                                                                          
036800*01  -COPY W0008  -PRE WDQ2-                                              
036900     05  FILLER                  PIC X.                                   
037000                                                                          
037100*01  -COPY W0008  -PRE WDQ2C-                                             
037200     05  FILLER                  PIC X.                                   
037300                                                                          
037400*01  -COPY W0008  -PRE WDQ3-                                              
037500     05  FILLER                  PIC X.                                   
037600                                                                          
037700*01  -COPY W0008  -PRE WDB6-                                              
037800     05  FILLER                  PIC X.                                   
037900                                                                          
038000*01  -COPY W0008  -PRE WDK5-                                              
038100     05  FILLER                  PIC X.                                   
038200                                                                          
038300 01  DNOT-ORQP-PCB               PIC X.                                   
038400 01  DNOT-ORQP2-PCB              PIC X.                                   
038500 01  DNOT-ORQP3-PCB              PIC X.                                   
038600 01  DNOT-4013-PCB               PIC X.                                   
038700 01  DNOT-BENA-PCB               PIC X.                                   
038800                                                                          
038900  PROCEDURE DIVISION USING 3440-WL013440                                  
039000                           4007-PCB 4017-PCB 4447-PCB                     
039100                           4487-PCB 4726-PCB                              
039200                           WDE4-PCB WDE6-PCB WDG6-PCB                     
039300                           WDQ2-PCB WDQ2C-PCB                             
039400                           WDQ3-PCB WDB6-PCB WDK5-PCB                     
039500                           DNOT-ORQP-PCB                                  
039600                           DNOT-ORQP2-PCB                                 
039700                           DNOT-ORQP3-PCB                                 
039800                           DNOT-4013-PCB                                  
039900                           DNOT-BENA-PCB.                                 
040000                                                                          
040100  MAIN SECTION.                                                           
040200                                                                          
040300     PERFORM A-INIT                                                       
040400     PERFORM B-RENSA-PLOCKSATS-ARBETSTABELL                               
040500                                                                          
040600     MOVE 3440-IDPRODNR-KEY TO W-4007-IDPRODNR                            
040700     MOVE 3440-IDPLKLST-KEY TO W-4007-IDPLKLST                            
040800                                                                          
040900     PERFORM IMS-03-GU-WDGX4008                                           
041000     PERFORM IMS-04-GHNP-WDGX4010-FIRST                                   
041100                                                                          
041200     PERFORM C-KOLLA-OM-4010-FINNS                                        
041300                                                                          
041400     PERFORM UNTIL SEG-4010-SAKNAS                                        
041500        PERFORM D-UPPDATERA-WDR4-HT4487                                   
041600        MOVE 4010-IDPRODNR    TO WS-SPAR-IDPRODNR                         
041700        MOVE 4010-IDPLKLST    TO WS-SPAR-IDPLKLST                         
041800                                                                          
041900        PERFORM E-LAES-FRAM-WDQ212                                        
042000        PERFORM F-FLYTTA-DATA-TILL-DB                                     
042100                                                                          
042200        PERFORM UNTIL  SEG-4010-SAKNAS OR                                 
042300                      (WS-4010-PRODNR-PLKLST NOT =                        
042400                       WS-SPAR-PRODNR-PLKLST)                             
042500           PERFORM G-BERAEKNA-FAELT                                       
042600           PERFORM H-FLYTTA-INSERT-WDE411                                 
042700                                                                          
042800             IF NDC-US OR NDC-CA                                          
042900               PERFORM S01-DATA-TILL-DEL-NOTE                             
043000             END-IF                                                       
043100                                                                          
043200           PERFORM I-FLYTTA-INSERT-WDG6                                   
043300           PERFORM N-FLYTTA-INSERT-WDE421                                 
043400           PERFORM IMS-05-DLET-WDGX4010                                   
043500           PERFORM IMS-06-GHNP-WDGX4010                                   
043600           PERFORM C-KOLLA-OM-4010-FINNS                                  
043700           IF SEG-4010-SAKNAS                                             
043800           OR (WS-4010-PRODNR-PLKLST NOT = WS-SPAR-PRODNR-PLKLST)         
043900             PERFORM O-UPPDAT-WDE611                                      
044000           END-IF                                                         
044100        END-PERFORM                                                       
044200                                                                          
044300*LK     IF  SEG-4010-SAKNAS                                               
044400*LK     AND 3440-IDTRANS = 'L138'                                         
044500*LK     AND ORDER-IX > +1                                                 
044600*LK       PERFORM P-FLYTTA-DATA-TILL-DB                                   
044700*LK     END-IF                                                            
044800                                                                          
044900        PERFORM J-UPPDATERA-WDE6                                          
045000        PERFORM K-UPPDAT-WDQ3-OM-AUT-PACK                                 
045100        PERFORM L-UPPDAT-WDQ2-OM-AUT-PACK                                 
045200        PERFORM M-UPPDATERA-WDE4                                          
045300        IF NOT AUTOMAT-PACKNING                                           
045400           PERFORM IMS-31-ISRT-WDGX4490                                   
045500        END-IF                                                            
045600     END-PERFORM                                                          
045700                                                                          
045800     MOVE ZERO TO RETURN-CODE                                             
045900     GOBACK.                                                              
046000                                                                          
046100 A-INIT SECTION.                                                          
046200     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
046300                                                                          
046400     MOVE 0  TO  KVORDRAD-RAEKNARE                                        
046500                 SUORDV-RAEKNARE                                          
046600                 SUORDV-RAEKNARE-EXP                                      
046700                 SUORDV-RAEKNARE-LOC                                      
046800                 SUORDV-RAEKNARE-LOCPREL                                  
046900                 VKORDNTO-RAEKNARE                                        
047000                 VLORDNTO-RAEKNARE                                        
047100                 WDG6-IDLOGLOP                                            
047200                 WS-ORAD-RAKNARE                                          
047300     .                                                                    
047400                                                                          
047500 B-RENSA-PLOCKSATS-ARBETSTABELL SECTION.                                  
047600     MOVE 'B-RENSA-PLOCKSATS-ARBETSTABELL' TO WS-CURRENT-SECTION          
047700                                                                          
047800     MOVE 3440-IDPRODNR-KEY  TO W-4017-IDPRODNR                           
047900     MOVE 3440-IDPLKLST-KEY  TO W-4017-IDPLKLST                           
048000                                                                          
048100     PERFORM IMS-01-GHU-WDGX4017                                          
048200     IF SEGMENT-FINNS                                                     
048300        PERFORM IMS-02-DLET-WDGX4017                                      
048400     END-IF                                                               
048500     .                                                                    
048600                                                                          
048700 C-KOLLA-OM-4010-FINNS SECTION.                                           
048800     MOVE 'C-KOLLA-OM-4010-FINNS  ' TO WS-CURRENT-SECTION                 
048900                                                                          
049000     MOVE ZERO TO SEG-4010-SW                                             
049100                                                                          
049200     PERFORM UNTIL SEG-4010-FINNS OR SEG-4010-SAKNAS                      
049300        IF SEGMENT-FINNS                                                  
049400           IF 4010-IDARTNR > 0                                            
049500              MOVE JA            TO SEG-4010-SW                           
049600              MOVE 4010-IDPRODNR TO WS-4010-PRODNR                        
049700              MOVE 4010-IDPLKLST TO WS-4010-IDPLKLST                      
049800              MOVE 4010-IDORDER TO W-IDORDER-WDQ3                         
049900              MOVE 4010-IDDC TO W-IDDC-WDQ3                               
050000              MOVE 4010-IDPRODNR TO W-IDPRODNR-WDQ3                       
050100              MOVE 4010-IDPLKLST TO W-IDPLKLST-WDQ3                       
050200           ELSE                                                           
050300              PERFORM IMS-05-DLET-WDGX4010                                
050400              PERFORM IMS-06-GHNP-WDGX4010                                
050500           END-IF                                                         
050600        ELSE                                                              
050700           MOVE NEJ TO SEG-4010-SW                                        
050800        END-IF                                                            
050900     END-PERFORM                                                          
051000     .                                                                    
051100                                                                          
051200 D-UPPDATERA-WDR4-HT4487 SECTION.                                         
051300     MOVE 'D-UPPDATERA-WDR4-HT4487' TO WS-CURRENT-SECTION                 
051400                                                                          
051500     MOVE 4010-IDDC         TO W-IDDC-4447                                
051600                               WS-IDDC                                    
051700     MOVE 4010-IDPRC        TO W-IDPRC-4448                               
051800                                                                          
051900     PERFORM IMS-07-GU-WDGX4448                                           
052000                                                                          
052100     MOVE 4010-IDDC            TO W-IDDC-4487                             
052200     MOVE 4448-KDPRCGRP        TO W-KDPRCGRP-4488                         
052300                                                                          
052400     PERFORM IMS-07-GU-WDGX4487                                           
052500                                                                          
052600     IF SEGMENT-SAKNAS                                                    
052700        MOVE '4487'            TO 4487-IDHTYP                             
052800        MOVE 4010-IDDC         TO 4487-IDDC                               
052900        MOVE LOW-VALUE         TO 4487-LOW-VALUE                          
053000        PERFORM IMS-08-ISRT-WDGX4487                                      
053100                                                                          
053200        MOVE 4448-KDPRCGRP     TO 4488-KDPRCGRP                           
053300        PERFORM IMS-09-ISRT-WDGX4488                                      
053400     ELSE                                                                 
053500        PERFORM IMS-10-GNP-WDGX4488                                       
053600        IF SEGMENT-SAKNAS                                                 
053700           MOVE 4448-KDPRCGRP   TO 4488-KDPRCGRP                          
053800           PERFORM IMS-09-ISRT-WDGX4488                                   
053900        END-IF                                                            
054000     END-IF                                                               
054100     .                                                                    
054200                                                                          
054300 E-LAES-FRAM-WDQ212 SECTION.                                              
054400     MOVE 'E-LAES-FRAM-WDQ212  ' TO WS-CURRENT-SECTION                    
054500                                                                          
054600     IF 4010-IDORDER NOT = W-IDORDER-WDQ2                                 
054700                                                                          
054800       MOVE 4010-IDORDER  TO W-IDORDER-WDQ2                               
054900       MOVE 4010-IDDC     TO W-IDDC-WDQ2                                  
055000                             WS-IDDC                                      
055100                                                                          
055200       PERFORM IMS-11-GU-WDQ201                                           
055300       PERFORM IMS-12-GHNP-WDQ212                                         
055400                                                                          
055500       ADD +1             TO ORDER-IX                                     
055600     END-IF                                                               
055700     .                                                                    
055800                                                                          
055900 F-FLYTTA-DATA-TILL-DB SECTION.                                           
056000     MOVE 'F-FLYTTA-DATA-TILL-DB' TO WS-CURRENT-SECTION                   
056100                                                                          
056200     PERFORM FA-FLYTTA-INSERT-WDE401                                      
056300     PERFORM FB-KOLLA-OM-AUTOMAT-PACKNING                                 
056400     PERFORM FC-KOLLA-OM-WDE601-FINNS                                     
056500                                                                          
056600     IF AUTOMAT-PACKNING                                                  
056700     OR 3440-IDTRANS = 'L138'                                             
056800*                                                                         
056900       MOVE 4010-IDORDER   TO W-IDORDER-WDQ3                              
057000       MOVE 4010-IDDC      TO W-IDDC-WDQ3                                 
057100       MOVE 4010-IDPRODNR TO W-IDPRODNR-WDQ3                              
057200       MOVE 4010-IDPLKLST TO W-IDPLKLST-WDQ3                              
057300       MOVE 4010-IDORDER TO WS-SPAR-IDORDER-WDQ3                          
057400       MOVE 4010-IDDC TO WS-SPAR-IDDC-WDQ3                                
057500       MOVE 4010-IDPLKLST TO WS-SPAR-IDPLKLST-WDQ3                        
057600       MOVE 4010-IDPRODNR TO WS-SPAR-IDPRODNR-WDQ3                        
057700                                                                          
057800       PERFORM FD-UPPDAT-WDE6-SOM-PACKAD                                  
057900       IF AUTOMAT-PACKNING                                                
058000         PERFORM FE-KOLLA-OM-AUTOMAT-FAKT                                 
058100       END-IF                                                             
058200       IF 3440-IDTRANS = 'L138'                                           
058300         MOVE 4010-TIRFS      TO 4490-DARFS                               
058400         IF 4010-TIRFS NOT = ZERO                                         
058500           IF 4010-TIRFS < 5000000000                                     
058600             MOVE 20          TO 4490-DARFS (1:2)                         
058700           ELSE                                                           
058800             IF 4010-TIRFS < 9999999999                                   
058900               MOVE 19        TO 4490-DARFS (1:2)                         
059000             ELSE                                                         
059100               MOVE 999999999999 TO 4490-DARFS                            
059200             END-IF                                                       
059300           END-IF                                                         
059400         END-IF                                                           
059500       END-IF                                                             
059600     ELSE                                                                 
059700       MOVE 4010-TIRFS        TO 4490-DARFS                               
059800       IF 4010-TIRFS NOT = ZERO                                           
059900         IF 4010-TIRFS < 5000000000                                       
060000           MOVE 20            TO 4490-DARFS (1:2)                         
060100         ELSE                                                             
060200           IF 4010-TIRFS < 9999999999                                     
060300             MOVE 19          TO 4490-DARFS (1:2)                         
060400           ELSE                                                           
060500             MOVE 999999999999 TO 4490-DARFS                              
060600           END-IF                                                         
060700         END-IF                                                           
060800       END-IF                                                             
060900     END-IF                                                               
061000     MOVE ZERO TO 4490-TIKLAR                                             
061100     .                                                                    
061200                                                                          
061300 FA-FLYTTA-INSERT-WDE401 SECTION.                                         
061400     MOVE 'FA-FLYTTA-INSERT-WDE401' TO WS-CURRENT-SECTION                 
061500                                                                          
061600     MOVE OHUV-IDDISTR           TO KORD-IDDISTR                          
061700                                    W-IDDISTR-WDE4                        
061800     MOVE OHUV-IDKUNDNR          TO KORD-IDKUNDNR                         
061900                                    W-IDKUNDNR-WDE4                       
062000     MOVE SPACE                  TO KORD-IDKUNDRF                         
062100     MOVE 4010-IDKUNDRF (3:5)    TO KORD-IDKUNDRF                         
062200                                    W-IDKUNDRF-WDE4                       
062300     MOVE 4010-IDPRODNR          TO KORD-IDPRODNR                         
062400                                    W-IDPRODNR-WDE4                       
062500     MOVE 4010-IDPLKLST          TO KORD-IDPLKLST                         
062600                                    W-IDPLKLST-WDE4                       
062700     MOVE OHUV-IDORDER           TO KORD-IDORDER                          
062800     MOVE 4010-IDUSER            TO KORD-IDUSER                           
062900     MOVE 4010-IDUSER            TO WS-KORD-IDUSER                        
063000     MOVE 4010-IDDC              TO KORD-IDDC                             
063100                                                                          
063200     IF OHUV-FLEMBORD = 'J'                                               
063300       MOVE +1                   TO KORD-KDFAKPAP                         
063400     ELSE                                                                 
063500       MOVE ZERO                 TO KORD-KDFAKPAP                         
063600     END-IF                                                               
063700                                                                          
063800     MOVE OHUV-KDFAKTYP          TO KORD-KDFAKTYP                         
063900     MOVE 4010-KDFDKRAV          TO KORD-KDFDKRAV                         
064000     MOVE ARB-KDFRAKT            TO KORD-KDFRAKT                          
064100     MOVE OHUV-KDORDKL           TO KORD-KDORDKL                          
064200     MOVE ARB-KDROPACK           TO KORD-KDROPACK                         
064300     IF 4010-PRAVCOST > ZERO                                              
064400        MOVE 4010-KDVALISO       TO KORD-KDVALISO-EXP                     
064500        MOVE 'SEK'               TO KORD-KDVALISO                         
064600     ELSE                                                                 
064700        MOVE SPACE               TO KORD-KDVALISO-EXP                     
064800        MOVE 4010-KDVALISO       TO KORD-KDVALISO                         
064900     END-IF                                                               
065000                                                                          
065100     IF KORD-KDVALISO > SPACE                                             
065200       CONTINUE                                                           
065300     ELSE                                                                 
065400       DISPLAY 'WL013410  KORD-KDVALISO ' KORD-KDVALISO                   
065500        MOVE 'W4L013410  KORD-KDVALISO ' TO  ERROR-TEXT                   
065600        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
065700     END-IF                                                               
065800     MOVE ZERO                   TO KORD-KDPERSON                         
065900                                    KORD-KVORDRAD                         
066000                                    KORD-KVORDRAD-LEVPL                   
066100                                    KORD-KVORDRAD-VO                      
066200                                    KORD-KVORDRAD-VO-LEVPL                
066300                                    KORD-SUORDV                           
066400                                    KORD-SUORDV-EXP                       
066500                                    KORD-SUORDV-LOC                       
066600                                    KORD-SUORDV-LOCPREL                   
066700                                    KORD-SUORDV-LEVPL                     
066800                                    KORD-SUORDV-LEVPL-LOC                 
066900                                    KORD-SUORDV-LEVPL-LOCPREL             
067000                                    KORD-TIBEGPAC                         
067100                                    KORD-TIUTSKR                          
067200                                    KORD-IDARTNR-SATS                     
067300                                    KORD-KVBEART-SATS                     
067400                                    KORD-VKORDNTO                         
067500                                    KORD-VLORDNTO                         
067600                                    KORD-KDPAKOLL                         
067700                                    KORD-KVORDRAD-PACK                    
067800                                                                          
067900     MOVE ARB-TIRFS              TO WS-TIRFS-ALFA                         
068000     MOVE WS-TIRFS-ALFA (2:6)    TO KORD-TIBEGPAC                         
068100     MOVE 4010-TIUTSKR           TO KORD-TIUTSKR                          
068200                                                                          
068300     MOVE OHUV-TIREGDAT          TO KORD-TIORDREG                         
068400     MOVE OHUV-FLLSBOK           TO KORD-FLLSBOK                          
068500     MOVE OHUV-FLORDSPE          TO KORD-FLORDSPE                         
068600     MOVE OHUV-FLOVRLEV          TO KORD-FLOVRLEV                         
068700     MOVE 'N'                    TO KORD-FLPAFEL                          
068800                                                                          
068900     PERFORM IMS-13-ISRT-WDE401                                           
069000     .                                                                    
069100                                                                          
069200 FB-KOLLA-OM-AUTOMAT-PACKNING SECTION.                                    
069300     MOVE 'FB-KOLLA-OM-AUTOMAT-PACKNING' TO WS-CURRENT-SECTION            
069400                                                                          
069500     MOVE NEJ TO AUTOMAT-PACK-SW                                          
069600                                                                          
069700     IF OHUV-FLORDSPE = 'J' OR                                            
069800        OHUV-FLOVRLEV = 'J'                                               
069900        IF OHUV-FLAUTPAC = 'J'                                            
070000           MOVE JA TO AUTOMAT-PACK-SW                                     
070100        END-IF                                                            
070200     END-IF                                                               
070300     .                                                                    
070400                                                                          
070500 FC-KOLLA-OM-WDE601-FINNS SECTION.                                        
070600     MOVE 'FC-KOLLA-OM-WDE601-FINNS' TO WS-CURRENT-SECTION                
070700                                                                          
070800     MOVE 4010-IDPRODNR          TO W-IDPRODNR-WDE6                       
070900     PERFORM IMS-14-GHU-WDE601                                            
071000                                                                          
071100     IF SEGMENT-SAKNAS                                                    
071200       MOVE NEJ                  TO SEG-WDE601-SW                         
071300                                                                          
071400       MOVE 4010-IDPRODNR        TO VORD-IDPRODNR                         
071500                                    VORD-IDPRODNR-SAMP                    
071600       MOVE OHUV-IDDISTR         TO VORD-IDDISTR                          
071700       MOVE OHUV-IDKUNDNR        TO VORD-IDKUNDNR                         
071800       MOVE OHUV-KDORDKL         TO VORD-KDORDKL                          
071900       MOVE 4010-IDLOPNR-PL      TO VORD-IDLOTNR                          
072000       MOVE 4010-IDPRC(3:2)      TO VORD-KDORDLOT                         
072100       MOVE ZERO                 TO VORD-KDPERSON                         
072200       MOVE ZERO                 TO VORD-ADLEVPL                          
072300       MOVE 'N'                  TO VORD-FLCONTL                          
072400       MOVE 'N'                  TO VORD-FLDIRLEV                         
072500       MOVE 'N'                  TO VORD-FLSPARR                          
072600       MOVE 'N'                  TO VORD-FLMANORD                         
072700       MOVE 4010-IDDC            TO VORD-IDDC                             
072800       MOVE OHUV-KDFAKTYP        TO VORD-KDFAKTYP                         
072900       MOVE ARB-KDFRAKT          TO VORD-KDFRAKT                          
073000       IF 3440-IDTRANS = 'L138' OR 'A138'                                 
073100         MOVE +3                 TO VORD-KDMETOD                          
073200       ELSE                                                               
073300         MOVE ZERO               TO VORD-KDMETOD                          
073400       END-IF                                                             
073500       MOVE +1                   TO VORD-KDORDSTA                         
073600       IF 4010-PRAVCOST > ZERO                                            
073700          MOVE 4010-KDVALISO     TO VORD-KDVALISO-EXP                     
073800          MOVE 'SEK'             TO VORD-KDVALISO                         
073900       ELSE                                                               
074000          MOVE SPACE             TO VORD-KDVALISO-EXP                     
074100          MOVE 4010-KDVALISO     TO VORD-KDVALISO                         
074200       END-IF                                                             
074300       MOVE ZERO                 TO VORD-KVKOLLI                          
074400                                    VORD-KVKOLLI-FAKT                     
074500                                    VORD-KVKOLLI-LAST                     
074600                                    VORD-KVKOLLI-FL                       
074700                                    VORD-KVKOLPAC                         
074800                                    VORD-KVORDRAD                         
074900                                    VORD-KVORDRAD-PACK                    
075000                                    VORD-SUORDV                           
075100                                    VORD-SUORDV-EXP                       
075200                                    VORD-SUORDV-LOC                       
075300                                    VORD-SUORDV-LOCPREL                   
075400                                    VORD-SUORDV-PACK                      
075500                                    VORD-SUORDV-PACK-LOC                  
075600                                    VORD-SUORDV-PACK-LOCPREL              
075700                                    VORD-SUORDV-FL                        
075800                                    VORD-SUORDV-FL-LOC                    
075900                                    VORD-SUORDV-FL-LOCPREL                
076000                                                                          
076100       MOVE SPACE                TO VORD-KDVIA                            
076200                                    VORD-IDLEVNR                          
076300                                                                          
076400       MOVE OHUV-TIREGTID        TO VORD-TIREGTID                         
076500                                                                          
076600       MOVE ARB-TIRFS            TO WS-TIRFS-ALFA                         
076700       MOVE ARB-BEGMRK           TO VORD-BEGMRK                           
076800       MOVE WS-TIRFS-ALFA (2:6)  TO VORD-DABEGPAC                         
076900       IF VORD-DABEGPAC NOT = ZERO                                        
077000         IF VORD-DABEGPAC < 500000                                        
077100           MOVE 20               TO VORD-DABEGPAC (1:2)                   
077200         ELSE                                                             
077300           IF VORD-DABEGPAC < 999999                                      
077400             MOVE 19             TO VORD-DABEGPAC (1:2)                   
077500           ELSE                                                           
077600             MOVE 99999999       TO VORD-DABEGPAC                         
077700           END-IF                                                         
077800         END-IF                                                           
077900       END-IF                                                             
078000                                                                          
078100       MOVE ZERO                 TO VORD-TIFAKT-SK                        
078200                                    VORD-TILASTN-SK                       
078300                                    VORD-TIPACKN-SK                       
078400       MOVE 4010-TIUTSTID        TO VORD-TIUTSTID                         
078500       MOVE 4010-TIUTSKR         TO VORD-TIUTSKR                          
078600       MOVE 4010-TIRFS           TO VORD-DARFS                            
078700       IF 4010-TIRFS NOT = ZERO                                           
078800         IF 4010-TIRFS < 5000000000                                       
078900           MOVE 20               TO VORD-DARFS (1:2)                      
079000         ELSE                                                             
079100           IF 4010-TIRFS < 9999999999                                     
079200             MOVE 19             TO VORD-DARFS (1:2)                      
079300           ELSE                                                           
079400             MOVE 999999999999   TO VORD-DARFS                            
079500           END-IF                                                         
079600         END-IF                                                           
079700       END-IF                                                             
079800       MOVE ZERO                 TO VORD-VKORDBTO                         
079900                                    VORD-VKORDBTO-FL                      
080000                                    VORD-VKORDNTO                         
080100                                    VORD-VLORDBTO                         
080200                                    VORD-VLORDBTO-FL                      
080300                                    VORD-VLORDNTO                         
080400                                                                          
080500       MOVE OHUV-IDDISTR         TO TEST-IDDISTR                          
080600       MOVE OHUV-FLAUTFAK        TO VORD-FLAUTFAK                         
080700       MOVE 'N'                  TO VORD-FLFRAKTS                         
080800                                                                          
080900       MOVE 4010-IDORDER         TO W-IDORDER-WDQ3                        
081000       MOVE 4010-IDDC            TO W-IDDC-WDQ3                           
081100       MOVE 4010-IDPRODNR        TO W-IDPRODNR-WDQ3                       
081200       MOVE 4010-IDPLKLST        TO W-IDPLKLST-WDQ3                       
081300       PERFORM IMS-35-GU-WDQ301                                           
081400       MOVE ODEL-IDDC-EXP        TO VORD-IDDC-EXP                         
081500     ELSE                                                                 
081600       MOVE JA                   TO SEG-WDE601-SW                         
081700     END-IF                                                               
081800     .                                                                    
081900                                                                          
082000 FD-UPPDAT-WDE6-SOM-PACKAD SECTION.                                       
082100     MOVE 'FD-UPPDAT-WDE6-SOM-PACKAD' TO WS-CURRENT-SECTION               
082200*L138 SKAPA KOLLI FÖR WEB LDC                                             
082300     IF 3440-IDTRANS = 'L138'                                             
082400       MOVE +2            TO VORD-KDORDSTA                                
082500     ELSE                                                                 
082600       MOVE +3            TO VORD-KDORDSTA                                
082700       MOVE +1            TO VORD-KVKOLPAC                                
082800       MOVE +1            TO VORD-KVKOLLI                                 
082900     END-IF                                                               
083000                                                                          
083100     MOVE 3440-TIAAMMDD   TO VORD-TIPACKN-SK                              
083200                                                                          
083300     IF SEG-WDE601-FINNS                                                  
083400       PERFORM IMS-15-REPL-WDE601                                         
083500     ELSE                                                                 
083600       PERFORM IMS-16-ISRT-WDE601                                         
083700     END-IF                                                               
083800                                                                          
083900     IF 3440-IDTRANS = 'L138'                                             
084000       MOVE 4010-IDDC                       TO W-IDDC-B6-X                
084100       MOVE 4010-IDPRC                      TO W-IDPRC-B6-X               
084200                                                                          
084300       PERFORM IMS-33-GU-WDB612                                           
084400       IF SEGMENT-FINNS                                                   
084500                                                                          
084600         MOVE PRC-IDKOLLI-PRCSTA            TO KKOLLI-IDKOLLI             
084700         MOVE PRC-IDKOLLI-PRCSTA            TO WS-PRC-IDKOLLI             
084800         MOVE PRC-IDKOLLI-PRCSTA            TO W-IDKOLLI-WDE6             
084900         MOVE PRC-KDKOLLI                   TO WS-PRC-KDKOLLI             
085000       ELSE                                                               
085100                                                                          
085200         MOVE '9999'                        TO W-IDPRC-B6-X               
085300         PERFORM IMS-33-GU-WDB612                                         
085400                                                                          
085500         IF SEGMENT-FINNS                                                 
085600           MOVE PRC-IDKOLLI-PRCSTA          TO KKOLLI-IDKOLLI             
085700           MOVE PRC-IDKOLLI-PRCSTA          TO WS-PRC-IDKOLLI             
085800           MOVE PRC-IDKOLLI-PRCSTA          TO W-IDKOLLI-WDE6             
085900           MOVE PRC-KDKOLLI                 TO WS-PRC-KDKOLLI             
086000         ELSE                                                             
086100           MOVE +00010                      TO KKOLLI-IDKOLLI             
086200           MOVE +00010                      TO WS-PRC-IDKOLLI             
086300           MOVE +00010                      TO W-IDKOLLI-WDE6             
086400           MOVE 'NC'                        TO WS-PRC-KDKOLLI             
086500         END-IF                                                           
086600       END-IF                                                             
086700     ELSE                                                                 
086800       MOVE +1            TO W-IDKOLLI-WDE6                               
086900     END-IF                                                               
087000                                                                          
087100     PERFORM IMS-17-GHU-WDE611                                            
087200                                                                          
087300     IF SEGMENT-SAKNAS                                                    
087400       MOVE ZERO          TO KOLLI-VKORDNTO-KOLLI                         
087500       MOVE ZERO          TO KOLLI-VKORDBTO-KOLLI                         
087600                                                                          
087700       IF 3440-IDTRANS = 'L138'                                           
087800*TARA WEIGHT                                                              
087900         MOVE WS-PRC-KDKOLLI      TO W-KDKOLLI-K5                         
088000         PERFORM IMS-GU-WDK5                                              
088100         IF WDK5-SEGMENT-FOUND                                            
088200            MOVE EMB-VKTARA       TO WS-VKTARA                            
088300         END-IF                                                           
088400       END-IF                                                             
088500       MOVE +0               TO WS-KOLLI-KDKOLSTA                         
088600     ELSE                                                                 
088700       IF KOLLI-KDKOLSTA NUMERIC                                          
088800         MOVE KOLLI-KDKOLSTA TO WS-KOLLI-KDKOLSTA                         
088900       ELSE                                                               
089000         MOVE +0             TO WS-KOLLI-KDKOLSTA                         
089100       END-IF                                                             
089200     END-IF                                                               
089300                                                                          
089400     IF 3440-IDTRANS = 'L138'                                             
089500                                                                          
089600       MOVE WS-PRC-IDKOLLI            TO KOLLI-IDKOLLI                    
089700       MOVE WS-PRC-KDKOLLI            TO KOLLI-KDKOLLI                    
089800                                                                          
089900       MOVE +1                        TO ORDER-IX                         
090000       PERFORM UNTIL 4010-IDPRODNR = 3440-IDPRODNR(ORDER-IX)              
090100                  OR ORDER-IX      = ORDER-IX-MAX                         
090200                                                                          
090300         ADD +1 TO ORDER-IX                                               
090400       END-PERFORM                                                        
090500                                                                          
090600       IF 4010-IDPRODNR = 3440-IDPRODNR(ORDER-IX)                         
090700         MOVE 3440-IDTRPTNR (ORDER-IX) TO KOLLI-IDTRPTNR                  
090800         MOVE 3440-ADFLGEO (ORDER-IX) TO KOLLI-ADFLGEO                    
090900         MOVE 3440-ADFLOMR (ORDER-IX) TO KOLLI-ADFLOMR                    
091000         MOVE 3440-ADRUTNIV (ORDER-IX) TO KOLLI-ADRUTNIV                  
091200       END-IF                                                             
091300                                                                          
091400       MOVE 3440-VKORDNTO (ORDER-IX) TO KOLLI-VKORDNTO-KOLLI              
091500       MOVE 3440-VKORDNTO (ORDER-IX) TO KOLLI-VKORDBTO-KOLLI              
091600                                                                          
091700       COMPUTE KOLLI-VKORDBTO-KOLLI =                                     
091800               KOLLI-VKORDBTO-KOLLI + WS-VKTARA                           
091900       END-COMPUTE                                                        
092000                                                                          
092100*ADD-1-HEKTO BECAUSE OF 1 DECIMAIL IN WEIGHT (VKORDBTO)                   
092200*MAKES WEIGHT ROUNDED DOWN DUE TO TRUNCATION.                             
092300                                                                          
092400       COMPUTE KOLLI-VKORDBTO-KOLLI =                                     
092500               KOLLI-VKORDBTO-KOLLI + ADD-1-HEKTO                         
092600       END-COMPUTE                                                        
092700                                                                          
092800       MOVE KOLLI-VKORDBTO-KOLLI       TO WS-KOLLI-VKORDBTO-KOLLI         
092900     ELSE                                                                 
093000       MOVE +1            TO KOLLI-IDKOLLI                                
093100       MOVE ZERO          TO KOLLI-IDTRPTNR                               
093200                             KOLLI-KDKOLLI                                
093300       MOVE ZERO          TO KOLLI-ADFLOMR                                
093400                             KOLLI-ADRUTNIV                               
093500       MOVE SPACE         TO KOLLI-ADFLGEO                                
093700     END-IF                                                               
093800                                                                          
093900     MOVE 4010-IDDC       TO KOLLI-IDDC                                   
094000     IF 3440-IDTRANS = 'L138'                                             
094100       MOVE 4010-IDUSER   TO KOLLI-IDPLOCK                                
094200     ELSE                                                                 
094300       MOVE ZERO          TO KOLLI-IDPLOCK                                
094400     END-IF                                                               
094500                                                                          
094600*LK  IF SEGMENT-SAKNAS                                                    
094700*LK  OR (SEGMENT-FINNS                                                    
094800*LK  AND WS-KOLLI-KDKOLSTA = +1)                                          
094900                                                                          
095000     MOVE ZERO            TO KOLLI-IDKOLLI-FLER                           
095100     MOVE ZERO            TO KOLLI-ADVMODUL                               
095200                             KOLLI-DIDMODUL                               
095300                             KOLLI-DIHMODUL                               
095400     MOVE ZERO            TO KOLLI-DIKOLLIL                               
095500                             KOLLI-DIKOLLIB                               
095600                             KOLLI-DIKOLLIH                               
095700     MOVE ZERO            TO KOLLI-IDFAKLOP                               
095800                             KOLLI-IDTULLNR                               
095900                             KOLLI-RETULKS                                
096000                             KOLLI-IDFAKT                                 
096100                             KOLLI-IDFAKT-EXP                             
096200                             KOLLI-IDSHIPM                                
096300     MOVE ZERO            TO KOLLI-KDEMBTYP                               
096400                             KOLLI-KVFALRAD                               
096500     MOVE ZERO            TO KOLLI-KVFLAMP-KOLLI                          
096600     MOVE ZERO            TO KOLLI-KVORDRAD                               
096700                             KOLLI-TIAAVVD-PATR                           
096800                             KOLLI-SUORDV-KOLLI                           
096900                             KOLLI-SUORDV-KLI-EXP                         
097000                             KOLLI-SUORDV-LOC                             
097100                             KOLLI-SUORDV-LOCPREL                         
097200                             KOLLI-TIFAKT                                 
097300                             KOLLI-TIFAKT-EXP                             
097400                             KOLLI-TIFAKTID                               
097500                             KOLLI-TIFAKTID-EXP                           
097600                             KOLLI-TILASTN                                
097700                             KOLLI-TILASTID                               
097800     MOVE ZERO            TO KOLLI-ADHMODUL                               
097900                             KOLLI-KDFARLIG-KOLLI                         
098000                             KOLLI-VLORDBTO-KOLLI                         
098100     MOVE ZERO            TO KOLLI-IDLASTN                                
098200                             KOLLI-IDTRP                                  
098300     IF 3440-IDTRANS = 'L138'                                             
098400       MOVE 'J'           TO KOLLI-FLUTLAST                               
098500     ELSE                                                                 
098600       MOVE 'N'           TO KOLLI-FLUTLAST                               
098700     END-IF                                                               
098800     MOVE 'N'             TO KOLLI-FLBANDST                               
098900                             KOLLI-FLFRSUTS                               
099000     MOVE SPACE           TO KOLLI-FLTULLG                                
099100     MOVE SPACE           TO KOLLI-IDTULFTG                               
099200     MOVE SPACE           TO KOLLI-IDLBBET                                
099300     MOVE SPACE           TO KOLLI-KDARTURS-KOLLI                         
099400     IF 3440-IDTRANS = 'L138'                                             
099500       MOVE +0            TO KOLLI-KDKOLSTA                               
099600     ELSE                                                                 
099700       MOVE +1            TO KOLLI-KDKOLSTA                               
099800     END-IF                                                               
099900     MOVE VORD-KDVALISO      TO KOLLI-KDVALISO                            
100000     MOVE VORD-KDVALISO-EXP  TO KOLLI-KDVALISO-EXP                        
100100     MOVE 3440-TIAAMMDD   TO KOLLI-TIPACKN                                
100200     MOVE 3440-TIHHMMSS   TO KOLLI-TIPACTID                               
100300     MOVE VORD-IDDISTR    TO KOLLI-IDDISTR                                
100400     MOVE VORD-IDKUNDNR   TO KOLLI-IDKUNDNR                               
100500     MOVE VORD-KDORDKL    TO KOLLI-KDORDKL                                
100600     MOVE VORD-FLAUTFAK   TO KOLLI-FLAUTFAK                               
100700                                                                          
100800     MOVE +1 TO FG-INDX                                                   
100900     PERFORM UNTIL FG-INDX > MAX-FG-INDX                                  
101000                                                                          
101100       MOVE ZERO          TO KOLLI-IDPSN(FG-INDX)                         
101200                             KOLLI-VKART-FG(FG-INDX)                      
101300                             KOLLI-VLFG(FG-INDX)                          
101400       ADD +1 TO FG-INDX                                                  
101500     END-PERFORM                                                          
101600     MOVE ZERO            TO KOLLI-SUEQFG                                 
101700     MOVE VORD-DARFS      TO KOLLI-DARFS                                  
101800     MOVE ZERO            TO KOLLI-IDKOLLI-SAMP                           
101900                                                                          
102000     MOVE SPACE           TO KOLLI-IDSUPREF                               
102100                             KOLLI-IDLEVNR                                
102200                             KOLLI-KDSTASKLI                              
102210                             KOLLI-FILLERX2                               
102300     MOVE ZERO            TO KOLLI-DASUPREF                               
102400                             KOLLI-TISUPTID                               
102500                             KOLLI-KDVIA                                  
102600*LK  END-IF                                                               
102700                                                                          
102800     IF SEGMENT-FINNS                                                     
102900       IF 3440-IDTRANS = 'L138'                                           
103000*LK    AND WS-KOLLI-KDKOLSTA > +0                                         
103100         ADD +00001                      TO KOLLI-IDKOLLI                 
103200         ADD +00001                      TO KKOLLI-IDKOLLI                
103300         ADD +00001                      TO WS-PRC-IDKOLLI                
103400         ADD +00001                      TO W-IDKOLLI-WDE6                
103500         MOVE +0                         TO KOLLI-KDKOLSTA                
103600         PERFORM IMS-36-ISRT-WDE611                                       
103700                                                                          
103800         IF SEGMENT-FINNS-REDAN                                           
103900           PERFORM UNTIL SEGMENT-FINNS                                    
104000             ADD +00001                  TO KOLLI-IDKOLLI                 
104100             ADD +00001                  TO KKOLLI-IDKOLLI                
104200             ADD +00001                  TO WS-PRC-IDKOLLI                
104300             ADD +00001                  TO W-IDKOLLI-WDE6                
104400             MOVE +0                     TO KOLLI-KDKOLSTA                
104500             PERFORM IMS-36-ISRT-WDE611                                   
104600           END-PERFORM                                                    
104700         END-IF                                                           
104701*LK CROSSDOCK                                                             
104710         IF 3440-IDDC-CROSS (ORDER-IX) > SPACES                           
104730          MOVE W-KDSEGKEY-X     TO  CROSS-KDSEGKEY                        
104740          MOVE KOLLI-IDDC       TO  CROSS-IDDC-SEND                       
104741          MOVE 3440-IDDC-CROSS (ORDER-IX)                                 
104750                                TO  CROSS-IDDC-CROSS                      
104760          MOVE KOLLI-IDDISTR    TO  CROSS-IDDISTR                         
104770          MOVE KOLLI-IDKUNDNR   TO  CROSS-IDKUNDNR                        
104780          MOVE VORD-IDPRODNR    TO  CROSS-IDPRODNR                        
104790          MOVE KOLLI-IDKOLLI    TO  CROSS-IDKOLLI                         
104791          MOVE KOLLI-IDLEVNR    TO  CROSS-IDLEVNR                         
104792          MOVE KOLLI-IDSUPREF   TO  CROSS-IDSUPREF                        
104793          MOVE KOLLI-DARFS(3:6) TO  CROSS-TIRFSDAT                        
104794          MOVE ZERO             TO  CROSS-IDTRPTNR-CROSS                  
104795          MOVE ZERO             TO  CROSS-TIRECXDAT                       
104796          MOVE ZERO             TO  CROSS-TIRECXTID                       
104797          MOVE ZERO             TO  CROSS-TISKEPPN                        
104798          MOVE ZERO             TO  CROSS-IDSHIPM-CROSS                   
104799          MOVE SPACE            TO  CROSS-IDLBBET-CROSS                   
104800          MOVE 1                TO  CROSS-KDKOLSTA-CROSS                  
104801          PERFORM IMS-ISRT-WDE621                                         
104802         END-IF                                                           
104803                                                                          
104810*VORD-KVKOLLI CAN ONLY BE UPDATED WITH THE CORRECT NUMBER                 
104900*OF CASES AFTER THE UPDATE OF THE ACTUAL CASE.                            
105000*                                                                         
105100         MOVE 4010-IDPRODNR      TO W-IDPRODNR-WDE6                       
105200         PERFORM IMS-14-GHU-WDE601                                        
105300         ADD  +1                    TO VORD-KVKOLLI                       
105400         PERFORM IMS-15-REPL-WDE601                                       
105500*                                                                         
105600       ELSE                                                               
105700         PERFORM IMS-18-REPL-WDE611                                       
105800       END-IF                                                             
105900     ELSE                                                                 
106000*VORD-KVKOLLI CAN ONLY BE UPDATED WITH THE CORRECT NUMBER                 
106100*OF CASES AFTER THE UPDATE OF THE ACTUAL CASE.                            
106200*                                                                         
106300       PERFORM IMS-19-ISRT-WDE611                                         
106310*LK CROSSDOCK                                                             
106320       IF 3440-IDDC-CROSS (ORDER-IX) > SPACES                             
106330        MOVE W-KDSEGKEY-X       TO  CROSS-KDSEGKEY                        
106331        MOVE KOLLI-IDDC         TO  CROSS-IDDC-SEND                       
106332        MOVE 3440-IDDC-CROSS (ORDER-IX)                                   
106333                                TO  CROSS-IDDC-CROSS                      
106360        MOVE KOLLI-IDDISTR      TO  CROSS-IDDISTR                         
106370        MOVE KOLLI-IDKUNDNR     TO  CROSS-IDKUNDNR                        
106380        MOVE VORD-IDPRODNR      TO  CROSS-IDPRODNR                        
106390        MOVE KOLLI-IDKOLLI      TO  CROSS-IDKOLLI                         
106391        MOVE KOLLI-IDLEVNR      TO  CROSS-IDLEVNR                         
106392        MOVE KOLLI-IDSUPREF     TO  CROSS-IDSUPREF                        
106393        MOVE KOLLI-DARFS(3:6) TO    CROSS-TIRFSDAT                        
106394        MOVE ZERO               TO  CROSS-IDTRPTNR-CROSS                  
106395        MOVE ZERO               TO  CROSS-TIRECXDAT                       
106396        MOVE ZERO               TO  CROSS-TIRECXTID                       
106397        MOVE ZERO               TO  CROSS-TISKEPPN                        
106398        MOVE ZERO               TO  CROSS-IDSHIPM-CROSS                   
106399        MOVE 1                  TO  CROSS-KDKOLSTA-CROSS                  
106400        MOVE SPACE              TO  CROSS-IDLBBET-CROSS                   
106401        PERFORM IMS-ISRT-WDE621                                           
106402       END-IF                                                             
106410       IF 3440-IDTRANS = 'L138'                                           
106500         MOVE 4010-IDPRODNR      TO W-IDPRODNR-WDE6                       
106600         PERFORM IMS-14-GHU-WDE601                                        
106700         IF VORD-KVKOLLI = ZERO                                           
106800           MOVE +1        TO VORD-KVKOLLI                                 
106900         ELSE                                                             
107000           ADD  +1        TO VORD-KVKOLLI                                 
107100         END-IF                                                           
107200         PERFORM IMS-15-REPL-WDE601                                       
107300       END-IF                                                             
107400     END-IF                                                               
107500     .                                                                    
107600                                                                          
107700 FE-KOLLA-OM-AUTOMAT-FAKT SECTION.                                        
107800     MOVE 'FE-KOLLA-OM-AUTOMAT-FAKT ' TO WS-CURRENT-SECTION               
107900                                                                          
108000     MOVE OHUV-IDDISTR TO TEST-IDDISTR                                    
108100     IF VORD-FLAUTFAK = 'J' OR                                            
108200                                                                          
108300        OHUV-IDSYSTEM = 'SOFT'                                            
108400                                                                          
108500        PERFORM FEA-FOERBERED-4726-ROT                                    
108600                                                                          
108700        MOVE VORD-IDDISTR   TO W-IDDISTR-4726                             
108800                               AUTFAKT-IDDISTR                            
108900        MOVE VORD-IDKUNDNR  TO W-IDKUNDNR-4726                            
109000                               AUTFAKT-IDKUNDNR                           
109100        MOVE VORD-IDDC      TO W-IDDC-4726                                
109200                               AUTFAKT-IDDC                               
109300        MOVE VORD-KDFAKTYP  TO W-KDFAKTYP-4726                            
109400                               AUTFAKT-KDFAKTYP                           
109500                                                                          
109600        PERFORM IMS-20-ISRT-WDGX4726                                      
109700                                                                          
109800        MOVE VORD-IDPRODNR  TO AUTFAKT-IDPRODNR                           
109900        MOVE ZERO           TO AUTFAKT-IDSKEPPN                           
110000                               AUTFAKT-PRFRAKT                            
110100                                                                          
110200        MOVE NEJ            TO AUTFAKT-FLLASTA                            
110300        PERFORM IMS-21-ISRT-WDGX4727                                      
110400     END-IF                                                               
110500     .                                                                    
110600                                                                          
110700 FEA-FOERBERED-4726-ROT SECTION.                                          
110800     MOVE 'FEA-FOERBERED-4726-ROT ' TO WS-CURRENT-SECTION                 
110900                                                                          
111000     IF OHUV-FLOVRLEV = 'J'                                               
111100       MOVE NEJ        TO W-FLBATCH-4726-ROT                              
111200     ELSE                                                                 
111300       IF DIST19-SATS                                                     
111400         MOVE NEJ      TO W-FLBATCH-4726-ROT                              
111500       ELSE                                                               
111600          MOVE NEJ     TO W-FLBATCH-4726-ROT                              
111700       END-IF                                                             
111800     END-IF                                                               
111900     .                                                                    
112000                                                                          
112100 G-BERAEKNA-FAELT SECTION.                                                
112200     MOVE 'G-BERAEKNA-FAELT ' TO WS-CURRENT-SECTION                       
112300                                                                          
112400     COMPUTE KVORDRAD-RAEKNARE = KVORDRAD-RAEKNARE + 1                    
112500                                                                          
112600     IF 4010-PRAVCOST > ZERO                                              
112700        COMPUTE SUORDV-RAEKNARE-EXP = SUORDV-RAEKNARE-EXP +               
112800                               ( 4010-PRAVCOST * 4010-KVAVBART )          
112900     END-IF                                                               
113000     IF 4010-PRARTNTO > ZERO                                              
113100        COMPUTE SUORDV-RAEKNARE = SUORDV-RAEKNARE +                       
113200                               ( 4010-PRARTNTO * 4010-KVAVBART )          
113300     END-IF                                                               
113400     COMPUTE SUORDV-RAEKNARE-LOC = SUORDV-RAEKNARE-LOC +                  
113500                        ( 4010-PRARTNTO-LOC * 4010-KVAVBART )             
113600     COMPUTE SUORDV-RAEKNARE-LOCPREL = SUORDV-RAEKNARE-LOCPREL +          
113700                        ( 4010-PRARTNTO-LOCPREL * 4010-KVAVBART )         
113800                                                                          
113900                                                                          
114000     COMPUTE VKORDNTO-RAEKNARE = VKORDNTO-RAEKNARE +                      
114100                                 4010-VKORDNTO                            
114200                                                                          
114300     COMPUTE VLORDNTO-RAEKNARE = VLORDNTO-RAEKNARE +                      
114400                                 4010-VLORDNTO                            
114500     .                                                                    
114600                                                                          
114700 H-FLYTTA-INSERT-WDE411 SECTION.                                          
114800     MOVE 'H-FLYTTA-INSERT-WDE411' TO WS-CURRENT-SECTION                  
114900                                                                          
115000     ADD  +1                     TO WS-ORAD-RAKNARE                       
115100     MOVE 4010-IDPURAD           TO ORAD-IDPURAD                          
115200                                    W-IDPURAD-WDE4                        
115300     MOVE 4010-IDARTNR           TO ORAD-IDARTNR                          
115400     MOVE 4010-IDDC-RO           TO ORAD-IDDC-RO                          
115500     MOVE 4010-REKSIFFR          TO ORAD-REKSIFFR                         
115600     MOVE 4010-ADLAGOMR-ORD      TO ORAD-ADLAGOMR                         
115700     MOVE ZERO                   TO ORAD-ADLEVPL                          
115800     MOVE 4010-BERADREF          TO ORAD-BERADREF                         
115900                                                                          
116000     IF 4010-IDLEVNR NOT = SPACE OR                                       
116100        OHUV-FLLSBOK = NEJ                                                
116200       MOVE 'J'                  TO ORAD-FLDIRLEV                         
116300     ELSE                                                                 
116400       MOVE 'N'                  TO ORAD-FLDIRLEV                         
116500     END-IF                                                               
116600                                                                          
116700     MOVE 4010-KDORDING          TO ORAD-KDORDING                         
116800     MOVE 4010-FLRESTN           TO ORAD-FLRESTN                          
116900                                                                          
117000     MOVE 4010-IDBIL             TO ORAD-IDBIL                            
117100     MOVE 4010-IDKLIENT          TO ORAD-IDKLIENT                         
117200     MOVE 4010-IDARBREF          TO ORAD-IDARBREF                         
117300     MOVE 4010-IDVIN             TO ORAD-IDVIN                            
117400     MOVE 4010-IDLEVNR           TO ORAD-IDLEVNR                          
117500     MOVE 4010-IDPRODNR          TO ORAD-IDPRODNR                         
117600                                                                          
117700     MOVE 4010-KDVRINFO          TO ORAD-KDVRINFO                         
117800     MOVE SPACE                  TO ORAD-IDKUNDRF-RO                      
117900     MOVE 4010-IDKUNDRF-RO (3:5) TO ORAD-IDKUNDRF-RO                      
118000     MOVE 4010-KDARTURS          TO ORAD-KDARTURS                         
118100     MOVE 4010-KDDSP             TO ORAD-KDDSP                            
118200     MOVE 4010-KDFARLIG          TO ORAD-KDFARLIG                         
118300                                                                          
118400     MOVE 4010-IDKONTO           TO WS-IDKONTO-ALFA                       
118500     MOVE WS-IDKONTO-ALFA (2:1)  TO ORAD-KDFTG                            
118600                                                                          
118700     MOVE ZERO                   TO ORAD-KDTVA                            
118800                                                                          
118900     MOVE ARB-KDFRAKT            TO ORAD-KDFRAKT                          
119000                                                                          
119100     MOVE 4010-KDKVBRYT          TO ORAD-KDKVBRYT                         
119200     MOVE ZERO                   TO ORAD-KDOFFERT                         
119300     MOVE 4010-KDOI              TO ORAD-KDOI                             
119400     MOVE 4010-CLEARGROUP        TO ORAD-CLEARGROUP                       
119500     MOVE 4010-KDORDKL           TO ORAD-KDORDKL                          
119600                                                                          
119700     MOVE OHUV-IDDISTR TO TEST-IDDISTR                                    
119800     IF DIST18-SKROT                                                      
119900       MOVE +3                   TO ORAD-KDORDTYP                         
120000     ELSE                                                                 
120100       MOVE ZERO                 TO ORAD-KDORDTYP                         
120200     END-IF                                                               
120300                                                                          
120400     MOVE ZERO                    TO ORAD-KDQPACK                         
120500     MOVE 4010-KDPRODSL           TO ORAD-KDPRODSL                        
120600     MOVE +3                      TO ORAD-KDRADSTA                        
120700     MOVE 4010-IDKONTO            TO ORAD-IDKONTO                         
120800     MOVE 4010-IDANALYS           TO ORAD-IDANALYS                        
120900     MOVE 4010-IDKST              TO ORAD-IDKST                           
121000     MOVE ZERO                    TO ORAD-KVANNANT                        
121100     MOVE 4010-KVAVBART           TO ORAD-KVAVBART                        
121200     MOVE 4010-KVBEART-Q          TO ORAD-KVBEART                         
121300     MOVE ZERO                    TO ORAD-KVFLAMP                         
121400     MOVE ZERO                    TO ORAD-KVLEVART                        
121500     MOVE 4010-KVSLATT            TO ORAD-KVSLATT                         
121600     MOVE 4010-PRARTNTO           TO ORAD-PRARTNTO                        
121700     MOVE 4010-DEAL-PR-LINE       TO ORAD-DEAL-PR-LINE                    
121800     MOVE ZERO                    TO ORAD-PRARTULL                        
121900     MOVE 4010-TIPRIS             TO ORAD-TIPRIS                          
122000     MOVE 4010-TIRODAT            TO ORAD-TIRODAT                         
122100     MOVE 4010-TIUTSKR            TO ORAD-TIUTSKR                         
122200                                                                          
122300     COMPUTE ORAD-VKARTNTO      = 4010-VKART / 1000                       
122400     END-COMPUTE                                                          
122500                                                                          
122600     COMPUTE ORAD-VKART-NTO-KG  = 4010-VKART-NTO / 1000                   
122700     END-COMPUTE                                                          
122800                                                                          
122900     MOVE 4010-VLARTNTO           TO ORAD-VLARTNTO                        
123000     MOVE 'N'                     TO ORAD-FLFYSAVV                        
123100                                                                          
123200*    MOVE 4010-BEART              TO ORAD-BEART                           
123300     PERFORM HA-BEART-TO-EBCDIC                                           
123400     MOVE ORAD-BEART              TO WS-ORAD-BEART                        
123500                                                                          
123600     MOVE 4010-BEVOLREF           TO ORAD-BEVOLREF                        
123700     MOVE 4010-FLINVEST           TO ORAD-FLINVEST                        
123800     MOVE 4010-FLPRTILL           TO ORAD-FLPRTILL                        
123900     MOVE 4010-FLTILLK            TO ORAD-FLTILLK                         
124000     MOVE 4010-FLSDCLEV           TO ORAD-FLSDCLEV                        
124100     MOVE 4010-IDKAMPRF           TO ORAD-IDKAMPRF                        
124200     MOVE 4010-IDLOPNR-RO         TO ORAD-IDLOPNR-RO                      
124300     MOVE 4010-IDSYSTEM           TO ORAD-IDSYSTEM                        
124400     MOVE 4010-KDPRTYP            TO ORAD-KDPRTYP                         
124500     MOVE 'N'                     TO ORAD-FLNOLLJ                         
124600     MOVE 4010-IDPSN              TO ORAD-IDPSN                           
124700     MOVE 4010-VKART-FG           TO ORAD-VKART-FG                        
124800     MOVE 4010-VLFG               TO ORAD-VLFG                            
124900     MOVE 4010-SUEQFG             TO ORAD-SUEQFG                          
125000                                                                          
125100     IF AUTOMAT-PACKNING                                                  
125200     OR 3440-IDTRANS = 'L138'                                             
125300       MOVE ORAD-KVAVBART         TO ORAD-KVLEVART                        
125400                                                                          
125500       IF AUTOMAT-PACKNING                                                
125600         MOVE +4                  TO ORAD-KDRADSTA                        
125700         MOVE 'Y'                 TO DATUM-SW                             
125800       END-IF                                                             
125900     END-IF                                                               
126000                                                                          
126100     MOVE ZERO                    TO ORAD-KDANNULL                        
126200                                     ORAD-TISLULEV                        
126300                                                                          
126400     MOVE 4010-IDKUNDRF-WIP       TO ORAD-IDKUNDRF-WIP                    
126500     MOVE 4010-PRAVCOST           TO ORAD-PRAVCOST                        
126600     IF 4010-PRAVCOST > ZERO                                              
126700        MOVE 4010-KDVALISO        TO ORAD-KDVALISO-EXP                    
126800        MOVE 'SEK'                TO ORAD-KDVALISO                        
126900     ELSE                                                                 
127000        MOVE SPACE                TO ORAD-KDVALISO-EXP                    
127100        MOVE 4010-KDVALISO        TO ORAD-KDVALISO                        
127200     END-IF                                                               
127300                                                                          
127400     PERFORM IMS-22-ISRT-WDE411                                           
127500     .                                                                    
127600                                                                          
127700 HA-BEART-TO-EBCDIC   SECTION.                                            
127800*    BEART WAS CONVERTED TO UNICODE IN WL013410                           
127900*    TRANSLATE BACK TO EBCDIC BEFORE IT IS STORED IN WDE4                 
128000     MOVE 'HA-BEART-TO-EBCDIC   ' TO WS-CURRENT-SECTION                   
128100                                                                          
128200*    -- CHECK FOR TRUNCATED CHARACTERS AT THE END OF 4010-BEART           
128300*    -- AND CHANGE SUCH BYTES TO UNICODE SPACE                            
128400     IF  4010-BEART(25:1) >= X'C0'                                        
128500*      -- ONLY FIRST BYTE OF TWO OR THREE (C0 = B'110...'                 
128600       MOVE X'20' TO 4010-BEART(25:1)                                     
128700     END-IF                                                               
128800     IF  4010-BEART(24:1) >= X'E0'                                        
128900*      -- ONLY FIRST TWO BYTES OF THREE (E0 = B'1110...'                  
129000       MOVE X'2020' TO 4010-BEART(24:2)                                   
129100     END-IF                                                               
129200                                                                          
129300*    -- FIRST CONVERT TEXT TO UNICODE UCS2 FORMAT                         
129400     MOVE FUNCTION NATIONAL-OF(4010-BEART, CP-UTF8)                       
129500          TO UCS-TEXT                                                     
129600*    -- THEN CONVERT IT TO EBCDIC                                         
129700     MOVE FUNCTION DISPLAY-OF(UCS-TEXT, CP-SWE-EBCDIC)                    
129800          TO ORAD-BEART                                                   
129900     .                                                                    
130000                                                                          
130100 I-FLYTTA-INSERT-WDG6 SECTION.                                            
130200     MOVE 'I-FLYTTA-INSERT-WDG6 ' TO WS-CURRENT-SECTION                   
130300                                                                          
130400     IF WDG6-IDLOGLOP = 9                                                 
130500        MOVE 0 TO WDG6-IDLOGLOP                                           
130600     END-IF                                                               
130700                                                                          
130800     PERFORM IA-FLYTTA-TILL-LOGGPOST                                      
130900     PERFORM IB-FLYTTA-TILL-SORTPOST                                      
131000     ACCEPT WDG6-TIAAMMDD FROM DATE                                       
131100     ACCEPT WDG6-TIKLOCK  FROM TIME                                       
131200     ADD  1 TO WDG6-IDLOGLOP                                              
131300     PERFORM IMS-23-ISRT-WDG601                                           
131400                                                                          
131500     PERFORM UNTIL SEGMENT-FINNS                                          
131600       IF WDG6-IDLOGLOP = 9                                               
131700          MOVE 0 TO WDG6-IDLOGLOP                                         
131800          ACCEPT WDG6-TIKLOCK  FROM TIME                                  
131900       END-IF                                                             
132000       ADD 1 TO WDG6-IDLOGLOP                                             
132100       PERFORM IMS-23-ISRT-WDG601                                         
132200     END-PERFORM                                                          
132300     .                                                                    
132400                                                                          
132500 IA-FLYTTA-TILL-LOGGPOST SECTION.                                         
132600     MOVE 'IA-FLYTTA-TILL-LOGGPOST' TO WS-CURRENT-SECTION                 
132700                                                                          
132800     MOVE 'RYE'              TO RYE-IDPTYP                                
132900     MOVE SPACE              TO RYE-BERADREF                              
133000                                RYE-BEVOLREF                              
133100     IF 4010-IDLEVNR NOT = SPACE                                          
133200       MOVE 'J'              TO RYE-FLDIRLEV                              
133300     ELSE                                                                 
133400       MOVE 'N'              TO RYE-FLDIRLEV                              
133500     END-IF                                                               
133600                                                                          
133700     MOVE OHUV-FLLSBOK       TO RYE-FLLSBOK                               
133800     MOVE OHUV-FLORDSPE      TO RYE-FLORDSPE                              
133900     MOVE NEJ                TO RYE-FLTILLK                               
134000     MOVE 4010-IDARTNR       TO RYE-IDARTNR                               
134100     MOVE 4010-IDKUNDRF      TO RYE-IDKUNDRF                              
134200     MOVE 4010-IDKUNDRF-RO   TO RYE-IDKUNDRF-RO                           
134300     MOVE 4010-IDDC          TO RYE-IDDC                                  
134400     MOVE ZERO               TO RYE-KDDSP                                 
134500     MOVE SPACE              TO RYE-KDFAKTYP                              
134600     MOVE ZERO               TO RYE-KDKVBRYT                              
134700                                RYE-KDORDBEK                              
134800     MOVE 4010-KDORDING      TO RYE-KDORDING                              
134900     MOVE 4010-KDPRODSL      TO RYE-KDPRODSL                              
135000     MOVE ZERO               TO RYE-KDVRINFO                              
135100                                RYE-KDVRTPO                               
135200     MOVE 4010-KVAVBART      TO RYE-KVAVBART                              
135300     MOVE 4010-KVBEART-Q     TO RYE-KVBEART-Q                             
135400     MOVE ZERO               TO RYE-KVRO                                  
135500                                RYE-REKSIFFR                              
135600                                RYE-TIDISPIN                              
135700     MOVE 4010-TIREGDAT      TO RYE-TIORDREG                              
135800     MOVE 4010-TIRODAT       TO RYE-TIRODAT                               
135900                                                                          
136000     MOVE W-LOGGPOST TO WDG6-LOGGPOST                                     
136100     .                                                                    
136200     EJECT                                                                
136300                                                                          
136400 IB-FLYTTA-TILL-SORTPOST SECTION.                                         
136500     MOVE 'IB-FLYTTA-TILL-SORTPOST' TO WS-CURRENT-SECTION                 
136600                                                                          
136700     MOVE OHUV-IDDISTR       TO RYES-IDDISTR                              
136800     MOVE OHUV-IDKUNDNR      TO RYES-IDKUNDNR                             
136900     IF  OHUV-FLVORKO = JA                                                
137000     OR  OHUV-FLVORKO = YES                                               
137100         MOVE JA             TO RYES-FLVORKO                              
137200     ELSE                                                                 
137300         MOVE OHUV-FLVORKO   TO RYES-FLVORKO                              
137400     END-IF                                                               
137500     MOVE OHUV-FLFORBI       TO RYES-FLFORBI                              
137600     MOVE OHUV-FLOVRLEV      TO RYES-FLOVRLEV                             
137700     MOVE ZERO               TO RYES-KDERS                                
137800     MOVE ZERO               TO RYES-KDFRAKT                              
137900     MOVE 4010-KDORDKL       TO RYES-KDORDKL                              
138000     MOVE ZERO               TO RYES-KDTPOTYP                             
138100     MOVE ZERO               TO RYES-KVBEART                              
138200     MOVE 4010-KVSLATT       TO RYES-KVSLATT                              
138300     MOVE ZERO               TO RYES-KVQPACK-1                            
138400     MOVE SPACE              TO RYES-FILLERX7                             
138500                                RYES-FILLERX2                             
138600                                                                          
138700     MOVE W-SORTPOST TO WDG6-SORTPOST                                     
138800     .                                                                    
138900                                                                          
139000 J-UPPDATERA-WDE6 SECTION.                                                
139100     MOVE 'J-UPPDATERA-WDE6' TO WS-CURRENT-SECTION                        
139200                                                                          
139300     IF AUTOMAT-PACKNING                                                  
139400     OR 3440-IDTRANS = 'L138'                                             
139500       PERFORM IMS-14-GHU-WDE601                                          
139600       IF SEGMENT-FINNS                                                   
139700         MOVE JA  TO SEG-WDE601-SW                                        
139800       ELSE                                                               
139900         MOVE NEJ TO SEG-WDE601-SW                                        
140000       END-IF                                                             
140100     END-IF                                                               
140200                                                                          
140300     IF SEG-WDE601-FINNS                                                  
140400       COMPUTE VORD-KVORDRAD = VORD-KVORDRAD + KVORDRAD-RAEKNARE          
140500                                                                          
140600       ADD SUORDV-RAEKNARE-EXP TO VORD-SUORDV-EXP                         
140700       ADD SUORDV-RAEKNARE     TO VORD-SUORDV                             
140800                                                                          
140900       COMPUTE VORD-SUORDV-LOC       =                                    
141000                  VORD-SUORDV-LOC + SUORDV-RAEKNARE-LOC                   
141100       END-COMPUTE                                                        
141200                                                                          
141300       COMPUTE VORD-SUORDV-LOCPREL  =                                     
141400                  VORD-SUORDV-LOCPREL + SUORDV-RAEKNARE-LOCPREL           
141500       END-COMPUTE                                                        
141600                                                                          
141700*ADD-1-HEKTO BECAUSE OF ROUND UP OF WEIGHT                                
141800*WEIGHT HAS 3 DECIMALS IN ORAD-VKART BUT GETS 1 DECIMAL IN                
141900*DATA ITEM 4010-VKORDNTO                                                  
142000                                                                          
142100       COMPUTE VKORDNTO-RAEKNARE =                                        
142200               VKORDNTO-RAEKNARE + ADD-1-HEKTO                            
142300       END-COMPUTE                                                        
142400                                                                          
142500       COMPUTE VORD-VKORDNTO = VORD-VKORDNTO + VKORDNTO-RAEKNARE          
142600       END-COMPUTE                                                        
142700*                                                                         
142800*ADD-1-HEKTO BECAUSE OF WEIGHT (VKORDNTO) NEEDS TO BE MORE THAN           
142900*PART WEIGHT HAS 3 DECIMAL                                                
143000*                                                                         
143100*      COMPUTE VORD-VKORDNTO =                                            
143200*              VORD-VKORDNTO + ADD-1-HEKTO                                
143300*      END-COMPUTE                                                        
143400                                                                          
143500       COMPUTE VORD-VLORDNTO = VORD-VLORDNTO + VLORDNTO-RAEKNARE          
143600       END-COMPUTE                                                        
143700                                                                          
143800       IF 3440-IDTRANS = 'L138'                                           
143900         ADD  WS-KOLLI-VKORDBTO-KOLLI  TO VORD-VKORDBTO                   
144000       END-IF                                                             
144100                                                                          
144200       IF OHUV-FLORDSPE = 'J'                                             
144300         MOVE VORD-VKORDNTO         TO VORD-VKORDBTO                      
144400         MOVE VORD-VLORDNTO         TO VORD-VLORDBTO                      
144500       END-IF                                                             
144600                                                                          
144700       IF AUTOMAT-PACKNING                                                
144800         IF VORD-FLAUTFAK = JA                                            
144900           MOVE +3          TO VORD-KDORDSTA                              
145000         END-IF                                                           
145100                                                                          
145200         MOVE VORD-KVORDRAD         TO VORD-KVORDRAD-PACK                 
145300         IF VORD-SUORDV-EXP > ZERO                                        
145400            MOVE VORD-SUORDV-EXP    TO VORD-SUORDV-PACK                   
145500         ELSE                                                             
145600            MOVE VORD-SUORDV        TO VORD-SUORDV-PACK                   
145700         END-IF                                                           
145800         MOVE VORD-SUORDV-LOC       TO VORD-SUORDV-PACK-LOC               
145900         MOVE VORD-SUORDV-LOCPREL   TO VORD-SUORDV-PACK-LOCPREL           
146000       END-IF                                                             
146100                                                                          
146200       PERFORM IMS-15-REPL-WDE601                                         
146300                                                                          
146400       PERFORM JA-UPPDAT-WDE611-OM-AUT-PACK                               
146500                                                                          
146600                                                                          
146700     ELSE                                                                 
146800       MOVE KVORDRAD-RAEKNARE       TO VORD-KVORDRAD                      
146900       MOVE SUORDV-RAEKNARE-EXP     TO VORD-SUORDV-EXP                    
147000       MOVE SUORDV-RAEKNARE         TO VORD-SUORDV                        
147100       IF 4010-PRAVCOST > ZERO                                            
147200          MOVE 'SEK'                TO VORD-KDVALISO                      
147300          MOVE 4010-KDVALISO        TO VORD-KDVALISO-EXP                  
147400       ELSE                                                               
147500          MOVE 4010-KDVALISO        TO VORD-KDVALISO                      
147600          MOVE SPACE                TO VORD-KDVALISO-EXP                  
147700       END-IF                                                             
147800       MOVE SUORDV-RAEKNARE-LOC     TO VORD-SUORDV-LOC                    
147900       MOVE SUORDV-RAEKNARE-LOCPREL TO VORD-SUORDV-LOCPREL                
148000       MOVE VKORDNTO-RAEKNARE       TO VORD-VKORDNTO                      
148100       MOVE VLORDNTO-RAEKNARE       TO VORD-VLORDNTO                      
148200                                                                          
148300       IF OHUV-FLORDSPE = 'J'                                             
148400         MOVE VKORDNTO-RAEKNARE     TO VORD-VKORDBTO                      
148500         MOVE VLORDNTO-RAEKNARE     TO VORD-VLORDBTO                      
148600       END-IF                                                             
148700                                                                          
148800                                                                          
148900       PERFORM IMS-16-ISRT-WDE601                                         
149000     END-IF                                                               
149100     .                                                                    
149200                                                                          
149300 JA-UPPDAT-WDE611-OM-AUT-PACK SECTION.                                    
149400     MOVE 'JA-UPPDAT-WDE611-OM-AUT-PACK' TO WS-CURRENT-SECTION            
149500                                                                          
149600     IF AUTOMAT-PACKNING                                                  
149700       MOVE 4010-IDPRODNR        TO W-IDPRODNR-WDE6                       
149800*SE FD-UPPDAT-WDE6-SOM-PACKAD FÖR W-IDKOLLI-WDE6.                         
149900                                                                          
150000       PERFORM IMS-17-GHU-WDE611                                          
150100         MOVE VORD-KVORDRAD      TO KOLLI-KVORDRAD                        
150200         MOVE VORD-SUORDV        TO KOLLI-SUORDV-KOLLI                    
150300         MOVE VORD-SUORDV-EXP    TO KOLLI-SUORDV-KLI-EXP                  
150400         MOVE VORD-SUORDV-LOC    TO KOLLI-SUORDV-LOC                      
150500         MOVE VORD-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                 
150600                                                                          
150700       IF OHUV-FLORDSPE = 'J'                                             
150800         MOVE VORD-VKORDNTO      TO KOLLI-VKORDBTO-KOLLI                  
150900         MOVE VORD-VLORDNTO      TO KOLLI-VLORDBTO-KOLLI                  
151000       END-IF                                                             
151100                                                                          
151200       PERFORM IMS-18-REPL-WDE611                                         
151300                                                                          
151400     END-IF                                                               
151500     .                                                                    
151600                                                                          
151700 K-UPPDAT-WDQ3-OM-AUT-PACK SECTION.                                       
151800     MOVE 'K-UPPDAT-WDQ3-OM-AUT-PACK' TO WS-CURRENT-SECTION               
151900                                                                          
152000     IF AUTOMAT-PACKNING                                                  
152100*    -- INTE FÖR KOLLI I WEB LDC                                          
152200       PERFORM IMS-25-GHU-WDQ301                                          
152300                                                                          
152400       MOVE 'P'                TO ODEL-KDODELSTA                          
152500       MOVE VORD-KVORDRAD-PACK TO ODEL-KVPACKRAD-OD                       
152600       MOVE 3440-TIAAMMDD      TO ODEL-TIPACKN                            
152700       MOVE 3440-TIHHMMSS      TO ODEL-TIPACTID                           
152800                                                                          
152900       PERFORM IMS-26-REPL-WDQ301                                         
153000                                                                          
153100     END-IF                                                               
153200     .                                                                    
153300                                                                          
153400 L-UPPDAT-WDQ2-OM-AUT-PACK SECTION.                                       
153500     MOVE 'L-UPPDAT-WDQ2-OM-AUT-PACK' TO WS-CURRENT-SECTION               
153600                                                                          
153700     IF AUTOMAT-PACKNING                                                  
153800     OR 3440-IDTRANS = 'L138'                                             
153900        IF 3440-IDTRANS = 'L138'                                          
154000           MOVE 'U*' TO ARB-KDORDSTA                                      
154100        ELSE                                                              
154200           MOVE 'P ' TO ARB-KDORDSTA                                      
154300        END-IF                                                            
154400        PERFORM IMS-28-REPL-WDQ212                                        
154500     END-IF                                                               
154600     .                                                                    
154700                                                                          
154800 M-UPPDATERA-WDE4 SECTION.                                                
154900     MOVE 'M-UPPDATERA-WDE4 ' TO WS-CURRENT-SECTION                       
155000                                                                          
155100     PERFORM IMS-29-GHU-WDE401                                            
155200                                                                          
155300     ADD KVORDRAD-RAEKNARE       TO KORD-KVORDRAD                         
155400                                    KORD-KVORDRAD-VO                      
155500     ADD SUORDV-RAEKNARE-EXP     TO KORD-SUORDV-EXP                       
155600     ADD SUORDV-RAEKNARE         TO KORD-SUORDV                           
155700     ADD SUORDV-RAEKNARE-LOC     TO KORD-SUORDV-LOC                       
155800     ADD SUORDV-RAEKNARE-LOCPREL TO KORD-SUORDV-LOCPREL                   
155900                                                                          
156000     IF AUTOMAT-PACKNING                                                  
156100       MOVE KVORDRAD-RAEKNARE    TO KORD-KVORDRAD-PACK                    
156200     END-IF                                                               
156300                                                                          
156400     IF DATUM-SW = 'Y'                                                    
156500       ACCEPT KORD-TIBEGPAC      FROM DATE                                
156600       MOVE 'N'                  TO DATUM-SW                              
156700     END-IF                                                               
156800                                                                          
156900     ADD VKORDNTO-RAEKNARE       TO KORD-VKORDNTO                         
157000     ADD VLORDNTO-RAEKNARE       TO KORD-VLORDNTO                         
157100                                                                          
157200     PERFORM IMS-30-REPL-WDE401                                           
157300                                                                          
157400     MOVE ZERO TO KVORDRAD-RAEKNARE                                       
157500                  SUORDV-RAEKNARE                                         
157600                  SUORDV-RAEKNARE-EXP                                     
157700                  SUORDV-RAEKNARE-LOC                                     
157800                  SUORDV-RAEKNARE-LOCPREL                                 
157900                  VKORDNTO-RAEKNARE                                       
158000                  VLORDNTO-RAEKNARE                                       
158100     .                                                                    
158200                                                                          
158300 N-FLYTTA-INSERT-WDE421   SECTION.                                        
158400     MOVE 'N-FLYTTA-INSERT-WDE421'  TO WS-CURRENT-SECTION                 
158500                                                                          
158600     IF AUTOMAT-PACKNING                                                  
158700     OR 3440-IDTRANS = 'L138'                                             
158800                                                                          
158900        PERFORM NA-GET-IDKOLLI-INFO                                       
159000                                                                          
159100        MOVE ORAD-KVLEVART        TO KKOLLI-KVLEVART                      
159200        IF KKOLLI-KVLEVART = ZERO                                         
159300         MOVE 'KKOLLI-KVLEVART-WDE421' TO WS-CURRENT-SECTION              
159400          CALL ABEND                                                      
159500        END-IF                                                            
159600                                                                          
159700        IF NDC-US OR NDC-CA                                               
159800           PERFORM NB-PACK-UPPG-DEL-NOTE                                  
159900        END-IF                                                            
160000                                                                          
160100        PERFORM IMS-24-ISRT-WDE421                                        
160200                                                                          
160300        IF 3440-IDTRANS = 'L138'                                          
160400          MOVE 4010-IDPRODNR      TO 4490-IDPRODNR                        
160500          MOVE 4010-IDPLKLST      TO 4490-IDPLKLST                        
160600          MOVE 4010-IDUSER        TO 4490-IDUSER                          
160700          MOVE 4010-KDORDKL       TO 4490-KDORDKL                         
160800        END-IF                                                            
160900     ELSE                                                                 
161000        MOVE 4010-IDPRODNR        TO 4490-IDPRODNR                        
161100        MOVE 4010-IDPLKLST        TO 4490-IDPLKLST                        
161200        MOVE 4010-IDUSER          TO 4490-IDUSER                          
161300        MOVE 4010-KDORDKL         TO 4490-KDORDKL                         
161400     END-IF                                                               
161500     .                                                                    
161600                                                                          
161700 NA-GET-IDKOLLI-INFO   SECTION.                                           
161800     MOVE 'NA-GET-IDKOLLI-INFO '   TO WS-CURRENT-SECTION                  
161900                                                                          
162000     MOVE VORD-IDPRODNR            TO KKOLLI-IDPRODNR                     
162100                                                                          
162200     IF 3440-IDTRANS = 'L138'                                             
162300       MOVE WS-PRC-IDKOLLI         TO KKOLLI-IDKOLLI                      
162400     ELSE                                                                 
162500       MOVE KOLLI-IDKOLLI          TO KKOLLI-IDKOLLI                      
162600     END-IF                                                               
162700     .                                                                    
162800                                                                          
162900 NB-PACK-UPPG-DEL-NOTE SECTION.                                           
163000     MOVE 'NB-PACK-UPPG-DEL-NOTE  ' TO WS-CURRENT-SECTION                 
163100                                                                          
163200     MOVE W-IDDISTR-WDE4             TO TEST-IDDISTR                      
163300     IF DIST07-USA-RETAILER-DNOTE                                         
163400     OR DIST07-CAN-RETAILER                                               
163500                                                                          
163600     INITIALIZE DNOT-ORDER-INFO                                           
163700                                                                          
163800     MOVE 'WL0134NY'               TO DNOT-IDPGM                          
163900     MOVE 4010-IDORDER             TO DNOT-IDORDER                        
164000     MOVE 4010-IDARTNR             TO DNOT-IDARTNR                        
164100     MOVE 4010-IDDC                TO DNOT-IDDC                           
164200     MOVE 4010-KVBEART-Q           TO DNOT-KVBEART                        
164300     MOVE 4010-KVBEART-Q           TO DNOT-KVBEART-Q                      
164400     MOVE 4010-FLTILLK             TO DNOT-FLTILLK                        
164500     MOVE 4010-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                    
164600     MOVE 4010-IDPURAD             TO DNOT-IDPURAD                        
164700     MOVE KKOLLI-IDKOLLI           TO DNOT-IDKOLLI                        
164800     MOVE KKOLLI-IDPRODNR          TO DNOT-IDPRODNR                       
164900     MOVE KKOLLI-KVLEVART          TO DNOT-KVLEVART                       
165000                                                                          
165100     CALL W411DNOT USING DNOT-W411DNOT                                    
165200                         DNOT-ORQP-PCB                                    
165300                         DNOT-ORQP2-PCB                                   
165400                         DNOT-ORQP3-PCB                                   
165500                         DNOT-4013-PCB                                    
165600                         DNOT-BENA-PCB                                    
165700     END-IF                                                               
165800     .                                                                    
165900     EJECT                                                                
166000                                                                          
166100 O-UPPDAT-WDE611    SECTION.                                              
166200     MOVE 'O-UPPDAT-WDE611'       TO WS-CURRENT-SECTION                   
166300                                                                          
166400     IF 3440-IDTRANS = 'L138'                                             
166500       MOVE WS-SPAR-IDORDER-WDQ3  TO W-IDORDER-WDQ3                       
166600       MOVE WS-SPAR-IDDC-WDQ3     TO W-IDDC-WDQ3                          
166700       MOVE WS-SPAR-IDPRODNR-WDQ3 TO W-IDPRODNR-WDQ3                      
166800       MOVE WS-SPAR-IDPLKLST-WDQ3 TO W-IDPLKLST-WDQ3                      
166900       PERFORM IMS-25-GHU-WDQ301                                          
167000                                                                          
167100       MOVE WS-SPAR-IDPRODNR-WDQ3 TO W-IDPRODNR-WDE6                      
167200       MOVE WS-PRC-IDKOLLI        TO W-IDKOLLI-WDE6                       
167300       PERFORM IMS-14-GHU-WDE601                                          
167400       PERFORM IMS-34-GHU-WDE611                                          
167500*                                                                         
167600       PERFORM IMS-35-GU-WDQ301                                           
167700       IF KOLLI-KVORDRAD > 0                                              
167800       AND KOLLI-KDKOLSTA = +0                                            
167900* KOLLIT/ORDERDELEN ÄR UTSKRIVEN MEN INTE PACKNINGSRAPPORTERAD.           
168000         COMPUTE KOLLI-KVORDRAD =                                         
168100              ODEL-KVRADER      + KOLLI-KVORDRAD                          
168200         END-COMPUTE                                                      
168300*        COMPUTE KOLLI-VLORDBTO-KOLLI =                                   
168400*             ODEL-VLORDNTO     + KOLLI-VLORDBTO-KOLLI                    
168500*        END-COMPUTE                                                      
168600*        COMPUTE KOLLI-VKORDNTO-KOLLI =                                   
168700*             ODEL-VKORDNTO     + KOLLI-VKORDNTO-KOLLI                    
168800*        END-COMPUTE                                                      
168900       ELSE                                                               
169000* ETT NYTT KOLLIT LÄGGS UPP.                                              
169100         MOVE ODEL-KVRADER        TO KOLLI-KVORDRAD                       
169200         MOVE ODEL-VLORDNTO       TO KOLLI-VLORDBTO-KOLLI                 
169300         MOVE ODEL-VKORDNTO       TO KOLLI-VKORDNTO-KOLLI                 
169400*WEIGHT                                                                   
169500*        COMPUTE KOLLI-VKORDNTO-KOLLI =                                   
169600*                KOLLI-VKORDNTO-KOLLI + ADD-1-HEKTO                       
169700*        END-COMPUTE                                                      
169800                                                                          
169900       END-IF                                                             
170000       MOVE +0                    TO WS-ORAD-RAKNARE                      
170100                                                                          
170200       PERFORM IMS-18-REPL-WDE611                                         
170300     END-IF                                                               
170400     .                                                                    
170500 P-FLYTTA-DATA-TILL-DB  SECTION.                                          
170600     MOVE 'P-FLYTTA-DATA-TILL-DB'  TO WS-CURRENT-SECTION                  
170700                                                                          
170800     PERFORM FC-KOLLA-OM-WDE601-FINNS                                     
170900                                                                          
171000     PERFORM FD-UPPDAT-WDE6-SOM-PACKAD                                    
171100     .                                                                    
171200 S01-DATA-TILL-DEL-NOTE SECTION.                                          
171300     MOVE 'S01-DATA-TILL-DEL-NOTE  ' TO WS-CURRENT-SECTION                
171400                                                                          
171500     MOVE OHUV-IDDISTR               TO TEST-IDDISTR                      
171600     IF DIST07-USA-RETAILER-DNOTE                                         
171700     OR DIST07-CAN-RETAILER                                               
171800                                                                          
171900        IF (ORAD-IDKUNDRF-RO(1:5) NOT = ZERO)                             
172000           PERFORM S01A-KOLLA-BEKUNDRF                                    
172100           IF BEKUNDRF-OK                                                 
172200              INITIALIZE DNOT-ORDER-INFO                                  
172300                                                                          
172400              MOVE IDPGM                 TO DNOT-IDPGM                    
172500              MOVE KORD-IDORDER          TO DNOT-IDORDER                  
172600              MOVE ORAD-IDARTNR          TO DNOT-IDARTNR                  
172700              MOVE KORD-IDDC             TO DNOT-IDDC                     
172800              MOVE OHUV-ADGMT            TO DNOT-ADGMT                    
172900              MOVE OHUV-BEGMT            TO DNOT-BEGMT                    
173000              MOVE OHUV-BEKUNDRF         TO DNOT-BEKUNDRF                 
173100              MOVE ORAD-BERADREF         TO DNOT-BERADREF                 
173200              MOVE KORD-IDDISTR          TO DNOT-IDDISTR                  
173300              MOVE KORD-IDKUNDNR         TO DNOT-IDKUNDNR                 
173400              MOVE KORD-IDORDNR5         TO DNOT-IDORDNR7                 
173500              MOVE OHUV-IDDC-PRIM        TO DNOT-IDDC-PRIM                
173600              MOVE SPACE                 TO DNOT-IDKUNDRF-RO              
173700              MOVE '00'                  TO DNOT-IDKUNDRF-RO(1:2)         
173800              MOVE ORAD-IDKUNDRF-RO      TO DNOT-IDKUNDRF-RO(3:5)         
173900              MOVE ORAD-FLDIRLEV         TO DNOT-FLDIRLEV                 
174000              MOVE ORAD-FLTILLK          TO DNOT-FLTILLK                  
174100              MOVE KORD-KDORDKL          TO DNOT-KDORDKL                  
174200              MOVE KORD-KDFRAKT          TO DNOT-KDFRAKT                  
174300              MOVE ORAD-KVBEART          TO DNOT-KVBEART                  
174400                                               DNOT-KVBEART-Q             
174500              MOVE ORAD-REKSIFFR         TO DNOT-REKSIFFR                 
174600              MOVE OHUV-TIREGDAT         TO DNOT-TIREGDAT                 
174700              MOVE OHUV-TIREGTID         TO DNOT-TIREGTID                 
174800                                                                          
174900                                                                          
175000              CALL W411DNOT USING DNOT-W411DNOT                           
175100                                  DNOT-ORQP-PCB                           
175200                                  DNOT-ORQP2-PCB                          
175300                                  DNOT-ORQP3-PCB                          
175400                                  DNOT-4013-PCB                           
175500                                  DNOT-BENA-PCB                           
175600           END-IF                                                         
175700        END-IF                                                            
175800     END-IF                                                               
175900     .                                                                    
176000     EJECT                                                                
176100                                                                          
176200 S01A-KOLLA-BEKUNDRF SECTION.                                             
176300     MOVE 'S01A-KOLLA-BEKUNDRF     ' TO WS-CURRENT-SECTION                
176400                                                                          
176500     MOVE JA                    TO BEKUNDRF-SW                            
176600     MOVE OHUV-IDDISTR          TO W-IDDISTR-CSEQ                         
176700     MOVE OHUV-IDKUNDNR         TO W-IDKUNDNR-CSEQ                        
176800     MOVE ORAD-IDKUNDRF-RO(1:5) TO W-IDORDNR5-CSEQ                        
176900                                                                          
177000     PERFORM IMS-GU-WDQ201-CSEQ                                           
177100     IF OC-OHUV-BEKUNDRF(1:2) = 'OC'                                      
177200*    ORDERHUVUDET ÄR ETT TOMHUVUD FÖR ORDERKONSOLIDERING                  
177300*    NÅGON EGENTLIG BIPACKNING ÄR DET INTE TAL OM I DET HÄR FALLET        
177400*    DÄRFÖR HELLER INGET ANROP PÅ W411DNOT                                
177500                                                                          
177600        MOVE NEJ TO BEKUNDRF-SW                                           
177700     END-IF                                                               
177800     .                                                                    
177900     EJECT                                                                
178000                                                                          
178100 IMS-01-GHU-WDGX4017 SECTION.                                             
178200     MOVE 'IMS-01'   TO WS-CURRENT-IMS-SECTION                            
178300                                                                          
178400     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
178500          DELIMITED BY SIZE INTO SSA1                                     
178600     MOVE '  GE'              TO GODK-STATUSKODER                         
178700     CALL CBLTDLI USING GHU 4017-PCB DLI-IO-WDGX4017 SSA1                 
178800     MOVE 4017-STATUS-CODE    TO STATUS-WS                                
178900     PERFORM IMS-STATUSKONTROLL                                           
179000     .                                                                    
179100                                                                          
179200 IMS-02-DLET-WDGX4017 SECTION.                                            
179300     MOVE 'IMS-02'   TO WS-CURRENT-IMS-SECTION                            
179400                                                                          
179500     MOVE '    '           TO GODK-STATUSKODER                            
179600     CALL CBLTDLI USING DLET 4017-PCB DLI-IO-WDGX4017                     
179700     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
179800     PERFORM IMS-STATUSKONTROLL                                           
179900     .                                                                    
180000                                                                          
180100 IMS-03-GU-WDGX4008 SECTION.                                              
180200     MOVE 'IMS-03'   TO WS-CURRENT-IMS-SECTION                            
180300                                                                          
180400     STRING 'WL400701(WDGXKEY  =' W-4007-IDHTYP-X ')'                     
180500          DELIMITED BY SIZE INTO SSA1                                     
180600     MOVE 'WL400711 '         TO SSA2                                     
180700     MOVE '  '                TO GODK-STATUSKODER                         
180800     CALL CBLTDLI USING GU 4007-PCB DLI-IO-WDGX4008 SSA1 SSA2             
180900     MOVE 4007-STATUS-CODE    TO STATUS-WS                                
181000     PERFORM IMS-STATUSKONTROLL                                           
181100     .                                                                    
181200                                                                          
181300 IMS-04-GHNP-WDGX4010-FIRST SECTION.                                      
181400     MOVE 'IMS-04'   TO WS-CURRENT-IMS-SECTION                            
181500                                                                          
181600     MOVE 'WL400721*F ' TO SSA1                                           
181700     MOVE '  '             TO GODK-STATUSKODER                            
181800     CALL CBLTDLI USING GHNP 4007-PCB DLI-IO-WDGX4010 SSA1                
181900     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
182000     PERFORM IMS-STATUSKONTROLL                                           
182100     .                                                                    
182200                                                                          
182300 IMS-05-DLET-WDGX4010 SECTION.                                            
182400     MOVE 'IMS-05'   TO WS-CURRENT-IMS-SECTION                            
182500                                                                          
182600     MOVE '    '           TO GODK-STATUSKODER                            
182700     CALL CBLTDLI USING DLET 4007-PCB DLI-IO-WDGX4010                     
182800     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
182900     PERFORM IMS-STATUSKONTROLL                                           
183000     .                                                                    
183100                                                                          
183200 IMS-06-GHNP-WDGX4010 SECTION.                                            
183300     MOVE 'IMS-06'   TO WS-CURRENT-IMS-SECTION                            
183400                                                                          
183500     MOVE 'WL400721 ' TO SSA1                                             
183600     MOVE '  GE'           TO GODK-STATUSKODER                            
183700     CALL CBLTDLI USING GHNP 4007-PCB DLI-IO-WDGX4010 SSA1                
183800     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
183900     PERFORM IMS-STATUSKONTROLL                                           
184000     .                                                                    
184100                                                                          
184200 IMS-07-GU-WDGX4448 SECTION.                                              
184300     MOVE 'IMS-07'   TO WS-CURRENT-IMS-SECTION                            
184400                                                                          
184500     STRING 'WLXXKH01(WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
184600          DELIMITED BY SIZE INTO SSA1                                     
184700     STRING 'WLXXKH11(WDGXKEY  =' W-WDGXKEY-4448-X ')'                    
184800          DELIMITED BY SIZE INTO SSA2                                     
184900     MOVE '  '                TO GODK-STATUSKODER                         
185000     CALL CBLTDLI USING GU 4447-PCB DLI-IO-WDGX4448 SSA1 SSA2             
185100     MOVE 4447-STATUS-CODE    TO STATUS-WS                                
185200     PERFORM IMS-STATUSKONTROLL                                           
185300     .                                                                    
185400                                                                          
185500 IMS-07-GU-WDGX4487 SECTION.                                              
185600     MOVE 'IMS-07'   TO WS-CURRENT-IMS-SECTION                            
185700                                                                          
185800     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4487-X ')'                    
185900          DELIMITED BY SIZE INTO SSA1                                     
186000     MOVE '  GE'              TO GODK-STATUSKODER                         
186100     CALL CBLTDLI USING GU   4487-PCB DLI-IO-WDGX4487 SSA1                
186200     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
186300     PERFORM IMS-STATUSKONTROLL                                           
186400     .                                                                    
186500                                                                          
186600 IMS-08-ISRT-WDGX4487 SECTION.                                            
186700     MOVE 'IMS-08'   TO WS-CURRENT-IMS-SECTION                            
186800                                                                          
186900     MOVE 'WDR401   '      TO SSA1                                        
187000     MOVE '  '             TO GODK-STATUSKODER                            
187100     CALL CBLTDLI USING ISRT 4487-PCB DLI-IO-WDGX4487 SSA1                
187200     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500                                                                          
187600 IMS-09-ISRT-WDGX4488  SECTION.                                           
187700     MOVE 'IMS-09'   TO WS-CURRENT-IMS-SECTION                            
187800                                                                          
187900     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4487-X ')'                    
188000          DELIMITED BY SIZE INTO SSA1                                     
188100     MOVE 'WDGX4488 '         TO SSA2                                     
188200     MOVE '  '                TO GODK-STATUSKODER                         
188300     CALL CBLTDLI USING ISRT 4487-PCB DLI-IO-WDGX4488 SSA1 SSA2           
188400     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
188500     PERFORM IMS-STATUSKONTROLL                                           
188600     .                                                                    
188700                                                                          
188800 IMS-10-GNP-WDGX4488  SECTION.                                            
188900     MOVE 'IMS-10'   TO WS-CURRENT-IMS-SECTION                            
189000                                                                          
189100     STRING 'WDGX4488(KDPRCGRP =' W-WDGXKEY-4488-X ')'                    
189200          DELIMITED BY SIZE INTO SSA1                                     
189300     MOVE '  GE'              TO GODK-STATUSKODER                         
189400     CALL CBLTDLI USING GNP  4487-PCB DLI-IO-WDGX4488 SSA1                
189500     MOVE 4487-STATUS-CODE    TO STATUS-WS                                
189600     PERFORM IMS-STATUSKONTROLL                                           
189700     .                                                                    
189800                                                                          
189900 IMS-GU-WDQ201-CSEQ SECTION.                                              
190000     MOVE 'IMS-GU-WDQ2-CSEQ'   TO WS-CURRENT-IMS-SECTION                  
190100                                                                          
190200     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                          
190300          DELIMITED BY SIZE INTO SSA1                                     
190400     MOVE '  ' TO GODK-STATUSKODER                                        
190500     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-AREA-WDQ201 SSA1              
190600     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
190700     PERFORM IMS-STATUSKONTROLL                                           
190800     .                                                                    
190900     EJECT                                                                
191000 IMS-11-GU-WDQ201   SECTION.                                              
191100     MOVE 'IMS-11'   TO WS-CURRENT-IMS-SECTION                            
191200                                                                          
191300     STRING 'WDQ201  (IDORDER  =' W-WDQ201-IDORDER-X ')'                  
191400          DELIMITED BY SIZE INTO SSA1                                     
191500     MOVE '  '                TO GODK-STATUSKODER                         
191600     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
191700     MOVE WDQ2-STATUS-CODE   TO STATUS-WS                                 
191800     PERFORM IMS-STATUSKONTROLL                                           
191900     .                                                                    
192000                                                                          
192100                                                                          
192200 IMS-12-GHNP-WDQ212 SECTION.                                              
192300     MOVE 'IMS-12'   TO WS-CURRENT-IMS-SECTION                            
192400                                                                          
192500     STRING 'WDQ212  (IDDC     =' W-WDQ212-IDDC-X ')'                     
192600          DELIMITED BY SIZE INTO SSA1                                     
192700     MOVE '  '                TO GODK-STATUSKODER                         
192800     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-WDQ212 SSA1                  
192900     MOVE WDQ2-STATUS-CODE   TO STATUS-WS                                 
193000     PERFORM IMS-STATUSKONTROLL                                           
193100     .                                                                    
193200                                                                          
193300 IMS-13-ISRT-WDE401 SECTION.                                              
193400     MOVE 'IMS-13'   TO WS-CURRENT-IMS-SECTION                            
193500                                                                          
193600     MOVE 'WDE401   '      TO SSA1                                        
193700     MOVE '  II'           TO GODK-STATUSKODER                            
193800     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE401 SSA1                  
193900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
194000     PERFORM IMS-STATUSKONTROLL                                           
194100     .                                                                    
194200                                                                          
194300 IMS-14-GHU-WDE601 SECTION.                                               
194400     MOVE 'IMS-14'   TO WS-CURRENT-IMS-SECTION                            
194500                                                                          
194600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
194700          DELIMITED BY SIZE INTO SSA1                                     
194800     MOVE '  GE'              TO GODK-STATUSKODER                         
194900     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
195000     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
195100     PERFORM IMS-STATUSKONTROLL                                           
195200     .                                                                    
195300                                                                          
195400 IMS-15-REPL-WDE601 SECTION.                                              
195500     MOVE 'IMS-15'   TO WS-CURRENT-IMS-SECTION                            
195600                                                                          
195700     MOVE '  '             TO GODK-STATUSKODER                            
195800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
195900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
196000     PERFORM IMS-STATUSKONTROLL                                           
196100     .                                                                    
196200                                                                          
196300 IMS-16-ISRT-WDE601 SECTION.                                              
196400     MOVE 'IMS-16'   TO WS-CURRENT-IMS-SECTION                            
196500                                                                          
196600     MOVE 'WDE601   '      TO SSA1                                        
196700     MOVE '  '             TO GODK-STATUSKODER                            
196800     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE601 SSA1                  
196900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
197000     PERFORM IMS-STATUSKONTROLL                                           
197100     .                                                                    
197200                                                                          
197300 IMS-17-GHU-WDE611 SECTION.                                               
197400     MOVE 'IMS-17'   TO WS-CURRENT-IMS-SECTION                            
197500                                                                          
197600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
197700          DELIMITED BY SIZE INTO SSA1                                     
197800     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
197900          DELIMITED BY SIZE INTO SSA2                                     
198000     MOVE '  GE'              TO GODK-STATUSKODER                         
198100     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
198200     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
198300     PERFORM IMS-STATUSKONTROLL                                           
198400     .                                                                    
198500                                                                          
198600 IMS-18-REPL-WDE611 SECTION.                                              
198700     MOVE 'IMS-18'   TO WS-CURRENT-IMS-SECTION                            
198800                                                                          
198900     MOVE '  '             TO GODK-STATUSKODER                            
199000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
199100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
199200     PERFORM IMS-STATUSKONTROLL                                           
199300     .                                                                    
199400                                                                          
199500 IMS-19-ISRT-WDE611 SECTION.                                              
199600     MOVE 'IMS-19'   TO WS-CURRENT-IMS-SECTION                            
199700                                                                          
199800     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
199900          DELIMITED BY SIZE INTO SSA1                                     
200000     MOVE 'WDE611   '         TO SSA2                                     
200100     MOVE '  '                TO GODK-STATUSKODER                         
200200     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE611 SSA1 SSA2             
200300     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
200400     PERFORM IMS-STATUSKONTROLL                                           
200500     .                                                                    
200600                                                                          
200700 IMS-20-ISRT-WDGX4726 SECTION.                                            
200800     MOVE 'IMS-20'   TO WS-CURRENT-IMS-SECTION                            
200900                                                                          
201000     STRING 'WLXXDV01(WDGXKEY  =' W-WDGXKEY-4726-ROT-X ')'                
201100          DELIMITED BY SIZE INTO SSA1                                     
201200     MOVE 'WLXXDV11 '         TO SSA2                                     
201300     MOVE '  II'              TO GODK-STATUSKODER                         
201400     CALL CBLTDLI USING ISRT 4726-PCB DLI-IO-WDGX4726 SSA1 SSA2           
201500     MOVE 4726-STATUS-CODE    TO STATUS-WS                                
201600     PERFORM IMS-STATUSKONTROLL                                           
201700     .                                                                    
201800                                                                          
201900 IMS-21-ISRT-WDGX4727 SECTION.                                            
202000     MOVE 'IMS-21'   TO WS-CURRENT-IMS-SECTION                            
202100                                                                          
202200     STRING 'WLXXDV01(WDGXKEY  =' W-WDGXKEY-4726-ROT-X ')'                
202300          DELIMITED BY SIZE INTO SSA1                                     
202400     STRING 'WLXXDV11(WDGXKEY  =' W-WDGXKEY-4726-X ')'                    
202500          DELIMITED BY SIZE INTO SSA2                                     
202600     MOVE 'WLXXDV21 '         TO SSA3                                     
202700     MOVE '  II' TO GODK-STATUSKODER                                      
202800     CALL CBLTDLI USING ISRT 4726-PCB DLI-IO-WDGX4727 SSA1 SSA2           
202900                                                      SSA3                
203000     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
203100     PERFORM IMS-STATUSKONTROLL                                           
203200     .                                                                    
203300                                                                          
203400 IMS-22-ISRT-WDE411 SECTION.                                              
203500     MOVE 'IMS-22'   TO WS-CURRENT-IMS-SECTION                            
203600                                                                          
203700     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
203800          DELIMITED BY SIZE INTO SSA1                                     
203900     MOVE 'WDE411   '         TO SSA2                                     
204000     MOVE '  '                TO GODK-STATUSKODER                         
204100     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE411 SSA1 SSA2             
204200     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
204300     PERFORM IMS-STATUSKONTROLL                                           
204400     .                                                                    
204500                                                                          
204600 IMS-23-ISRT-WDG601 SECTION.                                              
204700     MOVE 'IMS-23'   TO WS-CURRENT-IMS-SECTION                            
204800                                                                          
204900     MOVE 'WDG601   '      TO SSA1                                        
205000     MOVE '  II'           TO GODK-STATUSKODER                            
205100     CALL CBLTDLI USING ISRT WDG6-PCB DLI-IO-WDG601 SSA1                  
205200     MOVE WDG6-STATUS-CODE TO STATUS-WS                                   
205300     PERFORM IMS-STATUSKONTROLL                                           
205400     .                                                                    
205500                                                                          
205600 IMS-24-ISRT-WDE421 SECTION.                                              
205700     MOVE 'IMS-24'   TO WS-CURRENT-IMS-SECTION                            
205800                                                                          
205900     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
206000          DELIMITED BY SIZE INTO SSA1                                     
206100     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
206200          DELIMITED BY SIZE INTO SSA2                                     
206300     MOVE 'WDE421   '         TO SSA3                                     
206400     MOVE '  '                TO GODK-STATUSKODER                         
206500     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE421 SSA1 SSA2             
206600                                                    SSA3                  
206700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
206800     PERFORM IMS-STATUSKONTROLL                                           
206900     .                                                                    
207000                                                                          
207100 IMS-25-GHU-WDQ301 SECTION.                                               
207200     MOVE 'IMS-25'   TO WS-CURRENT-IMS-SECTION                            
207300                                                                          
207400     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
207500          DELIMITED BY SIZE INTO SSA1                                     
207600     MOVE '  '                TO GODK-STATUSKODER                         
207700     CALL CBLTDLI USING GHU WDQ3-PCB DLI-IO-WDQ301 SSA1                   
207800     MOVE WDQ3-STATUS-CODE    TO STATUS-WS                                
207900     PERFORM IMS-STATUSKONTROLL                                           
208000     .                                                                    
208100                                                                          
208200 IMS-26-REPL-WDQ301 SECTION.                                              
208300     MOVE 'IMS-26'   TO WS-CURRENT-IMS-SECTION                            
208400                                                                          
208500     MOVE '  '             TO GODK-STATUSKODER                            
208600     CALL CBLTDLI USING REPL WDQ3-PCB DLI-IO-WDQ301                       
208700     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
208800     PERFORM IMS-STATUSKONTROLL                                           
208900     .                                                                    
209000                                                                          
209100                                                                          
209200 IMS-28-REPL-WDQ212 SECTION.                                              
209300     MOVE 'IMS-28'   TO WS-CURRENT-IMS-SECTION                            
209400                                                                          
209500     MOVE '  '              TO GODK-STATUSKODER                           
209600     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ212                       
209700     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
209800     PERFORM IMS-STATUSKONTROLL                                           
209900     .                                                                    
210000                                                                          
210100 IMS-29-GHU-WDE401 SECTION.                                               
210200     MOVE 'IMS-29'   TO WS-CURRENT-IMS-SECTION                            
210300                                                                          
210400     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
210500          DELIMITED BY SIZE INTO SSA1                                     
210600     MOVE '  '                TO GODK-STATUSKODER                         
210700     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE401 SSA1                   
210800     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100                                                                          
211200 IMS-30-REPL-WDE401 SECTION.                                              
211300     MOVE 'IMS-30'   TO WS-CURRENT-IMS-SECTION                            
211400                                                                          
211500     MOVE '  '             TO GODK-STATUSKODER                            
211600     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE401                       
211700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
211800     PERFORM IMS-STATUSKONTROLL                                           
211900     .                                                                    
212000                                                                          
212100 IMS-31-ISRT-WDGX4490 SECTION.                                            
212200     MOVE 'IMS-31'   TO WS-CURRENT-IMS-SECTION                            
212300                                                                          
212400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4487-X ')'                    
212500          DELIMITED BY SIZE INTO SSA1                                     
212600     STRING 'WDGX4488(KDPRCGRP =' W-WDGXKEY-4488-X ')'                    
212700          DELIMITED BY SIZE INTO SSA2                                     
212800     MOVE 'WDGX4490 '         TO SSA3                                     
212900     MOVE '  II' TO GODK-STATUSKODER                                      
213000     CALL CBLTDLI USING ISRT 4487-PCB DLI-IO-WDGX4490 SSA1 SSA2           
213100                                                      SSA3                
213200     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
213300     PERFORM IMS-STATUSKONTROLL                                           
213400     .                                                                    
213500                                                                          
213600 IMS-32-GU-WDB601    SECTION.                                             
213700     MOVE 'IMS-08'     TO WS-CURRENT-IMS-SECTION                          
213800                                                                          
213900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
214000          DELIMITED BY SIZE INTO SSA1                                     
214100     MOVE '  ' TO GODK-STATUSKODER                                        
214200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
214300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
214400     PERFORM IMS-STATUSKONTROLL                                           
214500     .                                                                    
214600                                                                          
214700 IMS-33-GU-WDB612 SECTION.                                                
214800     MOVE 'IMS-GU-WDB612'    TO WS-CURRENT-IMS-SECTION                    
214900                                                                          
215000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X  ')'                        
215100          DELIMITED BY SIZE INTO SSA1                                     
215200     STRING 'WDB612  (IDPRC    =' W-IDPRC-B6-X ')'                        
215300          DELIMITED BY SIZE INTO SSA2                                     
215400     MOVE '  GE'             TO GODK-STATUSKODER                          
215500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB612 SSA1 SSA2               
215600     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
215700     PERFORM IMS-STATUSKONTROLL                                           
215800     .                                                                    
215900     EJECT                                                                
216000                                                                          
216100 IMS-34-GHU-WDE611 SECTION.                                               
216200     MOVE 'IMS-34'   TO WS-CURRENT-IMS-SECTION                            
216300                                                                          
216400*    STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
216500*         DELIMITED BY SIZE INTO SSA1                                     
216600     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
216700          DELIMITED BY SIZE INTO SSA1                                     
216800     MOVE '  '              TO GODK-STATUSKODER                           
216900     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1                   
217000     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
217100     PERFORM IMS-STATUSKONTROLL                                           
217200     .                                                                    
217300 IMS-35-GU-WDQ301 SECTION.                                                
217400     MOVE 'IMS-25'   TO WS-CURRENT-IMS-SECTION                            
217500                                                                          
217600     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
217700          DELIMITED BY SIZE INTO SSA1                                     
217800     MOVE '  '                TO GODK-STATUSKODER                         
217900     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
218000     MOVE WDQ3-STATUS-CODE    TO STATUS-WS                                
218100     PERFORM IMS-STATUSKONTROLL                                           
218200     .                                                                    
218300                                                                          
218400 IMS-36-ISRT-WDE611 SECTION.                                              
218500     MOVE 'IMS-36'   TO WS-CURRENT-IMS-SECTION                            
218600                                                                          
218700     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
218800          DELIMITED BY SIZE INTO SSA1                                     
218900     MOVE 'WDE611   '         TO SSA2                                     
219000     MOVE '  II'              TO GODK-STATUSKODER                         
219100     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE611 SSA1 SSA2             
219200     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
219300     PERFORM IMS-STATUSKONTROLL                                           
219400     .                                                                    
219500                                                                          
219510 IMS-ISRT-WDE621 SECTION.                                                 
219520                                                                          
219530     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
219540          DELIMITED BY SIZE INTO SSA1                                     
219550     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
219560          DELIMITED BY SIZE INTO SSA2                                     
219570     MOVE 'WDE621 ' TO SSA3                                               
219571     MOVE '  II'              TO GODK-STATUSKODER                         
219590     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
219591     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
219592     PERFORM IMS-STATUSKONTROLL                                           
219594     .                                                                    
219595     EJECT                                                                
219600 IMS-GU-WDK5     SECTION.                                                 
219700     MOVE 'IMS-GU-WDK5'   TO WS-CURRENT-IMS-SECTION                       
219800                                                                          
219900     STRING 'WDK501  (KDKOLLI  =' W-WDK501KY-X ')'                        
220000            DELIMITED BY SIZE INTO SSA1                                   
220100     MOVE '  GE'              TO GODK-STATUSKODER                         
220200     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
220300     MOVE WDK5-STATUS-CODE TO WDK5-STATUS-WS                              
220400     PERFORM IMS-STATUSKONTROLL                                           
220500     .                                                                    
220600     SKIP2                                                                
220700                                                                          
220800 IMS-STATUSKONTROLL SECTION.                                              
220900                                                                          
221000     SET STATUS-IX TO 1                                                   
221100     SEARCH GODK-STATUS                                                   
221200       AT END CALL FELLOG                                                 
221300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
221400     END-SEARCH                                                           
221500     .                                                                    
